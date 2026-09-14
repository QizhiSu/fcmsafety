# =============================================================================
# SQLite 库的底座：连接 / 建表 / 迁移 / 装载
#
# 本文件是所有数据库操作的**入口层**，别的模块一律通过
# get_db_connection() 拿连接，不自己 DBI::dbConnect()。
#
# 三种使用场景，对应三组函数：
#   日常用      get_db_connection() / .resolve_db_path()   拿连接、定位库文件
#   首次建库    initialize_database() / migrate_xlsx_to_sqlite()
#               import_xlsx() / resolve_xlsx_mapping() / match_col() / norm_col_name()
#               —— 从 inst/ 下的 xlsx 把数据搬进 SQLite
#               process_raw_for_migration()
#               —— 旧线"把库读进全局环境"的兼容路径，新代码不要用
#
# 库文件定位规则（.resolve_db_path）：优先显式 db_path 参数；否则用当前
# 工作目录下的 inst/fcmsafety.db。所以**必须在项目根目录跑**，换目录会拿到
# 另一个库（或新建一个空库）——排查"查不到数据"时先确认这一条。
#
# 本文件不负责：增量比对（见 incremental_update.R）、各法规源抓取
# （见 update_other_dbs.R / auto_update_svhc.R）、毒性定级
# （见 direct_sql_toxicity.R）。
# =============================================================================

#' SQLite Database Manager for FCMSafety
#'
#' This module provides comprehensive SQLite database management functionality
#' to replace the xlsx-based database system with enhanced performance,
#' update tracking, and audit capabilities.
#'
#' Creates or returns a connection to the FCMSafety SQLite database.
#' The database is created in the inst/ directory for local development
#' or in a user data directory for installed packages.
#'
#' @importFrom DBI dbConnect dbDisconnect dbExecute dbGetQuery dbWriteTable dbExistsTable
#' @importFrom RSQLite SQLite
#' @importFrom dplyr filter mutate rename select
#' @importFrom stringr str_replace str_remove_all
#' @importFrom rio import
#' @param db_path Optional custom path to database file
#' @return DBI connection object
#' @export
#' @export
get_db_connection <- function(db_path = NULL) {
  if (is.null(db_path)) {
    db_path <- .resolve_db_path(NULL)
    parent <- dirname(db_path)
    if (!dir.exists(parent)) {
      dir.create(parent, recursive = TRUE)
    }
  }

  # Create connection
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

  # Enable foreign key constraints
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON")

  return(con)
}

# ---- 连接与库文件定位 -------------------------------------------------------

#' Resolve which SQLite file a call will actually use
#'
#' `get_db_connection()` picks the database from the working directory: if
#' `inst/` exists next to the current directory it uses that development copy,
#' otherwise the copy under the user data directory. That makes a run depend on
#' where it was started from, and two runs can silently read two different
#' databases. This helper exposes the same rule so the choice can be reported
#' (see the `database` rows of the Summary sheet) instead of guessed at.
#'
#' @param db_path Explicit path, or NULL to apply the default rule
#' @return Character path (the file itself is not checked for existence)
#' @keywords internal
#' @export
.resolve_db_path <- function(db_path = NULL) {
  if (!is.null(db_path)) return(db_path)
  if (dir.exists(file.path(getwd(), "inst"))) {
    return(file.path(getwd(), "inst", "fcmsafety.db"))
  }
  file.path(tools::R_user_dir("fcmsafety", which = "data"), "fcmsafety.db")
}

# ---- 建库：加载 schema、建表 ------------------------------------------------

#' Locate Schema File
#'
#' Finds the fcmsafety_schema.sql file either in the installed package
#' or in the local inst/ directory.
#'
#' @return Path to the schema file
#' @keywords internal
#' @export
find_schema_file <- function() {
  schema_path <- system.file("fcmsafety_schema.sql", package = "fcmsafety")
  if (!file.exists(schema_path)) {
    # Try local development path
    schema_path <- file.path(getwd(), "inst", "fcmsafety_schema.sql")
  }
  if (!file.exists(schema_path)) {
    stop("Schema file not found. Please ensure fcmsafety_schema.sql exists.")
  }
  return(schema_path)
}

#' Split SQL Schema into Individual Statements
#'
#' Splits a schema SQL string into individual statements, correctly handling
#' BEGIN...END blocks (e.g. triggers) so semicolons inside trigger bodies
#' do not cause premature splitting.
#'
#' @param schema_sql Character string containing the full schema
#' @return Character vector of individual SQL statements
#' @keywords internal
#' @export
split_sql_statements <- function(schema_sql) {
  lines <- strsplit(schema_sql, "\n", fixed = TRUE)[[1]]
  # Remove comment lines and empty lines
  lines <- lines[!grepl("^\\s*--", lines)]
  sql <- paste(lines, collapse = "\n")

  chars <- strsplit(sql, "")[[1]]
  n <- length(chars)
  depth <- 0L
  in_string <- FALSE
  statements <- character(0)
  start <- 1L
  i <- 1L

  while (i <= n) {
    ch <- chars[i]
    if (in_string) {
      if (ch == "'") in_string <- FALSE
      i <- i + 1L
      next
    }
    if (ch == "'") {
      in_string <- TRUE
      i <- i + 1L
      next
    }
    # Detect BEGIN keyword (word boundary)
    if (i + 4L <= n && substr(sql, i, i + 4L) == "BEGIN" &&
        (i + 5L > n || !grepl("[A-Za-z]", substr(sql, i + 5L, i + 5L)))) {
      depth <- depth + 1L
      i <- i + 5L
      next
    }
    # Detect END keyword (word boundary)
    if (i + 2L <= n && substr(sql, i, i + 2L) == "END" &&
        (i + 3L > n || !grepl("[A-Za-z]", substr(sql, i + 3L, i + 3L)))) {
      depth <- max(0L, depth - 1L)
      i <- i + 3L
      next
    }
    if (ch == ";" && depth == 0L) {
      stmt <- trimws(substr(sql, start, i - 1L))
      if (nchar(stmt) > 0) {
        statements <- c(statements, stmt)
      }
      start <- i + 1L
    }
    i <- i + 1L
  }

  stmt <- trimws(substr(sql, start, n))
  if (nchar(stmt) > 0) {
    statements <- c(statements, stmt)
  }
  return(statements)
}

# ---- 按 schema 补表：给已存在的库补上新增的表 -------------------------------

#' Ensure a Single Table Exists, Creating It from the Schema If Missing
#'
#' `initialize_database()` only reads the schema when the database is created,
#' so a table added to the schema later never reaches existing databases. This
#' creates that one table (and the indexes declared for it) on demand, which is
#' what lets `unassigned_entries` appear in a database that predates it without
#' rebuilding the whole file.
#'
#' The DDL is taken from the schema file rather than duplicated here, so there
#' stays exactly one definition of the table. `IF NOT EXISTS` is added on the
#' way through, which makes repeated calls harmless — the second call is a
#' no-op and returns `FALSE`.
#'
#' @param table Name of the table to ensure
#' @param db_path Optional database path, defaulting to `.resolve_db_path()`
#' @param schema_path Optional schema file, defaulting to `find_schema_file()`
#' @return `TRUE` if the table was created, `FALSE` if it already existed
#'   (invisibly)
#' @keywords internal
#' @export
#' @encoding UTF-8
ensure_schema_table <- function(table, db_path = NULL, schema_path = NULL) {
  if (!is.character(table) || length(table) != 1L || is.na(table) ||
      !nzchar(trimws(table))) {
    stop("`table` must be a single non-empty table name.")
  }
  table <- trimws(table)
  # 表名必须是普通标识符：既要拼进正则（避免转义问题），也要拼进 SQL。
  if (!grepl("^[A-Za-z_][A-Za-z0-9_]*$", table)) {
    stop("`table` must be a plain SQL identifier (letters, digits, underscore): ",
         table)
  }

  if (is.null(schema_path)) schema_path <- find_schema_file()

  # 表已在就短路返回。这条路径每次增量更新都会走到（记账前先确保表在），
  # 不该为此把整个 schema 解析一遍。
  con <- get_db_connection(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  if (DBI::dbExistsTable(con, table)) return(invisible(FALSE))

  if (!file.exists(schema_path)) {
    stop("Schema file not found: ", schema_path)
  }

  schema_sql <- paste(readLines(schema_path, warn = FALSE, encoding = "UTF-8"),
                      collapse = "\n")
  stmts <- split_sql_statements(schema_sql)

  pat <- paste0("(?i)^\\s*CREATE\\s+TABLE\\s+(?:IF\\s+NOT\\s+EXISTS\\s+)?",
                "[`\"\\[]?", table, "[`\"\\]]?\\s*\\(")
  hit <- grep(pat, stmts, perl = TRUE)
  if (length(hit) == 0L) {
    stop("Schema file has no CREATE TABLE statement for table '", table,
         "': ", schema_path)
  }
  create_sql <- stmts[hit[1L]]
  # 索引要跟着一起补，否则新表没有唯一键约束，登记就退化成"每轮长一堆重复行"
  idx_pat <- paste0("(?i)^\\s*CREATE\\s+(?:UNIQUE\\s+)?INDEX\\s+",
                    "(?:IF\\s+NOT\\s+EXISTS\\s+)?[`\"\\[]?[A-Za-z0-9_]+[`\"\\]]?",
                    "\\s+ON\\s+[`\"\\[]?", table, "[`\"\\]]?\\s*\\(")
  idx_hits <- grep(idx_pat, stmts, perl = TRUE)
  idx_sql <- stmts[idx_hits]

  add_if_not_exists <- function(sql, what) {
    sub(paste0("(?i)^(\\s*CREATE\\s+", what, "\\s+)"),
        "\\1IF NOT EXISTS ", sql, perl = TRUE)
  }
  create_sql <- add_if_not_exists(create_sql, "TABLE")
  if (length(idx_sql) > 0L) {
    idx_sql <- vapply(idx_sql, add_if_not_exists, character(1),
                      what = "(?:UNIQUE\\s+)?INDEX", USE.NAMES = FALSE)
  }

  DBI::dbExecute(con, create_sql)
  for (s in idx_sql) DBI::dbExecute(con, s)
  message("   Created missing table: ", table)
  invisible(TRUE)
}

# ---- 表重建：按 schema 重造单表（修列类型） ---------------------------------

#' Rebuild a Table from the Schema Definition
#'
#' SQLite cannot change a column's declared type with `ALTER TABLE`, so a column
#' created with the wrong type can only be repaired by building a new table
#' from the schema and copying the rows across.
#'
#' This is the fix for `iarc.volume`, which was declared `INTEGER` while IARC's
#' official values are multi-value text such as `"41, Sup 7, 71, 106"`. Storing
#' text in an INTEGER column loses nothing by itself; the loss happens on the
#' way back out. RSQLite picks the R type of a whole column from the first
#' non-missing value it sees, and `iarc.id = 1` holds an integer volume — so the
#' column is read as integers and every later multi-value string is truncated to
#' its leading number (`"41, Sup 7, 71, 106"` becomes `41`). The incremental
#' diff then reports a modification on every run and can never converge.
#' Declaring the column TEXT makes the read deterministic instead of
#' order-dependent.
#'
#' The copy runs entirely inside SQLite
#' (`INSERT INTO <table>__rebuild SELECT ... FROM <table>`). Round-tripping the
#' values through R would already have truncated them, because the truncation
#' happens when the value enters the R session.
#'
#' Note that a rebuild repairs the *declaration*, not the data: values already
#' written as NULL by an earlier `as.integer()` stay NULL and have to be
#' re-written by the next update.
#'
#' Indexes and triggers that sit on the table are collected from
#' `sqlite_master` before the swap and recreated in the same transaction.
#' Foreign key enforcement is switched off for the duration (the pragma is a
#' no-op inside a transaction, so it is toggled around it) and re-verified at
#' the end.
#'
#' @param table Name of the table to rebuild
#' @param db_path Optional database path, defaulting to `.resolve_db_path()`
#' @param schema_path Optional schema file, defaulting to `find_schema_file()`
#' @return Number of rows in the rebuilt table (integer, invisibly)
#' @keywords internal
#' @export
#' @encoding UTF-8
rebuild_table_from_schema <- function(table, db_path = NULL, schema_path = NULL) {
  if (!is.character(table) || length(table) != 1L || is.na(table) ||
      !nzchar(trimws(table))) {
    stop("`table` must be a single non-empty table name.")
  }
  table <- trimws(table)
  # 表名要整词匹配，否则 "cmr" 会命中 "cmr_raw" / "cmr_suspect"。把名字限定成
  # 普通标识符后就能直接拼进正则，不必再处理正则元字符的转义。
  if (!grepl("^[A-Za-z_][A-Za-z0-9_]*$", table)) {
    stop("`table` must be a plain SQL identifier (letters, digits, underscore): ",
         table)
  }

  if (is.null(schema_path)) schema_path <- find_schema_file()
  if (!file.exists(schema_path)) {
    stop("Schema file not found: ", schema_path)
  }

  schema_sql <- paste(readLines(schema_path, warn = FALSE, encoding = "UTF-8"),
                      collapse = "\n")
  stmts <- split_sql_statements(schema_sql)

  pat <- paste0("(?i)^\\s*CREATE\\s+TABLE\\s+(?:IF\\s+NOT\\s+EXISTS\\s+)?",
                "[`\"\\[]?", table, "[`\"\\]]?\\s*\\(")
  hit <- grep(pat, stmts, perl = TRUE)
  if (length(hit) == 0L) {
    stop("Schema file has no CREATE TABLE statement for table '", table,
         "': ", schema_path)
  }

  create_sql <- stmts[hit[1L]]
  tmp <- paste0(table, "__rebuild")
  pat_cap <- paste0(
    "(?i)^(\\s*CREATE\\s+TABLE\\s+(?:IF\\s+NOT\\s+EXISTS\\s+)?[`\"\\[]?)",
    table, "([`\"\\]]?\\s*\\()")
  create_tmp_sql <- sub(pat_cap, paste0("\\1", tmp, "\\2"), create_sql,
                        perl = TRUE)
  if (identical(create_tmp_sql, create_sql)) {
    stop("Could not rewrite the CREATE TABLE statement for table '", table, "'.")
  }

  con <- get_db_connection(db_path)
  # on.exit 按注册顺序执行，收尾必须在同一条表达式里显式排序：先复位 pragma，
  # 再断连。分成多条 add = TRUE 会先跑断连，后面的 PRAGMA 就落在死连接上。
  on.exit({
    try(DBI::dbExecute(con, "PRAGMA legacy_alter_table = OFF"), silent = TRUE)
    try(DBI::dbExecute(con, "PRAGMA foreign_keys = ON"), silent = TRUE)
    DBI::dbDisconnect(con)
  }, add = TRUE)

  # 外键开关在事务内是空操作，只能事务外切换；重建期间必须关掉，
  # 否则 DROP 旧表会连带子表动作。
  DBI::dbExecute(con, "PRAGMA foreign_keys = OFF")
  # 临时名 -> 正式名的改名不该去改写其它对象里的引用，legacy 模式正合适。
  # 老版本 SQLite 没有这个 pragma，失败就按默认行为走。
  try(DBI::dbExecute(con, "PRAGMA legacy_alter_table = ON"), silent = TRUE)

  qid <- function(x) paste0('"', gsub('"', '""', x, fixed = TRUE), '"')
  qlit <- function(x) paste0("'", gsub("'", "''", x, fixed = TRUE), "'")

  old_info <- DBI::dbGetQuery(con, paste0("PRAGMA table_info(", qid(table), ")"))
  if (nrow(old_info) == 0L) {
    stop("Table '", table, "' not found in ", .resolve_db_path(db_path), ".")
  }
  fk_before <- nrow(DBI::dbGetQuery(
    con, paste0("PRAGMA foreign_key_check(", qid(table), ")")))

  keep_objs <- DBI::dbGetQuery(con, paste0(
    "SELECT type, name, sql FROM sqlite_master WHERE tbl_name = ", qlit(table),
    " AND type IN ('index', 'trigger') AND sql IS NOT NULL"))

  DBI::dbWithTransaction(con, {
    DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", qid(tmp)))
    DBI::dbExecute(con, create_tmp_sql)

    new_info <- DBI::dbGetQuery(con, paste0("PRAGMA table_info(", qid(tmp), ")"))
    dropped <- setdiff(old_info$name, new_info$name)
    if (length(dropped) > 0L) {
      stop("Rebuild would drop column(s) that exist in the current table but ",
           "not in the schema: ", paste(dropped, collapse = ", "))
    }
    common <- intersect(old_info$name, new_info$name)
    if (length(common) == 0L) {
      stop("Rebuilt table shares no column name with the current table.")
    }
    cols <- paste(qid(common), collapse = ", ")

    DBI::dbExecute(con, paste0(
      "INSERT INTO ", qid(tmp), " (", cols, ") SELECT ", cols, " FROM ",
      qid(table)))
    DBI::dbExecute(con, paste0("DROP TABLE ", qid(table)))
    DBI::dbExecute(con, paste0("ALTER TABLE ", qid(tmp), " RENAME TO ",
                               qid(table)))

    for (k in seq_len(nrow(keep_objs))) {
      DBI::dbExecute(con, keep_objs$sql[k])
    }
  })

  fk_after <- nrow(DBI::dbGetQuery(
    con, paste0("PRAGMA foreign_key_check(", qid(table), ")")))
  if (fk_after > fk_before) {
    stop("Rebuild of '", table, "' introduced ", fk_after - fk_before,
         " foreign key violation(s).")
  }

  n <- DBI::dbGetQuery(con, paste0("SELECT COUNT(*) AS n FROM ", qid(table)))$n
  invisible(as.integer(n))
}

# ---- cmr_suspect 主键迁移：补 index_no 并从旧源回填 -------------------------

#' Give `cmr_suspect` its CLP Index Number and Backfill It
#'
#' `cmr_suspect` was migrated without the source's `Index No` column, so the
#' incremental diff had to fall back to `substance_name` as its primary key.
#' A substance name is free text: when the upstream CLP export re-spells
#' `O,O-di-methyl` as `O,O-dimethyl`, or writes `0.5` as `0,5`, or swaps
#' `…%` for `...%`, the same row is reported as one removal plus one addition.
#' Measured on the live data: 45 rows were reported removed, of which 26 were
#' upstream punctuation edits, 15 differed only in whitespace, and 4 came from
#' upstream merging two entries. The sibling table `cmr`, built from the very
#' same source but keyed on `index_no`, reported 2.
#'
#' `Index No` is the official CLP identifier and is stable across editions. It
#' is 495/495 non-empty and 495/495 unique in the `cmr_suspect` subset of the
#' current source.
#'
#' Adding the column is not enough — existing rows have no value for it, and a
#' NULL key is worse than a mutable one. The values are recovered from the
#' previous migration source, `inst/clp_cmr_meta.xlsx`, whose `cmr_suspect`
#' sheet carries the same `Index No` column and matches all 359 live rows by
#' name. Rows whose `index_no` is already set are left alone, so the function is
#' idempotent and safe to re-run.
#'
#' @param db_path Optional database path, defaulting to `.resolve_db_path()`
#' @param index_map Optional data frame with `substance_name` and `index_no`
#'   columns. When `NULL`, read from `meta_file`.
#' @param meta_file Previous migration source; defaults to
#'   `inst/clp_cmr_meta.xlsx` next to the schema file.
#' @param meta_sheet Worksheet within `meta_file`
#' @return `list(added = , filled = , unmatched = , total = )`, where `added`
#'   says whether the column had to be created and `filled` counts the rows that
#'   received a value on this run
#' @keywords internal
#' @export
#' @encoding UTF-8
migrate_cmr_suspect_index_no <- function(db_path = NULL, index_map = NULL,
                                         meta_file = NULL,
                                         meta_sheet = "cmr_suspect") {
  if (is.null(db_path)) db_path <- .resolve_db_path()
  if (!file.exists(db_path)) stop("Database not found: ", db_path)

  if (is.null(index_map)) {
    if (is.null(meta_file)) {
      meta_file <- file.path(dirname(find_schema_file()), "clp_cmr_meta.xlsx")
    }
    if (!file.exists(meta_file)) {
      stop("Migration source not found: ", meta_file)
    }
    raw <- import_xlsx(meta_file, sheet = meta_sheet)
    index_map <- data.frame(
      substance_name = match_col(raw, "International Chemical Identification"),
      index_no = match_col(raw, "Index No"),
      stringsAsFactors = FALSE)
  }
  index_map <- as.data.frame(index_map, stringsAsFactors = FALSE)
  if (!all(c("substance_name", "index_no") %in% names(index_map))) {
    stop("`index_map` must have `substance_name` and `index_no` columns.")
  }
  keep <- !is.na(index_map$index_no) & nzchar(trimws(index_map$index_no))
  index_map <- index_map[keep, , drop = FALSE]

  squash <- function(x) gsub("[[:space:]]+", "", as.character(x))
  map_exact <- squash(index_map$substance_name)
  # 重复名（同一格被拆过）取第一个，与 diff 的"同键取一行"口径一致
  map_exact <- map_exact[!duplicated(map_exact)]
  map_idx <- index_map$index_no[!duplicated(squash(index_map$substance_name))]

  con <- get_db_connection(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  if (!"cmr_suspect" %in% DBI::dbListTables(con)) {
    stop("Table 'cmr_suspect' not found in ", db_path, ".")
  }
  cols <- DBI::dbGetQuery(con, "PRAGMA table_info(cmr_suspect)")$name
  added <- FALSE
  if (!"index_no" %in% cols) {
    DBI::dbExecute(con, "ALTER TABLE cmr_suspect ADD COLUMN index_no TEXT")
    added <- TRUE
  }
  # 索引与 schema 对齐（缺了不影响正确性，只影响按 index_no 查的速度）
  DBI::dbExecute(con, paste("CREATE INDEX IF NOT EXISTS",
                            "idx_cmr_suspect_index_no ON cmr_suspect(index_no)"))

  rows <- DBI::dbGetQuery(con, "SELECT id, substance_name, index_no FROM cmr_suspect")
  need <- is.na(rows$index_no) | !nzchar(trimws(rows$index_no))
  filled <- 0L

  if (any(need)) {
    keys <- squash(rows$substance_name[need])
    hit <- match(keys, map_exact)
    DBI::dbWithTransaction(con, {
      for (j in which(!is.na(hit))) {
        DBI::dbExecute(con, "UPDATE cmr_suspect SET index_no = ? WHERE id = ?",
                       params = list(map_idx[hit[j]], rows$id[need][j]))
      }
    })
    filled <- sum(!is.na(hit))
  }

  n_total <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM cmr_suspect")$n
  n_ok <- DBI::dbGetQuery(con,
    "SELECT COUNT(*) AS n FROM cmr_suspect
      WHERE index_no IS NOT NULL AND TRIM(index_no) <> ''")$n
  list(added = added, filled = as.integer(filled),
       unmatched = as.integer(n_total - n_ok), total = as.integer(n_total))
}

# ---- 列名归一与 xlsx 读取（服务于下面的迁移） --------------------------------

#' Fuzzy Column Lookup
#'
#' Finds a column in a data frame by matching against the normalized column
#' name. Matching order:
#' 1. exact match on whitespace-stripped names (case-sensitive)
#' 2. exact match on whitespace-stripped lowercase names (case-insensitive)
#' 3. contains-match on whitespace-stripped lowercase names
#' Returns a vector of the column values or a default value if no match.
#'
#' @param data Data frame to search in
#' @param pattern Column name pattern (normalized before matching)
#' @param default Value to return if column not found
#' @return Vector of column values or default
#' @keywords internal
#' @export
match_col <- function(data, pattern, default = NA) {
  if (!is.data.frame(data) || nrow(data) == 0) {
    return(rep(default, if (is.data.frame(data)) nrow(data) else 0))
  }
  strip <- function(x) gsub("[[:space:]]+", "", x)
  names_stripped <- strip(names(data))
  pattern_stripped <- strip(pattern)

  # 1) Exact, case-sensitive
  idx <- which(names_stripped == pattern_stripped)
  if (length(idx) > 0) {
    return(data[[idx[1]]])
  }
  # 2) Exact, case-insensitive
  idx <- which(tolower(names_stripped) == tolower(pattern_stripped))
  if (length(idx) > 0) {
    return(data[[idx[1]]])
  }
  # 3) Contains, case-insensitive
  idx <- which(vapply(tolower(names_stripped),
                      function(nm) grepl(tolower(pattern_stripped), nm, fixed = TRUE),
                      logical(1)))
  if (length(idx) > 0) {
    return(data[[idx[1]]])
  }
  return(rep(default, nrow(data)))
}

#' Resolve XLSX File Mapping
#'
#' Returns the mapping from database table names to their source xlsx files
#' and sheet names. Uses the actual file names present in the inst/ directory.
#'
#' @return Named list of lists with elements `file` and `sheet` (or NULL)
#' @keywords internal
#' @export
resolve_xlsx_mapping <- function() {
  list(
    svhc = list(file = "svhc_meta.xlsx", sheet = NULL),
    cmr = list(file = "clp_cmr_meta.xlsx", sheet = "cmr"),
    cmr_suspect = list(file = "clp_cmr_meta.xlsx", sheet = "cmr_suspect"),
    iarc = list(file = "iarc_meta.xlsx", sheet = NULL),
    eu_sml = list(file = "eu10_2011_meta.xlsx", sheet = NULL),
    eu_sml_group = list(file = "eu10_2011.xlsx", sheet = "SML_group"),
    edc = list(file = "edc_meta.xlsx", sheet = NULL),
    china_sml = list(file = "china_sml_meta_cleaned.xlsx", sheet = NULL)
  )
}

#' Import XLSX with Optional Sheet
#'
#' Reads an xlsx file using rio, optionally selecting a specific sheet.
#' Warnings from readxl column-type guessing are suppressed (they are
#' noise when mixed-type Chinese columns are present).
#'
#' @param file_path Path to the xlsx file
#' @param sheet Sheet name or NULL to use the first sheet
#' @return Data frame
#' @keywords internal
#' @export
import_xlsx <- function(file_path, sheet = NULL) {
  if (is.null(sheet)) {
    suppressWarnings(rio::import(file_path))
  } else {
    suppressWarnings(rio::import(file_path, sheet = sheet))
  }
}

# ---- 建库入口与状态查询 -----------------------------------------------------

#' Initialize Database
#'
#' Creates the SQLite database with the complete schema if it doesn't exist.
#' This function is idempotent - safe to run multiple times.
#'
#' @param force_recreate Logical, whether to drop and recreate existing database
#' @param db_path Optional custom path to database file (for testing)
#' @return Logical indicating success
#' @export
#' @export
initialize_database <- function(force_recreate = FALSE, db_path = NULL) {
  message("🔧 Initializing FCMSafety SQLite database...")

  # 重建分支必须**在建连接之前**。迁移最后一步要 file.remove(db_path) 再做原子交换，
  # 而 Windows 上文件被连接占用时删除会报"拒绝访问"。原先这里先 get_db_connection()
  # 并注册 on.exit 断连，再 return(migrate_xlsx_to_sqlite(...)) —— return() 会先求值
  # 迁移、on.exit 要等函数退出才跑，于是连接一直开着，重建必定失败（库不会被破坏，
  # 但也永远不会被重建）。放在这里就结构性地不可能再踩。
  if (force_recreate) {
    # Atomic recreation via migration (writes temp db, then swaps)
    message("🗑️  Recreating database atomically via xlsx migration...")
    return(migrate_xlsx_to_sqlite(db_path = db_path))
  }

  tryCatch({
    con <- get_db_connection(db_path)
    on.exit(DBI::dbDisconnect(con))

    # Check if database already exists
    if (DBI::dbExistsTable(con, "chemicals")) {
      message("✅ Database already exists and initialized")
      return(TRUE)
    }

    # Read and execute schema inside a transaction
    schema_path <- find_schema_file()
    message("📄 Reading database schema...")
    schema_sql <- paste(readLines(schema_path, warn = FALSE), collapse = "\n")
    statements <- split_sql_statements(schema_sql)

    message("🔨 Creating database tables and indexes...")
    DBI::dbWithTransaction(con, {
      for (stmt in statements) {
        tryCatch({
          DBI::dbExecute(con, stmt)
          if (grepl("CREATE TABLE", stmt, ignore.case = TRUE)) {
            table_name <- gsub(".*CREATE TABLE\\s+(\\w+).*", "\\1", stmt, ignore.case = TRUE)
            message("   ✅ Created table: ", table_name)
          }
        }, error = function(e) {
          message("   ❌ Error executing statement: ", e$message)
          stop(e)  # roll back the transaction
        })
      }
    })

    message("✅ Database initialization completed successfully!")
    return(TRUE)

  }, error = function(e) {
    message("❌ Database initialization failed: ", e$message)
    return(FALSE)
  })
}

#' Check Database Status
#'
#' Provides information about the current database status, including
#' table counts, last update times, and version information.
#'
#' @param db_path Optional custom path to database file (for testing)
#' @return List with database status information
#' @export
#' @export
check_database_status <- function(db_path = NULL) {
  tryCatch({
    con <- get_db_connection(db_path)
    on.exit(DBI::dbDisconnect(con))

    # Check if database is initialized
    if (!DBI::dbExistsTable(con, "chemicals")) {
      return(list(
        initialized = FALSE,
        message = "Database not initialized. Run initialize_database() first."
      ))
    }

    # Get table counts
    tables <- c("chemicals", "svhc", "cmr", "cmr_suspect", "iarc",
                "eu_sml", "eu_sml_group", "edc", "china_sml")

    counts <- list()
    for (table in tables) {
      if (DBI::dbExistsTable(con, table)) {
        count_query <- paste("SELECT COUNT(*) as count FROM", table)
        counts[[table]] <- DBI::dbGetQuery(con, count_query)$count[1]
      } else {
        counts[[table]] <- 0
      }
    }

    # Get metadata information (with table existence guard)
    metadata <- if (DBI::dbExistsTable(con, "database_metadata")) {
      DBI::dbGetQuery(con, "SELECT * FROM database_metadata ORDER BY database_name")
    } else {
      data.frame()
    }

    # Get recent update history (with table existence guard)
    recent_updates <- if (DBI::dbExistsTable(con, "update_history")) {
      DBI::dbGetQuery(con,
        "SELECT database_name, update_timestamp, update_type, records_added, records_removed
         FROM update_history
         ORDER BY update_timestamp DESC
         LIMIT 10")
    } else {
      data.frame()
    }

    return(list(
      initialized = TRUE,
      table_counts = counts,
      metadata = metadata,
      recent_updates = recent_updates,
      total_chemicals = counts$chemicals,
      database_path = con@dbname
    ))

  }, error = function(e) {
    return(list(
      initialized = FALSE,
      error = e$message
    ))
  })
}

# ---- 迁移：xlsx -> SQLite（建库后把数据搬进去） ------------------------------
#
# 流程：process_raw_for_migration() / process_database_for_migration() 先把源表
# 对齐到库表列，再由 migrate_xlsx_to_sqlite() 走"临时库 + 原子替换"落盘 ——
# 任何一步失败都不会碰现有数据库。所以重建失败时旧库还在，别急着手工恢复。

#' Prepare Raw Data for _raw Tables
#'
#' Maps raw xlsx columns onto the _raw table schema (which preserves ALL
#' original rows, including those without InChIKey). Only svhc and cmr
#' have _raw tables in the schema.
#'
#' @param data Raw data frame from xlsx
#' @param db_name Database name (svhc or cmr)
#' @return Data frame matching the _raw table columns
#' @keywords internal
#' @export
process_raw_for_migration <- function(data, db_name) {
  safe_get_col <- function(col_name, default = NA) {
    match_col(data, col_name, default = default)
  }

  if (db_name == "svhc") {
    data.frame(
      substance_name = safe_get_col("Substance name"),
      description = safe_get_col("Description"),
      ec_no = safe_get_col("EC No."),
      cas_no = safe_get_col("CAS No."),
      reason_for_inclusion = safe_get_col("Reason for inclusion"),
      date_of_inclusion = safe_get_col("Date of inclusion"),
      decision = safe_get_col("Decision"),
      iuclid_dataset = safe_get_col("IUCLID dataset"),
      support_document = safe_get_col("Support document"),
      response_to_comments = safe_get_col("Response to comments"),
      remarks = safe_get_col("Remarks"),
      CID = as.integer(safe_get_col("CID", default = NA_integer_)),
      Formula = safe_get_col("MolecularFormula"),
      SMILES = safe_get_col("IsomericSMILES"),
      InChIKey = safe_get_col("InChIKey"),
      IUPACName = NA_character_,
      ExactMass = as.numeric(safe_get_col("ExactMass", default = NA_real_)),
      stringsAsFactors = FALSE
    )
  } else if (db_name == "cmr") {
    # 图示+信号词、限值+M 系数在 CLP 导出里各是一列，必须拆开落库。旧代码用
    # 子串匹配分别取这两列，整串同时写进了两对列（库里 332 行两对列全部相同）。
    lab <- split_clp_label_cell(
      col_or_na(data, find_clp_label_col(names(data)), nrow(data)))
    lim <- split_clp_limit_cell(
      col_or_na(data, find_clp_limit_col(names(data)), nrow(data)))
    data.frame(
      index_no = safe_get_col("Index No"),
      international_chemical_identification = safe_get_col("International Chemical Identification"),
      ec_no = safe_get_col("EC No"),
      cas_no = safe_get_col("CAS No"),
      hazard_class_and_category_codes = safe_get_col("Hazard Class and Category Code(s)"),
      hazard_statement_codes = safe_get_col("Hazard Statement Code(s)"),
      pictogram = lab$pictogram,
      signal_word_codes = lab$signal_word_codes,
      hazard_statement_codes_alt = safe_get_col("Hazard statement Code(s)"),
      suppl_hazard_statement_codes = safe_get_col("Suppl. Hazard statement Code(s)"),
      specific_conc_limits = lim$specific_conc_limits,
      m_factors = lim$m_factors,
      notes = safe_get_col("Notes"),
      atp_inserted_updated = safe_get_col("ATP inserted/ATP Updated"),
      CID = as.integer(safe_get_col("CID", default = NA_integer_)),
      Formula = safe_get_col("MolecularFormula"),
      SMILES = safe_get_col("IsomericSMILES"),
      InChIKey = safe_get_col("InChIKey"),
      IUPACName = NA_character_,
      ExactMass = as.numeric(safe_get_col("ExactMass", default = NA_real_)),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame()
  }
}

#' Migrate Data from XLSX to SQLite
#'
#' Rebuilds the SQLite database from the xlsx source files using an
#' atomic swap: writes a temporary database first, validates it, then
#' replaces the production database. If anything fails, the existing
#' database is left untouched.
#'
#' @param source_dir Directory containing xlsx files (default: inst/)
#' @param backup_existing Whether to backup existing database before replacement
#' @param db_path Optional custom path to database file (for testing)
#' @return Logical indicating success
#' @export
#' @export
migrate_xlsx_to_sqlite <- function(source_dir = NULL, backup_existing = TRUE, db_path = NULL) {
  message("🔄 Starting migration from XLSX to SQLite (atomic swap)...")

  if (is.null(source_dir)) {
    source_dir <- file.path(getwd(), "inst")
  }
  if (is.null(db_path)) {
    db_path <- file.path(source_dir, "fcmsafety.db")
  }

  if (!dir.exists(source_dir)) {
    stop("Source directory does not exist: ", source_dir)
  }

  tmp_path <- file.path(dirname(db_path), "fcmsafety_tmp.db")

  migration_ok <- tryCatch({
    # Clean up any stale temp database
    if (file.exists(tmp_path)) {
      file.remove(tmp_path)
    }

    con <- DBI::dbConnect(RSQLite::SQLite(), tmp_path)
    # Foreign keys are handled after all data is loaded; disable during migration
    DBI::dbExecute(con, "PRAGMA foreign_keys = OFF")

    # Track migration progress
    migration_start <- Sys.time()
    total_records <- 0
    migration_errors <- list()

    # 1) Create schema inside a transaction
    schema_path <- find_schema_file()
    schema_sql <- paste(readLines(schema_path, warn = FALSE), collapse = "\n")
    statements <- split_sql_statements(schema_sql)
    DBI::dbWithTransaction(con, {
      for (stmt in statements) {
        DBI::dbExecute(con, stmt)
      }
    })
    # Schema sets PRAGMA foreign_keys = ON; re-disable for data loading
    DBI::dbExecute(con, "PRAGMA foreign_keys = OFF")

    # 2) Collect all unique chemicals first (needed by FK references)
    message("📊 Collecting chemical metadata...")
    mappings <- resolve_xlsx_mapping()
    all_chemicals <- data.frame(
      InChIKey = character(0), CID = integer(0),
      Formula = character(0), SMILES = character(0),
      IUPACName = character(0), ExactMass = numeric(0),
      stringsAsFactors = FALSE
    )

    for (db_name in names(mappings)) {
      mapping <- mappings[[db_name]]
      file_path <- file.path(source_dir, mapping$file)
      if (!file.exists(file_path)) next

      data <- tryCatch(import_xlsx(file_path, mapping$sheet), error = function(e) NULL)
      if (is.null(data)) next

      inchikey_col <- match_col(data, "InChIKey")
      if (length(inchikey_col) > 0 && !all(is.na(inchikey_col))) {
        cid_col <- match_col(data, "CID", default = NA_integer_)
        formula_col <- match_col(data, "MolecularFormula")
        smiles_col <- match_col(data, "IsomericSMILES")
        mass_col <- match_col(data, "ExactMass", default = NA_real_)

        chem_data <- data.frame(
          InChIKey = as.character(inchikey_col),
          CID = as.integer(cid_col),
          Formula = as.character(formula_col),
          SMILES = as.character(smiles_col),
          IUPACName = NA_character_,
          ExactMass = as.numeric(mass_col),
          stringsAsFactors = FALSE
        )
        chem_data <- chem_data[!is.na(chem_data$InChIKey) & chem_data$InChIKey != "", ]
        chem_data <- chem_data[!duplicated(chem_data$InChIKey), ]
        all_chemicals <- rbind(all_chemicals, chem_data)
        all_chemicals <- all_chemicals[!duplicated(all_chemicals$InChIKey), ]
      }
    }

    # Insert chemical metadata
    if (nrow(all_chemicals) > 0) {
      message("💾 Inserting ", nrow(all_chemicals), " unique chemicals...")
      DBI::dbWriteTable(con, "chemicals", all_chemicals, append = TRUE)
    } else {
      message("⚠️  No chemical metadata found to insert")
    }

    # 3) Migrate each database table inside a transaction
    message("🔄 Migrating data tables...")
    DBI::dbWithTransaction(con, {
      for (db_name in names(mappings)) {
        mapping <- mappings[[db_name]]
        file_path <- file.path(source_dir, mapping$file)

        if (!file.exists(file_path)) {
          message("   ⚠️  File not found: ", mapping$file, " (table ", db_name, " skipped)")
          next
        }

        tryCatch({
          data <- import_xlsx(file_path, mapping$sheet)

          # Write full raw data to _raw table if it exists (svhc_raw, cmr_raw)
          raw_table <- paste0(db_name, "_raw")
          if (DBI::dbExistsTable(con, raw_table)) {
            raw_data <- process_raw_for_migration(data, db_name)
            if (nrow(raw_data) > 0) {
              DBI::dbWriteTable(con, raw_table, raw_data, append = TRUE)
              message("   ✅ ", raw_table, ": ", nrow(raw_data), " raw records preserved")
            }
          }

          processed_data <- process_database_for_migration(data, db_name)

          if (is.null(processed_data) || nrow(processed_data) == 0) {
            message("   ⚠️  ", db_name, ": no rows after processing")
            next
          }

          DBI::dbWriteTable(con, db_name, processed_data, append = TRUE)
          n_rows <- nrow(processed_data)
          total_records <- total_records + n_rows

          # Update metadata
          if (DBI::dbExistsTable(con, "database_metadata")) {
            DBI::dbExecute(con,
              "UPDATE database_metadata
               SET total_records = ?, last_updated = CURRENT_TIMESTAMP
               WHERE database_name = ?",
              params = list(n_rows, db_name))
          }
          message("   ✅ ", db_name, ": ", n_rows, " records migrated")
        }, error = function(e) {
          migration_errors[[db_name]] <<- e$message
          message("   ❌ Migration failed for ", db_name, ": ", e$message)
        })
      }

      # Record migration in update history
      if (DBI::dbExistsTable(con, "update_history")) {
        DBI::dbExecute(con,
          "INSERT INTO update_history (database_name, update_type, records_added, source_file, user_notes)
           VALUES (?, ?, ?, ?, ?)",
          params = list("all", "migration", total_records, "xlsx_files",
                        "Full migration from xlsx to SQLite"))
      }
    })

    # 4) Validate the temp database before swapping
    message("🔍 Validating temporary database...")
    tables <- DBI::dbListTables(con)
    required_tables <- c("chemicals", "svhc", "cmr", "cmr_suspect", "iarc",
                         "eu_sml", "eu_sml_group", "edc", "china_sml",
                         "database_metadata", "update_history", "change_log")
    missing <- setdiff(required_tables, tables)
    if (length(missing) > 0) {
      stop("Temp database missing required tables: ", paste(missing, collapse = ", "))
    }
    for (tbl in c("svhc", "cmr", "cmr_suspect", "iarc", "eu_sml", "edc", "china_sml")) {
      cnt <- DBI::dbGetQuery(con, paste("SELECT COUNT(*) AS n FROM", tbl))$n[1]
      if (cnt == 0) {
        message("   ⚠️  Table ", tbl, " has 0 rows (source file may be missing data)")
      }
    }

    migration_time <- as.numeric(difftime(Sys.time(), migration_start, units = "secs"))
    message("📊 Total records migrated: ", total_records)
    message("⏱️  Migration time: ", round(migration_time, 2), " seconds")

    # 5) Disconnect, backup, and atomically swap
    DBI::dbDisconnect(con)

    if (backup_existing && file.exists(db_path)) {
      backup_path <- paste0(db_path, ".bak_", format(Sys.time(), "%Y%m%d_%H%M%S"))
      if (file.copy(db_path, backup_path)) {
        message("💾 Backed up existing database to: ", basename(backup_path))
      }
    }

    # Windows 上 file.rename() 无法覆盖已存在的目标文件（返回"拒绝访问"），
    # 备份完成后先移除旧库再换名，否则原子替换会在最后一步失败。
    if (file.exists(db_path)) {
      if (!file.remove(db_path)) {
        stop("Failed to remove existing database before swap")
      }
    }

    if (!file.rename(tmp_path, db_path)) {
      stop("Failed to swap temp database into place")
    }
    message("✅ Database migrated and atomically swapped. Old database preserved as backup.")

    if (length(migration_errors) > 0) {
      message("⚠️  Migration completed with per-table errors:")
      for (db in names(migration_errors)) {
        message("   ❌ ", db, ": ", migration_errors[[db]])
      }
      return(FALSE)
    }
    return(TRUE)

  }, error = function(e) {
    message("❌ Migration failed: ", e$message)
    # Clean up temp database; production database untouched
    if (file.exists(tmp_path)) {
      file.remove(tmp_path)
    }
    return(FALSE)
  })

  return(migration_ok)
}

#' Process Database for Migration
#'
#' Internal function to process and clean database-specific data
#' during migration from xlsx to SQLite format. Uses fuzzy column
#' matching so column names with varying whitespace/newlines still map.
#'
#' @param data Raw data from xlsx file
#' @param db_name Database name for specific processing rules
#' @return Processed data frame ready for SQLite insertion
#' @export
process_database_for_migration <- function(data, db_name) {
  safe_get_col <- function(col_name, default = NA) {
    match_col(data, col_name, default = default)
  }

  if (db_name == "svhc") {
    # Map SVHC columns to schema
    processed <- data.frame(
      InChIKey = safe_get_col("InChIKey"),
      substance_name = safe_get_col("Substance name"),
      description = safe_get_col("Description"),
      ec_no = safe_get_col("EC No."),
      cas_no = safe_get_col("CAS No."),
      reason_for_inclusion = safe_get_col("Reason for inclusion"),
      date_of_inclusion = safe_get_col("Date of inclusion"),
      decision = safe_get_col("Decision"),
      iuclid_dataset = safe_get_col("IUCLID dataset"),
      support_document = safe_get_col("Support document"),
      response_to_comments = safe_get_col("Response to comments"),
      remarks = safe_get_col("Remarks"),
      stringsAsFactors = FALSE
    )

  } else if (db_name == "cmr") {
    # 同 process_raw_for_migration：两对复合列必须拆开落库
    lab <- split_clp_label_cell(
      col_or_na(data, find_clp_label_col(names(data)), nrow(data)))
    lim <- split_clp_limit_cell(
      col_or_na(data, find_clp_limit_col(names(data)), nrow(data)))
    processed <- data.frame(
      InChIKey = safe_get_col("InChIKey"),
      index_no = safe_get_col("Index No"),
      international_chemical_identification = safe_get_col("International Chemical Identification"),
      ec_no = safe_get_col("EC No"),
      cas_no = safe_get_col("CAS No"),
      hazard_class_and_category_codes = safe_get_col("Hazard Class and Category Code(s)"),
      hazard_statement_codes = safe_get_col("Hazard Statement Code(s)"),
      pictogram = lab$pictogram,
      signal_word_codes = lab$signal_word_codes,
      hazard_statement_codes_alt = safe_get_col("Hazard statement Code(s)"),
      suppl_hazard_statement_codes = safe_get_col("Suppl. Hazard statement Code(s)"),
      specific_conc_limits = lim$specific_conc_limits,
      m_factors = lim$m_factors,
      notes = safe_get_col("Notes"),
      atp_inserted_updated = safe_get_col("ATP inserted/ATP Updated"),
      stringsAsFactors = FALSE
    )

  } else if (db_name == "eu_sml") {
    # Handle complex EU SML processing (replicate current logic with fuzzy matching)
    sml_raw <- safe_get_col("SML [mg/kg]")
    sml_group_raw <- safe_get_col("SML(T) [mg/kg] (Group restriction No)")

    # Process SML values: comma->dot, ND->0.01, strip newline suffixes, numeric
    sml_num <- as.character(sml_raw)
    sml_num <- stringr::str_replace(sml_num, ",", ".")
    sml_num <- stringr::str_replace(sml_num, "ND", "0.01")
    sml_num <- stringr::str_remove_all(sml_num, "\n.*$")
    sml_num <- suppressWarnings(as.numeric(trimws(sml_num)))
    sml_group_clean <- stringr::str_remove_all(as.character(sml_group_raw), "\\(|\\)")

    processed <- data.frame(
      InChIKey = safe_get_col("InChIKey"),
      fcm_substance_no = safe_get_col("FCM substance No"),
      ref_no = safe_get_col("Ref. No"),
      cas_no = safe_get_col("CAS No"),
      substance_name = safe_get_col("Substance name"),
      use_as_additive = safe_get_col("Use as additive"),
      use_as_monomer = safe_get_col("Use as monomer"),
      frf_applicable = safe_get_col("FRF applicable"),
      sml = sml_num,
      sml_group = sml_group_clean,
      restrictions_and_specifications = safe_get_col("Restrictions and specifications"),
      notes_on_verification = safe_get_col("Notes on verification"),
      stringsAsFactors = FALSE
    )

  } else if (db_name == "cmr_suspect") {
    # cmr_suspect sheet uses "International Chemical Identification" as the name column
    # 原实现写的是 "CAS No." / "EC No."（带尾点）。源里从来没有带尾点的列名，
    # match_col 的"包含"级匹配也救不回来（"casno." 不是 "casno" 的子串），
    # 于是这两列迁进来永远是空的 —— cas_no 297 行、ec_no 273 行全空就是这个原因。
    processed <- data.frame(
      InChIKey = safe_get_col("InChIKey"),
      index_no = safe_get_col("Index No"),
      substance_name = safe_get_col("International Chemical Identification"),
      cas_no = safe_get_col("CAS No"),
      ec_no = safe_get_col("EC No"),
      classification = safe_get_col("Hazard Statement Code(s)"),
      source = safe_get_col("Source"),
      notes = safe_get_col("Notes"),
      stringsAsFactors = FALSE
    )

  } else if (db_name == "iarc") {
    processed <- data.frame(
      InChIKey = safe_get_col("InChIKey"),
      cas_no = safe_get_col("CAS No."),
      agent = safe_get_col("Agent"),
      group_classification = safe_get_col("Group"),
      # volume / volume_publication_year 原样存文本。iarc_meta.xlsx 的值是
      # "61, 100B" / "Sup 7, 56"（1039 个非空里 714 个带逗号）与 "2018 online" /
      # "In prep."，旧代码的 as.integer() 把这一整类值写成 NULL
      # （as.integer("61, 100B") = NA）。而 iarc.id = 1 的 volume 恰好是整数，
      # 整列读回时按整数走，多值文本会被截成前导数字。列在 schema 里声明为 TEXT
      # （见 fcmsafety_schema.sql 的 iarc 段）。
      volume = safe_get_col("Volume"),
      volume_publication_year = safe_get_col("Year"),
      # iarc_meta.xlsx 没有 "Evaluation year" 列，迁移期只能是空值；这一列由增量源
      # 补写（IARC 在线页，见 download_sources.R 里的 yeareval）。仍按列名取，
      # 万一 meta 文件日后补上这列就能自动接住。
      evaluation_year = suppressWarnings(as.integer(
        safe_get_col("Evaluation year", default = NA_integer_))),
      additional_information = safe_get_col("Additional information"),
      stringsAsFactors = FALSE
    )

  } else if (db_name == "edc") {
    # edc_meta.xlsx uses Group/Name/CAS/cid/CID/IsomericSMILES/InChI/InChIKey columns
    processed <- data.frame(
      InChIKey = safe_get_col("InChIKey"),
      substance_name = safe_get_col("Name"),
      cas_no = safe_get_col("CAS"),
      ec_no = NA_character_,
      classification = safe_get_col("Group"),
      evidence_level = NA_character_,
      source = "EDC",
      notes = NA_character_,
      stringsAsFactors = FALSE
    )

  } else if (db_name == "eu_sml_group") {
    # eu10_2011.xlsx SML_group sheet has no InChIKey column; map raw columns
    sml_raw <- safe_get_col("SML (T) [mg/kg]")

    # Process SML values: comma->dot, ND->0.01, numeric
    sml_num <- as.character(sml_raw)
    sml_num <- stringr::str_replace(sml_num, ",", ".")
    sml_num <- stringr::str_replace(sml_num, "ND", "0.01")
    sml_num <- suppressWarnings(as.numeric(trimws(sml_num)))

    processed <- data.frame(
      InChIKey = NA_character_,
      group_no = safe_get_col("Group Restriction No"),
      substance_name = safe_get_col("FCM substance No"),
      cas_no = NA_character_,
      sml = sml_num,
      restrictions = safe_get_col("Group restriction specification"),
      stringsAsFactors = FALSE
    )

  } else if (db_name == "china_sml") {
    # china_sml_meta_cleaned.xlsx 已含 PubChem 结构列（CID/MolecularFormula/
    # IsomericSMILES/InChIKey/ExactMass），须直接取 InChIKey 以支撑结构式展示。
    # 中文列名（中文名称/标准/其它要求）带不可见字符，match_col 匹配不上，
    # 故物质名改用英文列 "Chemical name"（与全库其余表的 substance_name 一致）。
    sml_num <- suppressWarnings(as.numeric(safe_get_col("SML", default = NA_real_)))
    processed <- data.frame(
      InChIKey = safe_get_col("InChIKey"),
      substance_name = safe_get_col("Chemical name"),
      cas_no = safe_get_col("CAS"),
      sml_value = sml_num,
      unit = "mg/kg",
      food_type = NA_character_,
      regulation_reference = safe_get_col("标准"),
      notes = safe_get_col("其它要求"),
      stringsAsFactors = FALSE
    )

  } else {
    # For other databases, create a generic mapping
    processed <- data
  }

  # Filter out rows with empty InChIKey for databases that require it
  if (db_name != "china_sml" && db_name != "eu_sml_group" &&
      "InChIKey" %in% names(processed)) {
    processed <- processed[!is.na(processed$InChIKey) & processed$InChIKey != "", ]
  }

  return(processed)
}

