# 新库（蛇形 schema）适配回归测试。
#
# 背景：2026-09-03 DB 重建后，更新链路（公共流水线 incremental_update.R /
# update_other_dbs.R）与库结构脱节：map_to_db_columns 无法把源文件的驼峰列名
# 对齐到库表的蛇形列名（"Index No" 匹配不到 index_no），写库时显式 NULL 覆盖
# created_at/updated_at 默认值。本文件用与真实库同形的蛇形临时表验证三件事：
#   1) 列对齐兜底（超归一 + db_col_candidates 候选表）能命中真实源列名；
#   2) 写库自动排除系统列（id/时间戳交给 SQLite DEFAULT）；
#   3) run_incremental_update 对蛇形表端到端可跑（diff + 写库 + 明细留痕）。

make_snake_cmr_table <- function(con) {
  DBI::dbExecute(con, "CREATE TABLE cmr (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    InChIKey TEXT NOT NULL,
    index_no TEXT,
    international_chemical_identification TEXT,
    ec_no TEXT,
    cas_no TEXT,
    hazard_class_and_category_codes TEXT,
    hazard_statement_codes TEXT,
    pictogram TEXT,
    signal_word_codes TEXT,
    hazard_statement_codes_alt TEXT,
    suppl_hazard_statement_codes TEXT,
    specific_conc_limits TEXT,
    m_factors TEXT,
    notes TEXT,
    atp_inserted_updated TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  )")
}

make_snake_audit <- function(con) {
  DBI::dbExecute(con, "CREATE TABLE update_history (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    database_name TEXT, update_type TEXT,
    records_added INTEGER, records_removed INTEGER, records_modified INTEGER,
    source_file TEXT, user_notes TEXT, success INTEGER)")
  DBI::dbExecute(con, "CREATE TABLE change_log (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    update_history_id INTEGER, database_name TEXT, InChIKey TEXT,
    substance_identifier TEXT, change_type TEXT, field_name TEXT,
    old_value TEXT, new_value TEXT)")
}

# 模拟 clp_cmr_meta.xlsx cmr 表头（含被 normalize_cmr_df 改名的备用 H 代码列）
cmr_source_df <- function() {
  data.frame(
    "Index No" = c("001-01-00-0", "002-01-00-0"),
    "International Chemical Identification" = c("Substance A", "Substance B"),
    "EC No" = c("200-000-0", "200-001-6"),
    "CAS No" = c("50-00-0", "75-07-0"),
    "Hazard Statement Code(s)" = c("H340", "H350"),
    "Hazard Statement Code Alternative" = c("H341 **", "H351"),
    "ATP inserted/ATP Updated" = c("CLP00", "CLP01"),
    "InChIKey" = c("ABC123XYZ", "DEF456UVW"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

# ---- 1) map_to_db_columns 蛇形对齐：超归一 + 候选表 ----

test_that("map_to_db_columns aligns snake cmr columns from camel source names", {
  db_path <- tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  make_snake_cmr_table(con)
  DBI::dbDisconnect(con)

  mapped <- fcmsafety:::map_to_db_columns(cmr_source_df(), "cmr", db_path)

  # 超归一命中：Index No -> index_no；EC No -> ec_no；CAS No -> cas_no
  expect_equal(mapped$index_no, c("001-01-00-0", "002-01-00-0"))
  expect_equal(mapped$ec_no, c("200-000-0", "200-001-6"))
  expect_equal(mapped$cas_no, c("50-00-0", "75-07-0"))
  # 候选表命中：改名后的备用 H 代码列 / ATP 列
  expect_equal(mapped$hazard_statement_codes_alt, c("H341 **", "H351"))
  expect_equal(mapped$atp_inserted_updated, c("CLP00", "CLP01"))
  # 源未提供的列（m_factors）保持 NA 且 mapped_from 记为 NA
  expect_true(all(is.na(mapped$m_factors)))
  mf <- attr(mapped, "mapped_from", exact = TRUE)
  expect_true(is.na(mf[["m_factors"]]))
  expect_equal(mf[["index_no"]], "Index No")
  unlink(db_path)
})

# ---- 2) 写库排除系统列：id/时间戳由 SQLite DEFAULT 维护 ----

test_that("write_changes_to_db lets SQLite fill id/created_at/updated_at on snake tables", {
  db_path <- tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  make_snake_cmr_table(con)
  make_snake_audit(con)
  DBI::dbDisconnect(con)

  # 与真实链路一致：先 map_to_db_columns 对齐成蛇形列，再交给写库
  added <- fcmsafety:::map_to_db_columns(cmr_source_df(), "cmr", db_path)
  changes <- list(
    total_added = nrow(added), total_removed = 0L, total_modified = 0L,
    added = added, removed = added[0, ], modified = added[0, ]
  )

  fcmsafety:::write_changes_to_db("cmr", changes,
    key_col = "index_no", fallback_col = "cas_no",
    db_path = db_path, backup = FALSE)

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  rows <- DBI::dbGetQuery(con, "SELECT id, index_no, created_at, updated_at FROM cmr")
  hist <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM update_history")
  logs <- DBI::dbGetQuery(con, "SELECT change_type, substance_identifier FROM change_log")
  DBI::dbDisconnect(con)

  expect_equal(nrow(rows), 2L)
  expect_equal(sort(rows$index_no), c("001-01-00-0", "002-01-00-0"))
  # id 自增、时间戳非空：证明插入时没有把系统列显式写 NULL
  expect_true(all(!is.na(rows$id)))
  expect_true(all(!is.na(rows$created_at)))
  expect_true(all(!is.na(rows$updated_at)))
  expect_equal(hist$n, 1L)
  # 明细标识：蛇形库没有 Substance name 列，应取 international_chemical_identification
  expect_true(any(logs$change_type == "added"))
  expect_true(all(logs$substance_identifier %in% c("Substance A", "Substance B")))
  unlink(db_path)
})

# ---- 3) run_incremental_update 对蛇形表端到端（diff + 写库 + 明细） ----

test_that("run_incremental_update applies added/modified on a snake cmr table", {
  db_path <- tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  make_snake_cmr_table(con)
  make_snake_audit(con)
  # 种子：库里已有一条 index_no = 001-01-00-0（旧 EC No 值 999-999-9）
  DBI::dbExecute(con,
    "INSERT INTO cmr (InChIKey, index_no, international_chemical_identification, ec_no, cas_no, hazard_statement_codes)
     VALUES ('ABC123XYZ', '001-01-00-0', 'Substance A', '999-999-9', '50-00-0', 'H340')")
  DBI::dbDisconnect(con)

  # 新源清单：同 index_no 的 001 行改了 ec_no（modified）+ 一条全新 002 行（added）
  new_df <- cmr_source_df()
  new_df$`EC No`[1] <- "200-000-0"

  res <- fcmsafety:::run_incremental_update(
    db_name = "cmr", new_df = new_df,
    key_col = "index_no", fallback_col = "cas_no",
    cas_col = "cas_no", name_col = "international_chemical_identification",
    content_cols = NULL,
    interactive = FALSE, auto_apply = TRUE, max_auto_changes = 99,
    enrich = FALSE, db_path = db_path, backup = FALSE
  )

  expect_true(isTRUE(res$success))
  expect_equal(res$changes$total_added, 1L)
  expect_equal(res$changes$total_removed, 0L)
  expect_equal(res$changes$total_modified, 1L)

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  rows <- DBI::dbGetQuery(con, "SELECT index_no, ec_no, created_at FROM cmr ORDER BY index_no")
  logs <- DBI::dbGetQuery(con,
    "SELECT change_type, field_name, old_value, new_value FROM change_log ORDER BY id")
  DBI::dbDisconnect(con)

  expect_equal(nrow(rows), 2L)
  expect_true(all(!is.na(rows$created_at)))  # 系统列默认值未被 NULL 覆盖
  expect_equal(rows$ec_no, c("200-000-0", "200-001-6"))
  # 明细：added 一行；modified 至少含 ec_no 的 old->new
  expect_true(any(logs$change_type == "added"))
  ec_row <- logs[logs$change_type == "modified" & logs$field_name == "ec_no", ]
  expect_equal(nrow(ec_row), 1L)
  expect_equal(ec_row$old_value, "999-999-9")
  expect_equal(ec_row$new_value, "200-000-0")
  unlink(db_path)
})

# =============================================================================
# SVHC 老轨道适配蛇形 schema：normalize 输出 / UVCB 过滤 / FK + chemicals upsert
# =============================================================================

# 真实 svhc snake 业务表 + chemicals（同构 DDL，含外键）
make_snake_svhc_env <- function(con) {
  DBI::dbExecute(con, "CREATE TABLE chemicals (
    InChIKey TEXT PRIMARY KEY, CID INTEGER, Formula TEXT, SMILES TEXT,
    IUPACName TEXT, ExactMass REAL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP)")
  DBI::dbExecute(con, "CREATE TABLE svhc (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    InChIKey TEXT REFERENCES chemicals(InChIKey),
    substance_name TEXT, description TEXT, ec_no TEXT, cas_no TEXT,
    reason_for_inclusion TEXT, date_of_inclusion TEXT, decision TEXT,
    iuclid_dataset TEXT, support_document TEXT, response_to_comments TEXT,
    remarks TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP)")
  make_snake_audit(con)
}

# 驼峰源（模拟 ECHA 导出 / Wikipedia 表头）-> normalize_svhc_df 应输出蛇形
test_that("normalize_svhc_df maps camel source to snake columns", {
  src <- data.frame(
    "Substance name" = c("Bisphenol A", "MCCP"),
    "EC No." = c("201-245-8", "-"),
    "CAS No." = c("80-05-7", "85535-84-8"),
    "Date of inclusion" = c("2011-06-20", "19/12/2011"),
    "Reason for inclusion" = c("CMR", "PBT"),
    InChIKey = c("IISBACOOKDOVLU-UHFFFAOYSA-N", NA_character_),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  out <- fcmsafety:::normalize_svhc_df(src)
  expect_true(all(c("substance_name", "ec_no", "cas_no",
                    "date_of_inclusion", "reason_for_inclusion") %in% names(out)))
  expect_false(any(c("Substance name", "CAS No.", "EC No.") %in% names(out)))
  expect_equal(out$substance_name, c("Bisphenol A", "MCCP"))
  expect_equal(out$cas_no, c("80-05-7", "85535-84-8"))
  # 占位符 "-" 转 NA；日期统一 dd/mm/yyyy（与库内 ECHA 原样透传格式一致）
  expect_true(is.na(out$ec_no[2]))
  expect_equal(out$date_of_inclusion, c("20/06/2011", "19/12/2011"))
})

test_that("diff_svhc_data filters rows without InChIKey (UVCB semantics)", {
  db_path <- tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  make_snake_svhc_env(con)
  # 老物质已在库（带 InChIKey）
  DBI::dbExecute(con,
    "INSERT INTO chemicals (InChIKey) VALUES ('OLDKEY123')")
  DBI::dbExecute(con,
    "INSERT INTO svhc (InChIKey, substance_name, cas_no) VALUES ('OLDKEY123', 'Old Sub', '266309-43-7')")
  DBI::dbDisconnect(con)

  # 新清单：老物质（内容变了）+ 全新带 InChIKey 物质 + UVCB（无 InChIKey）
  new_df <- fcmsafety:::normalize_svhc_df(data.frame(
    "Substance name" = c("Old Sub", "Brand New", "UVCB polymer"),
    "CAS No." = c("266309-43-7", "50-00-0", "9002-86-2"),
    Remarks = c("changed remark", NA_character_, NA_character_),
    InChIKey = c("OLDKEY123", "NEWKEY999", NA_character_),
    check.names = FALSE, stringsAsFactors = FALSE))
  chg <- fcmsafety:::diff_svhc_data(new_df, db_path = db_path)

  expect_equal(chg$total_added, 1L)     # Brand New
  expect_equal(chg$total_removed, 0L)
  expect_equal(chg$total_modified, 1L)  # Old Sub（remarks 变化）
  expect_true("Brand New" %in% chg$added$substance_name)
  expect_true("Old Sub" %in% chg$modified$substance_name)
  expect_false(any(grepl("UVCB", c(chg$added$substance_name, chg$modified$substance_name))))
  unlink(db_path)
})

test_that("write_svhc_to_db survives FK via chemicals upsert on snake tables", {
  db_path <- tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  make_snake_svhc_env(con)
  DBI::dbExecute(con, "INSERT INTO chemicals (InChIKey) VALUES ('OLDKEY123')")
  DBI::dbExecute(con,
    "INSERT INTO svhc (InChIKey, substance_name, cas_no, remarks)
     VALUES ('OLDKEY123', 'Old Sub', '266309-43-7', 'old remark')")
  DBI::dbDisconnect(con)

  # get_db_connection 默认 PRAGMA foreign_keys = ON：新 InChIKey 必须先进 chemicals
  new_df <- fcmsafety:::normalize_svhc_df(data.frame(
    "Substance name" = c("Old Sub", "Brand New"),
    "CAS No." = c("266309-43-7", "50-00-0"),
    Remarks = c("new remark", NA_character_),
    InChIKey = c("OLDKEY123", "NEWKEY999"),
    check.names = FALSE, stringsAsFactors = FALSE))
  chg <- fcmsafety:::diff_svhc_data(new_df, db_path = db_path)

  res <- fcmsafety:::write_svhc_to_db(new_df, chg, db_path = db_path, backup = FALSE)
  expect_equal(res$records_added, 1L)
  expect_equal(res$records_modified, 1L)

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  svhc_rows <- DBI::dbGetQuery(con,
    "SELECT substance_name, remarks FROM svhc ORDER BY substance_name")
  chem_new <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM chemicals WHERE InChIKey = 'NEWKEY999'")
  hist <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM update_history")
  logs <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM change_log")
  DBI::dbDisconnect(con)

  expect_equal(nrow(svhc_rows), 2L)
  expect_true(all(c("Old Sub", "Brand New") %in% svhc_rows$substance_name))
  # modified 已重写为新 remark（证明 DELETE+INSERT 走了 snake 列）
  expect_equal(svhc_rows$remarks[svhc_rows$substance_name == "Old Sub"], "new remark")
  # FK 未炸 = chemicals upsert 生效；审计表有记录
  expect_equal(chem_new$n, 1L)
  expect_equal(hist$n, 1L)
  expect_true(logs$n >= 2L)  # 1 added + 1 field-level modified
  unlink(db_path)
})
