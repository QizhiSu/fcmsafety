# cmr_suspect 的 diff 主键：用源的稳定标识 Index No，不用会变的物质名
#
# 背景（探针实测，2026-09-11）：
#   cmr 表与 cmr_suspect 表来自**同一个** CLP 源。cmr 用 index_no 当主键，
#   cmr_suspect 用 substance_name 当主键。跑同一份源：
#       cmr          added=820  removed=  2  modified=330
#       cmr_suspect  added=184  removed= 46  modified=311
#   45 个 removed 键拆开看：15 个只差空白（"O,O-di-methyl" 的空格位）、
#   4 个是上游把两条来源合并、26 个是上游改了标点或字符（O,O-di-methyl ->
#   O,O-dimethyl、0.5 -> 0,5、"…%" -> "...%"、"ß" -> "β"、"(+/-)" -> "(+/–)"）。
#   —— 行一条都没走，是"拿自由文本当主键"把它们判走了。
#
# 源侧事实：CLP 的 "Index No" 在 cmr_suspect 子集里 495/495 非空、495 个唯一值。
# 库里 359 行全部能从旧源 inst/clp_cmr_meta.xlsx 的 cmr_suspect 工作表补上键。

make_bare_db <- function() {
  db <- tempfile(fileext = ".db")
  con <- fcmsafety:::get_db_connection(db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  sql <- paste(readLines(fcmsafety:::find_schema_file(), warn = FALSE), collapse = "\n")
  for (s in fcmsafety:::split_sql_statements(sql)) DBI::dbExecute(con, s)
  db
}

# 只建 cmr_suspect，且**不带** index_no 列 —— 复现改动前的表结构
make_legacy_cmr_suspect_db <- function(rows) {
  db <- tempfile(fileext = ".db")
  con <- fcmsafety:::get_db_connection(db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE chemicals (InChIKey TEXT PRIMARY KEY)")
  DBI::dbExecute(con, "
    CREATE TABLE cmr_suspect (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      InChIKey TEXT NOT NULL,
      substance_name TEXT,
      cas_no TEXT,
      ec_no TEXT,
      classification TEXT,
      source TEXT,
      notes TEXT,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY (InChIKey) REFERENCES chemicals(InChIKey))")
  if (nrow(rows) > 0) {
    for (i in seq_len(nrow(rows))) DBI::dbExecute(
      con, "INSERT INTO chemicals (InChIKey) VALUES (?)",
      params = list(rows$InChIKey[i]))
    DBI::dbAppendTable(con, "cmr_suspect",
                       rows[, c("InChIKey", "substance_name")])
  }
  db
}

test_that("schema 里 cmr_suspect 有 index_no 列（CLP 的官方标识）", {
  sql <- paste(readLines(fcmsafety:::find_schema_file(), warn = FALSE),
               collapse = "\n")
  block <- regmatches(sql, regexpr(
    "(?s)CREATE TABLE cmr_suspect \\(.*?\\);", sql, perl = TRUE))
  expect_true(nzchar(block))
  expect_match(block, "index_no")
})

test_that("migrate_cmr_suspect_index_no 给老库补列并按名字回填", {
  db <- make_legacy_cmr_suspect_db(data.frame(
    InChIKey = c("AAA", "BBB", "CCC"),
    substance_name = c("1,4-dioxane", "phosmet (ISO); O,O-di-methyl ester",
                       "upstream never heard of this"),
    stringsAsFactors = FALSE))

  map <- data.frame(
    substance_name = c("1,4-dioxane", "phosmet (ISO); O,O-dimethyl ester",
                       "phosmet (ISO); O,O-di-methyl ester"),
    index_no = c("603-024-00-5", "015-101-00-5", "015-101-00-5"),
    stringsAsFactors = FALSE)

  res <- fcmsafety:::migrate_cmr_suspect_index_no(db_path = db, index_map = map)

  con <- fcmsafety:::get_db_connection(db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  got <- DBI::dbGetQuery(con, "SELECT substance_name, index_no FROM cmr_suspect
                                ORDER BY id")
  expect_equal(got$index_no, c("603-024-00-5", "015-101-00-5", NA))
  expect_equal(res$filled, 2L)
  expect_equal(res$unmatched, 1L)
})

test_that("回填优先按精确名字，压空白后才匹配的算次选", {
  db <- make_legacy_cmr_suspect_db(data.frame(
    InChIKey = c("AAA", "BBB"),
    substance_name = c("formaldehyde …%", "alpha, alpha-x"),
    stringsAsFactors = FALSE))
  map <- data.frame(
    substance_name = c("formaldehyde … %", "alpha,alpha-x"),
    index_no = c("605-001-00-5", "601-000-00-1"),
    stringsAsFactors = FALSE)
  fcmsafety:::migrate_cmr_suspect_index_no(db_path = db, index_map = map)
  con <- fcmsafety:::get_db_connection(db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  got <- DBI::dbGetQuery(con, "SELECT index_no FROM cmr_suspect ORDER BY id")
  expect_equal(got$index_no, c("605-001-00-5", "601-000-00-1"))
})

test_that("migrate_cmr_suspect_index_no 幂等，重复跑不覆盖已填好的键", {
  db <- make_legacy_cmr_suspect_db(data.frame(
    InChIKey = "AAA", substance_name = "1,4-dioxane", stringsAsFactors = FALSE))
  map <- data.frame(substance_name = "1,4-dioxane",
                    index_no = "603-024-00-5", stringsAsFactors = FALSE)

  fcmsafety:::migrate_cmr_suspect_index_no(db_path = db, index_map = map)
  res2 <- fcmsafety:::migrate_cmr_suspect_index_no(db_path = db, index_map = map)

  con <- fcmsafety:::get_db_connection(db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_equal(DBI::dbGetQuery(con, "SELECT index_no FROM cmr_suspect")$index_no,
               "603-024-00-5")
  expect_equal(res2$filled, 0L)
})

# ---- 键切换到 index_no 之后，上游改名不再被误判 ------------------------------

diff_by_index_no <- function(db_rows, src_rows) {
  db <- make_legacy_cmr_suspect_db(db_rows)
  on.exit(unlink(db), add = TRUE)
  con <- fcmsafety:::get_db_connection(db)
  DBI::dbExecute(con, "ALTER TABLE cmr_suspect ADD COLUMN index_no TEXT")
  for (i in seq_len(nrow(db_rows))) {
    DBI::dbExecute(con, "UPDATE cmr_suspect SET index_no = ? WHERE id = ?",
                   params = list(db_rows$index_no[i], i))
  }
  DBI::dbDisconnect(con)
  src <- as.data.frame(src_rows, stringsAsFactors = FALSE)
  fcmsafety:::diff_incremental(src, db_rows, "index_no", "cas_no", NULL,
                               cas_col = "cas_no")$total_removed
}

test_that("上游只改了名字里的标点，用 index_no 当键就不再判 removed", {
  # 库里是旧拼写，源里是上游改过的新拼写；物质本体是同一条
  db_rows <- data.frame(
    InChIKey = c("AAA", "BBB"),
    substance_name = c("phosmet (ISO); O,O-di-methyl phosphorodithioate",
                       "1,4-dioxane"),
    index_no = c("015-101-00-5", "603-024-00-5"),
    cas_no = c(NA_character_, NA_character_),
    stringsAsFactors = FALSE)
  src_rows <- data.frame(
    InChIKey = c("AAA", "BBB"),
    substance_name = c("phosmet (ISO); O,O-dimethyl phosphorodithioate",
                       "1,4-dioxane"),
    index_no = c("015-101-00-5", "603-024-00-5"),
    cas_no = c(NA_character_, NA_character_),
    stringsAsFactors = FALSE)

  expect_equal(diff_by_index_no(db_rows, src_rows), 0L)
  # 对照：用 substance_name 当键，同一条物质会被判"删一条 + 加一条"
  d <- fcmsafety:::diff_incremental(
    as.data.frame(src_rows, stringsAsFactors = FALSE),
    as.data.frame(db_rows, stringsAsFactors = FALSE),
    "substance_name", "cas_no", NULL, cas_col = "cas_no")
  expect_equal(d$total_removed, 1L)
  expect_equal(d$total_added, 1L)
})

test_that("上游真删了一条时，index_no 当键仍然能报出来", {
  db_rows <- data.frame(
    InChIKey = c("AAA", "BBB", "CCC"),
    substance_name = c("a", "b", "c"),
    index_no = c("601-000-00-1", "601-000-00-2", "601-000-00-3"),
    cas_no = NA_character_, stringsAsFactors = FALSE)
  src_rows <- data.frame(
    InChIKey = c("AAA", "CCC"),
    substance_name = c("a", "c"),
    index_no = c("601-000-00-1", "601-000-00-3"),
    cas_no = NA_character_, stringsAsFactors = FALSE)
  expect_equal(diff_by_index_no(db_rows, src_rows), 1L)
})

test_that("源带 Index No 列时，映射到库的 index_no 上", {
  db <- make_bare_db()
  con <- fcmsafety:::get_db_connection(db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  src <- data.frame(
    `Index No` = c("015-101-00-5", "603-024-00-5"),
    `International Chemical Identification` = c("phosmet", "1,4-dioxane"),
    `CAS No` = c(NA, "123-91-1"),
    check.names = FALSE, stringsAsFactors = FALSE)
  mapped <- fcmsafety:::map_to_db_columns(src, "cmr_suspect", db)
  expect_equal(as.character(mapped$index_no),
               c("015-101-00-5", "603-024-00-5"))
})
