# Regression tests for CAS canonicalization and the SVHC key / backfill /
# delete logic around the "0266309-43-7 vs 266309-43-7" format mismatch bug.
# In that bug, format mismatch made backfill fail, so hundreds of old
# substances were misclassified as new and looked up on PubChem.
# These tests use temp SQLite dbs so they run offline and fast.
# Column names follow the rebuilt snake schema (svhc 业务表).

svhc_test_cols <- c(
  "substance_name", "description", "ec_no", "cas_no",
  "reason_for_inclusion", "date_of_inclusion", "decision", "iuclid_dataset",
  "support_document", "response_to_comments", "remarks", "CID", "Formula",
  "SMILES", "InChIKey", "IUPACName", "ExactMass"
)

make_svhc_df <- function(...) {
  rows <- list(...)
  out <- data.frame(matrix(NA_character_, nrow = length(rows), ncol = length(svhc_test_cols)),
                    stringsAsFactors = FALSE)
  names(out) <- svhc_test_cols
  for (i in seq_along(rows)) {
    for (nm in names(rows[[i]])) out[[nm]][i] <- rows[[i]][[nm]]
  }
  out
}

# 真实 svhc snake 业务表同构（11 内容列 + InChIKey + 系统列）
make_svhc_table <- function(con) {
  DBI::dbExecute(con, 'CREATE TABLE svhc (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    InChIKey TEXT,
    substance_name TEXT, description TEXT, ec_no TEXT, cas_no TEXT,
    reason_for_inclusion TEXT, date_of_inclusion TEXT, decision TEXT,
    iuclid_dataset TEXT, support_document TEXT, response_to_comments TEXT,
    remarks TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  )')
}

# ---- canonicalize_cas() ----

test_that("canonicalize_cas strips leading zeros (the 0266309 vs 266309 bug)", {
  expect_equal(fcmsafety:::canonicalize_cas("0266309-43-7"), "266309-43-7")
  expect_equal(fcmsafety:::canonicalize_cas("0000050-00-0"), "50-00-0")
})

test_that("canonicalize_cas is idempotent", {
  expect_equal(fcmsafety:::canonicalize_cas("266309-43-7"), "266309-43-7")
  expect_equal(fcmsafety:::canonicalize_cas("50-00-0"), "50-00-0")
})

test_that("canonicalize_cas pads middle segment to 2 digits", {
  expect_equal(fcmsafety:::canonicalize_cas("50-0-0"), "50-00-0")
})

test_that("canonicalize_cas handles multiple CAS separated by ; or newline", {
  expect_equal(fcmsafety:::canonicalize_cas("0266309-43-7;0000050-00-0"),
               "266309-43-7;50-00-0")
  expect_equal(fcmsafety:::canonicalize_cas("0266309-43-7\n0000050-00-0"),
               "266309-43-7;50-00-0")
  expect_equal(fcmsafety:::canonicalize_cas("266309-43-7; 50-00-0"), "266309-43-7;50-00-0")
})

test_that("canonicalize_cas blanks and placeholders become NA", {
  expect_true(is.na(fcmsafety:::canonicalize_cas(NA_character_)))
  expect_true(is.na(fcmsafety:::canonicalize_cas("")))
  expect_true(is.na(fcmsafety:::canonicalize_cas("-")))
  expect_true(is.na(fcmsafety:::canonicalize_cas("n/a")))
})

test_that("canonicalize_cas leaves non-standard values as-is", {
  expect_equal(fcmsafety:::canonicalize_cas("12345"), "12345")
  expect_equal(fcmsafety:::canonicalize_cas(" 266309-43-7 "), "266309-43-7")
})

# ---- svhc_key_of() ----

test_that("svhc_key_of prefers InChIKey regardless of CAS format", {
  df <- make_svhc_df(
    list(substance_name = "A", cas_no = "266309-43-7", InChIKey = "ABC123"),
    list(substance_name = "A", cas_no = "0266309-43-7", InChIKey = "ABC123")
  )
  expect_equal(fcmsafety:::svhc_key_of(df), c("ABC123", "ABC123"))
})

test_that("svhc_key_of CAS fallback is format-insensitive", {
  df <- make_svhc_df(
    list(substance_name = "A", cas_no = "0266309-43-7"),
    list(substance_name = "A", cas_no = "266309-43-7")
  )
  expect_equal(fcmsafety:::svhc_key_of(df), c("CAS:266309-43-7", "CAS:266309-43-7"))
  df2 <- make_svhc_df(
    list(substance_name = "A", cas_no = "0000050-00-0"),
    list(substance_name = "A", cas_no = "50-00-0")
  )
  expect_equal(fcmsafety:::svhc_key_of(df2), c("CAS:50-00-0", "CAS:50-00-0"))
})

test_that("svhc_key_of falls back to name when CAS is blank", {
  df <- make_svhc_df(list(substance_name = "Sub A"), list(substance_name = "Sub B"))
  expect_equal(fcmsafety:::svhc_key_of(df), c("NAME:Sub A", "NAME:Sub B"))
})

# ---- backfill regression (the original misclassification scenario) ----

test_that("backfill_meta_from_db matches across CAS formats", {
  db_path <- tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  make_svhc_table(con)
  # 蛇形业务表：老库行的化学信息在 chemicals（此处只关心 InChIKey 回填）
  DBI::dbExecute(con,
    'INSERT INTO svhc (substance_name, cas_no, InChIKey) VALUES (?, ?, ?)',
    params = list("Old substance", "266309-43-7", "OLDKEY123"))
  DBI::dbDisconnect(con)

  # 新清单：带前导 0 的 CAS，InChIKey 为空（bug 场景：回填失败 -> 误查 PubChem）
  new_df <- make_svhc_df(
    list(substance_name = "Old substance", cas_no = "0266309-43-7")
  )
  out <- fcmsafety:::backfill_meta_from_db(new_df, "svhc", "cas_no", db_path = db_path)
  expect_false(is.na(out[["InChIKey"]][1]))
  expect_equal(out[["InChIKey"]][1], "OLDKEY123")
  unlink(db_path)
})

# ---- write_svhc_to_db delete regression ----

test_that("write_svhc_to_db deletes rows whose stored CAS differs in format", {
  db_path <- tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  make_svhc_table(con)
  # 库里 InChIKey 为空、CAS 带前导 0 的老行
  DBI::dbExecute(con,
    'INSERT INTO svhc (substance_name, cas_no) VALUES (?, ?)',
    params = list("Legacy row", "0266309-43-7"))
  DBI::dbDisconnect(con)

  # removed 行：来自库里（原始格式 CAS），svhc_key_of 会归一化 CAS 键
  removed_df <- make_svhc_df(list(substance_name = "Legacy row", cas_no = "0266309-43-7"))
  empty <- make_svhc_df()
  changes <- list(
    total_added = 0L, total_removed = 1L, total_modified = 0L,
    added = empty, removed = removed_df, modified = empty
  )
  fcmsafety:::write_svhc_to_db(empty, changes, db_path = db_path, backup = FALSE)

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  remain <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM svhc")
  DBI::dbDisconnect(con)
  expect_equal(remain$n, 0L)
  unlink(db_path)
})
