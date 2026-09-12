# assign_toxicity: toxtree 可选化（P1-③）与校验顺序回归
# 全部离线：临时 SQLite（chemicals + svhc）+ 手工 toxtree CSV，不碰 jar

make_assign_tox_fixture <- function(hit_ik = "AAAABBBBCCCCDD-UHFFFAOYSA-N") {
  db_path <- tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbExecute(con, "CREATE TABLE chemicals (InChIKey TEXT PRIMARY KEY)")
  DBI::dbExecute(con, "CREATE TABLE svhc (InChIKey TEXT)")
  DBI::dbExecute(con, "INSERT INTO svhc (InChIKey) VALUES (?)",
                 params = list(hit_ik))
  DBI::dbDisconnect(con)
  db_path
}

test_that("no SMILES + missing toxtree file: skips Cramer, still matches lists", {
  db_path <- make_assign_tox_fixture()
  d <- data.frame(
    NAME = c("Hit compound", "Unknown compound"),
    CAS = c("", ""),
    InChIKey = c("AAAABBBBCCCCDD-UHFFFAOYSA-N", "ZZZZYYYYXXXXWW-VVHHHHHHHH-N"),
    concentration_mg_per_kg = c(0.5, 1.2),
    stringsAsFactors = FALSE
  )
  missing_tox <- tempfile(fileext = ".csv")   # does not exist

  # must NOT error anymore (was stop before P1-③)
  res <- assign_toxicity(d, toxtree_result = missing_tox, db_path = db_path)

  expect_true("Cramer_rules" %in% names(res))       # column still produced
  expect_true(all(res$Cramer_rules == "-"))          # all NA -> "-"
  expect_identical(res$SVHC[1], "Y")                 # list matching intact
  expect_true(all(res$SVHC[-1] == "-"))
  expect_equal(nrow(res), 2)

  unlink(db_path)
})

test_that("with SMILES + existing toxtree file: Cramer backfilled correctly", {
  db_path <- make_assign_tox_fixture()
  d <- data.frame(
    NAME = c("Ethanol", "Formaldehyde"),
    CAS = c("64-17-5", "50-00-0"),
    InChIKey = c("AAAABBBBCCCCDD-UHFFFAOYSA-N", "WSFSSNUMVMOOMR-UHFFFAOYSA-N"),
    SMILES = c("CCO", "C=O"),
    stringsAsFactors = FALSE
  )
  tox_csv <- tempfile(fileext = ".csv")
  utils::write.csv(
    data.frame(
      NAME = c("Ethanol", "Formaldehyde"),
      CAS = c("64-17-5", "50-00-0"),
      SMILES = c("CCO", "C=O"),
      Cramer.rules = c("Low (Class I)", "High (Class III)"),
      stringsAsFactors = FALSE
    ),
    tox_csv, row.names = FALSE
  )

  res <- assign_toxicity(d, toxtree_result = tox_csv, db_path = db_path)

  expect_identical(res$Cramer_rules,
                   c("Low (Class I)", "High (Class III)"))
  expect_identical(res$SVHC[1], "Y")                 # list matching still works
  unlink(db_path)
})

test_that("missing InChIKey column errors with InChIKey message (before toxtree check)", {
  db_path <- make_assign_tox_fixture()
  d <- data.frame(NAME = "Ethanol", SMILES = "CCO", stringsAsFactors = FALSE)
  missing_tox <- tempfile(fileext = ".csv")   # does not exist
  expect_error(
    assign_toxicity(d, toxtree_result = missing_tox, db_path = db_path),
    "InChIKey"
  )
  unlink(db_path)
})

test_that("auto-rerun when file missing but SMILES present (needs jar cache)", {
  skip_on_cran()
  skip_if(!nzchar(Sys.which("java")), "Java not available")
  cache_dir <- tools::R_user_dir("fcmsafety", "cache")
  skip_if(!dir.exists(file.path(cache_dir, "toxtree_app")),
          "Toxtree app not in cache")

  db_path <- make_assign_tox_fixture()
  d <- data.frame(
    NAME = c("Ethanol", "Formaldehyde"),
    CAS = c("64-17-5", "50-00-0"),
    InChIKey = c("AAAABBBBCCCCDD-UHFFFAOYSA-N", "WSFSSNUMVMOOMR-UHFFFAOYSA-N"),
    SMILES = c("CCO", "C=O"),
    stringsAsFactors = FALSE
  )
  missing_tox <- tempfile(fileext = ".csv")   # does not exist -> auto run
  res <- assign_toxicity(d, toxtree_result = missing_tox, db_path = db_path)

  expect_true(all(!is.na(res$Cramer_rules) & res$Cramer_rules != "-"))
  unlink(db_path)
})

# ---------------------------------------------------------------------------
# CMR 定级证据（H 码）
# 规则表（inst/toxicity_levels.png）：H340/H350/H360 -> 等级 V，
# H341/H351/H361 -> 等级 IV。之前 assign_toxicity 只输出 CMR=Y，拿不到具体码。
# ---------------------------------------------------------------------------

make_cmr_fixture <- function() {
  db_path <- tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbExecute(con, "CREATE TABLE chemicals (InChIKey TEXT PRIMARY KEY)")
  DBI::dbExecute(con, "CREATE TABLE svhc (InChIKey TEXT)")
  DBI::dbExecute(con, "CREATE TABLE cmr (InChIKey TEXT, hazard_statement_codes TEXT)")
  DBI::dbExecute(con, "CREATE TABLE cmr_suspect (InChIKey TEXT, substance_name TEXT)")

  ins_cmr <- function(ik, codes) DBI::dbExecute(
    con, "INSERT INTO cmr (InChIKey, hazard_statement_codes) VALUES (?, ?)",
    params = list(ik, codes))

  # Carc. 1B + Muta. 2：V 类与 IV 类码同时存在
  ins_cmr("CARC1BMUTA2AA-UHFFFAOYSA-N", "H350\r\r\nH341\r\r\nH302\r\r\n")
  # 只有 V 类码，且带吸入途径后缀
  ins_cmr("CARC1BONLYAA-UHFFFAOYSA-N", "H350i\r\r\nH400\r\r\n")
  # 同一 InChIKey 两行（同族条目），H 码互补 -> 必须合并，不能只取第一行
  ins_cmr("DUPLICATEKEYAA-UHFFFAOYSA-N", "H350\r\r\n")
  ins_cmr("DUPLICATEKEYAA-UHFFFAOYSA-N", "H360Df\r\r\n")

  # 只出现在 cmr_suspect（H341/H351/H361），cmr 表里没有
  DBI::dbExecute(con, "INSERT INTO cmr_suspect (InChIKey, substance_name) VALUES (?, ?)",
                 params = list("MUTA2ONLYAAA-UHFFFAOYSA-N", "suspect only"))

  DBI::dbDisconnect(con)
  db_path
}

test_that("extract_cmr_h_codes normalises CLP suffixes and lists tier V first", {
  x <- c(
    "H350i\r\r\nH330\r\r\n",         # 后缀 + 无关码
    "H360Df\r\r\nH361f ***\r\r\n",   # 组合码 + 星号（特定浓度限值标记）
    "H361fd\r\r\nH341\r\r\n",        # 输入里 IV 类在前，输出仍按 V 类优先排序
    "H302\r\r\nH317\r\r\n",          # 与 CMR 无关
    NA_character_,
    ""
  )
  expect_identical(
    fcmsafety:::extract_cmr_h_codes(x),
    c("H350", "H360; H361", "H341; H361", NA_character_, NA_character_, NA_character_)
  )
})

test_that("assign_toxicity reports the CMR evidence codes behind each flag", {
  db_path <- make_cmr_fixture()
  d <- data.frame(
    NAME = c("dual", "tier V only", "duplicate rows", "suspect only", "unknown"),
    InChIKey = c("CARC1BMUTA2AA-UHFFFAOYSA-N", "CARC1BONLYAA-UHFFFAOYSA-N",
                 "DUPLICATEKEYAA-UHFFFAOYSA-N", "MUTA2ONLYAAA-UHFFFAOYSA-N",
                 "NOSUCHKEYAAAA-UHFFFAOYSA-N"),
    stringsAsFactors = FALSE
  )
  res <- assign_toxicity(d, toxtree_result = tempfile(fileext = ".csv"),
                         db_path = db_path)

  expect_true("CMR_H_codes" %in% names(res))
  # 同键两行合并；V 类码在 IV 类之前；无命中为 "-"
  expect_identical(res$CMR_H_codes,
                   c("H350; H341", "H350", "H350; H360", "-", "-"))
  # 两个标志位语义不变
  expect_identical(res$CMR, c("Y", "Y", "Y", "-", "-"))
  expect_identical(res$CMR_suspect, c("-", "-", "-", "Y", "-"))
  unlink(db_path)
})

test_that("missing cmr table degrades to '-' instead of failing", {
  db_path <- make_assign_tox_fixture()   # 该夹具不建 cmr / cmr_suspect 表
  d <- data.frame(NAME = "x", InChIKey = "AAAABBBBCCCCDD-UHFFFAOYSA-N",
                  stringsAsFactors = FALSE)
  res <- assign_toxicity(d, toxtree_result = tempfile(fileext = ".csv"),
                         db_path = db_path)

  expect_identical(res$CMR_H_codes, "-")
  expect_identical(res$CMR, "-")
  unlink(db_path)
})

