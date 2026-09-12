# run_screening: 总控入口（P1-1）—— 把 prepare_input() 与 assign_toxicity() 串起来
#
# 本文件只测「调度层」的契约，不重复测两步各自的内部逻辑
# （那两块分别由 test-prepare-input.R / test-assign-toxicity.R 覆盖）：
#   1) 两步都跑到，且入参真正透传下去（不是只写进签名里）
#   2) prepare_input() 的补全报告被透传到返回值上
#   3) 一行都没解析出 InChIKey 时必须告警 —— 否则用户会把全 "-" 的结果表
#      误读成"这些物质都干净"，这正是交接表里点名的静默失败风险
#   4) 安全边界：不暴露也不传递 check_updates / auto_update
#      （assign_toxicity() 是确定性匹配函数，不得默认联网、不得默认改库）
#   5) online 默认 FALSE
# 全部离线：端到端用例用「输入自带 InChIKey + 现成 toxtree CSV」绕开 CDK 与 jar。

make_rs_fixture <- function(hit_ik = "AAAABBBBCCCCDD-UHFFFAOYSA-N") {
  db_path <- tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  # 列要建全：prepare_input() 会按这些列名查 chemicals，缺列会刷 warning
  DBI::dbExecute(con, paste0(
    "CREATE TABLE chemicals (InChIKey TEXT PRIMARY KEY, CID TEXT, ",
    "Formula TEXT, SMILES TEXT, IUPACName TEXT, ExactMass REAL)"
  ))
  DBI::dbExecute(con, "CREATE TABLE svhc (InChIKey TEXT, substance_name TEXT)")
  DBI::dbExecute(con, "INSERT INTO svhc (InChIKey, substance_name) VALUES (?, ?)",
                 params = list(hit_ik, "Hit substance"))
  DBI::dbDisconnect(con)
  db_path
}

make_rs_tox_csv <- function() {
  tox_csv <- tempfile(fileext = ".csv")
  utils::write.csv(
    data.frame(NAME = "Ethanol", CAS = "64-17-5", SMILES = "CCO",
               Cramer.rules = "Low (Class I)", stringsAsFactors = FALSE),
    tox_csv, row.names = FALSE
  )
  tox_csv
}

# 构造一个带 prepare_report 的假「补全结果」，供 mock 用
fake_prepared <- function(unresolved = 0L, total = 1L) {
  out <- data.frame(
    NAME = "Ethanol", CAS = "64-17-5", SMILES = "CCO",
    InChIKey = "LFQSCWFLJHTTHZ-UHFFFAOYSA-N",
    stringsAsFactors = FALSE
  )
  attr(out, "prepare_report") <- list(
    total = total, unresolved = unresolved,
    unresolved_rows = data.frame(row = integer(0), NAME = character(0),
                                 SMILES = character(0), reason = character(0),
                                 stringsAsFactors = FALSE)
  )
  out
}

test_that("run_screening 两步都跑到，且入参真正透传", {
  seen <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    prepare_input = function(data, ...) {
      seen$prep_args <- list(...)
      seen$prep_data <- data
      fake_prepared()
    },
    assign_toxicity = function(data, ...) {
      seen$assign_args <- list(...)
      seen$assign_data <- data
      data
    },
    .package = "fcmsafety"
  )

  d <- data.frame(NAME = "Ethanol", SMILES = "CCO", stringsAsFactors = FALSE)
  run_screening(d, online = FALSE, output_file = "out.xlsx",
                toxtree_result = "tox.csv", group_membership = TRUE,
                delay = 0.1, db_path = "fake.db", verbose = FALSE)

  # 第一步：原始输入原样进去
  expect_identical(seen$prep_data, d)
  expect_false(seen$prep_args$online)          # 默认/显式都是不联网
  expect_equal(seen$prep_args$delay, 0.1)
  expect_identical(seen$prep_args$db_path, "fake.db")
  expect_false(seen$prep_args$verbose)

  # 第二步：拿到的是 prepare_input() 的输出，不是原始输入
  expect_true("InChIKey" %in% names(seen$assign_data))
  expect_identical(seen$assign_args$output_file, "out.xlsx")
  expect_identical(seen$assign_args$toxtree_result, "tox.csv")
  expect_true(seen$assign_args$group_membership)
  expect_identical(seen$assign_args$db_path, "fake.db")
})

test_that("prepare_input 的补全报告透传到返回值上", {
  rep_obj <- list(total = 1L, unresolved = 0L)
  prepared <- fake_prepared()
  attr(prepared, "prepare_report") <- rep_obj

  testthat::local_mocked_bindings(
    prepare_input = function(data, ...) prepared,
    assign_toxicity = function(data, ...) data,
    .package = "fcmsafety"
  )

  res <- run_screening(data.frame(NAME = "x", SMILES = "C", stringsAsFactors = FALSE),
                       verbose = FALSE)
  expect_identical(attr(res, "prepare_report"), rep_obj)
})

test_that("一行都没解析出 InChIKey 时必须告警", {
  testthat::local_mocked_bindings(
    prepare_input = function(data, ...) fake_prepared(unresolved = 2L, total = 2L),
    assign_toxicity = function(data, ...) data,
    .package = "fcmsafety"
  )
  d <- data.frame(NAME = c("a", "b"), SMILES = c("C", "CC"), stringsAsFactors = FALSE)
  expect_warning(
    run_screening(d, verbose = FALSE),
    "InChIKey"
  )
})

test_that("只是部分未解析时不告警（避免狼来了）", {
  testthat::local_mocked_bindings(
    prepare_input = function(data, ...) fake_prepared(unresolved = 1L, total = 3L),
    assign_toxicity = function(data, ...) data,
    .package = "fcmsafety"
  )
  d <- data.frame(NAME = c("a", "b", "c"), SMILES = c("C", "CC", "CCC"),
                  stringsAsFactors = FALSE)
  expect_no_warning(run_screening(d, verbose = FALSE))
})

test_that("安全边界：不暴露 check_updates / auto_update，online 默认 FALSE", {
  fmls <- formals(run_screening)
  expect_false("check_updates" %in% names(fmls))
  expect_false("auto_update" %in% names(fmls))
  expect_false("enrich" %in% names(fmls))
  expect_false(eval(fmls$online))          # 默认不联网
})

test_that("端到端：自带 InChIKey 的输入两步走通并命中 SVHC（全离线）", {
  hit_ik <- "AAAABBBBCCCCDD-UHFFFAOYSA-N"
  db_path <- make_rs_fixture(hit_ik)
  tox_csv <- make_rs_tox_csv()
  on.exit(unlink(c(db_path, tox_csv)), add = TRUE)

  d <- data.frame(
    NAME = "Ethanol", CAS = "64-17-5", SMILES = "CCO",
    InChIKey = hit_ik, stringsAsFactors = FALSE
  )
  res <- run_screening(d, toxtree_result = tox_csv, db_path = db_path,
                       verbose = FALSE)

  expect_equal(nrow(res), 1L)
  expect_identical(res$SVHC[1], "Y")                    # 法规匹配走到位
  expect_identical(res$Cramer_rules[1], "Low (Class I)") # Cramer 接得上
  rep <- attr(res, "prepare_report")
  expect_type(rep, "list")
  expect_equal(rep$unresolved, 0L)
})
