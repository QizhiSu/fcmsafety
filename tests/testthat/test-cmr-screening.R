# CMR 子集筛选回归（fetch_source_data 的 H 码筛选）。
#
# 背景：这两个函数的 H 码筛选原先只在 source = "download" 分支执行，local
# 分支（含"手动放入 clp_new.xlsx"和回退 clp_cmr_meta.xlsx）直接返回整表，
# 会把非 CMR 物质（只要 InChIKey 非空）写进 cmr 表。现在两条路径一律筛选。
#
# 夹具用 CSV：read_source_table() 对 .csv 走 read.csv 分支，不需要写 xlsx。

make_clp_csv <- function() {
  p <- tempfile(fileext = ".csv")
  utils::write.csv(
    data.frame(
      `Index No` = c("001-001-00-1", "002-002-00-2", "003-003-00-3", "004-004-00-4"),
      `Hazard Statement Code(s)` = c("H350\r\r\nH302", "H361f\r\r\nH317",
                                     "H302\r\r\nH317", "H340\r\r\n"),
      InChIKey = c("KEYVAAA", "KEYIVAAA", "KEYNONE", "KEYVB"),
      check.names = FALSE,
      stringsAsFactors = FALSE
    ),
    p, row.names = FALSE
  )
  p
}

test_that("fetch_source_data(cmr) screens the local source too", {
  p <- make_clp_csv()
  df <- fcmsafety:::fetch_source_data("cmr", source = "local", new_file = p,
                                      inst_dir = tempdir())

  # 只有含 H340/H350/H360 的行留下；纯 H302 行必须被剔除
  expect_setequal(df$InChIKey, c("KEYVAAA", "KEYVB"))
  unlink(p)
})

test_that("fetch_source_data(cmr_suspect) screens the local source too", {
  p <- make_clp_csv()
  df <- fcmsafety:::fetch_source_data("cmr_suspect", source = "local", new_file = p,
                                      inst_dir = tempdir())

  expect_setequal(df$InChIKey, "KEYIVAAA")
  unlink(p)
})

test_that("screen_clp keeps already-screened input unchanged (idempotent)", {
  d <- data.frame(
    `Index No` = c("001-001-00-1", "002-002-00-2"),
    `Hazard Statement Code(s)` = c("H350\r\r\nH341", "H360Df"),
    InChIKey = c("A", "B"),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  once <- fcmsafety:::screen_clp(d, "cmr")
  twice <- fcmsafety:::screen_clp(once, "cmr")
  expect_equal(nrow(once), 2L)
  expect_identical(twice$InChIKey, once$InChIKey)
})

test_that("screen_clp errors loudly when the hazard code column is absent", {
  d <- data.frame(`Index No` = "001-001-00-1", InChIKey = "A",
                  check.names = FALSE, stringsAsFactors = FALSE)
  expect_error(fcmsafety:::screen_clp(d, "cmr"), "Hazard Statement Code")
})
