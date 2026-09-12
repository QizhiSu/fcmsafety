# update_database_auto()（一键更新总入口）回归测试。
#
# 覆盖点：
#   1) resolve_db_names：databases 参数解析——"all" 展开为全部源、
#      子集原样返回、非法名报错；
#   2) 调度冒烟：多库调度不中断、单库失败只记一笔、汇总表结构正确。
#   （真实联网 / 真实源文件的 ok 分支不在自动测试里跑，需真库手动冒烟。）

# ---- 1) databases 参数解析 ----

test_that("resolve_db_names expands 'all' to the five sources", {
  dbs <- fcmsafety:::resolve_db_names("all")
  expect_equal(dbs, c("cmr", "cmr_suspect", "iarc", "eu_sml", "svhc"))
})

test_that("resolve_db_names passes a valid subset through unchanged", {
  expect_equal(fcmsafety:::resolve_db_names(c("cmr", "svhc")), c("cmr", "svhc"))
  expect_equal(fcmsafety:::resolve_db_names("iarc"), "iarc")
})

test_that("resolve_db_names stops on unknown database names", {
  expect_error(fcmsafety:::resolve_db_names("clp"), "Unknown database")
  expect_error(fcmsafety:::resolve_db_names(c("cmr", "nope")), "Unknown database")
})

# ---- 2) 调度冒烟：失败不中断 + 汇总结构 ----

test_that("update_database_auto runs all requested dbs and returns a summary", {
  # db_path 指向不存在的临时库：即使某库意外走到写库也不会碰真实 inst 库
  db_path <- tempfile(fileext = ".db")

  # source = "local" + 无本地源文件（tests 环境无 inst/）→ 该库走 failed 分支；
  # 正好验证"单库失败只记一笔、不中断、汇总照常返回"。
  res <- fcmsafety::update_database_auto(
    databases = c("cmr", "eu_sml"),
    source = "local",
    enrich = FALSE,
    interactive = FALSE, auto_apply = TRUE,
    db_path = db_path, backup = FALSE
  )

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2L)
  expect_equal(res$database, c("cmr", "eu_sml"))
  expect_true(all(c("status", "added", "removed", "modified", "message") %in% names(res)))
  expect_true(all(res$status %in% c("ok", "failed")))
  # 失败行的 message 非空（错误原因被记录）；ok 行 message 可为空/说明
  failed_rows <- res$status == "failed"
  if (any(failed_rows)) {
    expect_true(all(nzchar(res$message[failed_rows])))
  }
  unlink(db_path)
})

test_that("update_database_auto validates databases argument", {
  expect_error(
    fcmsafety::update_database_auto(databases = "not_a_db"),
    "Unknown database"
  )
})
