# 一键更新的两道守卫：防「积压点击」重跑 + 预演结果判读。
#
# 背景（2026-09-11 页面假死事故）：Shiny 的**入站**消息在 R 阻塞期间到不了主线程，
# 点击会排队，等任务跑完才补送。原来的守卫只看 values$quick_busy，可任务结束时
# 它早已复位成 FALSE，于是积压的点击会一连再跑好几轮全量更新 —— 用户永远等不到
# 页面空闲的那一刻，体感就是"按钮全哑、再也恢复不了"。
#
# 两个纯函数（app 里只做调用，判读逻辑全在这里）：
#   should_drop_run_request()：按"上一轮结束到现在过了多久"丢弃积压请求；
#   summarise_update_run()：把 update_database_auto() 的汇总翻译成
#     "哪些库能写、哪些会被闸门拦下、为什么"。
#
# 判读里有个坑必须钉死：**dry-run 时每个库的 status 都是 "failed"**
# （run_incremental_update() 在 auto_apply = FALSE 分支返回 done(FALSE, NULL, ...)），
# 但三个计数列是齐的 —— 不能据此判成"运行失败"。真正的失败是 tryCatch 兜底那条路：
# 三个计数列全 NA。用 status 判失败 = 所有库都报错，等于没判。

# ---- 1) should_drop_run_request：防积压点击 ----

test_that("从来没跑过时不会丢弃请求", {
  expect_false(fcmsafety:::should_drop_run_request(NULL))
  expect_false(fcmsafety:::should_drop_run_request(as.POSIXct(NA)))
})

test_that("刚结束后的积压点击被丢弃", {
  end <- as.POSIXct("2026-09-11 21:00:00", tz = "UTC")
  expect_true(fcmsafety:::should_drop_run_request(end, now = end + 1))
  expect_true(fcmsafety:::should_drop_run_request(end, now = end + 9))
})

test_that("过了宽限期就放行：用户真想再跑一次不该被拦", {
  end <- as.POSIXct("2026-09-11 21:00:00", tz = "UTC")
  expect_false(fcmsafety:::should_drop_run_request(end, now = end + 10))
  expect_false(fcmsafety:::should_drop_run_request(end, now = end + 61))
})

test_that("宽限期可调，边界取不丢（闭区间外）", {
  end <- as.POSIXct("2026-09-11 21:00:00", tz = "UTC")
  expect_true(fcmsafety:::should_drop_run_request(end, now = end + 5, grace_secs = 30))
  expect_false(fcmsafety:::should_drop_run_request(end, now = end + 30, grace_secs = 30))
  expect_false(fcmsafety:::should_drop_run_request(end, now = end + 5, grace_secs = 1))
})

test_that("时钟回拨或非法输入时放行，不误伤正常请求", {
  end <- as.POSIXct("2026-09-11 21:00:00", tz = "UTC")
  expect_false(fcmsafety:::should_drop_run_request(end, now = end - 30))
  expect_false(fcmsafety:::should_drop_run_request("2026-09-11 21:00:00"))
})

# ---- 2) summarise_update_run：预演结果判读 ----

# 造一张和 update_database_auto() 返回值同形的汇总表
make_summary <- function(database, status, added, removed, modified,
                        message = "") {
  n <- length(database)
  data.frame(
    database = database,
    status   = rep_len(status, n),
    added    = added,
    removed  = removed,
    modified = modified,
    message  = rep_len(message, n),
    stringsAsFactors = FALSE)
}

test_that("拿不到结果时给出空判读而不是报错", {
  s <- fcmsafety:::summarise_update_run(NULL)
  expect_false(s$has_changes)
  expect_equal(s$n_writable, 0L)
  expect_equal(nrow(s$table), 0L)
  expect_true(nzchar(s$headline))
})

test_that("全库零变更：不写库，且说清是'没有变更'不是'被拦下'", {
  df <- make_summary(c("cmr", "iarc"), "failed", c(0L, 0L), c(0L, 0L), c(0L, 0L),
                     "Dry run (no apply)")
  s <- fcmsafety:::summarise_update_run(df)
  expect_false(s$has_changes)
  expect_equal(s$n_writable, 0L)
  expect_false(any(s$table$will_write))
  expect_true(grepl("最新", s$headline))
})

test_that("小变更：可写入，条数计入 n_writable", {
  df <- make_summary(c("cmr", "iarc"), "failed", c(3L, 0L), c(0L, 0L), c(2L, 0L),
                     "Dry run (no apply)")
  s <- fcmsafety:::summarise_update_run(df)
  expect_true(s$has_changes)
  expect_equal(s$n_writable, 5L)
  expect_true(s$table$will_write[s$table$database == "cmr"])
  expect_false(s$table$will_write[s$table$database == "iarc"])
})

test_that("dry-run 的 status = failed 不算失败：计数齐全就是跑通了", {
  # 这是本次事故最容易踩错的地方：用 status 判失败会让每个库都变成"运行失败"
  df <- make_summary("iarc", "failed", 3L, 0L, 2L, "Dry run (no apply)")
  s <- fcmsafety:::summarise_update_run(df)
  expect_true(s$table$will_write)
  expect_equal(s$table$blocked_reason, "")
})

test_that("真失败（计数全 NA）才判成没跑完", {
  df <- make_summary("iarc", "failed", NA_integer_, NA_integer_, NA_integer_,
                     "boom")
  s <- fcmsafety:::summarise_update_run(df)
  expect_false(s$table$will_write)
  expect_true(grepl("失败|没跑完|未取得", s$table$blocked_reason))
})

test_that("变更数超上限：提前说出'会被闸门拦下'而不是等写库时才失败", {
  df <- make_summary("cmr_suspect", "failed", 101L, 0L, 349L, "Dry run (no apply)")
  s <- fcmsafety:::summarise_update_run(df, max_auto_changes = 20L)
  expect_false(s$table$will_write)
  expect_true(grepl("20", s$table$blocked_reason))
  expect_equal(s$n_writable, 0L)
  expect_true(grepl("450", s$headline))
})

test_that("有移除条目：算进移除计数并说明需人工确认", {
  df <- make_summary("eu_sml", "failed", 0L, 3L, 0L, "Dry run (no apply)")
  s <- fcmsafety:::summarise_update_run(df)
  expect_equal(s$n_removals, 3L)
  expect_equal(s$n_writable, 0L)
  expect_false(s$table$will_write)
  expect_true(grepl("移除|人工", s$table$blocked_reason))
})

test_that("上游英文 message 翻译成人话，别把 Dry run 当报错显示", {
  df <- make_summary(c("cmr", "iarc", "eu_sml"), "failed", 1L, 0L, 0L,
                     c("Dry run (no apply)",
                       "Cancelled: removals require manual review",
                       "Too many changes for auto_apply"))
  s <- fcmsafety:::summarise_update_run(df)
  expect_false(any(grepl("Dry run", s$table$message)))
  expect_true(any(grepl("预演", s$table$message)))
  expect_true(any(grepl("移除", s$table$message)))
})

# ---- 3) run_db_update_round：逐库跑一轮 ----
#
# 为什么不写成一个测试用的假 runner 就够了：这段循环原本写在 app 的 server
# 闭包里，闭包里的逻辑没法单独调用 —— 而"某个库失败不能中断整轮""各库结果要能
# 拼成一张表"恰恰最容易出错，出错了页面上只表现为"少了一行"，很难发现。

test_that("逐库跑完，顺序不变，日志与进度都收到通知", {
  logged <- character(0)
  seen <- list()
  runner <- function(db) make_summary(db, "ok", 1L, 0L, 0L, "Update applied")

  res <- fcmsafety:::run_db_update_round(
    c("cmr", "iarc", "svhc"), runner,
    log = function(msg) logged <<- c(logged, msg),
    progress = function(i, n, db) seen[[length(seen) + 1L]] <<- c(i, n, db))

  expect_equal(nrow(res), 3L)
  expect_equal(res$database, c("cmr", "iarc", "svhc"))
  expect_true(any(grepl("cmr", logged)))
  expect_equal(length(seen), 3L)
  expect_equal(seen[[1]], c("1", "3", "cmr"))
  expect_equal(seen[[3]], c("3", "3", "svhc"))
})

test_that("某个库抛错不中断整轮，错误写进日志", {
  logged <- character(0)
  runner <- function(db) {
    if (db == "iarc") stop("boom")
    make_summary(db, "ok", 1L, 0L, 0L, "Update applied")
  }
  res <- fcmsafety:::run_db_update_round(
    c("cmr", "iarc", "svhc"), runner, log = function(msg) logged <<- c(logged, msg))

  expect_equal(nrow(res), 2L)
  expect_equal(res$database, c("cmr", "svhc"))
  expect_true(any(grepl("iarc", logged) & grepl("boom", logged)))
})

test_that("全部失败返回 NULL，不炸", {
  runner <- function(db) stop("down")
  expect_null(fcmsafety:::run_db_update_round(c("cmr", "iarc"), runner))
  expect_null(fcmsafety:::run_db_update_round(character(0), runner))
})

test_that("runner 返回 NULL 的库被丢掉，不影响其它库", {
  runner <- function(db) if (db == "iarc") NULL else
    make_summary(db, "ok", 1L, 0L, 0L, "Update applied")
  res <- fcmsafety:::run_db_update_round(c("cmr", "iarc", "svhc"), runner)
  expect_equal(res$database, c("cmr", "svhc"))
})

test_that("各库汇总表列不一致时补 NA 再拼，不因 rbind 报错", {
  runner <- function(db) {
    if (db == "iarc") {
      data.frame(database = "iarc", status = "ok", added = 1L,
                 stringsAsFactors = FALSE)   # 少了 removed / modified / message
    } else {
      make_summary(db, "ok", 1L, 0L, 0L, "Update applied")
    }
  }
  res <- fcmsafety:::run_db_update_round(c("cmr", "iarc"), runner)
  expect_equal(nrow(res), 2L)
  expect_true(is.na(res$modified[res$database == "iarc"]))
})

test_that("日志/进度回调缺省时不报错", {
  runner <- function(db) make_summary(db, "ok", 1L, 0L, 0L, "Update applied")
  expect_equal(nrow(fcmsafety:::run_db_update_round("cmr", runner)), 1L)
})

