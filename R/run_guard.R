# ============================================================================
# 一键更新流程的两道守卫（供 Shiny app 调用，判读逻辑与 UI 分离以便测试）
#
# 为什么需要：2026-09-11 页面假死事故。用户点「联网更新全部」后按钮全哑，
# 连点多次也没恢复。查清的机制是 ——
#   * Shiny 的**出站**消息（进度条、日志）不依赖 R 主线程，阻塞期间照样送到
#     浏览器（httpuv 的事件循环跑在后台线程）；
#   * 但**入站**消息（点击）必须回到 R 主线程才能变成 input$xxx。R 在跑更新
#     时点击全在排队，任务结束后才一起补送。
# 于是原来那套「当前忙不忙」的守卫完全失效：任务结束时它早已复位，积压的
# 点击立刻再触发一轮，用户永远等不到页面空闲的那一刻。
#
# ADR 0011 记录了这个决策与代价。
# ============================================================================

# ---- 防积压点击 ----

#' 判断一次「一键更新」请求是否属于积压点击
#'
#' 阻塞期间到达的点击会在任务结束后集中补送，看上去就像用户又点了一次。
#' 守卫因此不看「现在忙不忙」，而是看「上一轮结束到现在过了多久」：落在
#' 宽限期内的请求一律丢弃。用户真想再跑一次，等过宽限期再点即可 —— 全量
#' 更新本就不会一分钟点两次。
#'
#' @param last_end 上一轮结束时刻（POSIXct）。从未跑过传 NULL 或 NA。
#' @param now 当前时刻，默认 Sys.time()。
#' @param grace_secs 宽限期秒数，落在 [0, grace_secs) 内的请求被丢弃。
#' @return 长度 1 的逻辑值：TRUE 表示本次请求应被丢弃。
#' @seealso \code{\link{summarise_update_run}}
#' @keywords internal
#' @encoding UTF-8
should_drop_run_request <- function(last_end, now = Sys.time(),
                                    grace_secs = 10) {
  ok_end <- inherits(last_end, "POSIXct") && length(last_end) == 1L &&
    !is.na(as.numeric(last_end)) && is.finite(as.numeric(last_end))
  if (!ok_end) return(FALSE)
  if (!inherits(now, "POSIXct") || length(now) != 1L) return(FALSE)

  secs <- as.numeric(difftime(now, last_end, units = "secs"))
  if (is.na(secs) || secs < 0) return(FALSE)   # 时钟回拨：不拦，宁可放行
  secs < grace_secs
}

# ---- 逐库跑一轮 ----

#' 逐库跑一轮更新，返回各库汇总表拼起来的 data.frame
#'
#' 从 app 里抽出来是为了**能测**：Shiny 的 server 是个闭包，写在里面的循环没法
#' 单独调用，而"某个库失败不能中断整轮""各库结果要能拼成一张表"恰恰是最容易
#' 出错、出错后又最难在页面上发现的地方（只表现为表里少了一行）。
#'
#' @param dbs 要跑的库名向量，顺序即执行顺序。
#' @param runner 真正干活的函数，签名 function(db)，返回一行的汇总表；
#'   返回 NULL 或抛错都按"这个库没结果"处理。app 里传的是
#'   \code{update_database_auto()} 的薄包装。
#' @param log 可选，function(msg)；app 里接到页面上的实时日志通道。
#' @param progress 可选，function(i, n, db)；app 里接到 incProgress。
#' @return 各库结果拼成的 data.frame；一个库都没出结果时返回 NULL。
#' @seealso \code{\link{summarise_update_run}}
#' @keywords internal
#' @encoding UTF-8
run_db_update_round <- function(dbs, runner, log = NULL, progress = NULL) {
  emit <- function(fn, ...) if (is.function(fn)) try(fn(...), silent = TRUE)
  if (length(dbs) == 0L) return(NULL)

  results <- vector("list", length(dbs))
  for (i in seq_along(dbs)) {
    db <- dbs[i]
    emit(progress, i, length(dbs), db)
    emit(log, paste0("-------- ", db, " --------"))
    # 单库失败不能中断整轮：汇总是"这轮更新发生了什么"的唯一记录，
    # 缺一整轮的代价远大于少一个库。
    results[[i]] <- tryCatch(runner(db), error = function(e) {
      emit(log, paste0("❌ ", db, " 出错: ", conditionMessage(e)))
      NULL
    })
  }

  results <- Filter(function(x) is.data.frame(x) && nrow(x) > 0L, results)
  if (length(results) == 0L) return(NULL)

  # 各库汇总表理论上同列，但不为此赌一次 rbind 报错：缺列补 NA 再按并集取齐
  cols <- unique(unlist(lapply(results, names)))
  results <- lapply(results, function(x) {
    for (m in setdiff(cols, names(x))) x[[m]] <- NA
    x[, cols, drop = FALSE]
  })
  out <- do.call(rbind, results)
  rownames(out) <- NULL
  out
}

# ---- 预演结果判读 ----

#' 把上游的英文状态话术翻译成页面上的中文
#'
#' 原文出现在 \code{update_database_auto()} 汇总表的 message 列，直接显示会让人
#' 误以为出错（尤其 "Dry run (no apply)" 是预演通过的正常结果）。
#'
#' @param x 字符向量。
#' @return 翻译后的字符向量，未登记的原样保留。
#' @keywords internal
#' @encoding UTF-8
translate_run_message <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  map <- c(
    "Dry run (no apply)"                       = "预演通过（本轮未写库）",
    "Cancelled: removals require manual review" = "安全阀拦下：有移除条目，需人工确认",
    "Cancelled: removed entries require manual review" = "安全阀拦下：有移除条目，需人工确认",
    "Too many changes for auto_apply"          = "变更过多，超过自动上限",
    "No changes"                               = "已是最新，无变更",
    "Update applied"                           = "已写入数据库",
    "User cancelled"                           = "已取消",
    "No changes written"                       = "无变更，未写库")
  hit <- x %in% names(map)
  x[hit] <- unname(map[x[hit]])
  x
}

#' 判读一轮预演（dry-run）的汇总，给出「能不能写、为什么不能写」
#'
#' 入参就是 \code{update_database_auto()} 的返回值（invisible 的按库汇总表，
#' 列：database / status / added / removed / modified / message）。
#'
#' 为什么要在预演阶段自己复算一遍闸门：\code{auto_apply = FALSE} 时
#' \code{run_incremental_update()} 走的是「Dry run」分支，**返回在两道闸门之前**，
#' 所以预演看得见变更数，却看不出真正写库时会不会被拦。两道闸门的规则很固定
#' （有移除条目 -> 一律 Cancelled；变更数 > max_auto_changes -> 不写），这里按同一
#' 套规则复算，把「会被拦下」提前告诉人，而不是等第二段跑完才发现没写成。
#'
#' 另一个必须记住的坑：预演阶段每个库的 status 都是 "failed"
#' （\code{done(FALSE, NULL, "Dry run (no apply)")}），但三个计数列是齐的 ——
#' 不能据此判成运行失败。真失败是 tryCatch 兜底那条路，三个计数列全 NA。
#'
#' @param summary_df \code{update_database_auto()} 的返回值，允许为 NULL。
#' @param max_auto_changes 与写库时一致的自动上限，用于复算闸门。
#' @return 具名 list：
#'   \describe{
#'     \item{table}{展示用 data.frame，比入参多了 will_write（预计可写）与
#'       blocked_reason（被拦下的原因，可写时为空串）两列}
#'     \item{has_changes}{是否有任何变更}
#'     \item{n_writable}{预计可写入的变更条数}
#'     \item{n_removals}{移除条目总数}
#'     \item{headline}{一句话结论，可直接显示}
#'   }
#' @title Summarise a batch of database update results
#' @seealso \code{\link{should_drop_run_request}}
#' @keywords internal
#' @encoding UTF-8
summarise_update_run <- function(summary_df, max_auto_changes = 20L) {
  empty_table <- data.frame(
    database = character(0), status = character(0),
    added = integer(0), removed = integer(0), modified = integer(0),
    will_write = logical(0), blocked_reason = character(0),
    message = character(0), stringsAsFactors = FALSE)
  blank <- list(table = empty_table, has_changes = FALSE, n_writable = 0L,
                n_removals = 0L, headline = "没有拿到任何库的更新结果。")

  if (is.null(summary_df) || !is.data.frame(summary_df) ||
      nrow(summary_df) == 0L) {
    return(blank)
  }
  need <- c("database", "status", "added", "removed", "modified", "message")
  if (!all(need %in% names(summary_df))) return(blank)

  df <- summary_df[, need, drop = FALSE]
  added_raw <- suppressWarnings(as.integer(df$added))
  removed_raw <- suppressWarnings(as.integer(df$removed))
  modified_raw <- suppressWarnings(as.integer(df$modified))

  # 真失败 = 三个计数列全 NA（tryCatch 兜底路径）。见上文关于 status 的说明。
  failed <- is.na(added_raw) & is.na(removed_raw) & is.na(modified_raw)

  df$database <- as.character(df$database)
  df$status   <- as.character(df$status)
  df$message  <- translate_run_message(df$message)
  df$added    <- ifelse(is.na(added_raw), 0L, added_raw)
  df$removed  <- ifelse(is.na(removed_raw), 0L, removed_raw)
  df$modified <- ifelse(is.na(modified_raw), 0L, modified_raw)

  n_changes <- df$added + df$modified
  can_consider <- !failed
  blocked_removal <- can_consider & df$removed > 0L
  blocked_limit <- can_consider & df$removed == 0L & n_changes > max_auto_changes

  df$blocked_reason <- ""
  df$blocked_reason[failed] <- "该库没跑完（未取得计数），本轮跳过"
  df$blocked_reason[blocked_removal] <-
    "有移除条目：安全阀会拦下，需到命令行人工确认"
  df$blocked_reason[blocked_limit] <- paste0(
    "变更 ", n_changes[blocked_limit], " 条 > 自动上限 ", max_auto_changes,
    "：写库会被拦下")

  df$will_write <- can_consider & df$removed == 0L & n_changes > 0L &
    n_changes <= max_auto_changes

  df <- df[, c("database", "status", "added", "removed", "modified",
               "will_write", "blocked_reason", "message")]

  n_writable <- as.integer(sum(n_changes[df$will_write]))
  n_removals <- as.integer(sum(df$removed))
  n_all <- as.integer(sum(n_changes) + n_removals)
  has_changes <- n_all > 0L

  headline <- if (!has_changes) {
    "所有库都是最新，没有任何变更 —— 本轮不会写库。"
  } else if (n_removals > 0L) {
    paste0("共 ", n_removals, " 条移除条目，安全阀会拦下，需到命令行人工确认。",
           if (n_writable > 0L) paste0("另有 ", n_writable, " 条变更可写入。") else "")
  } else if (n_writable > 0L) {
    paste0("共 ", n_writable, " 条变更待写入。")
  } else {
    paste0("共 ", n_all, " 条变更，全部超过自动上限或未跑完 —— 本轮不会写库。")
  }

  list(table = df, has_changes = has_changes, n_writable = n_writable,
       n_removals = n_removals, headline = headline)
}
