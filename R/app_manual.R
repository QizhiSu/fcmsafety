# =============================================================================
# 人工新清单探测（inst/ 下 *\_new.xlsx）
#
# 场景：ECHA / EFSA 等官网改版或反爬导致自动下载失败时，人工把新清单文件
# 放进 inst/（如 svhc_new.xlsx、clp_new.xlsx、iarc_new.xlsx、
# eu10_2011_new.xlsx），然后调 check_manual_lists() 让程序认出来并消费掉。
#
# 判定"是手动新加入的"两个条件（即下方 check_manual_lists() 文档所述）：
#   1) 文件修改时间晚于该库最近一次成功更新的完成时间；
#   2) 且晚于 1 分钟以上（吸收建库/备份收尾统一 touch 文件带来的偏差）。
#
# 时间列有个坑：update_history 里的 update_timestamp 存的是 **UTC 文本**，
# 与本机文件时间（本地时区）直接相比会差 8 小时，必须显式
# as.POSIXct(ts, tz = "UTC") 再比。改这个文件时先看这一条。
#
# 消费后文件改名归档（追加 .consumed_<时间戳> 后缀），不删除 —— 需要时可
# 手动改回原名重跑。
# =============================================================================

#' 检测人工手动放入的新清单文件
#'
#' 扫描 inst/ 下各法规库约定的"人工新清单"候选文件（例如 svhc_new.xlsx、
#' clp_new.xlsx、iarc_new.xlsx、eu10_2011_new.xlsx），判断哪些是最近被手动
#' 加入的：某候选文件的存在时间（修改时间）晚于该库最近一次成功更新的
#' 完成时间（update_history 账本；无账本记录的库以数据库文件时间为基准），
#' 且晚于 1 分钟以上（吸收建库/备份收尾统一 touch 文件带来的时间偏差），
#' 即视为"被手动加入"。
#'
#' 检测到新清单后，逐个显示文件名与加入时间，并提示是否更新：
#'   - \code{ask = TRUE}（默认）：逐个询问 "（文件名）被手动加入，是否更新？"；
#'     回答 Y/回车则调用对应源的 update_*_auto()（source = "local"，
#'     精确消费该文件）执行增量更新；
#'   - \code{auto_apply = TRUE}：不询问，直接自动更新全部新清单；
#'   - \code{ask = FALSE}：只列出检测结果，不更新。
#'
#' 更新成功的清单文件会被改名归档（追加 .consumed_<时间戳> 后缀），
#' 之后不再被识别为新清单，避免下次重复提示；文件不会被删除，需要时
#' 可手动改回原名再次使用。
#'
#' @param ask Logical，是否逐个询问确认（默认 TRUE）。非交互会话
#'   （!interactive()）下自动降级为只报告，避免 readline 卡死。
#' @param auto_apply Logical，是否对检测到的新清单自动执行更新
#'   （默认 FALSE）。为 TRUE 时忽略 ask 直接全部更新。
#' @param inst_dir inst 目录路径（默认当前工作目录下的 inst/）
#' @param db_path SQLite 数据库文件路径（默认与 get_db_connection() 相同）
#' @return data.frame，每行一个检测到的新清单文件，列为
#'   \code{db_name}（库名）、\code{file}（文件名）、\code{added_time}
#'   （加入时间）、\code{status}（detected / updated / skipped / failed）
#' @export
#' @examples
#' \dontrun{
#' check_manual_lists()            # detect and ask one by one
#' check_manual_lists(ask = FALSE) # report only, no update
#' }
#' @export
#' @encoding UTF-8
check_manual_lists <- function(ask = TRUE, auto_apply = FALSE,
                               inst_dir = file.path(getwd(), "inst"),
                               db_path = NULL) {

  # 各源"人工新清单"探测集：从 DB_SOURCES 注册表的 manual_candidates 派生，
  # 不再手抄第二份名单（fetch 层的 local_candidates 是"本地回退可用文件"，
  # 身份不同、各自登记；meta 备份系列如 svhc_meta.xlsx 已在注册表显式排除）。
  manual_map <- list()
  for (nm in names(DB_SOURCES)) {
    mc <- DB_SOURCES[[nm]]$manual_candidates
    if (length(mc) > 0L) manual_map[[nm]] <- mc
  }

  empty_out <- data.frame(db_name = character(), file = character(),
                          added_time = as.POSIXct(character()),
                          status = character())

  if (!dir.exists(inst_dir)) {
    message("inst directory not found: ", inst_dir)
    return(empty_out)
  }

  # 非交互会话不允许 readline：ask 自动降级为只报告
  if (ask && !interactive()) {
    message("(Non-interactive session: listing new lists without prompting)")
    ask <- FALSE
  }

  db_file <- if (!is.null(db_path)) db_path else file.path(inst_dir, "fcmsafety.db")
  db_exists <- file.exists(db_file)

  # 读取各库最近更新完成时间（update_history，UTC 文本）
  last_update <- list()
  if (db_exists) {
    con <- NULL
    tryCatch(con <- get_db_connection(db_path), error = function(e) NULL)
    if (!is.null(con)) {
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      has_history <- DBI::dbExistsTable(con, "update_history")
      if (has_history) {
        for (db_name in names(manual_map)) {
          ts <- tryCatch({
            DBI::dbGetQuery(con,
              "SELECT MAX(update_timestamp) AS ts FROM update_history
               WHERE database_name = ?", params = list(db_name))$ts[1]
          }, error = function(e) NA)
          if (!is.na(ts) && !is.null(ts)) {
            # SQLite CURRENT_TIMESTAMP 存的是 UTC；转成 POSIXct 绝对时间
            last_update[[db_name]] <- as.POSIXct(ts, tz = "UTC")
          }
        }
      }
    }
  }
  # 无 update_history 记录的库：以数据库文件时间为基准
  db_mtime <- if (db_exists) file.info(db_file)$mtime else NULL

  detected <- list()
  for (db_name in names(manual_map)) {
    for (fname in manual_map[[db_name]]) {
      path <- file.path(inst_dir, fname)
      if (!file.exists(path)) next

      fmtime <- file.info(path)$mtime
      base_time <- last_update[[db_name]]
      if (is.null(base_time)) base_time <- db_mtime
      if (is.null(base_time)) {
        # 库文件与账本都不存在（从未建库）：归为"库未就绪"提示
        message("⚠️  ", fname, " 存在于 inst/，但未找到数据库（", db_file, "）。",
                "请先运行 setup_fcmsafety_database() 建库。")
        next
      }

      # 严格晚于基准时间 > 60 秒才算"新增"：吸收建库/备份收尾统一
      # touch 文件的时间偏差（本仓库 svhc 收尾 touch 曾晚于更新记录约 50 秒）
      if (as.numeric(difftime(fmtime, base_time, units = "secs")) <= 60) next

      detected[[length(detected) + 1L]] <- list(
        db_name = db_name, file = path, added_time = fmtime)
    }
  }

  if (length(detected) == 0) {
    message("✅ 未检测到新清单：inst/ 中没有比最近更新更新的候选文件。")
    message("   提示：手动更新时，把新清单以固定名放入 inst/，如 svhc_new.xlsx、")
    message("   clp_new.xlsx、iarc_new.xlsx、eu10_2011_new.xlsx，再运行本函数。")
    return(empty_out)
  }

  message("📄 检测到 ", length(detected), " 个被手动加入的清单文件:")
  rows <- vector("list", length(detected))
  for (i in seq_along(detected)) {
    d <- detected[[i]]
    fname <- basename(d$file)
    added <- format(d$added_time, "%Y-%m-%d %H:%M")
    message(sprintf("   [%d] %s （添加于 %s，对应 %s 库）",
                    i, fname, added, toupper(d$db_name)))

    # 三种处理模式：
    #  - auto_apply = TRUE           -> 直接更新（status 将标 updated / failed）
    #  - ask = TRUE（默认交互）      -> 逐个询问，答 N 标 skipped
    #  - 两者皆否（纯报告）          -> 不更新，标 detected
    if (auto_apply) {
      do_update <- TRUE
      no_update_status <- NULL
    } else if (ask) {
      ans <- readline(sprintf("\n%s 被手动加入，是否更新？ [Y/n] ", fname))
      do_update <- !grepl("^n|^N", ans)   # 回车 / y / yes 都视为同意
      no_update_status <- "skipped"
    } else {
      do_update <- FALSE
      no_update_status <- "detected"
    }

    if (!do_update) {
      message("   ⏭ 已", if (no_update_status == "skipped") "跳过 " else "报告 ",
              fname, if (no_update_status == "skipped")
                "（保留原文件；不再提示可改名为 .consumed 后缀或删除）" else
                "（ask = FALSE 仅报告，未执行更新）")
      rows[[i]] <- data.frame(db_name = d$db_name, file = fname,
                              added_time = d$added_time,
                              status = no_update_status)
      next
    }

    ok <- FALSE
    err <- NULL
    tryCatch({
      # 分发也走注册表：svhc 独立一条线，其余走注册表驱动的公共入口
      res <- if (identical(DB_SOURCES[[d$db_name]]$line, "svhc")) {
        update_svhc_auto(source = "local", new_file = d$file,
                         interactive = FALSE, auto_apply = TRUE)
      } else {
        update_source_auto(d$db_name, source = "local", new_file = d$file,
                           interactive = FALSE, auto_apply = TRUE)
      }
      ok <- isTRUE(res$success)
      if (!ok && !is.null(res$error)) err <- res$error
    }, error = function(e) {
      err <<- conditionMessage(e)
    })

    if (ok) {
      # 更新成功：改名归档，避免下次重复提示
      stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      archived <- paste0(d$file, ".consumed_", stamp)
      renamed <- file.rename(d$file, archived)
      message("   ✅ ", fname, " 更新完成",
              if (renamed) paste0("；原文件已归档为 ",
                                  basename(archived)) else
                "（⚠️ 归档改名失败，请手动处理该文件以免重复提示）")
      rows[[i]] <- data.frame(db_name = d$db_name, file = fname,
                              added_time = d$added_time,
                              status = if (renamed) "updated" else "updated")
    } else {
      message("   ❌ ", fname, " 更新失败：",
              if (!is.null(err)) err else "未知错误")
      rows[[i]] <- data.frame(db_name = d$db_name, file = fname,
                              added_time = d$added_time,
                              status = "failed")
    }
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  invisible(out)
}
