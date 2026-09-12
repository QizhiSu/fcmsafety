# =============================================================================
# 法规库自动更新线体检：一次跑完下载层 -> 增量 diff -> 总调度
#
# 目标：回答"这条线现在还通不通"，而不是改数据。
#
# 安全约定（重要）：
#   全程只写**副本库**（%TEMP%/fcm_auto_test/fcmsafety.db）和临时目录，
#   真库 inst/fcmsafety.db 与 inst/*.xlsx 一个字节都不动。
#   下载文件统一落到 %TEMP%/fcm_auto_test/dl/，再以 new_file 传回增量链路，
#   因此不会覆盖 inst/ 里的缓存。脚本末尾会对比前后行数自证"没动真库"。
#
# 四段：
#   A 下载层   四个官方源（SVHC/CLP/EU SML/IARC）能不能抓到最新数据
#   B 离线源增量  用 inst/ 现有文件跑 dry-run
#   C 联网数据增量 用 A 段下载的文件跑 dry-run
#   E 总调度   update_database_auto() 全库跑一遍
#
# 用法（Git Bash，cwd = 包根）：
#   LC_ALL=zh_CN.UTF-8 "/c/Program Files/R/R-4.6.1/bin/Rscript.exe" --vanilla tools/test_auto_update_line.R
# 结果落盘：.workbuddy/auto_update_test_result.txt
#
# 判读：diff 数字虚高时先用 tools/diagnose_diff_columns.R 定位到具体列。
# =============================================================================

LOG <- file.path(".workbuddy", "auto_update_test_result.txt")
if (file.exists(LOG)) file.remove(LOG)
log_line <- function(...) {
  msg <- paste0(...)
  cat(msg, "\n")
  cat(msg, "\n", file = LOG, append = TRUE)
}
section <- function(t) log_line("\n", strrep("=", 70), "\n", "## ", t, "\n", strrep("=", 70))

elapsed <- function(t0) sprintf("%.1fs", as.numeric(difftime(Sys.time(), t0, units = "secs")))

pkgload::load_all(".", quiet = TRUE)

TMP <- file.path(Sys.getenv("TEMP"), "fcm_auto_test")
DL  <- file.path(TMP, "dl")
dir.create(DL, showWarnings = FALSE, recursive = TRUE)
DB  <- file.path(TMP, "fcmsafety.db")
file.copy("inst/fcmsafety.db", DB, overwrite = TRUE)

log_line("R: ", R.version.string)
log_line("真库 : ", normalizePath("inst/fcmsafety.db"))
log_line("副本库: ", DB, " (", file.size(DB), " bytes)")
log_line("下载目录: ", DL)

db_counts <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), DB)
  on.exit(DBI::dbDisconnect(con))
  tabs <- c("svhc", "cmr", "cmr_suspect", "iarc", "eu_sml", "update_history")
  out <- sapply(tabs, function(t) {
    tryCatch(DBI::dbGetQuery(con, paste0('select count(*) n from "', t, '"'))$n,
             error = function(e) NA_integer_)
  })
  names(out) <- tabs
  out
}
before <- db_counts()
log_line("\n副本库起始行数: ", paste(names(before), before, sep = "=", collapse = "  "))

# ---- A. 下载层 --------------------------------------------------------------
section("A. 下载层：四个官方源能不能抓到最新数据")
for (nm in c("svhc", "clp", "eu_sml", "iarc")) {
  f <- file.path(DL, paste0(nm, ".xlsx"))
  t0 <- Sys.time()
  res <- tryCatch({
    get(paste0("download_", nm))(out = f)
    "ok"
  }, error = function(e) paste0("ERROR: ", conditionMessage(e)))
  sz <- if (file.exists(f)) file.size(f) else NA
  log_line(sprintf("[%-7s] %-8s %8s  %s bytes", nm, res, elapsed(t0), format(sz, big.mark = ",")))
  if (file.exists(f) && is.na(sz) == FALSE && sz > 0) {
    probe <- tryCatch({
      if (nm == "eu_sml") {
        sh <- rio::import(f, sheet = NULL, col_names = FALSE)
        paste0("sheets=", paste(names(sh), collapse = "/"))
      } else {
        d <- rio::import(f, col_names = FALSE, .name_repair = "minimal")
        paste0("rows=", nrow(d), " cols=", ncol(d))
      }
    }, error = function(e) paste0("parse ERROR: ", conditionMessage(e)))
    log_line("          解析: ", probe)
  }
}

# ---- B. 离线源增量（inst/ 现有文件，只读） ----------------------------------
section("B. 离线源增量 dry-run（source=local，对副本库）")
run_one <- function(label, expr) {
  t0 <- Sys.time()
  out <- tryCatch(expr, error = function(e) list(success = FALSE, message = paste("EXC:", conditionMessage(e))))
  ch <- out$changes
  log_line(sprintf("[%-11s] %-6s %8s | added=%s removed=%s modified=%s | %s",
                   label,
                   if (isTRUE(out$success)) "ok" else "stopped",
                   elapsed(t0),
                   if (is.null(ch$total_added)) "-" else ch$total_added,
                   if (is.null(ch$total_removed)) "-" else ch$total_removed,
                   if (is.null(ch$total_modified)) "-" else ch$total_modified,
                   substr(if (is.null(out$message)) "" else out$message, 1, 70)))
  invisible(out)
}

res_local <- list(
  cmr         = run_one("cmr/local", update_cmr_auto(source = "local", interactive = FALSE,
                                                     auto_apply = FALSE, enrich = FALSE, db_path = DB)),
  cmr_suspect = run_one("cmr_sus/local", update_cmr_suspect_auto(source = "local", interactive = FALSE,
                                                                 auto_apply = FALSE, enrich = FALSE, db_path = DB)),
  iarc        = run_one("iarc/local", update_iarc_auto(source = "local", interactive = FALSE,
                                                       auto_apply = FALSE, enrich = FALSE, db_path = DB)),
  eu_sml      = run_one("eu_sml/local", update_eu_sml_auto(source = "local", interactive = FALSE,
                                                           auto_apply = FALSE, enrich = FALSE, db_path = DB)),
  svhc        = run_one("svhc/local", update_svhc_auto(source = "local", interactive = FALSE,
                                                       auto_apply = FALSE, enrich = FALSE, db_path = DB))
)

# ---- C. 联网抓到的数据走增量（用 new_file 传入，避免覆盖 inst/） ------------
section("C. 联网数据增量 dry-run（new_file = A 段下载文件）")
res_dl <- list()
res_dl$cmr <- run_one("cmr/dl", update_cmr_auto(source = "local", new_file = file.path(DL, "clp.xlsx"),
                                                interactive = FALSE, auto_apply = FALSE,
                                                enrich = FALSE, db_path = DB))
res_dl$cmr_suspect <- run_one("cmr_sus/dl", update_cmr_suspect_auto(source = "local",
                                                                    new_file = file.path(DL, "clp.xlsx"),
                                                                    interactive = FALSE, auto_apply = FALSE,
                                                                    enrich = FALSE, db_path = DB))
res_dl$iarc <- run_one("iarc/dl", update_iarc_auto(source = "local", new_file = file.path(DL, "iarc.xlsx"),
                                                   interactive = FALSE, auto_apply = FALSE,
                                                   enrich = FALSE, db_path = DB))
res_dl$eu_sml <- run_one("eu_sml/dl", update_eu_sml_auto(source = "local",
                                                         new_file = file.path(DL, "eu10_2011.xlsx"),
                                                         interactive = FALSE, auto_apply = FALSE,
                                                         enrich = FALSE, db_path = DB))
res_dl$svhc <- run_one("svhc/dl", update_svhc_auto(source = "local", new_file = file.path(DL, "svhc.xlsx"),
                                                   interactive = FALSE, auto_apply = FALSE,
                                                   enrich = FALSE, db_path = DB))

# ---- E. 总调度层 ------------------------------------------------------------
section("E. update_database_auto() 调度层（source=local，dry-run，对副本库）")
t0 <- Sys.time()
sched <- tryCatch(
  update_database_auto(databases = "all", source = "local", svhc_source = "local",
                       interactive = FALSE, auto_apply = FALSE, enrich = FALSE, db_path = DB),
  error = function(e) { log_line("EXC: ", conditionMessage(e)); NULL }
)
log_line("\n调度总耗时: ", elapsed(t0))
if (!is.null(sched)) {
  log_line("汇总表:")
  for (i in seq_len(nrow(sched))) {
    log_line(sprintf("  %-12s %-7s added=%-4s removed=%-4s modified=%-4s %s",
                     sched$database[i], sched$status[i], sched$added[i],
                     sched$removed[i], sched$modified[i],
                     substr(sched$message[i], 1, 60)))
  }
}

# ---- 收尾：核对副本库与真库都没被改动 ---------------------------------------
section("F. 一致性核对")
after <- db_counts()
log_line("副本库结束行数: ", paste(names(after), after, sep = "=", collapse = "  "))
log_line("副本库是否被改动: ", if (identical(before, after)) "否（全程 dry-run，符合预期）" else "是")
con <- DBI::dbConnect(RSQLite::SQLite(), "inst/fcmsafety.db")
on.exit(DBI::dbDisconnect(con), add = TRUE)
real <- sapply(c("svhc", "cmr", "cmr_suspect", "iarc", "eu_sml", "update_history"),
               function(t) DBI::dbGetQuery(con, paste0('select count(*) n from "', t, '"'))$n)
log_line("真库行数    : ", paste(names(real), real, sep = "=", collapse = "  "))
log_line("真库是否被改动: ", if (identical(as.integer(before[names(real)]), as.integer(real))) "否（安全）" else "是（异常，需还原备份）")
log_line("\n完成时间: ", format(Sys.time()))
