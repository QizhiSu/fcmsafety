# Test script for fcmsafety four core data sources
# 目标：只测试、不修改真实数据库；所有写入都在临时数据库/临时文件

setwd("C:/Users/13432/WorkBuddy/2026-08-31-14-16-57/fcmsafety")

# 优先用 pkgload::load_all，失败再试 devtools::load_all
if (suppressWarnings(requireNamespace("pkgload", quietly = TRUE))) {
  pkgload::load_all(".", quiet = TRUE)
} else {
  library(devtools)
  load_all(".", quiet = TRUE)
}

library(DBI)

report <- list()

# ---- 0. 环境准备：复制真实数据库到临时库 ----
temp_dir <- file.path(tempdir(), "fcmsafety_test", format(Sys.time(), "%Y%m%d_%H%M%S"))
dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
real_db <- "C:/Users/13432/WorkBuddy/2026-08-31-14-16-57/fcmsafety/inst/fcmsafety.db"
test_db <- file.path(temp_dir, "fcmsafety_test.db")
file.copy(real_db, test_db, overwrite = TRUE)
report$db_path <- test_db

# ---- 0.1 基线计数 ----
con <- get_db_connection(test_db)
baseline <- sapply(c("svhc","cmr","cmr_suspect","iarc","eu_sml","eu_sml_group"), function(t) {
  if (dbExistsTable(con, t)) dbGetQuery(con, paste0("SELECT COUNT(*) FROM ", t))[[1]] else NA_integer_
})
dbDisconnect(con)
report$baseline <- baseline
cat("Baseline counts:\n")
print(baseline)

# ---- 测试辅助函数 ----
safe_test <- function(label, expr) {
  cat("\n====", label, "====\n")
  t0 <- Sys.time()
  res <- tryCatch(expr, error = function(e) list(error = conditionMessage(e)))
  t1 <- Sys.time()
  res$elapsed_sec <- as.numeric(difftime(t1, t0, units = "secs"))
  cat("Elapsed:", round(res$elapsed_sec, 2), "s\n")
  if (!is.null(res$error)) cat("ERROR:", res$error, "\n")
  res
}

report$fetches <- list()
report$downloads <- list()
report$updates <- list()

# ---- 1. 测试本地文件读取/标准化（数据源处理） ----
report$fetches$svhc <- safe_test("SVHC fetch local", {
  df <- fcmsafety:::fetch_svhc_data("local")
  list(rows = nrow(df), cols = ncol(df), sample_names = head(df[["Substance name"]], 3))
})

report$fetches$cmr <- safe_test("CMR fetch local", {
  df <- fcmsafety:::fetch_cmr_data("local")
  list(rows = nrow(df), cols = ncol(df), index_sample = head(df[["Index No"]], 3))
})

report$fetches$cmr_suspect <- safe_test("CMR_suspect fetch local", {
  df <- fcmsafety:::fetch_cmr_suspect_data("local")
  list(rows = nrow(df), cols = ncol(df))
})

report$fetches$iarc <- safe_test("IARC fetch local", {
  df <- fcmsafety:::fetch_iarc_data("local")
  list(rows = nrow(df), cols = ncol(df), agent_sample = head(df[["Agent"]], 3))
})

report$fetches$eu_sml <- safe_test("EU SML fetch local", {
  df <- fcmsafety:::fetch_eu_sml_data("local")
  list(rows = nrow(df), cols = ncol(df), fcm_sample = head(df[["FCM substance No"]], 3))
})

# ---- 2. 测试 dry-run 增量更新（不写入） ----
# 注意：全部设置 enrich=FALSE，避免测试阶段访问 PubChem
report$updates$svhc <- safe_test("SVHC dry-run update (local, no enrich)", {
  update_svhc_auto(source = "local", interactive = FALSE, auto_apply = FALSE,
                   enrich = FALSE, db_path = test_db, backup = FALSE)
})

report$updates$cmr <- safe_test("CMR dry-run update (local, no enrich)", {
  update_cmr_auto(source = "local", interactive = FALSE, auto_apply = FALSE,
                  enrich = FALSE, db_path = test_db, backup = FALSE)
})

report$updates$cmr_suspect <- safe_test("CMR_suspect dry-run update (local, no enrich)", {
  update_cmr_suspect_auto(source = "local", interactive = FALSE, auto_apply = FALSE,
                          enrich = FALSE, db_path = test_db, backup = FALSE)
})

report$updates$iarc <- safe_test("IARC dry-run update (local, no enrich)", {
  update_iarc_auto(source = "local", interactive = FALSE, auto_apply = FALSE,
                   enrich = FALSE, db_path = test_db, backup = FALSE)
})

report$updates$eu_sml <- safe_test("EU SML dry-run update (local, no enrich)", {
  update_eu_sml_auto(source = "local", interactive = FALSE, auto_apply = FALSE,
                     enrich = FALSE, db_path = test_db, backup = FALSE)
})

# ---- 3. 测试自动下载（HTTP/解析） ----
# 下载到临时目录，避免覆盖 inst/ 里的文件
dl_dir <- file.path(temp_dir, "downloads")
dir.create(dl_dir, showWarnings = FALSE)

report$downloads$svhc <- safe_test("SVHC download", {
  path <- file.path(dl_dir, "svhc.xlsx")
  download_svhc(out = path)
  list(success = file.exists(path), size = as.numeric(file.size(path)))
})

report$downloads$clp <- safe_test("CLP download", {
  path <- file.path(dl_dir, "clp.xlsx")
  download_clp(out = path)
  list(success = file.exists(path), size = as.numeric(file.size(path)))
})

report$downloads$iarc <- safe_test("IARC download", {
  path <- file.path(dl_dir, "iarc.xlsx")
  download_iarc(out = path)
  list(success = file.exists(path), size = as.numeric(file.size(path)))
})

report$downloads$eu_sml <- safe_test("EU SML download", {
  path <- file.path(dl_dir, "eu10_2011.xlsx")
  download_eu_sml(out = path)
  list(success = file.exists(path), size = as.numeric(file.size(path)))
})

# ---- 4. 下载后标准化/读取测试 ----
# 用 source="local" + new_file 读临时下载文件，避免 "download" 分支把文件
# 重新写进 inst/（覆盖真实数据）。下载失败时 temp 文件不存在，会自动回退本地源。
report$downloads$cmr_parsed <- safe_test("Parse downloaded CLP + screen CMR", {
  df <- fcmsafety:::fetch_cmr_data("local", new_file = file.path(dl_dir, "clp.xlsx"))
  list(rows = nrow(df), hcol = names(df)[grepl("Hazard Statement", names(df))][1])
})

report$downloads$cmr_suspect_parsed <- safe_test("Parse downloaded CLP + screen CMR_suspect", {
  df <- fcmsafety:::fetch_cmr_suspect_data("local", new_file = file.path(dl_dir, "clp.xlsx"))
  list(rows = nrow(df))
})

report$downloads$iarc_parsed <- safe_test("Parse downloaded IARC", {
  df <- fcmsafety:::fetch_iarc_data("local", new_file = file.path(dl_dir, "iarc.xlsx"))
  list(rows = nrow(df))
})

report$downloads$eu_sml_parsed <- safe_test("Parse downloaded EU SML", {
  df <- fcmsafety:::fetch_eu_sml_data("local", new_file = file.path(dl_dir, "eu10_2011.xlsx"))
  list(rows = nrow(df))
})

# ---- 5. 汇总并保存报告 ----
report$timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
report_file <- "C:/Users/13432/WorkBuddy/2026-08-31-14-16-57/fcmsafety/four_sources_test_report.rds"
saveRDS(report, report_file)

cat("\n==== TEST SUMMARY ====\n")
cat("Report saved to:", report_file, "\n\n")

# 打印简洁表格
fmt_status <- function(x) {
  if (!is.null(x$error)) return("ERROR")
  # update_*_auto 返回的列表里有 changes，优先显示 diff 结果
  if (!is.null(x$changes)) {
    ch <- x$changes
    return(sprintf("+%d / -%d / ~%d", ch$total_added, ch$total_removed, ch$total_modified))
  }
  if (!is.null(x$success)) return(if (x$success) "OK" else "FAIL")
  "OK"
}

fmt_rows <- function(x) {
  if (!is.null(x$error)) return(NA_integer_)
  if (!is.null(x$rows)) return(x$rows)
  if (!is.null(x$success) && x$success) return(x$size)
  NA_integer_
}

cat("Source fetch tests:\n")
cat(sprintf("%-18s %10s %10s %s\n", "source", "rows", "elapsed", "status"))
for (nm in names(report$fetches)) {
  x <- report$fetches[[nm]]
  cat(sprintf("%-18s %10s %8.2fs %s\n", nm, fmt_rows(x), x$elapsed_sec, fmt_status(x)))
}

cat("\nDry-run update tests:\n")
cat(sprintf("%-18s %10s %8s %s\n", "source", "changes", "elapsed", "status"))
for (nm in names(report$updates)) {
  x <- report$updates[[nm]]
  cat(sprintf("%-18s %10s %8.2fs %s\n", nm, fmt_status(x), x$elapsed_sec,
              if (is.null(x$error)) "OK" else "ERROR"))
}

cat("\nDownload tests:\n")
cat(sprintf("%-18s %10s %8s %s\n", "source", "size", "elapsed", "status"))
for (nm in names(report$downloads)) {
  x <- report$downloads[[nm]]
  cat(sprintf("%-18s %10s %8.2fs %s\n", nm, fmt_rows(x), x$elapsed_sec, fmt_status(x)))
}
