#!/usr/bin/env Rscript
# Launch FCMSafety Database Inspector（网页一键启动）
#
# 用法（任选其一，都在 VSCode 终端 / Git Bash 里执行）：
#   1) "/c/Program Files/R/R-4.6.1/bin/Rscript.exe" launch_inspector.R
#   2) 或先把 R 加进 PATH 后直接：Rscript launch_inspector.R
#
# 浏览器会自动打开 http://localhost:3838；结束后回终端按 Ctrl+C 停止。

# --- 自动把工作目录切到本项目根目录（脚本从任何位置运行都行）---
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
if (length(file_arg) && nzchar(file_arg)) {
  setwd(dirname(normalizePath(file_arg)))
}
if (!file.exists("DESCRIPTION")) {
  stop("找不到项目根目录（DESCRIPTION 文件）。请确认 launch_inspector.R 位于 fcmsafety 项目内。")
}

cat("🚀 正在启动 FCMSafety 网页（首次运行会加载全部代码，请稍等）...\n")

# --- 设置 UTF-8 locale ---
# Windows 默认 R locale = "C"，会让 emoji/中文在 Shiny HTML 里变成 <U+XXXX> 占位符
# 设成 Windows 自带的 UTF-8 locale 后，所有 Unicode 字符会原样输出
Sys.setlocale("LC_ALL", "Chinese (Simplified)_China.utf8")

# --- 以开发模式加载整个包（等效于把 R/ 下所有代码都跑一遍）---
# 注意：不能只 source 个别文件——网页代码会用到 sqlite_database_manager.R
# 里的 get_db_connection()、update_other_dbs.R 里的一键更新等函数。
if (requireNamespace("pkgload", quietly = TRUE)) {
  suppressMessages(pkgload::load_all(".", quiet = TRUE))
} else {
  for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)
}

# 确保数据库存在
db_path <- file.path(getwd(), "inst", "fcmsafety.db")
if (!file.exists(db_path)) {
  cat("📊 未找到数据库，正在创建...\n")
  setup_fcmsafety_database(force_reinit = TRUE)
}

# 启动网页
cat("📱 浏览器将自动打开 http://localhost:3838\n")
cat("🛑 结束后回本终端按 Ctrl+C 停止\n")

launch_database_inspector(port = 3838, launch_browser = TRUE)
