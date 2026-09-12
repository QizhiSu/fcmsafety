# R CMD check 自检脚本（fcmsafety）
#
# 用法：在项目根目录（C:\Users\13432\WorkBuddy\2026-08-31-14-16-57\fcmsafety），
# 用 VSCode 终端（Git Bash）跑：
#     "/c/Program Files/R/R-4.6.1/bin/Rscript.exe" tools/run_check.R
#
# ---------------------------------------------------------------------------
# 本机跑 check 的两个坑（都是环境问题，2026-09-10 实测确认）
# ---------------------------------------------------------------------------
# 1) locale 必须显式设成 Windows 认得的 UTF-8 名字（zh_CN.UTF-8）。
#    - Git Bash 默认导出 LC_ALL=C.UTF-8，Windows 版 R 不认这个名字，
#      check 会在 DESCRIPTION 阶段误报「无正文」ERROR。
#    - 若改用 LC_ALL=C，R CMD build 把包源码复制到临时目录时会因文件名
#      含中文（如 筛查100种物质.R）而静默失败，症状是
#      "Removed empty directory 'fcmsafety'" 紧接着
#      "file 'fcmsafety/DESCRIPTION' does not exist"。
#
# 2) 必须加 --no-install，且**不能**用 devtools::check() 包一层。
#    - 本机 R 4.6.1 装包时，lazy-load 阶段会被 WorkBuddy 沙箱注入的
#      tsbx.dll 拖崩，进程退出时 segfault（exit 139 / 0xC0000005）。
#      R CMD check 本身已经跑完并写出了 00check.log，只是退出码非 0；
#      而 devtools::check() 只看退出码，会把成功的检查报成
#      "Error: R CMD check process failed"。所以这里直接调 R CMD check，
#      无视退出码，改从 00check.log 读真实的 Status 行。
#    - 跳过安装步后，代码语法、Rd、NAMESPACE、依赖、文档一致性等静态检查
#      照常全部执行；只有 examples 和 tests 两项显示 SKIPPED。
#      测试请另外跑：
#        NOT_CRAN=true "/c/Program Files/R/R-4.6.1/bin/Rscript.exe" -e \
#          'pkgload::load_all("."); testthat::test_dir("tests/testthat")'
#      （测试进程同样会在退出时崩，但结果已在崩之前打印完毕。）
#
# 3) Rd 编码：新增文档块必须以 `#' @encoding UTF-8` 结尾（且必须在块尾，
#    不能插在块中间——roxygen 会把标签之后的文字当作该标签的续行）。
#    漏了就报 `Non-ASCII contents without declared encoding`。详见
#    docs/adr/0006-20260910-rd-encoding-declaration.md。
#    只剩 1 个 WARNING（`checking code files for non-ASCII characters`）
#    属预期：中文文案常量不打算改成 \uXXXX 转义。
# ---------------------------------------------------------------------------

# Windows 的可执行名是 R.exe；Unix 是 R。写死 R.exe 会让 macOS/Linux 在
# 第一步 build 就静默失败（system2 找不到命令，日志为空、无 tarball）。
rbin <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R")
pkgdir <- normalizePath(".")
pkgname <- basename(pkgdir)

Sys.setenv("_R_CHECK_FORCE_SUGGESTS_" = "false")

tmp <- file.path(tempdir(), "fcmsafety_check")
unlink(tmp, recursive = TRUE)
dir.create(tmp, recursive = TRUE, showWarnings = FALSE)

old_wd <- setwd(tmp)
on.exit(setwd(old_wd), add = TRUE)

step <- 0L
quiet_run <- function(args) {
  step <<- step + 1L
  log <- file.path(tmp, sprintf("step%d.log", step))
  suppressWarnings(system2(rbin, args, stdout = log, stderr = log))
  invisible(log)
}

message("==> R CMD build (", pkgname, ")")
quiet_run(c("CMD", "build", "--no-build-vignettes", "--no-manual",
            shQuote(pkgdir)))

tarballs <- list.files(tmp, pattern = "\\.tar\\.gz$", full.names = TRUE)
if (length(tarballs) == 0L) {
  stop("Build failed; see the step_*.log files under ", tmp, call. = FALSE)
}
tarball <- tarballs[which.max(file.mtime(tarballs))]
message("    built: ", basename(tarball))

message("==> R CMD check --no-install (examples/tests will be SKIPPED)")
outdir <- file.path(tmp, "check")
dir.create(outdir, showWarnings = FALSE)
quiet_run(c("CMD", "check", "--no-install", "--no-manual",
            "--no-build-vignettes", "-o", shQuote(outdir), shQuote(tarball)))

logfile <- file.path(outdir, paste0(pkgname, ".Rcheck"), "00check.log")
if (!file.exists(logfile)) {
  stop("No check log produced at ", logfile, call. = FALSE)
}

lines <- readLines(logfile, warn = FALSE)
status <- grep("^Status:", lines, value = TRUE)
findings <- grep("^\\* checking .*(\\.\\.\\.)? ?(ERROR|WARNING|NOTE)",
                 lines, value = TRUE)

cat("\n================ R CMD check 结果 ================\n")
cat(findings, sep = "\n")
cat("\n", if (length(status)) status else "Status: (unknown)", "\n", sep = "")
cat("==================================================\n")
cat("完整日志: ", logfile, "\n", sep = "")

if (length(status) && grepl("OK$", status)) {
  cat("结论: 无 ERROR / WARNING / NOTE。\n")
} else {
  cat("结论: 请查看上面列出的条目。注意 examples/tests 显示 SKIPPED 属正常",
      "（--no-install），测试需另跑。\n", sep = "")
}
