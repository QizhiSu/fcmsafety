# 单文件测试运行器（TDD 红/绿循环用）
#
# 用法（项目根目录，VSCode Git Bash）：
#   LC_ALL=zh_CN.UTF-8 "/c/Program Files/R/R-4.6.1/bin/Rscript.exe" \
#     --vanilla tools/run_one_test.R tests/testthat/test-xxx.R
#
# 与 tools/run_tests.R 同样包一层，绕开沙箱注入导致的退出码假失败。
Sys.setenv(NOT_CRAN = "true")
suppressMessages(pkgload::load_all(".", quiet = TRUE))

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) stop("用法: run_one_test.R <test-file>")
f <- args[1]
if (!file.exists(f)) stop("找不到测试文件: ", f)

res <- testthat::test_file(f, reporter = "summary", stop_on_failure = FALSE)
df <- as.data.frame(res)
cat("\n============ 单文件结果 ============\n")
cat("用例: ", nrow(df), "  失败: ", sum(df$failed),
    "  报错: ", sum(df$error), "  跳过: ", sum(df$skipped), "\n", sep = "")
cat("====================================\n")
