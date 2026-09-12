# 测试运行脚本（fcmsafety）——绕开 WorkBuddy 沙箱的退出码问题
#
# 用法（项目根目录，VSCode Git Bash）：
#     NOT_CRAN=true "/c/Program Files/R/R-4.6.1/bin/Rscript.exe" tools/run_tests.R
#
# 为什么要包一层：本机 R 在 WorkBuddy 终端里退出时会被沙箱注入的 tsbx.dll
# 拖崩（exit 139 / 0xC0000005），而 testthat 的结果在崩之前已经打印完了。
# 直接用 -e 内联长脚本更容易在解析阶段就出问题，所以写成文件跑。

Sys.setenv(NOT_CRAN = "true")

suppressMessages(pkgload::load_all(".", quiet = TRUE))
res <- testthat::test_dir("tests/testthat", reporter = "summary", stop_on_failure = FALSE)

df <- as.data.frame(res)
cat("\n================ 测试结果汇总 ================\n")
cat("用例总数 : ", nrow(df), "\n", sep = "")
cat("失败     : ", sum(df$failed), "\n", sep = "")
cat("报错     : ", sum(df$error), "\n", sep = "")
cat("警告     : ", sum(df$warning), "\n", sep = "")
cat("跳过     : ", sum(df$skipped), "\n", sep = "")
cat("==============================================\n")

if (sum(df$failed) == 0 && sum(df$error) == 0) {
  cat("结论: 全部通过（0 failed / 0 error）。\n")
} else {
  cat("结论: 存在失败或报错，见上方明细。\n")
}
