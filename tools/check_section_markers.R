# 分区标记安全校验（fcmsafety）
#
# 用法（项目根目录，VSCode Git Bash）：
#     "/c/Program Files/R/R-4.6.1/bin/Rscript.exe" tools/check_section_markers.R
#
# 为什么需要它：
#   R 文件里的 `# ---- 分区名 ----` 是给人和 AI 看的路标。若它被插进一个
#   roxygen 文档块（连续的 `#'` 行）中间，roxygen2 会把普通 `#` 注释视为
#   文档块结束，于是该块被劈成两半：前半截成了 title，后半截（原本的描述、
#   @param）变成失主内容，生成的 Rd 要么缺描述要么参数丢失 —— 而代码本身
#   照常运行，测试也照样绿。所以这个问题**只有 check 或人工比对 Rd 才能发现**，
#   属于"改了注释却破坏了文档"的隐蔽故障。
#
# 判据：分区标记行的**上一行以 `#'` 开头** → 违规（说明标记嵌在文档块里）。
#       标记行之后紧跟 `#'` 是正常的（标记在前，文档块完整在后）。
#
# 退出码：0 = 全部安全；1 = 发现违规（列出文件与行号）。

r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
problems <- list()

for (f in r_files) {
  lines <- readLines(f, warn = FALSE)
  # 允许前导空白：核心文件 database_inspector_app.R 的 ui/server 都嵌在
  # launch_database_inspector() 函数体内，分区标记按函数体缩进（2 空格）书写。
  hit <- grep("^[[:space:]]*#\\s*-{3,}", lines)
  for (i in hit) {
    if (i > 1 && grepl("^#'", lines[i - 1])) {
      problems[[length(problems) + 1]] <- data.frame(
        file = f, line = i,
        prev = substr(lines[i - 1], 1, 60),
        self = substr(lines[i], 1, 60),
        stringsAsFactors = FALSE
      )
    }
  }
}

if (length(problems) == 0) {
  cat("OK: 所有分区标记都不在 roxygen 块内部。\n")
  cat("扫描文件数: ", length(r_files), "\n", sep = "")
  quit(status = 0)
}

bad <- do.call(rbind, problems)
cat("发现 ", nrow(bad), " 处分区标记嵌在 roxygen 文档块内部：\n\n", sep = "")
for (k in seq_len(nrow(bad))) {
  cat(sprintf("%s:%d\n  上一行: %s\n  标记行: %s\n\n",
              bad$file[k], bad$line[k], bad$prev[k], bad$self[k]))
}
quit(status = 1)
