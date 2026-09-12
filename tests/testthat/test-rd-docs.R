# 文档自检：Rd 编码声明 + roxygen 里不得出现控制字符转义字面量
#
# 这两个坑各踩过一次（ADR 0006 / 0007）：
#   1) 中文 Rd 没有 \encoding{UTF-8} -> check 报 Non-ASCII contents without
#      declared encoding（LC_ALL=C 跑的时候看不出来）；
#   2) roxygen 正文里写 "\r\r\n" 这类字面量 -> Rd 把 \r \n 当宏名，报
#      unknown macro。第一次在 extract_cmr_h_codes()，第二次在 extract_group_nos()。

rd_root <- function(sub) {
  p <- testthat::test_path("..", "..", sub)
  if (dir.exists(p)) p else NA_character_
}

test_that("每个 man/*.Rd 都能通过 checkRd", {
  man_dir <- rd_root("man")
  skip_if(is.na(man_dir), "man/ 不在源码树里")

  files <- list.files(man_dir, pattern = "\\.Rd$", full.names = TRUE)
  expect_gt(length(files), 0L)

  bad <- list()
  for (f in files) {
    msgs <- as.character(unlist(tools::checkRd(f)))
    if (length(msgs)) bad[[basename(f)]] <- msgs
  }

  if (length(bad)) {
    fail(paste(c("以下 Rd 未通过 checkRd（编码声明见 docs/adr/0006）:",
                 utils::capture.output(print(bad))), collapse = "\n"))
  }
  succeed()
})

test_that("roxygen 注释里没有控制字符转义字面量", {
  r_dir <- rd_root("R")
  skip_if(is.na(r_dir), "R/ 不在源码树里")

  files <- list.files(r_dir, pattern = "\\.R$", full.names = TRUE)
  # 单个反斜杠 + r/n/t，且后面不跟字母（排除 \name / \tabular 这类真 Rd 宏），
  # 前面也不能是反斜杠（排除刻意写成 \\n 的转义）
  pat <- "(?<!\\\\)\\\\[rnt](?![A-Za-z])"

  offenders <- character(0)
  for (f in files) {
    lines <- readLines(f, warn = FALSE)
    hits <- grep("^\\s*#'", lines)
    hits <- hits[grepl(pat, lines[hits], perl = TRUE)]
    if (length(hits)) {
      offenders <- c(offenders,
                     sprintf("%s:%d: %s", basename(f), hits, trimws(lines[hits])))
    }
  }

  if (length(offenders)) {
    fail(paste(c("roxygen 正文里出现了控制字符转义字面量，Rd 会当成未定义宏。",
                 "改成用文字描述，例如「两个回车符加一个换行符」：",
                 offenders), collapse = "\n"))
  }
  succeed()
})
