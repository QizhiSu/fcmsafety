# 审计 IARC 组条目的实际命中情况：每条条目命中多少行、各置信度多少。
# 用真实函数跑，不做复刻——置信度依赖 layer 判定与多处降级逻辑，
# 用旁路实现重算会得出错误结论。
#
# 用法（仓库根目录下）：
#   LC_ALL=zh_CN.UTF-8 "/c/Program Files/R/R-4.6.1/bin/Rscript.exe" tools/audit_group_hits.R
suppressMessages(pkgload::load_all(".", quiet = TRUE))

con <- DBI::dbConnect(RSQLite::SQLite(), "inst/fcmsafety.db")
d <- DBI::dbGetQuery(con, "SELECT InChIKey, Formula, SMILES FROM chemicals")
DBI::dbDisconnect(con)
cat("chemicals rows:", nrow(d), "\n\n")

hits <- assign_group_membership_table(d, source = "iarc")
cat("total hit rows:", nrow(hits), " / distinct inputs:", length(unique(hits$input_index)), "\n\n")

cat("=== 按条目 x 置信度（参与定级的标 *）===\n")
tab <- as.data.frame(table(entry = hits$matched_entry, conf = hits$confidence))
tab <- tab[tab$Freq > 0, ]
tab <- tab[order(-tab$Freq), ]
for (i in seq_len(nrow(tab))) {
  cat(sprintf("  %s %-16s %6d  %s\n",
              if (tab$conf[i] %in% c("auto_confirmed", "probable")) "*" else " ",
              tab$conf[i], tab$Freq[i], substr(as.character(tab$entry[i]), 1, 70)))
}

cat("\n=== 只看会参与定级的（auto_confirmed / probable）===\n")
grade <- hits[hits$confidence %in% c("auto_confirmed", "probable"), ]
if (nrow(grade) == 0) {
  cat("   (none)\n")
} else {
  gt <- as.data.frame(table(entry = grade$matched_entry, conf = grade$confidence))
  gt <- gt[gt$Freq > 0, ]
  for (i in seq_len(nrow(gt))) {
    cat(sprintf("  %-16s %6d  %s\n", gt$conf[i], gt$Freq[i],
                substr(as.character(gt$entry[i]), 1, 70)))
  }
}

cat("\n=== 有组级（1/2A/2B）且参与定级的命中 ===\n")
if (nrow(grade) > 0) {
  g <- grade[!is.na(grade$iarc_group) & grade$iarc_group %in% c("1", "2A", "2B"), ]
  if (nrow(g) == 0) cat("   (none)\n") else {
    gg <- as.data.frame(table(entry = g$matched_entry, grp = g$iarc_group))
    gg <- gg[gg$Freq > 0, ]
    for (i in seq_len(nrow(gg))) {
      cat(sprintf("  grp %-3s %6d  %s\n", gg$grp[i], gg$Freq[i],
                  substr(as.character(gg$entry[i]), 1, 70)))
    }
  }
}
cat("\nAUDIT_DONE\n")
