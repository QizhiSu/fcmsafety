# 对比 group_membership 开 / 关两种情况下最终的毒性等级分布。
#
# 回答的问题是：组条目（IARC 的"某类化合物"条目）到底改变了哪些行的等级，
# 以及改得对不对。tools/audit_group_hits.R 看的是匹配器自身的命中分布，
# 这个脚本看的是它对最终结论的影响。
#
# 传空 Toxtree 结果文件以跳过 Toxtree 跑批：组条目走 IARC 路径，与 Cramer
# 分级正交，省略它不影响本次对比，但能把运行时间从十几分钟压到两分钟。
#
# 用法（仓库根目录下）：
#   LC_ALL=zh_CN.UTF-8 "/c/Program Files/R/R-4.6.1/bin/Rscript.exe" tools/compare_group_membership.R
#
# 注意：R 在 WorkBuddy 终端里退出时会 segfault（沙箱注入所致，非本脚本问题）。
# 结果看输出，不看退出码。
suppressMessages(pkgload::load_all(".", quiet = TRUE))

con <- DBI::dbConnect(RSQLite::SQLite(), "inst/fcmsafety.db")
d <- DBI::dbGetQuery(con, "SELECT InChIKey, Formula, SMILES FROM chemicals")
DBI::dbDisconnect(con)

tf <- file.path(tempdir(), "empty_tox.csv")
utils::write.csv(data.frame(SMILES = character(0), Cramer.rules = character(0)),
                 tf, row.names = FALSE)

cat("=== 输入:", nrow(d), "行 ===\n\n")
res_off <- assign_toxicity(d, toxtree_result = tf)
res_on <- assign_toxicity(d, toxtree_result = tf, group_membership = TRUE)

lv_off <- as.character(res_off$Toxic_level)
lv_on <- as.character(res_on$Toxic_level)

cat("\n\n================ 对比 ================\n")
cat("关闭 group_membership:\n")
print(table(lv_off))
cat("\n开启 group_membership:\n")
print(table(lv_on))

chg <- which(lv_off != lv_on)
cat("\n等级变化行数:", length(chg), "\n")
if (length(chg)) {
  cat("\n明细:\n")
  for (i in chg) {
    cat(sprintf("  %-4s -> %-4s | %-30s %-26s %s\n",
                lv_off[i], lv_on[i],
                substr(d$Formula[i], 1, 30), substr(d$SMILES[i], 1, 26),
                substr(res_on$Toxic_level_basis[i], 1, 34)))
  }
}

cat("\n需人工复核（Group_review 非空）:", sum(res_on$Group_review != "-", na.rm = TRUE), "行\n")
cat("\nCOMPARE_DONE\n")
