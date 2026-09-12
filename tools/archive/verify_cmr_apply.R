# Post-apply sanity check
suppressMessages({ library(devtools) })
load_all(".", quiet = TRUE)
con <- get_db_connection(NULL)
for (t in c("cmr", "cmr_suspect")) {
  n <- DBI::dbGetQuery(con, sprintf("SELECT COUNT(*) n FROM %s", t))$n
  cid <- DBI::dbGetQuery(con, sprintf("SELECT COUNT(*) n FROM %s WHERE CID IS NOT NULL AND CID != ''", t))$n
  ik <- DBI::dbGetQuery(con, sprintf("SELECT COUNT(*) n FROM %s WHERE InChIKey IS NOT NULL AND InChIKey != ''", t))$n
  cat(sprintf("%-12s rows=%d  withCID=%d  withInChIKey=%d\n", t, n, cid, ik))
}
q <- DBI::dbGetQuery(con, 'SELECT "Index No", "International Chemical Identification" FROM cmr WHERE "Index No" = "022-006-00-2"')
cat("TiO2 present in cmr (expect 0):", nrow(q), "\n")
q2 <- DBI::dbGetQuery(con, 'SELECT "Index No" FROM cmr_suspect WHERE "Index No" = "022-006-00-2"')
cat("TiO2 present in cmr_suspect (expect 0):", nrow(q2), "\n")
new_cmr <- DBI::dbGetQuery(con, 'SELECT "Index No", "International Chemical Identification", CID FROM cmr WHERE "Index No" IN ("008-004-00-4", "601-027-00-6")')
cat("new ATP23 cmr entries (ozone, 2-phenylpropene):\n")
for (i in seq_len(nrow(new_cmr))) cat("  ", new_cmr[i, 1], "|", new_cmr[i, 2], "| CID", new_cmr[i, 3], "\n")
DBI::dbDisconnect(con)
cat("OK\n")
