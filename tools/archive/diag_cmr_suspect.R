# Inspect cmr_suspect table + find the removed key in the dry run
suppressMessages({ library(devtools) })
load_all(".", quiet = TRUE)

f <- "C:/Users/13432/WorkBuddy/2026-08-31-14-16-57/fcmsafety/backups/clp_auto_test.xlsx"
raw <- read_source_table(f, expected_col = "Index No", two_row_header = TRUE)
df <- normalize_cmr_df(raw)
sus <- screen_clp(df, "cmr_suspect")

con <- get_db_connection(NULL)
info <- DBI::dbGetQuery(con, "PRAGMA table_info(cmr_suspect)")
cat("== cmr_suspect db cols ==\n"); cat(paste(info$name, collapse = " | "), "\n")
cur <- DBI::dbGetQuery(con, "SELECT * FROM cmr_suspect")
DBI::dbDisconnect(con)
cat("db rows:", nrow(cur), "| new rows:", nrow(sus), "\n")

new_keys <- trimws(as.character(sus[["Index No"]]))
cur_keys <- trimws(as.character(cur[["Index No"]]))
added <- setdiff(new_keys, cur_keys)
removed <- setdiff(cur_keys, new_keys)
cat("added:", length(added), "| removed:", length(removed), "\n")
if (length(removed) > 0) {
  cat("-- removed keys (in db but not in new ATP23) --\n")
  for (k in removed) {
    row <- cur[cur_keys == k, , drop = FALSE][1, ]
    cat(sprintf("  %s | %s | EC %s | CAS %s\n", k,
                row[["International Chemical Identification"]],
                row[["EC No"]], row[["CAS No"]]))
  }
}
if (length(added) > 0) {
  cat("-- added keys (new in ATP23) --\n")
  for (k in added) {
    row <- sus[new_keys == k, , drop = FALSE][1, ]
    cat(sprintf("  %s | %s | EC %s | CAS %s\n", k,
                row[["Chemical Name"]], row[["EC No"]], row[["CAS No"]]))
  }
}
