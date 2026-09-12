# Diagnose why ~all CMR rows show as modified in the ATP23 dry run.
suppressMessages({ library(devtools) })
load_all(".", quiet = TRUE)

f <- "C:/Users/13432/WorkBuddy/2026-08-31-14-16-57/fcmsafety/backups/clp_auto_test.xlsx"
raw <- read_source_table(f, expected_col = "Index No", two_row_header = TRUE)
df <- normalize_cmr_df(raw)
cmr <- screen_clp(df, "cmr")

cat("== duplicate Index No in new cmr subset? ==\n")
dup <- sum(duplicated(cmr[["Index No"]]))
cat("duplicated rows:", dup, "of", nrow(cmr), "\n")

con <- get_db_connection(NULL)
cur <- DBI::dbGetQuery(con, "SELECT * FROM cmr")
DBI::dbDisconnect(con)
cat("db cmr rows:", nrow(cur), "\n")

cat("\n== sample: one common key, both sides ==\n")
common <- intersect(cmr[["Index No"]], cur[["Index No"]])
k <- common[1]
cat("key:", k, "\n")
new_row <- cmr[cmr[["Index No"]] == k, , drop = FALSE][1, ]
cur_row <- cur[cur[["Index No"]] == k, , drop = FALSE][1, ]
cat("--- new row ---\n"); print(as.data.frame(new_row))
cat("--- db row ---\n"); print(as.data.frame(cur_row))

cat("\n== column name overlap ==\n")
cat("new cols:", paste(names(cmr), collapse = " | "), "\n")
cat("db  cols:", paste(names(cur), collapse = " | "), "\n")
