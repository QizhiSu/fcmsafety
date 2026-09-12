# Inspect legacy meta file header + DB column fill rates
suppressMessages({ library(devtools) })
load_all(".", quiet = TRUE)

cat("== legacy meta file: inst/clp_cmr_meta.xlsx ==\n")
f2 <- "C:/Users/13432/WorkBuddy/2026-08-31-14-16-57/fcmsafety/inst/clp_cmr_meta.xlsx"
raw0 <- suppressWarnings(rio::import(f2))
cat("nrow:", nrow(raw0), "ncol:", ncol(raw0), "\n")
for (i in 1:min(3, nrow(raw0))) {
  cat(sprintf("row %d:", i))
  print(as.character(raw0[i, ]))
}
cat("cols:", paste(names(raw0), collapse = " | "), "\n")

cat("\n== DB cmr fill rates + sample ATP values ==\n")
con <- get_db_connection(NULL)
cur <- DBI::dbGetQuery(con, "SELECT * FROM cmr")
DBI::dbDisconnect(con)
for (col in names(cur)) {
  v <- trimws(as.character(cur[[col]]))
  nonempty <- sum(!is.na(v) & v != "" & !v %in% c("-", "NA"))
  cat(sprintf("  %-45s fill=%4d/%-4d (%.0f%%)\n", col, nonempty, nrow(cur), 100 * nonempty / nrow(cur)))
}
cat("\nATP distinct sample:", paste(utils::head(sort(unique(trimws(as.character(cur[["ATP inserted/ATP Updated"]])))), 20), collapse = " | "), "\n")
cat("ATP full distinct count:", length(unique(trimws(as.character(cur[["ATP inserted/ATP Updated"]])))), "\n")
cat("Alternative full distinct count:", length(unique(trimws(as.character(cur[["Hazard Statement Code Alternative"]])))), "\n")
