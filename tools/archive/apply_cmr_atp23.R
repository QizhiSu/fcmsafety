# Apply ATP23 official export to cmr + cmr_suspect (FULL format upgrade).
# Decision: user-approved on 2026-09-03 after dry-run + semantic verification.
#
# Pipeline mirrors run_incremental_update() but with programmatic preflight
# instead of interactive confirmation:
#   read -> normalize -> screen -> map_to_db_columns -> backfill_meta_from_db
#   -> enrich_new_compounds (PubChem, fills chem cols DB-wide) -> read current
#   -> backfill_unmapped_cols (legacy cols w/o source) -> diff_incremental
#   -> preflight asserts (added/removed/modified == dry-run expectations)
#   -> write_changes_to_db (transaction + backup).
suppressMessages({ library(devtools) })
load_all(".", quiet = TRUE)

f <- "C:/Users/13432/WorkBuddy/2026-08-31-14-16-57/fcmsafety/backups/clp_auto_test.xlsx"
if (!file.exists(f)) stop("missing file: ", f)

# dry-run expectations (from tools/dry_run_cmr_atp23.R after fixes)
EXPECT <- list(
  cmr         = list(added = 11L, removed = 0L, modified = 987L),
  cmr_suspect = list(added = 5L,  removed = 1L, modified = 344L)
)

cat("== parse ATP23 export ==\n")
raw <- read_source_table(f, expected_col = "Index No", two_row_header = TRUE)
df <- normalize_cmr_df(raw)
cat("raw rows:", nrow(raw), "\n")

apply_one <- function(db_name, kind, exp) {
  cat(sprintf("\n================ %s (%s) ================\n", db_name, kind))
  new_df <- screen_clp(df, kind)
  cat("screened rows:", nrow(new_df), "\n")

  cat("[1/6] map_to_db_columns ...\n")
  new_df <- map_to_db_columns(new_df, db_name, NULL)

  cat("[2/6] backfill_meta_from_db (chem cols from DB) ...\n")
  new_df <- backfill_meta_from_db(new_df, db_name, "CAS No", db_path = NULL)

  cat("[3/6] enrich_new_compounds (PubChem lookups) ...\n")
  t0 <- Sys.time()
  new_df <- enrich_new_compounds(new_df, "CAS No",
                                 "International Chemical Identification",
                                 delay = 0.35, verbose = TRUE)
  cat(sprintf("      enrich took %.1f min\n", as.numeric(difftime(Sys.time(), t0, units = "mins"))))

  cat("[4/6] read current DB rows ...\n")
  con <- get_db_connection(NULL)
  current <- DBI::dbGetQuery(con, paste("SELECT * FROM", db_name))
  DBI::dbDisconnect(con)
  cat("      current db rows:", nrow(current), "\n")

  cat("[5/6] backfill_unmapped_cols + diff ...\n")
  new_df <- backfill_unmapped_cols(new_df, current, "Index No")
  changes <- diff_incremental(new_df, current, "Index No", fallback_col = "CAS No",
                              content_cols = NULL, cas_col = "CAS No")
  cat("      diff: +", changes$total_added, " / -", changes$total_removed,
      " / ~", changes$total_modified, "\n", sep = "")

  # preflight: numbers must match the user-approved dry run
  if (changes$total_added != exp$added ||
      changes$total_removed != exp$removed ||
      changes$total_modified != exp$modified) {
    stop(sprintf("%s: preflight mismatch! expected +%d/-%d/~%d, got +%d/-%d/~%d",
                 db_name, exp$added, exp$removed, exp$modified,
                 changes$total_added, changes$total_removed, changes$total_modified))
  }
  cat("      preflight OK (matches approved dry-run)\n")

  cat("[6/6] write_changes_to_db (backup + transaction) ...\n")
  db_write <- write_changes_to_db(db_name, changes, "Index No",
                                  fallback_col = "CAS No", db_path = NULL, backup = TRUE)
  cat("      write done: +", db_write$records_added, " / -", db_write$records_removed,
      " / ~", db_write$records_modified, "\n", sep = "")
  invisible(changes)
}

res_cmr <- apply_one("cmr", "cmr", EXPECT$cmr)
res_sus <- apply_one("cmr_suspect", "cmr_suspect", EXPECT$cmr_suspect)

cat("\n== ALL DONE ==")
cat("\ncmr        : +", res_cmr$total_added, "/ -", res_cmr$total_removed,
    "/ ~", res_cmr$total_modified, sep = "")
cat("\ncmr_suspect: +", res_sus$total_added, "/ -", res_sus$total_removed,
    "/ ~", res_sus$total_modified, sep = "")
cat("\n")
