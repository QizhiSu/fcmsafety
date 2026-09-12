# Dry-run: consume the freshly auto-downloaded ECHA ATP23 xlsx
# (backups/clp_auto_test.xlsx) through the real CMR pipeline and report the
# incremental diff vs the current DB. NEVER writes the DB (interactive=FALSE,
# auto_apply=FALSE -> "Dry run (no apply)").
suppressMessages({ library(devtools) })
load_all(".", quiet = TRUE)

f <- "C:/Users/13432/WorkBuddy/2026-08-31-14-16-57/fcmsafety/backups/clp_auto_test.xlsx"
if (!file.exists(f)) stop("missing file: ", f)

cat("== parse ATP23 export ==\n")
raw <- read_source_table(f, expected_col = "Index No", two_row_header = TRUE)
cat("raw rows:", nrow(raw), "| cols:", ncol(raw), "\n")

df <- normalize_cmr_df(raw)
cmr <- screen_clp(df, "cmr")
sus <- screen_clp(df, "cmr_suspect")
cat("after CMR screen:", nrow(cmr), "rows | after CMR_suspect screen:", nrow(sus), "rows\n")

cat("\n== DRY-RUN diff: cmr ==\n")
r1 <- run_incremental_update(
  db_name = "cmr", new_df = cmr,
  key_col = "Index No", fallback_col = "CAS No",
  cas_col = "CAS No", name_col = "International Chemical Identification",
  interactive = FALSE, auto_apply = FALSE, enrich = FALSE
)
cat("outcome:", r1$message, "\n")

cat("\n== DRY-RUN diff: cmr_suspect ==\n")
r2 <- run_incremental_update(
  db_name = "cmr_suspect", new_df = sus,
  key_col = "Index No", fallback_col = "CAS No",
  cas_col = "CAS No", name_col = "International Chemical Identification",
  interactive = FALSE, auto_apply = FALSE, enrich = FALSE
)
cat("outcome:", r2$message, "\n")

cat("\nDONE (no DB writes performed)\n")
