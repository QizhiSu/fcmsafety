# Semantic-level check: strip pure formatting noise (whitespace, ordering, trailing
# newlines, decimal commas) and see if any REGULATORY content really changed.
suppressMessages({ library(devtools) })
load_all(".", quiet = TRUE)

f <- "C:/Users/13432/WorkBuddy/2026-08-31-14-16-57/fcmsafety/backups/clp_auto_test.xlsx"
raw <- read_source_table(f, expected_col = "Index No", two_row_header = TRUE)
df <- normalize_cmr_df(raw)
cmr <- screen_clp(df, "cmr")

con <- get_db_connection(NULL)
cur <- DBI::dbGetQuery(con, "SELECT * FROM cmr")
DBI::dbDisconnect(con)
new_mapped <- map_to_db_columns(cmr, "cmr", NULL)
new_mapped <- backfill_unmapped_cols(new_mapped, cur, "Index No")

new_keys <- trimws(as.character(new_mapped[["Index No"]]))
cur_keys <- trimws(as.character(cur[["Index No"]]))
cur_by <- split(seq_len(nrow(cur)), cur_keys)
new_by <- split(seq_len(nrow(new_mapped)), new_keys)
common <- intersect(new_keys, cur_keys)

# semantic token set of a cell: split by newline/;/space-ish boundaries where
# meaningful, trim each token, drop empties, sort, dedupe
tok <- function(x) {
  if (is.na(x)) return(character(0))
  x <- gsub("\r\r\n", "\n", x); x <- gsub("\r\n", "\n", x); x <- gsub("\r", "\n", x)
  parts <- unlist(strsplit(x, "\n", fixed = TRUE))
  parts <- trimws(parts)
  parts <- parts[nzchar(parts)]
  # collapse internal runs of spaces (chemical-formula spacing noise)
  parts <- gsub("[[:space:]]+", " ", parts)
  parts <- gsub("^\\s+|\\s+$", "", parts)
  sort(unique(parts[nzchar(parts)]))
}
sem <- function(x) paste(tok(x), collapse = "\x01")

cols <- c("Hazard Class and Category Code(s)", "Hazard Statement Code(s)",
          "Pictogram, Signal Word Code(s)", "Notes",
          "Suppl. Hazard statement Code(s)", "Specific Conc. Limits, M-factors",
          "International Chemical Identification")
cat(sprintf("%-45s %8s\n", "column", "real diffs"))
for (cc in cols) {
  n <- 0L; ex <- NULL
  for (k in common) {
    ci <- cur_by[[k]][1]; ni <- new_by[[k]][1]
    if (identical(sem(cur[[cc]][ci]), sem(new_mapped[[cc]][ni]))) next
    n <- n + 1L
    if (is.null(ex) && n <= 1) ex <- c(k, cur[[cc]][ci], new_mapped[[cc]][ni])
  }
  cat(sprintf("%-45s %8d\n", cc, n))
  if (!is.null(ex)) {
    cat("   example key:", ex[1], "\n")
    cat("   db :", substr(gsub("\r\r\n", " / ", ex[2]), 1, 120), "\n")
    cat("   new:", substr(gsub("\r\r\n", " / ", ex[3]), 1, 120), "\n")
  }
}
