# Diagnose column mismatch between fresh ECHA ATP23 export and DB cmr table.
# Step 1: what does the new file actually look like at every stage?
# Step 2: which DB column does each source column get mapped to (match_col)?
suppressMessages({ library(devtools) })
load_all(".", quiet = TRUE)

f <- "C:/Users/13432/WorkBuddy/2026-08-31-14-16-57/fcmsafety/backups/clp_auto_test.xlsx"
cat("== file:", basename(f), "==\n")

# --- raw read with NO skip: see the true header rows ---
raw0 <- suppressWarnings(rio::import(f))
cat("\n-- raw0 (skip=0): nrow =", nrow(raw0), "ncol =", ncol(raw0), "--\n")
for (i in 1:min(3, nrow(raw0))) {
  cat(sprintf("row %d:", i))
  print(as.character(raw0[i, ]))
}

# --- what merge_clp_subheader would do: simulate on raw0 ---
cat("\n-- merge_clp_subheader(raw0) column names --\n")
merged <- fcmsafety:::merge_clp_subheader(raw0)
print(names(merged))
cat("nrow after merge:", nrow(merged), "\n")

# --- compare: normalize path used by pipeline ---
raw <- read_source_table(f, expected_col = "Index No", two_row_header = TRUE)
cat("\n-- read_source_table(two_row_header=TRUE) nrow:", nrow(raw), "ncol:", ncol(raw), "--\n")
cat("cols:", paste(names(raw), collapse = " | "), "\n")
df <- normalize_cmr_df(raw)
cat("\n-- after normalize_cmr_df: ncol:", ncol(df), "--\n")
cat("cols:", paste(names(df), collapse = " | "), "\n")

# --- DB structure ---
con <- get_db_connection(NULL)
info <- DBI::dbGetQuery(con, "PRAGMA table_info(cmr)")
db_cols <- info$name
cur <- DBI::dbGetQuery(con, "SELECT * FROM cmr")
DBI::dbDisconnect(con)
cat("\n-- DB cmr table: nrow =", nrow(cur), "ncol =", ncol(cur), "--\n")
cat("db cols:", paste(db_cols, collapse = " | "), "\n")

# --- mapping report: for each DB column, which source column wins under match_col ---
cat("\n-- match_col mapping: db col -> (raw col | matched stage) --\n")
strip <- function(x) gsub("[[:space:]]+", "", x)
for (col in db_cols) {
  # simulate match_col precedence against normalize_cmr_df output names
  hit <- NA_character_
  if (col %in% names(df)) {
    hit <- col
  } else {
    nm_strip <- strip(names(df)); pat <- strip(col)
    idx <- which(nm_strip == pat)
    if (length(idx) == 0) idx <- which(tolower(nm_strip) == tolower(pat))
    if (length(idx) == 0) idx <- which(vapply(tolower(nm_strip), function(nm) grepl(tolower(pat), nm, fixed = TRUE), logical(1)))
    if (length(idx) > 0) hit <- names(df)[idx[1]]
  }
  cat(sprintf("  %-42s -> %s\n", col, ifelse(is.na(hit), "<MISSING>", hit)))
}

# --- cross-check with one known common substance (Beryllium) ---
cat("\n-- Beryllium row: db vs mapped new --\n")
db_be <- cur[grepl("^004-001-00-7$", trimws(cur[["Index No"]])) | grepl("^004-001-00-7$", cur[["Index No"]]), , drop = FALSE]
be_key <- intersect(df[["Index No"]], cur[["Index No"]])[1]
cat("first common Index No:", be_key, "\n")
new_row <- df[df[["Index No"]] == be_key, , drop = FALSE][1, ]
db_row <- cur[cur[["Index No"]] == be_key, , drop = FALSE][1, ]
for (col in db_cols) {
  nv <- as.character(new_row[[col]])
  dv <- as.character(db_row[[col]])
  if (length(nv) != 1) nv <- paste(nv, collapse = ";")
  if (length(dv) != 1) dv <- paste(dv, collapse = ";")
  flag <- ifelse(is.na(nv) && is.na(dv), "  same(NA)",
          ifelse(identical(trimws(nv), trimws(dv)), "  same", "  DIFF"))
  cat(sprintf("  %-42s | new=%s | db=%s %s\n", col,
              substr(gsub("\n", "\\n", nv), 1, 60),
              substr(gsub("\n", "\\n", dv), 1, 60), flag))
}
