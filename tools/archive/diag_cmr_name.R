# Zoom into International Chemical Identification diffs
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

canon <- function(r) {
  r <- as.character(r); r[is.na(r)] <- ""
  Encoding(r) <- "UTF-8"
  r <- gsub("\r\r\n", "\n", r); r <- gsub("\r\n", "\n", r)
  r <- gsub("\r", "\n", r); r <- gsub("\n+", "\n", r)
  trimws(r)
}

new_keys <- trimws(as.character(new_mapped[["Index No"]]))
cur_keys <- trimws(as.character(cur[["Index No"]]))
cur_by <- split(seq_len(nrow(cur)), cur_keys)
new_by <- split(seq_len(nrow(new_mapped)), new_keys)
common <- intersect(new_keys, cur_keys)

NAME <- "International Chemical Identification"
shown <- 0L
for (k in common) {
  ci <- cur_by[[k]][1]; ni <- new_by[[k]][1]
  dv <- canon(cur[[NAME]][ci]); nv <- canon(new_mapped[[NAME]][ni])
  if (identical(dv, nv)) next
  if (shown >= 4) break
  shown <- shown + 1L
  cat(sprintf("\n===== key %s =====\n", k))
  cat("--- db ---\n");   cat(encodeString(dv, quote = ""), "\n")
  cat("--- new ---\n");  cat(encodeString(nv, quote = ""), "\n")
  # 找第一个不同字符位置
  dl <- strsplit(dv, "", fixed = TRUE)[[1]]; nl <- strsplit(nv, "", fixed = TRUE)[[1]]
  m <- min(length(dl), length(nl))
  first <- which(dl[1:m] != nl[1:m])[1]
  cat(sprintf("char-level first diff at pos %s (len db=%d new=%d)\n",
              ifelse(is.na(first), "NA(end)", first), length(dl), length(nl)))
  if (!is.na(first)) {
    cat("db  around: ", substr(dv, max(1, first - 25), min(nchar(dv), first + 25)), "\n")
    cat("new around: ", substr(nv, max(1, first - 25), min(nchar(nv), first + 25)), "\n")
  }
}
cat("\n(done)\n")
