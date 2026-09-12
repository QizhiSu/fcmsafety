# Column-level diff attribution: after map+backfill, which DB columns still differ?
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
common <- intersect(new_keys, cur_keys)

content_cols <- setdiff(intersect(names(new_mapped), names(cur)),
                        c(chem_cols, "id", "created_at", "updated_at"))
cat("content cols compared:", paste(content_cols, collapse = " | "), "\n\n")

canon <- function(r) {
  r <- as.character(r); r[is.na(r)] <- ""
  Encoding(r) <- "UTF-8"
  r <- gsub("\r\r\n", "\n", r); r <- gsub("\r\n", "\n", r)
  r <- gsub("\r", "\n", r); r <- gsub("\n+", "\n", r)
  trimws(r)
}

cur_by <- split(seq_len(nrow(cur)), cur_keys)
new_by <- split(seq_len(nrow(new_mapped)), new_keys)

col_diff <- setNames(integer(length(content_cols)), content_cols)
examples <- list()
for (k in common) {
  ci <- cur_by[[k]]; ni <- new_by[[k]]
  if (is.null(ci) || is.null(ni)) next
  # 用第一行代表性比较（本表 key 唯一）
  cur_row <- canon(cur[ci[1], content_cols, drop = FALSE])
  new_row <- canon(new_mapped[ni[1], content_cols, drop = FALSE])
  for (j in seq_along(content_cols)) {
    cc <- content_cols[j]
    if (!identical(cur_row[j], new_row[j])) {
      col_diff[[cc]] <- col_diff[[cc]] + 1L
      if (length(examples) < 6 || !is.null(examples[[cc]])) {
        if (is.null(examples[[cc]])) examples[[cc]] <- list()
        if (length(examples[[cc]]) < 2) {
          examples[[cc]] <- c(examples[[cc]], list(list(key = k, db = substr(cur_row[j], 1, 90),
                                                        new = substr(new_row[j], 1, 90))))
        }
      }
    }
  }
}

ord <- sort(col_diff, decreasing = TRUE)
for (cc in names(ord)) {
  if (ord[[cc]] == 0) next
  cat(sprintf("%-45s DIFF on %5d / %d rows\n", cc, ord[[cc]], length(common)))
  for (ex in examples[[cc]]) {
    cat(sprintf("    key %-16s db: [%s]\n", ex$key, ex$db))
    cat(sprintf("                 new: [%s]\n", ex$new))
  }
}
