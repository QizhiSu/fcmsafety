# Tests for the "official key preferred, CAS/Agent fallback" strategy now used by
# cmr / cmr_suspect / iarc / eu_sml. Goal: when the official key (Index No,
# FCM substance No, CAS No.) is missing, we still match and delete rows using
# the fallback column, instead of mis-reporting them as removed + added.

make_minimal_df <- function(cols, ...) {
  rows <- list(...)
  out <- data.frame(matrix(NA_character_, nrow = length(rows), ncol = length(cols)),
                    stringsAsFactors = FALSE)
  names(out) <- cols
  for (i in seq_along(rows)) {
    for (nm in names(rows[[i]])) out[[nm]][i] <- rows[[i]][[nm]]
  }
  out
}

iarc_cols <- c("Agent", "CAS No.", "Group", "Volume publication year",
               "CID", "Formula", "SMILES", "InChIKey", "IUPACName", "ExactMass")
cmr_cols <- c("Index No", "CAS No", "EC No", "International Chemical Identification",
              "Hazard Statement Code(s)", "CID", "Formula", "SMILES", "InChIKey",
              "IUPACName", "ExactMass")
eu_cols <- c("FCM substance No", "CAS No", "Substance name", "SML",
             "CID", "Formula", "SMILES", "InChIKey", "IUPACName", "ExactMass")

# ---- diff_incremental: IARC CAS preferred, Agent fallback ----

test_that("diff_incremental matches IARC rows by CAS when Agent changes", {
  cur <- make_minimal_df(iarc_cols,
    list(Agent = "Benzene", "CAS No." = "71-43-2", Group = "1"),
    list(Agent = "Toluene", "CAS No." = "108-88-3", Group = "2A")
  )
  new <- make_minimal_df(iarc_cols,
    list(Agent = "Benzene (updated)", "CAS No." = "71-43-2", Group = "1"),
    list(Agent = "Toluene", "CAS No." = "108-88-3", Group = "2A")
  )

  changes <- fcmsafety:::diff_incremental(new, cur,
    key_col = "CAS No.", fallback_col = "Agent", content_cols = NULL,
    cas_col = "CAS No.")

  expect_equal(changes$total_added, 0L)
  expect_equal(changes$total_removed, 0L)
  expect_equal(changes$total_modified, 1L)
  expect_equal(changes$modified[["CAS No."]], "71-43-2")
})

test_that("diff_incremental uses Agent fallback when IARC CAS is blank", {
  cur <- make_minimal_df(iarc_cols,
    list(Agent = "Some mixture", "CAS No." = NA_character_, Group = "2B")
  )
  new <- make_minimal_df(iarc_cols,
    list(Agent = "Some mixture", "CAS No." = NA_character_, Group = "2A")
  )

  changes <- fcmsafety:::diff_incremental(new, cur,
    key_col = "CAS No.", fallback_col = "Agent", content_cols = NULL,
    cas_col = "CAS No.")

  expect_equal(changes$total_added, 0L)
  expect_equal(changes$total_removed, 0L)
  expect_equal(changes$total_modified, 1L)
  expect_equal(changes$modified[["Agent"]], "Some mixture")
})

test_that("diff_incremental reports IARC fallback row removal when Agent disappears", {
  cur <- make_minimal_df(iarc_cols,
    list(Agent = "Only by name", "CAS No." = NA_character_, Group = "2B")
  )
  new <- make_minimal_df(iarc_cols)

  changes <- fcmsafety:::diff_incremental(new, cur,
    key_col = "CAS No.", fallback_col = "Agent", content_cols = NULL,
    cas_col = "CAS No.")

  expect_equal(changes$total_added, 0L)
  expect_equal(changes$total_removed, 1L)
  expect_equal(changes$total_modified, 0L)
  expect_equal(changes$removed[["Agent"]], "Only by name")
})

# ---- diff_incremental: CMR / EU SML CAS fallback ----

test_that("diff_incremental matches CMR rows by CAS when Index No is blank", {
  cur <- make_minimal_df(cmr_cols,
    list("Index No" = NA_character_, "CAS No" = "71-43-2",
         "International Chemical Identification" = "Benzene")
  )
  new <- make_minimal_df(cmr_cols,
    list("Index No" = NA_character_, "CAS No" = "71-43-2",
         "International Chemical Identification" = "Benzene (updated)")
  )

  changes <- fcmsafety:::diff_incremental(new, cur,
    key_col = "Index No", fallback_col = "CAS No", content_cols = NULL,
    cas_col = "CAS No")

  expect_equal(changes$total_added, 0L)
  expect_equal(changes$total_removed, 0L)
  expect_equal(changes$total_modified, 1L)
  expect_equal(changes$modified[["CAS No"]], "71-43-2")
})

test_that("diff_incremental matches EU SML rows by CAS when FCM No is blank", {
  cur <- make_minimal_df(eu_cols,
    list("FCM substance No" = NA_character_, "CAS No" = "80-05-7",
         "Substance name" = "Bisphenol A")
  )
  new <- make_minimal_df(eu_cols,
    list("FCM substance No" = NA_character_, "CAS No" = "80-05-7",
         "Substance name" = "Bisphenol A (updated)")
  )

  changes <- fcmsafety:::diff_incremental(new, cur,
    key_col = "FCM substance No", fallback_col = "CAS No", content_cols = NULL,
    cas_col = "CAS No")

  expect_equal(changes$total_added, 0L)
  expect_equal(changes$total_removed, 0L)
  expect_equal(changes$total_modified, 1L)
  expect_equal(changes$modified[["CAS No"]], "80-05-7")
})

# ---- write_changes_to_db: fallback-row deletion ----

make_iarc_table <- function(con) {
  DBI::dbExecute(con, 'CREATE TABLE iarc (
    "Agent" TEXT, "CAS No." TEXT, "Group" TEXT,
    "Volume publication year" TEXT,
    "CID" TEXT, "Formula" TEXT, "SMILES" TEXT, "InChIKey" TEXT,
    "IUPACName" TEXT, "ExactMass" TEXT
  )')
}

test_that("write_changes_to_db deletes IARC fallback rows by Agent when CAS is blank", {
  db_path <- tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  make_iarc_table(con)
  DBI::dbExecute(con,
    'INSERT INTO iarc ("Agent", "CAS No.", "Group") VALUES (?, ?, ?)',
    params = list("Only by name", NA_character_, "2B"))
  DBI::dbDisconnect(con)

  # removed row has blank CAS but the same Agent -> delete by fallback_col
  removed <- make_minimal_df(iarc_cols,
    list(Agent = "Only by name", "CAS No." = NA_character_, Group = "2B")
  )
  empty <- make_minimal_df(iarc_cols)
  changes <- list(
    total_added = 0L, total_removed = 1L, total_modified = 0L,
    added = empty, removed = removed, modified = empty
  )

  fcmsafety:::write_changes_to_db("iarc", changes,
    key_col = "CAS No.", fallback_col = "Agent",
    db_path = db_path, backup = FALSE)

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  remain <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM iarc")
  DBI::dbDisconnect(con)
  expect_equal(remain$n, 0L)
  unlink(db_path)
})

test_that("write_changes_to_db deletes IARC rows by CAS when present", {
  db_path <- tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  make_iarc_table(con)
  DBI::dbExecute(con,
    'INSERT INTO iarc ("Agent", "CAS No.", "Group") VALUES (?, ?, ?)',
    params = list("Benzene", "71-43-2", "1"))
  DBI::dbDisconnect(con)

  modified <- make_minimal_df(iarc_cols,
    list(Agent = "Benzene (updated)", "CAS No." = "71-43-2", Group = "1")
  )
  empty <- make_minimal_df(iarc_cols)
  changes <- list(
    total_added = 0L, total_removed = 0L, total_modified = 1L,
    added = empty, removed = empty, modified = modified
  )

  fcmsafety:::write_changes_to_db("iarc", changes,
    key_col = "CAS No.", fallback_col = "Agent",
    db_path = db_path, backup = FALSE)

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  rows <- DBI::dbGetQuery(con, 'SELECT "Agent", "Group" FROM iarc')
  DBI::dbDisconnect(con)
  expect_equal(nrow(rows), 1L)
  expect_equal(rows$Agent, "Benzene (updated)")
  expect_equal(rows$Group, "1")
  unlink(db_path)
})
