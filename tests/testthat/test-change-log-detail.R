# Tests: write_changes_to_db() / write_svhc_to_db() record per-substance detail
# rows into change_log (added/removed: one row each; modified: one row per
# changed field with old_value -> new_value), linked to the update_history row.
#
# Table column layout follows the rebuilt snake schema (svhc 业务表列名
# substance_name / cas_no ..., 见 auto_update_svhc.R 的 svhc_db_columns)。

mkdf <- function(cols, ...) {
  rows <- list(...)
  out <- data.frame(matrix(NA_character_, nrow = length(rows), ncol = length(cols)),
                    stringsAsFactors = FALSE)
  names(out) <- cols
  for (i in seq_along(rows)) {
    for (nm in names(rows[[i]])) out[[nm]][i] <- rows[[i]][[nm]]
  }
  out
}

# update_history + change_log tables exactly as defined in fcmsafety_schema.sql
# (FKs omitted: SQLite foreign_keys are off by default in the test connection).
make_audit_tables <- function(con) {
  DBI::dbExecute(con, 'CREATE TABLE update_history (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    database_name TEXT NOT NULL,
    update_timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    update_type TEXT NOT NULL,
    records_added INTEGER DEFAULT 0,
    records_removed INTEGER DEFAULT 0,
    records_modified INTEGER DEFAULT 0,
    source_file TEXT,
    user_notes TEXT,
    success BOOLEAN DEFAULT TRUE,
    error_message TEXT
  )')
  DBI::dbExecute(con, 'CREATE TABLE change_log (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    update_history_id INTEGER NOT NULL,
    database_name TEXT NOT NULL,
    InChIKey TEXT,
    substance_identifier TEXT,
    change_type TEXT NOT NULL,
    field_name TEXT,
    old_value TEXT,
    new_value TEXT,
    timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  )')
}

# ---- generic path (cmr/iarc/eu_sml share write_changes_to_db) ----

iarc_cols <- c("Agent", "CAS No.", "Group", "Volume publication year",
               "CID", "Formula", "SMILES", "InChIKey", "IUPACName", "ExactMass")

make_iarc_tables <- function(con) {
  DBI::dbExecute(con, 'CREATE TABLE iarc (
    "Agent" TEXT, "CAS No." TEXT, "Group" TEXT,
    "Volume publication year" TEXT,
    "CID" TEXT, "Formula" TEXT, "SMILES" TEXT, "InChIKey" TEXT,
    "IUPACName" TEXT, "ExactMass" TEXT
  )')
  make_audit_tables(con)
}

test_that("write_changes_to_db records added/removed/modified details in change_log", {
  db_path <- tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  make_iarc_tables(con)
  # old state: Benzene (will be modified) + o-Toluidine (will be removed)
  DBI::dbExecute(con,
    'INSERT INTO iarc ("Agent", "CAS No.", "Group") VALUES (?, ?, ?)',
    params = list("Benzene", "71-43-2", "1"))
  DBI::dbExecute(con,
    'INSERT INTO iarc ("Agent", "CAS No.", "Group") VALUES (?, ?, ?)',
    params = list("o-Toluidine", "95-53-4", "2B"))
  DBI::dbDisconnect(con)

  added <- mkdf(iarc_cols, list(Agent = "Toluene", "CAS No." = "108-88-3", Group = "2A"))
  removed <- mkdf(iarc_cols, list(Agent = "o-Toluidine", "CAS No." = "95-53-4", Group = "2B"))
  modified <- mkdf(iarc_cols,
    list(Agent = "Benzene (updated)", "CAS No." = "71-43-2", Group = "2A"))
  empty <- mkdf(iarc_cols)
  changes <- list(
    total_added = 1L, total_removed = 1L, total_modified = 1L,
    added = added, removed = removed, modified = modified
  )

  fcmsafety:::write_changes_to_db("iarc", changes,
    key_col = "CAS No.", fallback_col = "Agent",
    db_path = db_path, backup = FALSE)

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  hist <- DBI::dbGetQuery(con, "SELECT * FROM update_history")
  logs <- DBI::dbGetQuery(con,
    "SELECT * FROM change_log ORDER BY id")
  DBI::dbDisconnect(con)

  # one session row with the summary counts
  expect_equal(nrow(hist), 1L)
  expect_equal(hist$database_name, "iarc")
  expect_equal(hist$records_added, 1L)
  expect_equal(hist$records_removed, 1L)
  expect_equal(hist$records_modified, 1L)

  # detail rows: 1 added + 1 removed + 2 field-level modified = 4
  expect_equal(nrow(logs), 4L)
  expect_true(all(logs$update_history_id == hist$id[1]))

  a <- logs[logs$change_type == "added", ]
  expect_equal(nrow(a), 1L)
  expect_equal(a$substance_identifier, "Toluene")
  expect_true(is.na(a$field_name))

  r <- logs[logs$change_type == "removed", ]
  expect_equal(nrow(r), 1L)
  expect_equal(r$substance_identifier, "o-Toluidine")

  m <- logs[logs$change_type == "modified", ]
  expect_equal(nrow(m), 2L)
  agent_row <- m[m$field_name == "Agent", ]
  expect_equal(nrow(agent_row), 1L)
  expect_equal(agent_row$old_value, "Benzene")
  expect_equal(agent_row$new_value, "Benzene (updated)")
  group_row <- m[m$field_name == "Group", ]
  expect_equal(nrow(group_row), 1L)
  expect_equal(group_row$old_value, "1")
  expect_equal(group_row$new_value, "2A")
  # substance_identifier comes from the NEW row (name already updated)
  expect_equal(group_row$substance_identifier, "Benzene (updated)")

  unlink(db_path)
})

test_that("write_changes_to_db writes nothing when change_log is absent", {
  # regression: old behaviour (no audit tables) must keep working silently
  db_path <- tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbExecute(con, 'CREATE TABLE iarc (
    "Agent" TEXT, "CAS No." TEXT, "Group" TEXT,
    "CID" TEXT, "Formula" TEXT, "SMILES" TEXT, "InChIKey" TEXT,
    "IUPACName" TEXT, "ExactMass" TEXT
  )')
  DBI::dbDisconnect(con)

  added <- mkdf(iarc_cols, list(Agent = "Toluene", "CAS No." = "108-88-3", Group = "2A"))
  empty <- mkdf(iarc_cols)
  changes <- list(
    total_added = 1L, total_removed = 0L, total_modified = 0L,
    added = added, removed = empty, modified = empty
  )

  expect_error(fcmsafety:::write_changes_to_db("iarc", changes,
    key_col = "CAS No.", fallback_col = "Agent",
    db_path = db_path, backup = FALSE), NA)

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  n <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM iarc")$n
  DBI::dbDisconnect(con)
  expect_equal(n, 1L)
  unlink(db_path)
})

# ---- SVHC path (write_svhc_to_db + svhc_key_of + svhc_content_columns) ----
# snake 业务表同构：11 内容列 + InChIKey（真实表另有 id/created_at/updated_at，
# 由 write 的 ins_cols 逻辑排除，测试表不建系统列也能覆盖该逻辑）

svhc_cols <- c("substance_name", "description", "ec_no", "cas_no",
               "reason_for_inclusion", "date_of_inclusion", "decision",
               "iuclid_dataset", "support_document", "response_to_comments",
               "remarks", "CID", "Formula", "SMILES", "InChIKey",
               "IUPACName", "ExactMass")

make_svhc_tables <- function(con) {
  DBI::dbExecute(con, 'CREATE TABLE svhc (
    "substance_name" TEXT, "description" TEXT, "ec_no" TEXT, "cas_no" TEXT,
    "reason_for_inclusion" TEXT, "date_of_inclusion" TEXT, "decision" TEXT,
    "iuclid_dataset" TEXT, "support_document" TEXT, "response_to_comments" TEXT,
    "remarks" TEXT, "InChIKey" TEXT
  )')
  make_audit_tables(con)
}

test_that("write_svhc_to_db records field-level modified details in change_log", {
  db_path <- tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  make_svhc_tables(con)
  DBI::dbExecute(con,
    'INSERT INTO svhc (substance_name, cas_no, remarks, "InChIKey") VALUES (?, ?, ?, ?)',
    params = list("Bisphenol A", "80-05-7", "old remark",
                  "IISBACOOKDOVLU-UHFFFAOYSA-N"))
  DBI::dbDisconnect(con)

  # new full data set (what update_svhc_auto would pass): same substance,
  # remark changed; one brand-new substance added
  new_df <- mkdf(svhc_cols,
    list(substance_name = "Bisphenol A", cas_no = "80-05-7",
         remarks = "new remark", InChIKey = "IISBACOOKDOVLU-UHFFFAOYSA-N"),
    list(substance_name = "Some new SVHC", cas_no = "12-34-5",
         remarks = NA_character_, InChIKey = NA_character_))
  added <- new_df[2, , drop = FALSE]
  modified <- new_df[1, , drop = FALSE]
  empty <- new_df[0, , drop = FALSE]
  changes <- list(
    total_added = 1L, total_removed = 0L, total_modified = 1L,
    added = added, removed = empty, modified = modified
  )

  fcmsafety:::write_svhc_to_db(new_df, changes, db_path = db_path, backup = FALSE)

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  hist <- DBI::dbGetQuery(con, "SELECT * FROM update_history")
  logs <- DBI::dbGetQuery(con, "SELECT * FROM change_log ORDER BY id")
  DBI::dbDisconnect(con)

  expect_equal(nrow(hist), 1L)
  expect_equal(hist$database_name, "svhc")
  expect_equal(hist$records_added, 1L)
  expect_equal(hist$records_modified, 1L)

  # 1 added + 1 field-level modified (Remarks) = 2 detail rows
  expect_equal(nrow(logs), 2L)
  expect_true(all(logs$update_history_id == hist$id[1]))

  a <- logs[logs$change_type == "added", ]
  expect_equal(a$substance_identifier, "Some new SVHC")

  m <- logs[logs$change_type == "modified", ]
  expect_equal(nrow(m), 1L)
  expect_equal(m$field_name, "remarks")
  expect_equal(m$old_value, "old remark")
  expect_equal(m$new_value, "new remark")
  expect_equal(m$substance_identifier, "Bisphenol A")
  expect_equal(m$InChIKey, "IISBACOOKDOVLU-UHFFFAOYSA-N")

  unlink(db_path)
})
