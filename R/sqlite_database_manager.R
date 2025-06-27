#' SQLite Database Manager for FCMSafety
#'
#' This module provides comprehensive SQLite database management functionality
#' to replace the xlsx-based database system with enhanced performance,
#' update tracking, and audit capabilities.
#'
#' @importFrom DBI dbConnect dbDisconnect dbExecute dbGetQuery dbWriteTable dbExistsTable
#' @importFrom RSQLite SQLite
#' @importFrom dplyr filter mutate rename select
#' @importFrom stringr str_replace str_remove_all
#' @importFrom rio import

#' Get Database Connection
#'
#' Creates or returns a connection to the FCMSafety SQLite database.
#' The database is created in the inst/ directory for local development
#' or in a user data directory for installed packages.
#'
#' @param db_path Optional custom path to database file
#' @return DBI connection object
#' @export
get_db_connection <- function(db_path = NULL) {
  if (is.null(db_path)) {
    # Determine database location
    if (dir.exists(file.path(getwd(), "inst"))) {
      # Development mode - use inst/ directory
      db_path <- file.path(getwd(), "inst", "fcmsafety.db")
    } else {
      # Installed package mode - use user data directory
      user_dir <- tools::R_user_dir("fcmsafety", which = "data")
      if (!dir.exists(user_dir)) {
        dir.create(user_dir, recursive = TRUE)
      }
      db_path <- file.path(user_dir, "fcmsafety.db")
    }
  }

  # Create connection
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

  # Enable foreign key constraints
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON")

  return(con)
}

#' Initialize Database
#'
#' Creates the SQLite database with the complete schema if it doesn't exist.
#' This function is idempotent - safe to run multiple times.
#'
#' @param force_recreate Logical, whether to drop and recreate existing database
#' @return Logical indicating success
#' @export
initialize_database <- function(force_recreate = FALSE) {
  message("🔧 Initializing FCMSafety SQLite database...")

  tryCatch({
    con <- get_db_connection()
    on.exit(DBI::dbDisconnect(con))

    # Check if database already exists
    if (DBI::dbExistsTable(con, "chemicals") && !force_recreate) {
      message("✅ Database already exists and initialized")
      return(TRUE)
    }

    if (force_recreate) {
      message("🗑️  Dropping existing database for recreation...")
      # Drop all tables if they exist
      tables <- DBI::dbListTables(con)
      for (table in tables) {
        DBI::dbExecute(con, paste("DROP TABLE IF EXISTS", table))
      }
    }

    # Read and execute schema
    schema_path <- system.file("fcmsafety_schema.sql", package = "fcmsafety")
    if (!file.exists(schema_path)) {
      # Try local development path
      schema_path <- file.path(getwd(), "inst", "fcmsafety_schema.sql")
    }

    if (!file.exists(schema_path)) {
      stop("Schema file not found. Please ensure fcmsafety_schema.sql exists.")
    }

    message("📄 Reading database schema...")
    schema_sql <- readLines(schema_path, warn = FALSE)
    schema_sql <- paste(schema_sql, collapse = "\n")

    # Split into individual statements and execute
    # Remove comments and empty lines first
    schema_lines <- strsplit(schema_sql, "\n")[[1]]
    schema_lines <- schema_lines[!grepl("^\\s*--", schema_lines)]  # Remove comment lines
    schema_lines <- schema_lines[nchar(trimws(schema_lines)) > 0]  # Remove empty lines
    schema_sql_clean <- paste(schema_lines, collapse = "\n")

    # Split by semicolon
    statements <- strsplit(schema_sql_clean, ";")[[1]]
    statements <- trimws(statements)
    statements <- statements[nchar(statements) > 0]

    message("🔨 Creating database tables and indexes...")
    for (i in seq_along(statements)) {
      stmt <- trimws(statements[i])
      if (nchar(stmt) > 0) {
        tryCatch({
          DBI::dbExecute(con, stmt)
          if (grepl("CREATE TABLE", stmt, ignore.case = TRUE)) {
            table_name <- gsub(".*CREATE TABLE\\s+(\\w+).*", "\\1", stmt, ignore.case = TRUE)
            message("   ✅ Created table: ", table_name)
          }
        }, error = function(e) {
          message("   ❌ Error executing statement ", i, ": ", e$message)
          message("   Statement: ", substr(stmt, 1, 100), "...")
        })
      }
    }

    message("✅ Database initialization completed successfully!")
    return(TRUE)

  }, error = function(e) {
    message("❌ Database initialization failed: ", e$message)
    return(FALSE)
  })
}

#' Check Database Status
#'
#' Provides information about the current database status, including
#' table counts, last update times, and version information.
#'
#' @return List with database status information
#' @export
check_database_status <- function() {
  tryCatch({
    con <- get_db_connection()
    on.exit(DBI::dbDisconnect(con))

    # Check if database is initialized
    if (!DBI::dbExistsTable(con, "chemicals")) {
      return(list(
        initialized = FALSE,
        message = "Database not initialized. Run initialize_database() first."
      ))
    }

    # Get table counts
    tables <- c("chemicals", "svhc", "cmr", "cmr_suspect", "iarc",
                "eu_sml", "eu_sml_group", "edc", "china_sml")

    counts <- list()
    for (table in tables) {
      if (DBI::dbExistsTable(con, table)) {
        count_query <- paste("SELECT COUNT(*) as count FROM", table)
        counts[[table]] <- DBI::dbGetQuery(con, count_query)$count[1]
      } else {
        counts[[table]] <- 0
      }
    }

    # Get metadata information
    metadata <- DBI::dbGetQuery(con, "SELECT * FROM database_metadata ORDER BY database_name")

    # Get recent update history
    recent_updates <- DBI::dbGetQuery(con,
      "SELECT database_name, update_timestamp, update_type, records_added, records_removed
       FROM update_history
       ORDER BY update_timestamp DESC
       LIMIT 10")

    return(list(
      initialized = TRUE,
      table_counts = counts,
      metadata = metadata,
      recent_updates = recent_updates,
      total_chemicals = counts$chemicals,
      database_path = con@dbname
    ))

  }, error = function(e) {
    return(list(
      initialized = FALSE,
      error = e$message
    ))
  })
}

#' Migrate Data from XLSX to SQLite
#'
#' One-time migration function to transfer all existing xlsx data
#' to the new SQLite database structure.
#'
#' @param source_dir Directory containing xlsx files (default: inst/)
#' @param backup_existing Whether to backup existing database
#' @return Logical indicating success
#' @export
migrate_xlsx_to_sqlite <- function(source_dir = NULL, backup_existing = TRUE) {
  message("🔄 Starting migration from XLSX to SQLite...")

  if (is.null(source_dir)) {
    source_dir <- file.path(getwd(), "inst")
  }

  if (!dir.exists(source_dir)) {
    stop("Source directory does not exist: ", source_dir)
  }

  tryCatch({
    # Initialize database
    if (!initialize_database()) {
      stop("Failed to initialize database")
    }

    con <- get_db_connection()
    on.exit(DBI::dbDisconnect(con))

    # Start transaction for atomic migration
    DBI::dbExecute(con, "BEGIN TRANSACTION")

    # Set up error handling to prevent rollback on individual table failures
    migration_errors <- list()

    # Track migration progress
    migration_start <- Sys.time()
    total_records <- 0

    # Define file mappings for dual-storage migration
    file_mappings <- list(
      svhc = "svhc.xlsx",
      cmr = "cmr.xlsx",
      cmr_suspect = "suspect_cmr.xlsx",
      iarc = "iarc.xlsx",
      eu_sml = "eu10_2011.xlsx",
      eu_sml_group = "eu10_2011_group.xlsx",
      edc = "edc.xlsx",
      china_sml = "china_sml_cleaned.xlsx"
    )

    # Collect all unique chemicals first
    message("📊 Collecting chemical metadata...")
    all_chemicals <- data.frame()

    for (db_name in names(file_mappings)) {
      file_path <- file.path(source_dir, file_mappings[[db_name]])

      if (file.exists(file_path)) {
        message("   Processing ", db_name, "...")
        data <- rio::import(file_path)

        # Extract chemical metadata if available
        if (all(c("InChIKey", "CID", "Formula", "SMILES", "IUPACName", "ExactMass") %in% names(data))) {
          chem_data <- data[, c("InChIKey", "CID", "Formula", "SMILES", "IUPACName", "ExactMass")]
          chem_data <- chem_data[!is.na(chem_data$InChIKey), ]
          chem_data <- chem_data[!duplicated(chem_data$InChIKey), ]

          if (nrow(all_chemicals) == 0) {
            all_chemicals <- chem_data
          } else {
            all_chemicals <- rbind(all_chemicals, chem_data)
            all_chemicals <- all_chemicals[!duplicated(all_chemicals$InChIKey), ]
          }
        }
      }
    }

    # Insert chemical metadata
    if (nrow(all_chemicals) > 0) {
      message("💾 Inserting ", nrow(all_chemicals), " unique chemicals...")
      DBI::dbWriteTable(con, "chemicals", all_chemicals, append = TRUE)
    }

    # Migrate each database
    for (db_name in names(file_mappings)) {
      file_path <- file.path(source_dir, file_mappings[[db_name]])

      if (file.exists(file_path)) {
        message("🔄 Migrating ", db_name, " database...")

        tryCatch({
          # Load and process data
          data <- rio::import(file_path)
          processed_data <- process_database_for_migration(data, db_name)

          if (nrow(processed_data) > 0) {
            # Insert into appropriate table
            DBI::dbWriteTable(con, db_name, processed_data, append = TRUE)

            # Update metadata
            DBI::dbExecute(con,
              "UPDATE database_metadata
               SET total_records = ?, last_updated = CURRENT_TIMESTAMP
               WHERE database_name = ?",
              params = list(nrow(processed_data), db_name))

            total_records <- total_records + nrow(processed_data)
            message("   ✅ Migrated ", nrow(processed_data), " records")
          }
        }, error = function(e) {
          migration_errors[[db_name]] <<- e$message
          message("   ❌ Migration failed for ", db_name, ": ", e$message)
        })
      } else {
        message("   ⚠️  File not found: ", file_path)
      }
    }

    # Record migration in update history
    DBI::dbExecute(con,
      "INSERT INTO update_history (database_name, update_type, records_added, source_file, user_notes)
       VALUES (?, ?, ?, ?, ?)",
      params = list("all", "migration", total_records, "xlsx_files", "Initial migration from xlsx to SQLite"))

    # Commit transaction
    DBI::dbExecute(con, "COMMIT")

    migration_time <- as.numeric(difftime(Sys.time(), migration_start, units = "secs"))

    # Report results
    if (length(migration_errors) > 0) {
      message("⚠️  Migration completed with some errors:")
      for (db in names(migration_errors)) {
        message("   ❌ ", db, ": ", migration_errors[[db]])
      }
    } else {
      message("✅ Migration completed successfully!")
    }

    message("📊 Total records migrated: ", total_records)
    message("⏱️  Migration time: ", round(migration_time, 2), " seconds")

    return(length(migration_errors) == 0)

  }, error = function(e) {
    message("❌ Migration failed: ", e$message)
    # Rollback transaction if still active
    tryCatch(DBI::dbExecute(con, "ROLLBACK"), error = function(e) {})
    return(FALSE)
  })
}

#' Process Database for Migration
#'
#' Internal function to process and clean database-specific data
#' during migration from xlsx to SQLite format.
#'
#' @param data Raw data from xlsx file
#' @param db_name Database name for specific processing rules
#' @return Processed data frame ready for SQLite insertion
process_database_for_migration <- function(data, db_name) {
  # Filter by InChIKey for most databases (except china_sml)
  if (db_name != "china_sml" && "InChIKey" %in% names(data)) {
    data <- data[!is.na(data$InChIKey), ]
  }

  # Database-specific processing
  if (db_name == "svhc") {
    # Map SVHC columns to schema
    processed <- data.frame(
      InChIKey = data$InChIKey,
      substance_name = data$`Substance name`,
      description = data$Description,
      ec_no = data$`EC No.`,
      cas_no = data$`CAS No.`,
      reason_for_inclusion = data$`Reason for inclusion`,
      date_of_inclusion = data$`Date of inclusion`,
      decision = data$Decision,
      iuclid_dataset = data$`IUCLID dataset`,
      support_document = data$`Support document`,
      response_to_comments = data$`Response to comments`,
      remarks = data$Remarks,
      stringsAsFactors = FALSE
    )

  } else if (db_name == "cmr") {
    # Map CMR columns to schema with safe column access
    safe_get_col <- function(data, col_name, default = NA) {
      if (col_name %in% names(data)) {
        return(data[[col_name]])
      } else {
        return(rep(default, nrow(data)))
      }
    }

    processed <- data.frame(
      InChIKey = safe_get_col(data, "InChIKey"),
      index_no = safe_get_col(data, "Index No"),
      international_chemical_identification = safe_get_col(data, "International Chemical Identification"),
      ec_no = safe_get_col(data, "EC No"),
      cas_no = safe_get_col(data, "CAS No"),
      hazard_class_and_category_codes = safe_get_col(data, "Hazard Class and Category Code(s)"),
      hazard_statement_codes = safe_get_col(data, "Hazard Statement Code(s)"),
      pictogram = safe_get_col(data, "Pictogram"),
      signal_word_codes = safe_get_col(data, "Signal Word Code(s)"),
      hazard_statement_codes_alt = safe_get_col(data, "Hazard statement Code(s)"),
      suppl_hazard_statement_codes = safe_get_col(data, "Suppl. Hazard statement Code(s)"),
      specific_conc_limits = safe_get_col(data, "Specific Conc. Limits"),
      m_factors = safe_get_col(data, "M-factors"),
      notes = safe_get_col(data, "Notes"),
      atp_inserted_updated = safe_get_col(data, "ATP inserted/ATP Updated"),
      stringsAsFactors = FALSE
    )

  } else if (db_name == "eu_sml") {
    # Handle complex EU SML processing (replicate current logic)
    processed <- data %>%
      dplyr::rename(
        SML = `SML\n                           [mg/kg]`,
        SML_group = `SML(T)\n                           [mg/kg]\n                           (Group restriction No)`
      ) %>%
      dplyr::mutate(
        SML = SML %>%
          stringr::str_replace(",", ".") %>%
          stringr::str_replace("ND", "0.01") %>%
          stringr::str_remove_all("\n.*$") %>%
          trimws() %>%
          as.numeric(),
        SML_group = stringr::str_remove_all(SML_group, "\\(|\\)")
      ) %>%
      dplyr::select(
        InChIKey,
        fcm_substance_no = `FCM substance No`,
        ref_no = `Ref. No`,
        cas_no = `CAS No`,
        substance_name = `Substance name`,
        use_as_additive = `Use as additive or polymer production aid\n                           (yes/no)`,
        use_as_monomer = `Use as monomer or other starting substance or macromolecule obtained from microbial fermentation\n                           (yes/no)`,
        frf_applicable = `FRF applicable\n                           (yes/no)`,
        sml = SML,
        sml_group = SML_group,
        restrictions_and_specifications = `Restrictions and specifications`,
        notes_on_verification = `Notes on verification of compliance`
      )

  } else if (db_name == "cmr_suspect") {
    # Map CMR Suspect columns to schema
    safe_get_col <- function(data, col_name, default = NA) {
      if (col_name %in% names(data)) {
        return(data[[col_name]])
      } else {
        return(rep(default, nrow(data)))
      }
    }

    processed <- data.frame(
      InChIKey = safe_get_col(data, "InChIKey"),
      substance_name = safe_get_col(data, "Substance name"),
      cas_no = safe_get_col(data, "CAS No."),
      ec_no = safe_get_col(data, "EC No."),
      classification = safe_get_col(data, "Classification"),
      source = safe_get_col(data, "Source"),
      notes = safe_get_col(data, "Notes"),
      stringsAsFactors = FALSE
    )

  } else if (db_name == "iarc") {
    # Map IARC columns to schema
    safe_get_col <- function(data, col_name, default = NA) {
      if (col_name %in% names(data)) {
        return(data[[col_name]])
      } else {
        return(rep(default, nrow(data)))
      }
    }

    processed <- data.frame(
      InChIKey = safe_get_col(data, "InChIKey"),
      cas_no = safe_get_col(data, "CAS No."),
      agent = safe_get_col(data, "Agent"),
      group_classification = safe_get_col(data, "Group"),
      volume = safe_get_col(data, "Volume"),
      volume_publication_year = safe_get_col(data, "Volume publication year"),
      evaluation_year = safe_get_col(data, "Evaluation year"),
      additional_information = safe_get_col(data, "Additional information"),
      stringsAsFactors = FALSE
    )

  } else if (db_name == "edc") {
    # Map EDC columns to schema
    safe_get_col <- function(data, col_name, default = NA) {
      if (col_name %in% names(data)) {
        return(data[[col_name]])
      } else {
        return(rep(default, nrow(data)))
      }
    }

    processed <- data.frame(
      InChIKey = safe_get_col(data, "InChIKey"),
      substance_name = safe_get_col(data, "Substance name"),
      cas_no = safe_get_col(data, "CAS No."),
      ec_no = safe_get_col(data, "EC No."),
      classification = safe_get_col(data, "Classification"),
      evidence_level = safe_get_col(data, "Evidence Level"),
      source = safe_get_col(data, "Source"),
      notes = safe_get_col(data, "Notes"),
      stringsAsFactors = FALSE
    )

  } else if (db_name == "eu_sml_group") {
    # Map EU SML Group columns to schema
    safe_get_col <- function(data, col_name, default = NA) {
      if (col_name %in% names(data)) {
        return(data[[col_name]])
      } else {
        return(rep(default, nrow(data)))
      }
    }

    processed <- data.frame(
      InChIKey = safe_get_col(data, "InChIKey"),
      group_no = safe_get_col(data, "Group Restriction No"),
      substance_name = safe_get_col(data, "FCM substance No"),
      cas_no = safe_get_col(data, "CAS No"),
      sml = safe_get_col(data, "SML (T)\n                     [mg/kg]"),
      restrictions = safe_get_col(data, "Group restriction specification"),
      stringsAsFactors = FALSE
    )

    # Process SML values similar to EU SML
    if ("sml" %in% names(processed)) {
      processed$sml <- processed$sml %>%
        stringr::str_replace(",", ".") %>%
        stringr::str_replace("ND", "0.01") %>%
        trimws() %>%
        as.numeric()
    }

  } else if (db_name == "china_sml") {
    # Map China SML columns to schema
    safe_get_col <- function(data, col_name, default = NA) {
      if (col_name %in% names(data)) {
        return(data[[col_name]])
      } else {
        return(rep(default, nrow(data)))
      }
    }

    processed <- data.frame(
      InChIKey = safe_get_col(data, "InChIKey"),
      substance_name = safe_get_col(data, "Substance name"),
      cas_no = safe_get_col(data, "CAS No."),
      sml_value = safe_get_col(data, "SML Value"),
      unit = safe_get_col(data, "Unit"),
      food_type = safe_get_col(data, "Food Type"),
      regulation_reference = safe_get_col(data, "Regulation Reference"),
      notes = safe_get_col(data, "Notes"),
      stringsAsFactors = FALSE
    )

  } else {
    # For other databases, create a generic mapping
    # This would need to be expanded for each specific database
    processed <- data
  }

  return(processed)
}
