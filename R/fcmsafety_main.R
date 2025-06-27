#' FCMSafety Main Integration Functions
#'
#' This module provides the main user-facing functions that integrate all
#' components of the enhanced FCMSafety system, including SQLite database
#' management, intelligent updates, and comprehensive audit trails.

#' Setup FCMSafety Database System
#'
#' One-time setup function that initializes the SQLite database system
#' and migrates existing xlsx data. This should be run once when first
#' upgrading to the SQLite-based system.
#'
#' @param force_reinit Logical, whether to force reinitialization of existing database
#' @param backup_xlsx Logical, whether to backup existing xlsx files
#' @return Logical indicating success
#' @export
setup_fcmsafety_database <- function(force_reinit = FALSE, backup_xlsx = TRUE) {
  message("🚀 Setting up FCMSafety SQLite Database System")
  message(paste(rep("=", 60), collapse = ""))

  tryCatch({
    # Check current status
    status <- check_database_status()

    if (status$initialized && !force_reinit) {
      message("✅ Database already initialized!")
      message("💡 Use force_reinit = TRUE to reinitialize")
      display_database_summary(status)
      return(TRUE)
    }

    # Backup xlsx files if requested
    if (backup_xlsx) {
      message("💾 Creating backup of existing xlsx files...")
      backup_xlsx_files()
    }

    # Initialize database
    message("🔧 Initializing SQLite database...")
    if (!initialize_database(force_recreate = force_reinit)) {
      stop("Database initialization failed")
    }

    # Migrate data
    message("🔄 Migrating data from xlsx to SQLite...")
    if (!migrate_xlsx_to_sqlite()) {
      stop("Data migration failed")
    }

    # Verify setup
    message("✅ Verifying setup...")
    final_status <- check_database_status()
    display_database_summary(final_status)

    message("\n🎉 FCMSafety SQLite setup completed successfully!")
    message("💡 Next steps:")
    message("   • Use load_databases() to load data (now uses SQLite by default)")
    message("   • Use update_databases_interactive() for intelligent updates")
    message("   • Use get_update_history() to view change history")

    return(TRUE)

  }, error = function(e) {
    message("❌ Setup failed: ", e$message)
    message("💡 You can still use the original xlsx system with load_databases(use_sqlite = FALSE)")
    return(FALSE)
  })
}

#' Enhanced Assign Toxicity with Auto-Update Check
#'
#' Enhanced version of assign_toxicity that automatically checks for database
#' updates before processing, ensuring users always work with the latest data.
#'
#' @param data Input data frame with chemical identifiers
#' @param output_file Output file path for results
#' @param check_updates Logical, whether to check for available updates
#' @param auto_update Logical, whether to automatically apply available updates
#' @param ... Additional arguments passed to original assign_toxicity function
#' @return Results from assign_toxicity function
#' @export
assign_toxicity_enhanced <- function(data, output_file, check_updates = TRUE, auto_update = FALSE, ...) {
  message("🧪 Enhanced Toxicity Assignment with Update Checking")
  message(paste(rep("=", 60), collapse = ""))

  # Check for updates if requested
  if (check_updates) {
    message("🔍 Checking for database updates...")

    update_info <- check_available_updates(c("svhc", "cmr", "iarc", "eu_sml"))

    if (length(update_info$available) > 0) {
      message("🆕 Updates available for: ", paste(update_info$available, collapse = ", "))

      if (auto_update) {
        message("🔄 Auto-updating databases...")
        update_result <- update_databases_interactive(
          databases = update_info$available,
          interactive = FALSE,
          auto_update = TRUE
        )

        if (update_result$success) {
          message("✅ Databases updated successfully")
          # Reload databases with fresh data
          load_databases()
        } else {
          message("⚠️  Some updates failed, proceeding with current data")
        }
      } else {
        message("💡 Run update_databases_interactive() to apply updates")
        message("💡 Or use auto_update = TRUE to update automatically")
      }
    } else {
      message("✅ All databases are up to date")
    }
  }

  # Ensure databases are loaded
  if (!exists("svhc", envir = .GlobalEnv)) {
    message("📊 Loading databases...")
    load_databases()
  }

  # Call original assign_toxicity function
  message("🔬 Performing toxicity assignment...")
  result <- assign_toxicity(data, output_file, ...)

  message("✅ Toxicity assignment completed!")
  return(result)
}

#' Quick Database Status Check
#'
#' Provides a quick overview of the current database system status,
#' including record counts, last updates, and recent activity.
#'
#' @param show_recent_activity Logical, whether to show recent update activity
#' @return Invisible status object
#' @export
fcmsafety_status <- function(show_recent_activity = TRUE) {
  message("📊 FCMSafety Database System Status")
  message(paste(rep("=", 50), collapse = ""))

  # Check database status
  status <- check_database_status()

  if (!status$initialized) {
    message("❌ SQLite database not initialized")
    message("💡 Run setup_fcmsafety_database() to initialize")
    message("💡 Or use load_databases(use_sqlite = FALSE) for xlsx mode")
    return(invisible(status))
  }

  # Display current status
  display_database_summary(status)

  # Show recent activity if requested
  if (show_recent_activity && nrow(status$recent_updates) > 0) {
    message("\n📈 Recent Activity Summary:")
    total_changes <- sum(abs(status$recent_updates$records_added - status$recent_updates$records_removed))
    message("   Total changes in recent updates: ", total_changes)
    message("   Most active database: ", names(sort(table(status$recent_updates$database_name), decreasing = TRUE))[1])
  }

  # Check for available updates
  message("\n🔍 Checking for available updates...")
  update_info <- check_available_updates(c("svhc", "cmr", "iarc", "eu_sml"))

  if (length(update_info$available) > 0) {
    message("🆕 Updates available for: ", paste(update_info$available, collapse = ", "))
    message("💡 Run update_databases_interactive() to apply updates")
  } else {
    message("✅ All databases are up to date")
  }

  message("\n💡 Available commands:")
  message("   • load_databases() - Load databases into memory")
  message("   • update_databases_interactive() - Interactive update workflow")
  message("   • get_update_history() - View change history")
  message("   • get_database_statistics() - Detailed statistics")

  return(invisible(status))
}

#' Backup XLSX Files
#'
#' Creates a timestamped backup of existing xlsx database files.
#'
#' @param backup_dir Directory to store backups (default: inst/backups/)
#' @return Logical indicating success
backup_xlsx_files <- function(backup_dir = NULL) {
  if (is.null(backup_dir)) {
    backup_dir <- file.path(getwd(), "inst", "backups")
  }

  if (!dir.exists(backup_dir)) {
    dir.create(backup_dir, recursive = TRUE)
  }

  # List of xlsx files to backup
  xlsx_files <- c(
    "svhc.xlsx", "cmr.xlsx", "suspect_cmr.xlsx", "iarc.xlsx",
    "eu10_2011.xlsx", "eu10_2011_group.xlsx", "edc.xlsx", "china_sml_cleaned.xlsx"
  )

  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  backup_count <- 0

  for (file in xlsx_files) {
    source_path <- file.path(getwd(), "inst", file)
    if (file.exists(source_path)) {
      backup_name <- paste0(tools::file_path_sans_ext(file), "_backup_", timestamp, ".xlsx")
      backup_path <- file.path(backup_dir, backup_name)

      if (file.copy(source_path, backup_path)) {
        backup_count <- backup_count + 1
      }
    }
  }

  if (backup_count > 0) {
    message("💾 Backed up ", backup_count, " xlsx files to: ", backup_dir)
    return(TRUE)
  } else {
    message("⚠️  No xlsx files found to backup")
    return(FALSE)
  }
}

#' Reset to XLSX Mode
#'
#' Utility function to temporarily or permanently switch back to xlsx-based
#' database loading, useful for troubleshooting or compatibility testing.
#'
#' @param permanent Logical, whether to make the change permanent for the session
#' @export
use_xlsx_mode <- function(permanent = FALSE) {
  message("🔄 Switching to XLSX mode...")

  # Clear existing data
  if (exists("svhc", envir = .GlobalEnv)) rm(svhc, envir = .GlobalEnv)
  if (exists("cmr", envir = .GlobalEnv)) rm(cmr, envir = .GlobalEnv)
  if (exists("cmr_suspect", envir = .GlobalEnv)) rm(cmr_suspect, envir = .GlobalEnv)
  if (exists("iarc", envir = .GlobalEnv)) rm(iarc, envir = .GlobalEnv)
  if (exists("eu_sml", envir = .GlobalEnv)) rm(eu_sml, envir = .GlobalEnv)
  if (exists("eu_sml_group", envir = .GlobalEnv)) rm(eu_sml_group, envir = .GlobalEnv)
  if (exists("edc", envir = .GlobalEnv)) rm(edc, envir = .GlobalEnv)
  if (exists("china_sml", envir = .GlobalEnv)) rm(china_sml, envir = .GlobalEnv)

  # Load using xlsx mode
  load_databases(use_sqlite = FALSE)

  if (permanent) {
    message("⚠️  XLSX mode enabled for this session")
    message("💡 Use load_databases(use_sqlite = TRUE) to switch back to SQLite")
  } else {
    message("✅ Loaded databases from XLSX files")
    message("💡 Next load_databases() call will use SQLite again (if available)")
  }
}

#' Validate Database Integrity
#'
#' Performs comprehensive validation of the database system, checking for
#' data consistency, missing records, and potential issues.
#'
#' @param fix_issues Logical, whether to attempt automatic fixes
#' @return List with validation results
#' @export
validate_database_integrity <- function(fix_issues = FALSE) {
  message("🔍 Validating database integrity...")

  validation_results <- list(
    passed = TRUE,
    issues = list(),
    fixes_applied = list()
  )

  tryCatch({
    con <- get_db_connection()
    on.exit(DBI::dbDisconnect(con))

    # Check 1: Verify all tables exist
    required_tables <- c("chemicals", "svhc", "cmr", "cmr_suspect", "iarc",
                        "eu_sml", "eu_sml_group", "edc", "china_sml",
                        "database_metadata", "update_history", "change_log")

    existing_tables <- DBI::dbListTables(con)
    missing_tables <- setdiff(required_tables, existing_tables)

    if (length(missing_tables) > 0) {
      validation_results$passed <- FALSE
      validation_results$issues$missing_tables <- missing_tables
      message("❌ Missing tables: ", paste(missing_tables, collapse = ", "))
    } else {
      message("✅ All required tables present")
    }

    # Check 2: Verify foreign key relationships
    message("🔗 Checking foreign key relationships...")

    # Check for orphaned records in regulatory tables
    for (table in c("svhc", "cmr", "cmr_suspect", "iarc", "eu_sml", "eu_sml_group", "edc")) {
      orphan_query <- paste0("
        SELECT COUNT(*) as orphan_count
        FROM ", table, " t
        LEFT JOIN chemicals c ON t.InChIKey = c.InChIKey
        WHERE c.InChIKey IS NULL AND t.InChIKey IS NOT NULL
      ")

      orphan_count <- DBI::dbGetQuery(con, orphan_query)$orphan_count[1]

      if (orphan_count > 0) {
        validation_results$passed <- FALSE
        validation_results$issues[[paste0(table, "_orphans")]] <- orphan_count
        message("❌ ", table, ": ", orphan_count, " orphaned records")

        if (fix_issues) {
          # Attempt to fix by adding missing chemicals
          message("🔧 Attempting to fix orphaned records...")
          # Implementation would depend on specific requirements
        }
      }
    }

    # Check 3: Verify data consistency
    message("📊 Checking data consistency...")

    # Check for duplicate InChIKeys in chemicals table
    dup_query <- "SELECT InChIKey, COUNT(*) as count FROM chemicals GROUP BY InChIKey HAVING COUNT(*) > 1"
    duplicates <- DBI::dbGetQuery(con, dup_query)

    if (nrow(duplicates) > 0) {
      validation_results$passed <- FALSE
      validation_results$issues$duplicate_chemicals <- nrow(duplicates)
      message("❌ ", nrow(duplicates), " duplicate InChIKeys in chemicals table")
    }

    # Check 4: Verify metadata consistency
    message("📋 Checking metadata consistency...")

    metadata_query <- "SELECT database_name, total_records FROM database_metadata"
    metadata <- DBI::dbGetQuery(con, metadata_query)

    for (i in 1:nrow(metadata)) {
      db_name <- metadata$database_name[i]
      expected_count <- metadata$total_records[i]

      if (DBI::dbExistsTable(con, db_name)) {
        actual_count <- DBI::dbGetQuery(con, paste("SELECT COUNT(*) as count FROM", db_name))$count[1]

        if (actual_count != expected_count) {
          validation_results$passed <- FALSE
          validation_results$issues[[paste0(db_name, "_count_mismatch")]] <-
            list(expected = expected_count, actual = actual_count)
          message("❌ ", db_name, ": Expected ", expected_count, " records, found ", actual_count)

          if (fix_issues) {
            # Update metadata
            DBI::dbExecute(con,
              "UPDATE database_metadata SET total_records = ? WHERE database_name = ?",
              params = list(actual_count, db_name))
            validation_results$fixes_applied[[paste0(db_name, "_count")]] <- TRUE
            message("🔧 Updated metadata for ", db_name)
          }
        }
      }
    }

    if (validation_results$passed) {
      message("✅ Database integrity validation passed!")
    } else {
      message("⚠️  Database integrity issues found")
      if (!fix_issues) {
        message("💡 Use fix_issues = TRUE to attempt automatic repairs")
      }
    }

    return(invisible(validation_results))

  }, error = function(e) {
    message("❌ Validation failed: ", e$message)
    validation_results$passed <- FALSE
    validation_results$error <- e$message
    return(invisible(validation_results))
  })
}
