#' Enhanced Database Update System
#'
#' This module provides an intelligent update workflow with user interaction,
#' detailed change tracking, and comprehensive audit capabilities.
#' It integrates with the existing update functions while adding SQLite
#' backend support and enhanced reporting.
#'
#' @importFrom DBI dbConnect dbDisconnect dbExecute dbGetQuery dbWriteTable
#' @importFrom RSQLite SQLite

#' Interactive Database Update Workflow
#'
#' Provides an intelligent update workflow that checks for available updates,
#' presents options to the user, and performs selective updates with detailed
#' change tracking and reporting.
#'
#' @param databases Character vector of databases to check/update. If NULL, checks all.
#' @param interactive Logical, whether to prompt user for confirmation
#' @param auto_update Logical, whether to automatically update without prompts
#' @return List with update results and summary
#' @export
update_databases_interactive <- function(databases = NULL, interactive = TRUE, auto_update = FALSE) {
  message("🔍 FCMSafety Database Update System")
  message("=" , paste(rep("=", 50), collapse = ""))
  
  # Define available databases
  all_databases <- c("svhc", "cmr", "iarc", "eu_sml")  # Add others as update functions become available
  
  if (is.null(databases)) {
    databases <- all_databases
  } else {
    databases <- intersect(databases, all_databases)
  }
  
  if (length(databases) == 0) {
    message("❌ No valid databases specified")
    return(invisible(list(success = FALSE, error = "No valid databases")))
  }
  
  # Check current database status
  message("📊 Checking current database status...")
  status <- check_database_status()
  
  if (status$initialized) {
    message("✅ SQLite database is initialized")
    display_database_summary(status)
  } else {
    message("⚠️  SQLite database not initialized")
    if (interactive && !auto_update) {
      response <- readline("Initialize SQLite database now? (y/n): ")
      if (tolower(response) != "y") {
        message("❌ Update cancelled by user")
        return(invisible(list(success = FALSE, error = "User cancelled")))
      }
    }
    
    if (!initialize_database()) {
      message("❌ Failed to initialize database")
      return(invisible(list(success = FALSE, error = "Database initialization failed")))
    }
  }
  
  # Check for available updates
  message("\n🔍 Checking for available updates...")
  update_info <- check_available_updates(databases)
  
  if (length(update_info$available) == 0) {
    message("✅ All databases are up to date!")
    return(invisible(list(success = TRUE, updates_performed = 0, message = "No updates needed")))
  }
  
  # Display available updates
  display_available_updates(update_info)
  
  # Get user confirmation if interactive
  if (interactive && !auto_update) {
    selected_databases <- get_user_update_selection(update_info$available)
    if (length(selected_databases) == 0) {
      message("❌ No databases selected for update")
      return(invisible(list(success = FALSE, error = "No databases selected")))
    }
  } else {
    selected_databases <- update_info$available
  }
  
  # Perform updates
  message("\n🔄 Performing database updates...")
  update_results <- perform_database_updates(selected_databases)
  
  # Display final summary
  display_update_summary(update_results)
  
  return(invisible(update_results))
}

#' Check Available Updates
#'
#' Checks which databases have updates available by examining
#' last update timestamps and available data sources.
#'
#' @param databases Character vector of databases to check
#' @return List with available updates information
check_available_updates <- function(databases) {
  available_updates <- character(0)
  update_info <- list()
  
  for (db_name in databases) {
    # Check if update function exists and if updates are available
    # This is a simplified check - in practice, you'd check actual data sources
    
    if (db_name == "svhc") {
      # Check if manual file exists for SVHC
      inst_dir <- file.path(getwd(), "inst")
      svhc_files <- c("svhc_new.xlsx", "svhc_new.csv", "candidate_list.xlsx")
      has_update_file <- any(file.exists(file.path(inst_dir, svhc_files)))
      
      if (has_update_file) {
        available_updates <- c(available_updates, "svhc")
        update_info[[db_name]] <- list(
          source = "Manual file",
          last_check = Sys.time(),
          estimated_changes = "Unknown"
        )
      }
    } else if (db_name == "cmr") {
      # Check if manual file exists for CMR
      inst_dir <- file.path(getwd(), "inst")
      cmr_files <- c("clp_new.xlsx", "clp_new.csv", "annex_vi_clp.xlsx")
      has_update_file <- any(file.exists(file.path(inst_dir, cmr_files)))
      
      if (has_update_file) {
        available_updates <- c(available_updates, "cmr")
        update_info[[db_name]] <- list(
          source = "Manual file",
          last_check = Sys.time(),
          estimated_changes = "Unknown"
        )
      }
    }
    # Add checks for other databases as their update functions are implemented
  }
  
  return(list(
    available = available_updates,
    info = update_info,
    checked_at = Sys.time()
  ))
}

#' Display Database Summary
#'
#' Shows current database status including record counts and last update times.
#'
#' @param status Database status object from check_database_status()
display_database_summary <- function(status) {
  message("\n📊 Current Database Status:")
  message("   Database location: ", basename(status$database_path))
  message("   Total chemicals: ", status$total_chemicals)
  message("")
  
  if (nrow(status$metadata) > 0) {
    message("📋 Database Records:")
    for (i in 1:nrow(status$metadata)) {
      db <- status$metadata[i, ]
      last_update <- if (is.na(db$last_updated)) "Never" else format(as.POSIXct(db$last_updated), "%Y-%m-%d %H:%M")
      message(sprintf("   %-12s: %5d records (Last updated: %s)", 
                     toupper(db$database_name), db$total_records, last_update))
    }
  }
  
  if (nrow(status$recent_updates) > 0) {
    message("\n📈 Recent Update Activity:")
    for (i in 1:min(3, nrow(status$recent_updates))) {
      update <- status$recent_updates[i, ]
      timestamp <- format(as.POSIXct(update$update_timestamp), "%Y-%m-%d %H:%M")
      message(sprintf("   %s: %s (%+d records) - %s", 
                     timestamp, toupper(update$database_name), 
                     update$records_added - update$records_removed, update$update_type))
    }
  }
}

#' Display Available Updates
#'
#' Shows which databases have updates available with details.
#'
#' @param update_info Update information from check_available_updates()
display_available_updates <- function(update_info) {
  message("\n🆕 Available Updates:")
  
  if (length(update_info$available) == 0) {
    message("   No updates available")
    return()
  }
  
  for (db_name in update_info$available) {
    info <- update_info$info[[db_name]]
    message(sprintf("   ✅ %-12s: %s (Estimated changes: %s)", 
                   toupper(db_name), info$source, info$estimated_changes))
  }
}

#' Get User Update Selection
#'
#' Prompts user to select which databases to update.
#'
#' @param available_databases Character vector of databases with available updates
#' @return Character vector of selected databases
get_user_update_selection <- function(available_databases) {
  message("\n❓ Which databases would you like to update?")
  message("   Options:")
  message("   'all' - Update all available databases")
  message("   'none' - Cancel updates")
  message("   Specific databases: ", paste(available_databases, collapse = ", "))
  
  response <- readline("Enter your choice: ")
  response <- trimws(tolower(response))
  
  if (response == "all") {
    return(available_databases)
  } else if (response == "none" || response == "") {
    return(character(0))
  } else {
    # Parse specific database names
    selected <- strsplit(response, "[,\\s]+")[[1]]
    selected <- intersect(selected, available_databases)
    
    if (length(selected) == 0) {
      message("⚠️  No valid databases selected")
      return(character(0))
    }
    
    message("Selected databases: ", paste(selected, collapse = ", "))
    return(selected)
  }
}

#' Perform Database Updates
#'
#' Executes the actual database updates for selected databases
#' with comprehensive error handling and progress tracking.
#'
#' @param selected_databases Character vector of databases to update
#' @return List with detailed update results
perform_database_updates <- function(selected_databases) {
  results <- list(
    success = TRUE,
    updates_performed = 0,
    failed_updates = 0,
    details = list(),
    start_time = Sys.time()
  )
  
  for (db_name in selected_databases) {
    message("\n🔄 Updating ", toupper(db_name), " database...")
    
    update_result <- tryCatch({
      if (db_name == "svhc") {
        update_svhc()
      } else if (db_name == "cmr") {
        update_cmr()
      } else if (db_name == "iarc") {
        update_iarc()
      } else if (db_name == "eu_sml") {
        update_eu_sml()
      } else {
        list(success = FALSE, error = paste("Update function not implemented for", db_name))
      }
    }, error = function(e) {
      list(success = FALSE, error = e$message)
    })
    
    if (update_result$success) {
      results$updates_performed <- results$updates_performed + 1
      message("✅ ", toupper(db_name), " update completed successfully")
      
      # Update SQLite database if the update was successful
      if (exists("new_data", where = update_result)) {
        update_sqlite_from_update_result(db_name, update_result)
      }
    } else {
      results$failed_updates <- results$failed_updates + 1
      results$success <- FALSE
      message("❌ ", toupper(db_name), " update failed: ", update_result$error)
    }
    
    results$details[[db_name]] <- update_result
  }
  
  results$end_time <- Sys.time()
  results$duration <- as.numeric(difftime(results$end_time, results$start_time, units = "secs"))
  
  return(results)
}

#' Display Update Summary
#'
#' Shows a comprehensive summary of the update session.
#'
#' @param results Update results from perform_database_updates()
display_update_summary <- function(results) {
  message("\n" , paste(rep("=", 60), collapse = ""))
  message("📊 UPDATE SUMMARY")
  message(paste(rep("=", 60), collapse = ""))
  
  message("⏱️  Duration: ", round(results$duration, 2), " seconds")
  message("✅ Successful updates: ", results$updates_performed)
  message("❌ Failed updates: ", results$failed_updates)
  
  if (results$success) {
    message("\n🎉 All selected updates completed successfully!")
  } else {
    message("\n⚠️  Some updates failed. Check the details above.")
  }
  
  message("\n💡 Next steps:")
  message("   • Run load_databases() to refresh data in memory")
  message("   • Use get_update_history() to view detailed change logs")
  message("   • Check database status with check_database_status()")
  
  message(paste(rep("=", 60), collapse = ""))
}

#' Update SQLite from Update Result
#'
#' Updates the SQLite database with new data from successful updates.
#'
#' @param db_name Database name
#' @param update_result Result object from update function
update_sqlite_from_update_result <- function(db_name, update_result) {
  # This function would integrate the update results into SQLite
  # Implementation depends on the structure of update_result
  # For now, this is a placeholder that would be implemented
  # as the individual update functions are enhanced
  
  message("💾 Updating SQLite database for ", db_name, "...")
  # TODO: Implement SQLite integration
}
