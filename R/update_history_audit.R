# =============================================================================
# 更新账本与审计追踪（谁在什么时候往库里加了什么）
#
# 每隔一段时间有人问"这条数据什么时候进来的、跟上次比变了什么"，答案在
# SQLite 的 update_history 表（每次 update_*_auto() 成功都会写一条），
# 本文件就是读它、展示它、导出它的那一层。
#
# 三层粒度的函数：
#   一次更新   get_update_history() / display_update_history()
#              —— 按库名与日期筛，看"哪几次更新、各加了几行"
#   一条变更   get_detailed_changes() / display_detailed_changes()
#              —— 下钻到字段级（哪个物质的哪个列从什么变成了什么），
#                 数据来自 log_change_detail() 在入库时顺手写下的明细
#   统计概览   get_database_statistics() / display_database_statistics()
#              —— 近 N 天的更新频次、活跃物质数，用于"最近是不是没人管了"巡检
#   导出       export_update_history()  把账本写成 csv/xlsx 交差
#
# 注意：update_history 表在**迁移前的老库**里可能不存在。读之前一律先
# dbExistsTable() 判一下并返回空表，不要直接查询——老库上会报错。
# =============================================================================

# ---- 一次更新：查账 + 展示 --------------------------------------------------

#' Update History and Audit Trail Functions
#'
#' This module provides comprehensive audit trail functionality for tracking
#' all database updates, changes, and modifications over time. It enables
#' users to understand what changed, when, and provides detailed historical
#' records for compliance and debugging purposes.
#'
#' Retrieves and displays historical update records with filtering options.
#' This function provides comprehensive visibility into all database changes
#' over time, supporting compliance and audit requirements.
#'
#' @importFrom DBI dbConnect dbDisconnect dbGetQuery
#' @importFrom RSQLite SQLite
#' @importFrom dplyr filter arrange desc
#' @param database_name Optional filter by specific database name
#' @param date_from Optional start date for filtering (YYYY-MM-DD format)
#' @param date_to Optional end date for filtering (YYYY-MM-DD format)
#' @param limit Maximum number of records to return (default: 50)
#' @param show_details Whether to include detailed change information
#' @param db_path Optional custom path to database file (for testing)
#' @return Data frame with update history records
#' @export
#' @encoding UTF-8
get_update_history <- function(database_name = NULL, date_from = NULL, date_to = NULL, 
                              limit = 50, show_details = TRUE, db_path = NULL) {
  message("📚 Retrieving update history...")
  
  tryCatch({
    con <- get_db_connection(db_path)
    on.exit(DBI::dbDisconnect(con))
    
    # Guard: if update_history table does not exist (e.g. pre-migration db),
    # return empty data frame instead of crashing
    if (!DBI::dbExistsTable(con, "update_history")) {
      message("ℹ️  update_history table not present in this database")
      return(data.frame())
    }
    
    # Build query with filters
    query <- "
      SELECT 
        id,
        database_name,
        update_timestamp,
        update_type,
        records_added,
        records_removed,
        records_modified,
        source_file,
        user_notes,
        success,
        error_message
      FROM update_history
      WHERE 1=1
    "
    
    params <- list()
    
    if (!is.null(database_name)) {
      query <- paste(query, "AND database_name = ?")
      params <- append(params, database_name)
    }
    
    if (!is.null(date_from)) {
      query <- paste(query, "AND DATE(update_timestamp) >= ?")
      params <- append(params, date_from)
    }
    
    if (!is.null(date_to)) {
      query <- paste(query, "AND DATE(update_timestamp) <= ?")
      params <- append(params, date_to)
    }
    
    query <- paste(query, "ORDER BY update_timestamp DESC LIMIT ?")
    params <- append(params, limit)
    
    # Execute query
    if (length(params) > 0) {
      history <- DBI::dbGetQuery(con, query, params = params)
    } else {
      history <- DBI::dbGetQuery(con, query)
    }
    
    if (nrow(history) == 0) {
      message("📭 No update history found matching the criteria")
      return(data.frame())
    }
    
    # Display formatted history
    display_update_history(history, show_details)
    
    # Get detailed changes if requested
    if (show_details && nrow(history) > 0) {
      message("\n🔍 Retrieving detailed change information...")
      detailed_changes <- get_detailed_changes(con, history$id[1:min(5, nrow(history))])
      if (nrow(detailed_changes) > 0) {
        display_detailed_changes(detailed_changes)
      }
    }
    
    return(invisible(history))
    
  }, error = function(e) {
    message("❌ Error retrieving update history: ", e$message)
    return(data.frame())
  })
}

#' Display Update History
#'
#' Formats and displays update history in a user-friendly format.
#'
#' @param history Data frame with update history records
#' @param show_details Whether to show detailed information
#' @encoding UTF-8
display_update_history <- function(history, show_details = TRUE) {
  message("\n📊 UPDATE HISTORY SUMMARY")
  message(paste(rep("=", 70), collapse = ""))
  
  for (i in 1:nrow(history)) {
    record <- history[i, ]
    
    # Format timestamp
    timestamp <- format(as.POSIXct(record$update_timestamp), "%Y-%m-%d %H:%M:%S")
    
    # Status indicator
    status_icon <- if (record$success) "✅" else "❌"
    
    # Calculate net change
    net_change <- record$records_added - record$records_removed
    change_text <- sprintf("%+d", net_change)
    if (net_change > 0) {
      change_text <- paste0("📈 ", change_text)
    } else if (net_change < 0) {
      change_text <- paste0("📉 ", change_text)
    } else {
      change_text <- "➖ 0"
    }
    
    # Main summary line
    message(sprintf("%s %s | %-12s | %s | %s", 
                   status_icon, timestamp, toupper(record$database_name), 
                   record$update_type, change_text))
    
    if (show_details) {
      # Detailed breakdown
      if (record$records_added > 0 || record$records_removed > 0 || record$records_modified > 0) {
        details <- sprintf("    📊 Added: %d | Removed: %d | Modified: %d", 
                          record$records_added, record$records_removed, record$records_modified)
        message(details)
      }
      
      # Source file information
      if (!is.na(record$source_file) && record$source_file != "") {
        message("    📁 Source: ", record$source_file)
      }
      
      # User notes
      if (!is.na(record$user_notes) && record$user_notes != "") {
        message("    📝 Notes: ", record$user_notes)
      }
      
      # Error information
      if (!record$success && !is.na(record$error_message)) {
        message("    ❌ Error: ", record$error_message)
      }
      
      message("")  # Blank line between records
    }
  }
  
  message(paste(rep("=", 70), collapse = ""))
  message("📈 Total records shown: ", nrow(history))
}

# ---- 字段级明细：某次更新具体改了哪个物质的哪一列 ----------------------------

#' Get Detailed Changes
#'
#' Retrieves detailed change information for specific update sessions.
#'
#' @param con Database connection
#' @param update_ids Vector of update history IDs to get details for
#' @return Data frame with detailed change records
#' @encoding UTF-8
get_detailed_changes <- function(con, update_ids) {
  if (length(update_ids) == 0) {
    return(data.frame())
  }
  
  # Build query for detailed changes
  placeholders <- paste(rep("?", length(update_ids)), collapse = ",")
  query <- paste0("
    SELECT 
      cl.update_history_id,
      cl.database_name,
      cl.InChIKey,
      cl.substance_identifier,
      cl.change_type,
      cl.field_name,
      cl.old_value,
      cl.new_value,
      cl.timestamp,
      uh.update_timestamp as session_timestamp
    FROM change_log cl
    JOIN update_history uh ON cl.update_history_id = uh.id
    WHERE cl.update_history_id IN (", placeholders, ")
    ORDER BY cl.timestamp DESC, cl.database_name, cl.change_type
  ")
  
  changes <- DBI::dbGetQuery(con, query, params = as.list(update_ids))
  return(changes)
}

#' Display Detailed Changes
#'
#' Formats and displays detailed change information.
#'
#' @param changes Data frame with detailed change records
#' @encoding UTF-8
display_detailed_changes <- function(changes) {
  if (nrow(changes) == 0) {
    message("📭 No detailed changes available")
    return()
  }
  
  message("\n🔍 DETAILED CHANGES (Most Recent Sessions)")
  message(paste(rep("=", 70), collapse = ""))
  
  # Group by update session
  sessions <- unique(changes$update_history_id)
  
  for (session_id in sessions[1:min(3, length(sessions))]) {
    session_changes <- changes[changes$update_history_id == session_id, ]
    session_time <- format(as.POSIXct(session_changes$session_timestamp[1]), "%Y-%m-%d %H:%M:%S")
    
    message(sprintf("\n📅 Session %d (%s):", session_id, session_time))
    
    # Group by change type
    change_types <- c("added", "removed", "modified")
    
    for (change_type in change_types) {
      type_changes <- session_changes[session_changes$change_type == change_type, ]
      
      if (nrow(type_changes) > 0) {
        type_icon <- switch(change_type,
                           "added" = "🆕",
                           "removed" = "🗑️",
                           "modified" = "✏️")
        
        message(sprintf("  %s %s (%d substances):", type_icon, toupper(change_type), nrow(type_changes)))
        
        # Show first few substances
        for (i in 1:min(5, nrow(type_changes))) {
          change <- type_changes[i, ]
          
          if (change_type == "modified" && !is.na(change$field_name)) {
            message(sprintf("    • %s [%s: %s → %s]", 
                           change$substance_identifier, change$field_name, 
                           substr(change$old_value, 1, 30), substr(change$new_value, 1, 30)))
          } else {
            message(sprintf("    • %s", change$substance_identifier))
          }
        }
        
        if (nrow(type_changes) > 5) {
          message(sprintf("    ... and %d more", nrow(type_changes) - 5))
        }
      }
    }
  }
  
  message(paste(rep("=", 70), collapse = ""))
}

# ---- 统计概览：近 N 天的更新频次与活跃物质（巡检用） --------------------------

#' Get Database Statistics
#'
#' Provides comprehensive statistics about database usage, updates, and trends.
#'
#' @param days_back Number of days to look back for trend analysis (default: 30)
#' @param db_path Optional custom path to database file (for testing)
#' @return List with database statistics
#' @export
#' @encoding UTF-8
get_database_statistics <- function(days_back = 30, db_path = NULL) {
  message("📊 Generating database statistics...")
  
  tryCatch({
    con <- get_db_connection(db_path)
    on.exit(DBI::dbDisconnect(con))
    
    # Guard: database_metadata / update_history may be missing on pre-migration db
    if (!DBI::dbExistsTable(con, "database_metadata") || !DBI::dbExistsTable(con, "update_history")) {
      message("ℹ️  metadata/history tables not present - returning empty statistics")
      return(list(current_stats = data.frame(), update_frequency = data.frame(),
                  daily_activity = data.frame(), active_substances = data.frame()))
    }
    
    # Current database sizes
    current_stats <- DBI::dbGetQuery(con, "
      SELECT 
        database_name,
        total_records,
        last_updated,
        version
      FROM database_metadata
      ORDER BY database_name
    ")
    
    # Update frequency over time period
    update_frequency <- DBI::dbGetQuery(con, "
      SELECT 
        database_name,
        COUNT(*) as update_count,
        SUM(records_added) as total_added,
        SUM(records_removed) as total_removed,
        AVG(records_added - records_removed) as avg_net_change
      FROM update_history
      WHERE update_timestamp >= datetime('now', '-' || ? || ' days')
      GROUP BY database_name
      ORDER BY update_count DESC
    ", params = list(days_back))
    
    # Recent activity trends
    daily_activity <- DBI::dbGetQuery(con, "
      SELECT 
        DATE(update_timestamp) as update_date,
        COUNT(*) as updates_count,
        SUM(records_added + records_removed) as total_changes
      FROM update_history
      WHERE update_timestamp >= datetime('now', '-' || ? || ' days')
      GROUP BY DATE(update_timestamp)
      ORDER BY update_date DESC
    ", params = list(days_back))
    
    # Most active substances (most frequently changed)
    active_substances <- DBI::dbGetQuery(con, "
      SELECT 
        substance_identifier,
        database_name,
        COUNT(*) as change_count,
        MAX(timestamp) as last_changed
      FROM change_log
      WHERE timestamp >= datetime('now', '-' || ? || ' days')
      GROUP BY substance_identifier, database_name
      HAVING COUNT(*) > 1
      ORDER BY change_count DESC
      LIMIT 10
    ", params = list(days_back))
    
    # Display statistics
    display_database_statistics(current_stats, update_frequency, daily_activity, active_substances, days_back)
    
    return(invisible(list(
      current_stats = current_stats,
      update_frequency = update_frequency,
      daily_activity = daily_activity,
      active_substances = active_substances,
      analysis_period = days_back
    )))
    
  }, error = function(e) {
    message("❌ Error generating statistics: ", e$message)
    return(list())
  })
}

#' Display Database Statistics
#'
#' Formats and displays comprehensive database statistics.
#'
#' @param current_stats Current database status
#' @param update_frequency Update frequency data
#' @param daily_activity Daily activity trends
#' @param active_substances Most frequently changed substances
#' @param days_back Analysis period in days
#' @encoding UTF-8
display_database_statistics <- function(current_stats, update_frequency, daily_activity, active_substances, days_back) {
  message("\n📊 DATABASE STATISTICS REPORT")
  message(paste(rep("=", 70), collapse = ""))
  message("📅 Analysis Period: Last ", days_back, " days")
  message("")
  
  # Current database sizes
  message("📋 Current Database Sizes:")
  total_records <- sum(current_stats$total_records, na.rm = TRUE)
  for (i in 1:nrow(current_stats)) {
    db <- current_stats[i, ]
    percentage <- round(db$total_records / total_records * 100, 1)
    last_update <- if (is.na(db$last_updated)) "Never" else format(as.POSIXct(db$last_updated), "%Y-%m-%d")
    message(sprintf("  %-12s: %5d records (%4.1f%%) | Last updated: %s", 
                   toupper(db$database_name), db$total_records, percentage, last_update))
  }
  message(sprintf("  %-12s: %5d records", "TOTAL", total_records))
  
  # Update frequency
  if (nrow(update_frequency) > 0) {
    message("\n📈 Update Activity (Last ", days_back, " days):")
    for (i in 1:nrow(update_frequency)) {
      freq <- update_frequency[i, ]
      net_change <- freq$total_added - freq$total_removed
      message(sprintf("  %-12s: %2d updates | Net change: %+d records", 
                     toupper(freq$database_name), freq$update_count, net_change))
    }
  } else {
    message("\n📈 Update Activity: No updates in the last ", days_back, " days")
  }
  
  # Daily activity trends
  if (nrow(daily_activity) > 0) {
    message("\n📅 Recent Daily Activity:")
    for (i in 1:min(7, nrow(daily_activity))) {
      activity <- daily_activity[i, ]
      message(sprintf("  %s: %d updates (%d total changes)", 
                     activity$update_date, activity$updates_count, activity$total_changes))
    }
  }
  
  # Most active substances
  if (nrow(active_substances) > 0) {
    message("\n🔄 Most Frequently Changed Substances:")
    for (i in 1:min(5, nrow(active_substances))) {
      substance <- active_substances[i, ]
      last_changed <- format(as.POSIXct(substance$last_changed), "%Y-%m-%d")
      message(sprintf("  %s (%s): %d changes | Last: %s", 
                     substance$substance_identifier, toupper(substance$database_name), 
                     substance$change_count, last_changed))
    }
  }
  
  message(paste(rep("=", 70), collapse = ""))
}

# ---- 导出账本（交给外部查看 / 存档） ----------------------------------------

#' Export Update History
#'
#' Exports update history and change logs to files for external analysis.
#'
#' @param output_dir Directory to save export files (default: current directory)
#' @param format Export format: "csv", "xlsx", or "both" (default: "csv")
#' @param date_from Optional start date for filtering
#' @param date_to Optional end date for filtering
#' @return Logical indicating success
#' @importFrom utils write.csv
#' @export
#' @encoding UTF-8
export_update_history <- function(output_dir = ".", format = "csv", date_from = NULL, date_to = NULL) {
  message("📤 Exporting update history...")
  
  tryCatch({
    con <- get_db_connection()
    on.exit(DBI::dbDisconnect(con))
    
    # Build queries with date filters
    history_query <- "SELECT * FROM update_history WHERE 1=1"
    changes_query <- "SELECT * FROM change_log WHERE 1=1"
    params <- list()
    
    if (!is.null(date_from)) {
      history_query <- paste(history_query, "AND DATE(update_timestamp) >= ?")
      changes_query <- paste(changes_query, "AND DATE(timestamp) >= ?")
      params <- append(params, date_from)
    }
    
    if (!is.null(date_to)) {
      history_query <- paste(history_query, "AND DATE(update_timestamp) <= ?")
      changes_query <- paste(changes_query, "AND DATE(timestamp) <= ?")
      params <- append(params, date_to)
    }
    
    history_query <- paste(history_query, "ORDER BY update_timestamp DESC")
    changes_query <- paste(changes_query, "ORDER BY timestamp DESC")
    
    # Get data
    if (length(params) > 0) {
      update_history <- DBI::dbGetQuery(con, history_query, params = params)
      change_log <- DBI::dbGetQuery(con, changes_query, params = params)
    } else {
      update_history <- DBI::dbGetQuery(con, history_query)
      change_log <- DBI::dbGetQuery(con, changes_query)
    }
    
    # Generate filenames with timestamp
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    
    if (format %in% c("csv", "both")) {
      # Export as CSV
      history_file <- file.path(output_dir, paste0("update_history_", timestamp, ".csv"))
      changes_file <- file.path(output_dir, paste0("change_log_", timestamp, ".csv"))
      
      write.csv(update_history, history_file, row.names = FALSE)
      write.csv(change_log, changes_file, row.names = FALSE)
      
      message("✅ CSV files exported:")
      message("   ", history_file)
      message("   ", changes_file)
    }
    
    if (format %in% c("xlsx", "both")) {
      # Export as Excel (requires openxlsx or similar)
      excel_file <- file.path(output_dir, paste0("fcmsafety_audit_", timestamp, ".xlsx"))
      
      # This would require additional package for Excel export
      message("⚠️  Excel export requires additional packages (openxlsx)")
      message("💡 Use format='csv' for now, or install openxlsx package")
    }
    
    return(TRUE)
    
  }, error = function(e) {
    message("❌ Export failed: ", e$message)
    return(FALSE)
  })
}
