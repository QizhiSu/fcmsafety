# =============================================================================
# 包的主入口（面向用户的一层）：建库 / 状态 / 更新 / 体检
#
# 本文件是用户"第一次上手"和"日常巡检"会碰到的函数，全部 @export：
#   setup_fcmsafety_database()    一次性建库 + 从 xlsx 迁移（新装机器第一步跑它）
#   fcmsafety_status()            看一眼数据库现在有什么、最近更新过什么
#   validate_database_integrity() 体检：表是否存在、行数是否合理、外键是否断链
#
# 内部辅助（不导出）：
#   display_database_summary()    控制台打印状态摘要
#   backup_xlsx_files()           迁移前把 inst/ 下的 xlsx 备份走
#
# 边界：本文件只做"调度与展示"。真正落库的逻辑在
# sqlite_database_manager.R（建表/迁移/连接），增量比对在
# incremental_update.R，各法规源的抓取在 update_other_dbs.R 与
# auto_update_svhc.R。想改数据怎么进来，别在这个文件里找。
#
# 注意：文件头说明用普通注释（而非 roxygen #'），否则会被 roxygen2
# 误配给其后第一个函数而抢占它的文档 title。
# =============================================================================

# ---- 内部：控制台打印状态摘要 -----------------------------------------------

# 内部函数：打印数据库状态摘要（原旧线 enhanced_update_system.R 中
# display_database_summary() 的打印逻辑，随旧线退役后保留为内部工具，
# 供 setup_fcmsafety_database() 与 fcmsafety_status() 使用）。
# 输入 status 来自 check_database_status()。
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

# ---- 主入口：建库 / 状态（@export，用户直接调） ------------------------------

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
#' @encoding UTF-8
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
    message("   • Use assign_toxicity() to screen chemicals against the databases")
    message("   • Use update_database_auto() for one-click database updates")
    message("   • Use get_update_history() to view change history")

    return(TRUE)

  }, error = function(e) {
    message("❌ Setup failed: ", e$message)
    message("💡 Fix the reported issue and rerun, or check inst/ for source files")
    return(FALSE)
  })
}

# assign_toxicity_enhanced() 已于 2026-09-09 删除：其"保存结果到 CSV"功能已
# 合并进 assign_toxicity()（新增 output_file 参数）；"默认 check_updates = TRUE"
# 属有意不合并的行为（基础版应保持确定性、默认不查更新）。见 R/direct_sql_toxicity.R。

#' Quick Database Status Check
#'
#' Provides a quick overview of the current database system status,
#' including record counts, last updates, and recent activity.
#'
#' @param show_recent_activity Logical, whether to show recent update activity
#' @return Invisible status object
#' @export
#' @encoding UTF-8
fcmsafety_status <- function(show_recent_activity = TRUE) {
  message("📊 FCMSafety Database System Status")
  message(paste(rep("=", 50), collapse = ""))

  # Check database status
  status <- check_database_status()

  if (!status$initialized) {
    message("❌ SQLite database not initialized")
    message("💡 Run setup_fcmsafety_database() to initialize")
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

  # 检测人工放入的新清单（旧线 check_available_updates() 已随旧线删除，
  # 由 check_manual_lists() 取代；此处只报告不询问）
  message("\n🔍 Checking for manually added list files...")
  check_manual_lists(ask = FALSE)

  message("\n💡 Available commands:")
  message("   • assign_toxicity() - Screen chemicals against regulatory databases")
  message("   • update_database_auto() - One-click update of all databases")
  message("   • get_update_history() - View change history")
  message("   • get_database_statistics() - Detailed statistics")

  return(invisible(status))
}

# ---- 内部：迁移前备份 inst/ 下的 xlsx ---------------------------------------

#' Backup XLSX Files
#'
#' Creates a timestamped backup of existing xlsx database files.
#'
#' @param backup_dir Directory to store backups (default: inst/backups/)
#' @return Logical indicating success
#' @encoding UTF-8
backup_xlsx_files <- function(backup_dir = NULL) {
  if (is.null(backup_dir)) {
    backup_dir <- file.path(getwd(), "inst", "backups")
  }

  if (!dir.exists(backup_dir)) {
    dir.create(backup_dir, recursive = TRUE)
  }

  # List of xlsx files to backup (matches actual inst/ file names)
  xlsx_files <- c(
    "svhc_meta.xlsx", "clp_cmr_meta.xlsx", "iarc_meta.xlsx",
    "eu10_2011_meta.xlsx", "eu10_2011.xlsx", "edc_meta.xlsx",
    "china_sml_meta_cleaned.xlsx", "china_sml_meta.xlsx"
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

# ---- 体检：结构完整性校验（@export） ----------------------------------------

#' Validate Database Integrity
#'
#' Performs comprehensive validation of the database system, checking for
#' data consistency, missing records, and potential issues.
#'
#' @param fix_issues Logical, whether to attempt automatic fixes
#' @param db_path Optional custom path to database file (for testing)
#' @return List with validation results
#' @export
#' @encoding UTF-8
validate_database_integrity <- function(fix_issues = FALSE, db_path = NULL) {
  message("🔍 Validating database integrity...")

  validation_results <- list(
    passed = TRUE,
    issues = list(),
    fixes_applied = list()
  )

  tryCatch({
    con <- get_db_connection(db_path)
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
      if (!DBI::dbExistsTable(con, table)) {
        message("   ℹ️  Table ", table, " missing - skipping orphan check")
        next
      }
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

    if (!DBI::dbExistsTable(con, "chemicals")) {
      message("ℹ️  chemicals table missing - skipping data consistency check")
    } else {
      # Check for duplicate InChIKeys in chemicals table
      dup_query <- "SELECT InChIKey, COUNT(*) as count FROM chemicals GROUP BY InChIKey HAVING COUNT(*) > 1"
      duplicates <- DBI::dbGetQuery(con, dup_query)

      if (nrow(duplicates) > 0) {
        validation_results$passed <- FALSE
        validation_results$issues$duplicate_chemicals <- nrow(duplicates)
        message("❌ ", nrow(duplicates), " duplicate InChIKeys in chemicals table")
      }
    }

    # Check 4: Verify metadata consistency
    message("📋 Checking metadata consistency...")

    if (!DBI::dbExistsTable(con, "database_metadata")) {
      message("ℹ️  database_metadata table missing - skipping metadata check")
    } else {
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
