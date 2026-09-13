# =============================================================================
# 包的主入口（面向用户的一层）：建库 / 状态 / 更新
#
# 本文件是用户"第一次上手"和"日常巡检"会碰到的函数，全部 @export：
#   setup_fcmsafety_database()    一次性建库 + 从 xlsx 迁移（新装机器第一步跑它）
#   fcmsafety_status()            看一眼数据库现在有什么、最近更新过什么
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

    # Migrate data. force_reinit 时 initialize_database() 已原子重建并完成
    # 迁移（直接 return migrate_xlsx_to_sqlite()），再跑是白做全套
    if (!force_reinit) {
      message("🔄 Migrating data from xlsx to SQLite...")
      if (!migrate_xlsx_to_sqlite()) {
        stop("Data migration failed")
      }
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

