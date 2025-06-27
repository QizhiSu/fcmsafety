#' Simple Direct XLSX to SQLite Migration
#'
#' 直接把XLSX文件内容迁移到SQLite，不做任何复杂的处理
#' 保持数据的原始结构和内容

library(dplyr)
library(rio)
library(DBI)
library(RSQLite)

#' 简单直接的数据迁移
#'
#' @param force_recreate 是否重新创建数据库
#' @return 逻辑值表示是否成功
simple_migrate_xlsx_to_sqlite <- function(force_recreate = TRUE) {
  message("🔄 开始简单直接的XLSX到SQLite迁移...")

  # 删除现有数据库
  db_path <- file.path(getwd(), "inst", "fcmsafety.db")
  if (force_recreate && file.exists(db_path)) {
    file.remove(db_path)
    message("🗑️  删除现有数据库")
  }

  tryCatch({
    # 创建数据库连接
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    on.exit(DBI::dbDisconnect(con))

    # 定义要迁移的文件
    files_to_migrate <- list(
      svhc = "inst/svhc.xlsx",
      cmr = "inst/cmr.xlsx",
      cmr_suspect = "inst/suspect_cmr.xlsx",
      iarc = "inst/iarc.xlsx",
      eu_sml = "inst/eu10_2011.xlsx",
      eu_sml_group = "inst/eu10_2011_group.xlsx",
      edc = "inst/edc.xlsx",
      china_sml = "inst/china_sml_cleaned.xlsx"
    )

    total_migrated <- 0

    # 逐个迁移文件
    for (table_name in names(files_to_migrate)) {
      file_path <- files_to_migrate[[table_name]]

      if (file.exists(file_path)) {
        message("📊 迁移 ", table_name, " 从 ", basename(file_path), "...")

        # 直接读取XLSX文件
        data <- rio::import(file_path)
        original_count <- nrow(data)

        # 修复重复列名问题（特别处理各种文件的列名冲突）
        if (table_name %in% c("cmr", "cmr_suspect")) {
          # 手动修复CMR文件的列名问题
          col_names <- names(data)
          # 第8列改名为避免与第6列冲突
          if (length(col_names) >= 8 && grepl("Hazard statement Code", col_names[8])) {
            col_names[8] <- "Hazard Statement Code Alternative"
            names(data) <- col_names
            message("   ⚠️  修复", table_name, "文件列名冲突")
          }
        } else if (table_name == "edc") {
          # 修复EDC文件的CID列名冲突（cid vs CID）
          col_names <- names(data)
          # 找到cid和CID列
          if ("cid" %in% col_names && "CID" %in% col_names) {
            # 将小写的cid改为cid_lower
            cid_index <- which(col_names == "cid")
            col_names[cid_index] <- "cid_lower"
            names(data) <- col_names
            message("   ⚠️  修复", table_name, "文件cid/CID列名冲突")
          }
        }

        # 通用的重复列名修复
        if (any(duplicated(names(data)))) {
          message("   ⚠️  发现重复列名，正在修复...")
          names(data) <- make.names(names(data), unique = TRUE)
        }

        message("   原始记录数: ", original_count)
        message("   列数: ", ncol(data))
        message("   列名: ", paste(names(data)[1:min(5, ncol(data))], collapse = ", "),
                if(ncol(data) > 5) "..." else "")

        # 直接写入SQLite，不做任何处理
        DBI::dbWriteTable(con, table_name, data, overwrite = TRUE)

        # 验证写入
        count_check <- DBI::dbGetQuery(con, paste("SELECT COUNT(*) as count FROM", table_name))$count[1]

        if (count_check == original_count) {
          message("   ✅ 成功迁移 ", count_check, " 条记录")
          total_migrated <- total_migrated + count_check
        } else {
          message("   ❌ 迁移失败: 期望 ", original_count, " 实际 ", count_check)
        }

      } else {
        message("   ⚠️  文件不存在: ", file_path)
      }
    }

    message("✅ 迁移完成！总共迁移了 ", total_migrated, " 条记录")
    return(TRUE)

  }, error = function(e) {
    message("❌ 迁移失败: ", e$message)
    return(FALSE)
  })
}

#' 检查迁移结果
#'
#' @return 数据框显示迁移结果
check_migration_results <- function() {
  message("🔍 检查迁移结果...")

  db_path <- file.path(getwd(), "inst", "fcmsafety.db")
  if (!file.exists(db_path)) {
    message("❌ 数据库文件不存在")
    return(data.frame())
  }

  tryCatch({
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    on.exit(DBI::dbDisconnect(con))

    # 获取所有表
    tables <- DBI::dbListTables(con)

    # 检查每个表的记录数
    results <- data.frame()

    for (table in tables) {
      count <- DBI::dbGetQuery(con, paste("SELECT COUNT(*) as count FROM", table))$count[1]

      # 获取列信息
      col_info <- DBI::dbGetQuery(con, paste("PRAGMA table_info(", table, ")"))
      col_count <- nrow(col_info)

      # 检查是否有InChIKey列
      has_inchikey <- "InChIKey" %in% col_info$name

      # 如果有InChIKey，统计有效的InChIKey数量
      valid_inchikey <- if (has_inchikey) {
        DBI::dbGetQuery(con, paste("SELECT COUNT(*) as count FROM", table, "WHERE InChIKey IS NOT NULL AND InChIKey != ''"))$count[1]
      } else {
        0
      }

      results <- rbind(results, data.frame(
        Table = table,
        Total_Records = count,
        Columns = col_count,
        Has_InChIKey = has_inchikey,
        Valid_InChIKey = valid_inchikey,
        stringsAsFactors = FALSE
      ))
    }

    # 显示结果
    message("📊 迁移结果汇总:")
    for (i in 1:nrow(results)) {
      row <- results[i, ]
      message(sprintf("   %-15s: %4d 记录, %2d 列, InChIKey: %s (%d 有效)",
                     row$Table, row$Total_Records, row$Columns,
                     ifelse(row$Has_InChIKey, "✅", "❌"), row$Valid_InChIKey))
    }

    return(results)

  }, error = function(e) {
    message("❌ 检查失败: ", e$message)
    return(data.frame())
  })
}

#' 比较XLSX和SQLite记录数
#'
#' @return 数据框显示比较结果
compare_xlsx_sqlite <- function() {
  message("📊 比较XLSX和SQLite记录数...")

  files_to_check <- list(
    svhc = "inst/svhc.xlsx",
    cmr = "inst/cmr.xlsx",
    cmr_suspect = "inst/suspect_cmr.xlsx",
    iarc = "inst/iarc.xlsx",
    eu_sml = "inst/eu10_2011.xlsx",
    eu_sml_group = "inst/eu10_2011_group.xlsx",
    edc = "inst/edc.xlsx",
    china_sml = "inst/china_sml_cleaned.xlsx"
  )

  db_path <- file.path(getwd(), "inst", "fcmsafety.db")

  if (!file.exists(db_path)) {
    message("❌ SQLite数据库不存在")
    return(data.frame())
  }

  tryCatch({
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    on.exit(DBI::dbDisconnect(con))

    comparison <- data.frame()

    for (table_name in names(files_to_check)) {
      file_path <- files_to_check[[table_name]]

      # XLSX记录数
      xlsx_count <- if (file.exists(file_path)) {
        nrow(rio::import(file_path))
      } else {
        0
      }

      # SQLite记录数
      sqlite_count <- if (table_name %in% DBI::dbListTables(con)) {
        DBI::dbGetQuery(con, paste("SELECT COUNT(*) as count FROM", table_name))$count[1]
      } else {
        0
      }

      # 计算差异
      difference <- xlsx_count - sqlite_count
      match_rate <- if (xlsx_count > 0) round((sqlite_count / xlsx_count) * 100, 1) else 0

      comparison <- rbind(comparison, data.frame(
        Database = table_name,
        XLSX_Records = xlsx_count,
        SQLite_Records = sqlite_count,
        Difference = difference,
        Match_Rate = paste0(match_rate, "%"),
        Status = ifelse(difference == 0, "✅ 完全匹配",
                       ifelse(difference > 0, "❌ 数据丢失", "⚠️ 数据增加")),
        stringsAsFactors = FALSE
      ))
    }

    # 显示比较结果
    message("📊 XLSX vs SQLite 比较结果:")
    for (i in 1:nrow(comparison)) {
      row <- comparison[i, ]
      message(sprintf("   %-15s: XLSX=%4d | SQLite=%4d | 差异=%3d | 匹配率=%6s | %s",
                     row$Database, row$XLSX_Records, row$SQLite_Records,
                     row$Difference, row$Match_Rate, row$Status))
    }

    return(comparison)

  }, error = function(e) {
    message("❌ 比较失败: ", e$message)
    return(data.frame())
  })
}
