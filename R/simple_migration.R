# =============================================================================
# xlsx -> SQLite 迁移的兼容包装（薄壳，无自有逻辑）
#
# 本文件里三个函数都只是**转发**，真正的迁移实现在
# sqlite_database_manager.R 的 migrate_xlsx_to_sqlite()：
#   simple_migrate_xlsx_to_sqlite()  转发迁移 + 打印提示
#   check_migration_results()        迁移后核对行数
#   compare_xlsx_sqlite()            xlsx 与库内逐表比对（排查"迁移漏了行"）
#
# 为什么还留着：这是旧线（xlsx 装载模式）的公开入口名，外部脚本可能还在调。
# 新代码请直接用 migrate_xlsx_to_sqlite()。
# =============================================================================

#' Simple Direct XLSX to SQLite Migration
#'
#' 直接把XLSX文件内容迁移到SQLite，不做任何复杂的处理
#' 保持数据的原始结构和内容
#'
#' 兼容性包装：实际委托给 sqlite_database_manager.R 中的
#' \code{migrate_xlsx_to_sqlite()}，以临时库 + 原子替换的方式执行，
#' 任何失败都不会破坏现有数据库。
#'
#' @param force_recreate 是否重新创建数据库
#' @return 逻辑值表示是否成功
#' @encoding UTF-8
simple_migrate_xlsx_to_sqlite <- function(force_recreate = TRUE) {
  message("🔄 开始简单直接的XLSX到SQLite迁移（原子替换模式）...")
  migrate_xlsx_to_sqlite(backup_existing = TRUE)
}

#' 检查迁移结果
#'
#' @return 数据框显示迁移结果
#' @encoding UTF-8
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
#' @encoding UTF-8
compare_xlsx_sqlite <- function() {
  message("📊 比较XLSX和SQLite记录数...")

  mapping <- resolve_xlsx_mapping()
  files_to_check <- lapply(mapping, function(m) m$file)

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
      file_name <- files_to_check[[table_name]]
      file_path <- file.path(getwd(), "inst", file_name)

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
