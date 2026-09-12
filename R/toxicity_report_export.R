# =============================================================================
# 导出报告（Excel 工作簿 / CSV）
#
# 本文件把 assign_toxicity() 的结果表写成"能直接交给人看"的工作簿，共 4 个
# sheet：Results（全表，冻结首行 + 筛选 + 按毒性等级着色）/ Summary（分级分布
# 与数据来源溯源）/ Unassigned（无证据的行单独列）/ Issues（查询失败、缺 H 码、
# 需人工确认的组条目等"让结果显得比实际干净"的问题）。
#
# 调用路径：assign_toxicity(output_file = "xxx.xlsx") 会按扩展名自动分派到
# 这里；也可以对已有结果手动再导出（不必重跑匹配）。
#
# 依赖 openxlsx（在 DESCRIPTION 的 Imports 里）。着色与列宽逻辑在下方
# `.level_style()` / `.style_table()` 等内部函数里，改样式只动那几个函数。
# =============================================================================

#' Export a toxicity report to Excel (or CSV)
#'
#' Writes the table produced by [assign_toxicity()] to a workbook with one sheet
#' per purpose, instead of a single flat CSV:
#'
#' \describe{
#'   \item{Results}{the full table, one row per input compound, with a frozen
#'     header, an autofilter and fitted column widths. The `Toxic_level` cell is
#'     shaded by tier (V dark red through I green, `-` grey).}
#'   \item{Summary}{level distribution, database match counts and provenance of
#'     the run (package version, database file and its timestamp, time of run).}
#'   \item{Unassigned}{only the rows whose `Toxic_level` is `-`, so they can be
#'     worked through separately.}
#'   \item{Issues}{anything that could make the result look cleaner than it is:
#'     a regulatory table that is missing or failed to query, rows listed in
#'     `cmr` without usable hazard codes, group-membership rows that need a
#'     human decision.}
#' }
#'
#' `assign_toxicity(output_file = ...)` calls this automatically when the path
#' ends in `.xlsx`; the function is exported so an existing result data.frame
#' can be re-exported (for example after editing it by hand) without re-running
#' the assignment.
#'
#' @param result_data data.frame returned by [assign_toxicity()]. Only the
#'   `Toxic_level` column is required; everything else is passed through.
#' @param path Output path. `.xlsx` is written as a styled workbook, `.csv` as
#'   a flat file with the same content as the Results sheet.
#' @param summary Optional data.frame with columns `Section`, `Metric`, `Value`.
#'   `NULL` means derive it from `result_data` alone.
#' @param issues Optional data.frame with columns `Source`, `Status`, `Rows`,
#'   `Message`. `NULL` means write a single "no issues recorded" row.
#' @param overwrite Logical, overwrite an existing file (default: TRUE).
#' @return The output path, invisibly.
#' @examples
#' \dontrun{
#' res <- assign_toxicity(data)
#' export_toxicity_report(res, "report.xlsx")
#' }
#' @export
#' @encoding UTF-8
export_toxicity_report <- function(result_data, path, summary = NULL,
                                   issues = NULL, overwrite = TRUE) {
  if (!is.data.frame(result_data)) {
    stop("result_data must be a data.frame", call. = FALSE)
  }
  ext <- tolower(tools::file_ext(path))
  if (identical(ext, "csv")) {
    utils::write.csv(result_data, path, row.names = FALSE)
    return(invisible(path))
  }
  if (!ext %in% c("xlsx", "xlsm")) {
    stop("Unsupported output extension '.", ext,
         "'. Use .xlsx (styled workbook) or .csv (flat file).", call. = FALSE)
  }

  if (is.null(summary)) summary <- .report_summary_table(result_data)
  if (is.null(issues)) {
    issues <- data.frame(Source = "run", Status = "ok", Rows = NA_integer_,
                         Message = "No issues recorded.", stringsAsFactors = FALSE)
  }

  wb <- openxlsx::createWorkbook()

  # ---- Results -------------------------------------------------------------
  openxlsx::addWorksheet(wb, "Results")
  openxlsx::writeData(wb, "Results", result_data)
  .style_table(wb, "Results", result_data)
  level_col <- match("Toxic_level", names(result_data))
  if (!is.na(level_col)) {
    grid <- as.character(result_data$Toxic_level)
    for (lv in names(.level_fills)) {
      rows <- which(grid == lv) + 1L
      if (length(rows) == 0) next
      openxlsx::addStyle(wb, "Results", .level_style(lv), rows = rows,
                         cols = level_col, gridExpand = TRUE, stack = TRUE)
    }
  }

  # ---- Summary -------------------------------------------------------------
  openxlsx::addWorksheet(wb, "Summary")
  openxlsx::writeData(wb, "Summary", summary)
  .style_table(wb, "Summary", summary)
  section_rows <- which(duplicated(summary$Section, fromLast = FALSE) == FALSE) + 1L
  openxlsx::addStyle(wb, "Summary",
                     openxlsx::createStyle(textDecoration = "bold",
                                           fgFill = "#EAF1F8"),
                     rows = section_rows, cols = 1:3, gridExpand = TRUE,
                     stack = TRUE)

  # ---- Unassigned ----------------------------------------------------------
  if (!is.na(level_col)) {
    unassigned <- result_data[!is.na(result_data$Toxic_level) &
                                as.character(result_data$Toxic_level) == "-", ,
                              drop = FALSE]
  } else {
    unassigned <- result_data[0, , drop = FALSE]
  }
  openxlsx::addWorksheet(wb, "Unassigned")
  openxlsx::writeData(wb, "Unassigned", unassigned)
  .style_table(wb, "Unassigned", unassigned, empty_note = paste0(
    "Every compound matched at least one rule (", nrow(result_data),
    " rows). Nothing to review here."))

  # ---- Issues --------------------------------------------------------------
  openxlsx::addWorksheet(wb, "Issues")
  openxlsx::writeData(wb, "Issues", issues)
  .style_table(wb, "Issues", issues)
  warn_rows <- which(tolower(as.character(issues$Status)) %in%
                       c("failed", "missing", "warn", "warning")) + 1L
  if (length(warn_rows) > 0) {
    openxlsx::addStyle(wb, "Issues",
                       openxlsx::createStyle(fgFill = "#F8CBCB",
                                             fontColour = "#8B1A1A"),
                       rows = warn_rows, cols = 1:ncol(issues),
                       gridExpand = TRUE, stack = TRUE)
  }

  openxlsx::saveWorkbook(wb, path, overwrite = overwrite)
  invisible(path)
}

# ---- 内部：样式与表格骨架 ---------------------------------------------------

# 等级 -> 填充色/字色。V 最危险用深红，I 最安全用绿（与"涨红跌绿"的国标习惯一致），
# 未定级的 "-" 用中性灰，避免被误读成"安全"。
.level_fills <- c(V = "#F8CBCB", IV = "#FBDCC0", III = "#FCEFA6",
                  II = "#DCEFD8", I = "#C9EAC9", "-" = "#EFEFEF")
.level_fonts <- c(V = "#8B1A1A", IV = "#8A4B08", III = "#7A6000",
                  II = "#2F6B2F", I = "#1E5C1E", "-" = "#808080")

.level_style <- function(level) {
  openxlsx::createStyle(fgFill = unname(.level_fills[[level]]),
                        fontColour = unname(.level_fonts[[level]]),
                        textDecoration = "bold", halign = "center")
}

.header_style <- function() {
  openxlsx::createStyle(fgFill = "#1F4E79", fontColour = "#FFFFFF",
                        textDecoration = "bold", halign = "center",
                        valign = "center", wrapText = TRUE,
                        border = "TopBottomLeftRight", borderColour = "#B7C9DA")
}

.body_style <- function() {
  openxlsx::createStyle(valign = "top", border = "TopBottomLeftRight",
                        borderColour = "#E3E9EF")
}

# 列宽按内容拟合。nchar(type = "width") 会把中日韩字符按两格算，中文表头不会被截断。
.autofit_width <- function(x, header, min_width = 8, max_width = 48) {
  vals <- as.character(x)
  vals <- vals[!is.na(vals)]
  widths <- c(nchar(header, type = "width"),
              if (length(vals)) nchar(vals, type = "width") else numeric(0))
  widths <- widths[!is.na(widths)]
  if (!length(widths)) return(min_width)
  max(min_width, min(max_width, max(widths) + 2))
}

.style_table <- function(wb, sheet, df, empty_note = NULL) {
  n_col <- ncol(df)
  if (n_col == 0) return(invisible(NULL))
  if (nrow(df) == 0 && !is.null(empty_note)) {
    openxlsx::writeData(wb, sheet, x = empty_note, startRow = 3, startCol = 1)
  }
  openxlsx::addStyle(wb, sheet, .header_style(), rows = 1, cols = seq_len(n_col),
                     gridExpand = TRUE, stack = TRUE)
  openxlsx::setRowHeights(wb, sheet, rows = 1, heights = 30)
  if (nrow(df) > 0) {
    openxlsx::addStyle(wb, sheet, .body_style(), rows = 2:(nrow(df) + 1L),
                       cols = seq_len(n_col), gridExpand = TRUE, stack = TRUE)
  }
  widths <- vapply(seq_len(n_col), function(j) {
    .autofit_width(df[[j]], names(df)[j])
  }, numeric(1))
  openxlsx::setColWidths(wb, sheet, cols = seq_len(n_col), widths = widths)
  openxlsx::freezePane(wb, sheet, firstRow = TRUE)
  if (nrow(df) > 0) {
    openxlsx::addFilter(wb, sheet, row = 1, cols = seq_len(n_col))
  }
  invisible(NULL)
}

# 从结果表本身推一份 Summary。
# run_info 是 assign_toxicity() 记的运行元信息（包版本、实际用的数据库文件及其
# 时间戳）；counts 是各库命中数。传入时按"哪来 / 查了什么 / 结果如何"三段排。
.report_summary_table <- function(result_data, run_info = NULL, counts = NULL) {
  section <- character(0)
  metric <- character(0)
  value <- character(0)
  add <- function(s, m, v) {
    section <<- c(section, s)
    metric <<- c(metric, m)
    value <<- c(value, as.character(v))
  }

  if (!is.null(run_info) && nrow(run_info) > 0) {
    for (i in seq_len(nrow(run_info))) {
      add("Run", run_info$Key[i], run_info$Value[i])
    }
  }
  add("Input", "Rows in input", nrow(result_data))

  if ("Toxic_level" %in% names(result_data)) {
    lv <- as.character(result_data$Toxic_level)
    for (t in c("V", "IV", "III", "II", "I")) {
      add("Toxicity level", paste0("Level ", t), sum(lv == t, na.rm = TRUE))
    }
    add("Toxicity level", "Not assigned (no rule matched)",
        sum(lv == "-", na.rm = TRUE))
  }

  if (!is.null(counts)) {
    for (nm in names(counts)) add("Database matches", nm, counts[[nm]])
  } else {
    for (col in c("SVHC", "CMR", "CMR_suspect", "EDC", "IARC",
                  "EU_SML", "China_SML")) {
      if (!col %in% names(result_data)) next
      add("Database matches", col,
          sum(as.character(result_data[[col]]) != "-", na.rm = TRUE))
    }
  }
  if ("Group_hits" %in% names(result_data)) {
    add("Group entries", "Rows with a group-level hit",
        sum(as.character(result_data$Group_hits) != "-", na.rm = TRUE))
    add("Group entries", "Rows needing manual review",
        sum(as.character(result_data$Group_review) != "-", na.rm = TRUE))
  }

  data.frame(Section = section, Metric = metric, Value = value,
             stringsAsFactors = FALSE)
}
