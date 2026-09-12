# =============================================================================
# Shiny 数据库查看器（launch_database_inspector）
#
# 本文件整个包在一个函数里：launch_database_inspector() 内部定义 ui 与 server
# 后调 runApp()。所以不要被 2800 行的体量吓到——它只有**一个**导出函数。
#
# 怎么找东西（文件里有 `# ---- 分区名 ----` 标记，按标记搜索比按行号可靠）：
#   `# ---- UI 定义`            侧栏（库选择 / 主题 / 语言）+ 选项卡
#   `# ---- Server`            响应式状态、文案输出、主数据表、结构式、一键操作
#   两个大板块另有 `# ====` 形式的长注释块（如"一键操作面板 handlers"）
#
# 三处容易踩的坑（改动前先读对应注释）：
#   1) **i18n**：文案不是硬编码，而是 texts[[key]][[lang]] 查表，且每个
#      output$* 里都显式读了 values$current_lang 来建立依赖——不读就不会随
#      语言切换刷新。新增文案要同时进文案表和 get_text()。
#   2) **结构式面板**：SMILES 由前端 JS 从已渲染表格的表头里取，不用 R 侧
#      按列序号反查（历史做法，会静默失败）。见"结构式面板"区。
#   3) **一键操作面板**：会调 update_*_auto() 写库。联网更新是**两段式**——
#      先 auto_apply = FALSE 预演、把每库的 diff 摆出来（注明哪些会被安全阀拦下、
#      哪些超过自动上限），人点「确认写入」才真写。防连点有三道：页面禁用按钮 +
#      服务端宽限期（should_drop_run_request）+ 单轮内早退。
#      ⚠️ 同步长任务跑着的时候，页面**输出**（进度条 / 日志）照样实时刷新 —— 已实测
#      （httpuv 的事件循环在后台线程），但**输入（点击）进不来**，要等任务结束才补送。
#      所以别在运行中加「取消」按钮，它按不下去；中止只能到 R 控制台 Ctrl+C
#      （复位逻辑在 run_quick_task 的 on.exit 里）。见 R/update_run_guard.R 与 ADR 0011。
#
# 依赖：shiny、DT 必需；rcdk 可选（缺了就退化成显示 SMILES 文本）。
# =============================================================================

#' FCMSafety Database Inspector Shiny App
#'
#' A comprehensive Shiny web application for inspecting and visualizing
#' the FCMSafety SQLite database contents, including data validation,
#' filtering, and chemical structure visualization.
#'
#' Launches the Shiny web application for database inspection and visualization.
#'
#' @importFrom shiny fluidPage titlePanel sidebarLayout sidebarPanel mainPanel
#' @importFrom shiny selectInput numericInput checkboxInput actionButton
#' @importFrom shiny tabsetPanel tabPanel dataTableOutput plotOutput
#' @importFrom shiny renderDataTable renderPlot renderText renderUI
#' @importFrom shiny reactive observeEvent req
#' @importFrom shiny h3 h4 h5 p br div column fluidRow
#' @importFrom shiny HTML textOutput htmlOutput textInput tags
#' @importFrom shiny reactiveValues observe showNotification
#' @importFrom shiny updateTextInput updateSelectInput updateActionButton
#' @importFrom grDevices png dev.off
#' @importFrom graphics par rasterImage
#' @importFrom DT datatable formatStyle
#' @importFrom DBI dbConnect dbDisconnect dbGetQuery dbListTables
#' @importFrom RSQLite SQLite
#' @param port Port number for the Shiny app (default: 3838)
#' @param launch_browser Whether to launch browser automatically (default: TRUE)
#' @export
#' @encoding UTF-8
launch_database_inspector <- function(port = 3838, launch_browser = TRUE) {

  # Check if required packages are available
  required_packages <- c("shiny", "DT")
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

  if (length(missing_packages) > 0) {
    stop("Missing required packages: ", paste(missing_packages, collapse = ", "),
         "\nPlease install with: install.packages(c(", paste0("'", missing_packages, "'", collapse = ", "), "))")
  }

  # Check for optional packages
  optional_packages <- c("rcdk", "base64enc")
  for (pkg in optional_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      message("Optional package '", pkg, "' not found. Some features may be limited.")
    }
  }

  # ---- UI 定义（主题 CSS + 侧栏 + 选项卡） -----------------------------------
  ui <- fluidPage(
    # ---- UI 之 CSS：主题变量 + 布局 + 结构式面板 + 表格样式 ----
    # Custom CSS for modern UI and themes
    tags$head(
      # --- Suppress DataTables alert() at the source ---
      # DataTables 1.13.10 (bundled with DT 0.34) does:
      #     window.DataTable = factory(jQuery, window, document)
      # inside its UMD wrapper, and the factory synchronously sets
      #     DataTable.ext = { ..., sErrMode: 'alert', ... }
      # then immediately DT 0.34's wrapper does
      #     $table.DataTable(options)
      # which calls into init() -> _fnLog(), which checks
      #     DataTable.ext.sErrMode === 'alert'
      # and runs alert(msg) SYNCHRONOUSLY. Our 5 ms polling watcher
      # (commit 88853f7) was too slow -- a sync init finishes before
      # the next tick. Setting errMode from a datatable() callback is
      # also too late (callback runs AFTER init).
      #
      # Fix: install a setter on window.DataTable via
      # Object.defineProperty. The DataTables UMD assignment
      # (window.DataTable = factory(...)) triggers the setter at
      # exactly the moment the DataTables function is created, and
      # the factory has already finished populating DataTable.ext
      # by the time it returns. The setter then immediately stamps
      # sErrMode='none' onto the SAME .ext object that init() will
      # read -- so init sees 'none' and skips the alert() branch.
      # A 1 ms polling fallback is kept in case defineProperty fails
      # (e.g. DataTable is already on window when this script runs).
      tags$script(HTML(paste0(
        "(function(){",
        "  try {",
        "    var _dtVal = window.DataTable;",
        "    Object.defineProperty(window, 'DataTable', {",
        "      configurable: true,",
        "      enumerable: true,",
        "      get: function(){ return _dtVal; },",
        "      set: function(v){",
        "        _dtVal = v;",
        "        if (v && v.ext) {",
        "          v.ext.sErrMode = 'none';",
        "          v.ext.errMode = 'none';",
        "        }",
        "      }",
        "    });",
        "    if (_dtVal && _dtVal.ext) {",
        "      _dtVal.ext.sErrMode = 'none';",
        "      _dtVal.ext.errMode = 'none';",
        "    }",
        "  } catch(e) {}",
        "  var iv = setInterval(function(){",
        "    if (window.DataTable && window.DataTable.ext) {",
        "      window.DataTable.ext.sErrMode = 'none';",
        "      window.DataTable.ext.errMode = 'none';",
        "    }",
        "  }, 1);",
        "  setTimeout(function(){ clearInterval(iv); }, 30000);",
        "})();"
      ))),
      tags$style(HTML("
        /* Modern UI Styling */
        .navbar-custom {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          border: none;
          box-shadow: 0 2px 10px rgba(0,0,0,0.1);
        }

        /* Gradient Title */
        h1 {
          background: linear-gradient(90deg, #d81b60 0%, #1e88e5 100%);
          -webkit-background-clip: text;
          -webkit-text-fill-color: transparent;
          background-clip: text;
          font-weight: bold;
          text-align: center;
          margin-bottom: 30px;
          font-size: 2.5rem;
        }

        /* Fallback for browsers that don't support background-clip */
        @supports not (-webkit-background-clip: text) {
          h1 {
            color: #d81b60;
          }
        }

        .theme-controls {
          position: fixed;
          top: 15px;
          right: 15px;
          z-index: 1000;
          display: flex;
          gap: 10px;
        }

        .theme-btn {
          padding: 8px 12px;
          border: none;
          border-radius: 20px;
          background: rgba(255,255,255,0.9);
          color: #333;
          cursor: pointer;
          transition: all 0.3s ease;
          font-size: 12px;
          box-shadow: 0 2px 5px rgba(0,0,0,0.1);
        }

        .theme-btn:hover {
          background: rgba(255,255,255,1);
          transform: translateY(-1px);
          box-shadow: 0 4px 10px rgba(0,0,0,0.15);
        }

        #quick_actions_title_out {
          font-weight: 700;
          margin-right: 6px;
        }

        /* Layout Consistency - Explicit properties to prevent theme-based shifts */
        * {
          box-sizing: border-box !important;
        }

        body {
          margin: 0 !important;
          padding: 0 !important;
          font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif !important;
          line-height: 1.5 !important;
        }

        .container-fluid {
          padding: 15px !important;
          margin: 0 !important;
          width: 100% !important;
        }

        .row {
          margin: 0 !important;
          display: flex !important;
          flex-wrap: wrap !important;
        }

        /* Ensure consistent spacing across themes */
        .light-theme, .dark-theme {
          margin: 0 !important;
          padding: 0 !important;
        }

        .sidebar-panel {
          padding: 15px !important;
          margin: 0 !important;
          border-radius: 8px !important;
          border: 1px solid !important;
          border-width: 1px !important;
          box-sizing: border-box !important;
        }

        .main-panel {
          padding: 15px !important;
          margin: 0 !important;
          border-radius: 8px !important;
          border: 1px solid !important;
          border-width: 1px !important;
          box-sizing: border-box !important;
        }

        .structure-panel {
          padding: 15px !important;
          margin: 0 !important;
          border-radius: 8px !important;
          border: 1px solid !important;
          border-width: 1px !important;
          min-height: 450px !important;
          width: 100% !important;
          box-sizing: border-box !important;
        }

        #structure_display {
          width: 100%;
          height: 400px;
          overflow: auto;
          border: 1px solid #ddd;
          border-radius: 8px;
          position: relative;
          background-color: #ffffff;
          padding: 0;
          box-sizing: border-box;
          display: block;
        }

        #structure_display .image-wrapper {
          min-width: 100%;
          min-height: 100%;
          display: flex;
          align-items: center;
          justify-content: center;
          padding: 150px;
          box-sizing: border-box;
        }

        .dark-theme #structure_display {
          background-color: #2d3748;
          border-color: #4a5568;
        }

        #structure_display img {
          display: block;
          margin: 0;
          cursor: grab;
          max-width: none;
          max-height: none;
        }

        #structure_display img:active {
          cursor: grabbing;
        }

        /* Modern Light Theme - Sophisticated Color Palette */
        .light-theme {
          background: linear-gradient(135deg, #f8fafc 0%, #e2e8f0 100%);
          color: #1e293b;
        }

        .light-theme .sidebar-panel {
          background: linear-gradient(145deg, #ffffff 0%, #f8fafc 100%);
          border-color: #cbd5e1;
          box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1), 0 2px 4px -1px rgba(0, 0, 0, 0.06);
        }

        .light-theme .main-panel {
          background: linear-gradient(145deg, #ffffff 0%, #f8fafc 100%);
          border-color: #cbd5e1;
          box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1), 0 2px 4px -1px rgba(0, 0, 0, 0.06);
        }

        .light-theme .structure-panel {
          background: linear-gradient(145deg, #f1f5f9 0%, #e2e8f0 100%);
          border-color: #94a3b8;
          box-shadow: inset 0 2px 4px 0 rgba(0, 0, 0, 0.06);
        }

        /* Modern Dark Theme - Rich and Sophisticated */
        .dark-theme {
          background: linear-gradient(135deg, #0f172a 0%, #1e293b 100%);
          color: #f1f5f9;
        }

        .dark-theme .sidebar-panel {
          background: linear-gradient(145deg, #1e293b 0%, #334155 100%);
          border-color: #475569;
          box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.3), 0 2px 4px -1px rgba(0, 0, 0, 0.2);
        }

        .dark-theme .main-panel {
          background: linear-gradient(145deg, #1e293b 0%, #334155 100%);
          border-color: #475569;
          box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.3), 0 2px 4px -1px rgba(0, 0, 0, 0.2);
        }

        .dark-theme .structure-panel {
          background: linear-gradient(145deg, #0f172a 0%, #1e293b 100%);
          border-color: #64748b;
          box-shadow: inset 0 2px 4px 0 rgba(0, 0, 0, 0.3);
        }

        /* Database info panel styling - Modern design */
        .database-info-panel {
          padding: 15px !important;
          margin: 0 !important;
          border-radius: 8px !important;
          border: 1px solid !important;
          border-width: 1px !important;
          font-size: 12px !important;
          line-height: 1.4 !important;
          box-sizing: border-box !important;
        }

        .light-theme .database-info-panel {
          background: linear-gradient(145deg, #f1f5f9 0%, #e2e8f0 100%);
          border-color: #94a3b8;
          color: #334155;
          box-shadow: inset 0 2px 4px 0 rgba(0, 0, 0, 0.06);
        }

        .dark-theme .database-info-panel {
          background: linear-gradient(145deg, #0f172a 0%, #1e293b 100%);
          border-color: #64748b;
          color: #cbd5e1;
          box-shadow: inset 0 2px 4px 0 rgba(0, 0, 0, 0.3);
        }

        /* Quick Actions panel - follows light/dark theme */
        .quick-update-panel {
          margin: 8px 20px 0 20px;
          padding: 10px 14px;
          border: 1px solid;
          border-radius: 10px;
          display: flex;
          flex-wrap: wrap;
          align-items: center;
          gap: 8px;
          box-sizing: border-box;
        }

        .light-theme .quick-update-panel {
          background: linear-gradient(145deg, #eef2ff 0%, #f4f7ff 100%);
          border-color: #dfe3ea;
        }

        .dark-theme .quick-update-panel {
          background: linear-gradient(145deg, #1e293b 0%, #334155 100%);
          border-color: #475569;
        }

        /* Log output box inside the quick actions panel */
        .quick-update-panel pre.shiny-text-output {
          border-radius: 6px;
          padding: 8px 10px;
          font-size: 12px;
          line-height: 1.5;
          margin: 4px 0 0 0;
        }

        .light-theme .quick-update-panel pre.shiny-text-output {
          background-color: #ffffff;
          border-color: #cbd5e1;
          color: #1e293b;
        }

        .dark-theme .quick-update-panel pre.shiny-text-output {
          background-color: #0f172a;
          border-color: #475569;
          color: #f1f5f9;
        }

        /* 一键操作：运行状态行 + 实时日志（2026-09-11 加）
           事故里用户全程看不到任何反馈，只能以为页面死了 —— 这两块就是
           页面还活着、跑到哪了的第一眼信号。 */
        .quick-progress-line {
          font-size: 13px;
          font-weight: 600;
          padding: 2px 0 6px 2px;
        }

        .quick-live-log {
          display: none;              /* 空的时候别占地方 */
          max-height: 190px;
          overflow-y: auto;
          border: 1px solid;
          border-radius: 6px;
          padding: 8px 10px;
          font-size: 12px;
          line-height: 1.5;
          white-space: pre-wrap;
          margin: 0 0 8px 0;
        }

        .light-theme .quick-progress-line { color: #1d4ed8; }
        .light-theme .quick-live-log {
          background-color: #ffffff;
          border-color: #cbd5e1;
          color: #1e293b;
        }

        .dark-theme .quick-progress-line { color: #93c5fd; }
        .dark-theme .quick-live-log {
          background-color: #0f172a;
          border-color: #475569;
          color: #f1f5f9;
        }

        /* Modern Input Fields */
        .form-control, .selectize-input, input[type=text], select {
          border-radius: 6px !important;
          border-width: 1px !important;
          padding: 8px 12px !important;
          font-size: 14px !important;
          transition: all 0.2s ease !important;
        }

        .light-theme .form-control, .light-theme .selectize-input, .light-theme input[type=text], .light-theme select {
          background-color: #ffffff !important;
          border-color: #cbd5e1 !important;
          color: #1e293b !important;
        }

        .light-theme .form-control:focus, .light-theme .selectize-input.focus, .light-theme input[type=text]:focus {
          border-color: #3b82f6 !important;
          box-shadow: 0 0 0 3px rgba(59, 130, 246, 0.1) !important;
        }

        .dark-theme .form-control, .dark-theme .selectize-input, .dark-theme input[type=text], .dark-theme select {
          background-color: #1e293b !important;
          border-color: #475569 !important;
          color: #f1f5f9 !important;
        }

        .dark-theme .form-control:focus, .dark-theme .selectize-input.focus, .dark-theme input[type=text]:focus {
          border-color: #60a5fa !important;
          box-shadow: 0 0 0 3px rgba(96, 165, 250, 0.2) !important;
        }

        /* Stable DataTable column styling */
        .dt-column-stable {
          white-space: nowrap !important;
          text-overflow: ellipsis !important;
          overflow: hidden !important;
        }

        /* Comprehensive empty row removal */
        table.dataTable tbody tr:empty,
        table.dataTable tbody tr td:empty:only-child {
          display: none !important;
          height: 0 !important;
          visibility: hidden !important;
        }

        /* Preserve ALL functional headers and filters */
        table.dataTable thead tr {
          display: table-row !important;
        }

        /* Only hide completely empty rows in table body */
        table.dataTable tbody tr:empty {
          display: none !important;
          height: 0 !important;
        }

        /* Hide rows with only empty cells in table body */
        table.dataTable tbody tr:has(td:empty:only-child):not(:has(td:not(:empty))) {
          display: none !important;
        }

        /* Specifically target phantom empty rows that scroll with content */
        table.dataTable tbody tr:first-child:empty,
        table.dataTable tbody tr:first-child:has(td:empty:only-child) {
          display: none !important;
          height: 0 !important;
        }

        /* Modern Interactive Elements */
        .btn, button, .shiny-input-container .btn {
          border-radius: 6px !important;
          font-weight: 500 !important;
          transition: all 0.2s ease !important;
          border: none !important;
          padding: 8px 16px !important;
          font-size: 14px !important;
        }

        .light-theme .btn, .light-theme button {
          background: linear-gradient(135deg, #60a5fa 0%, #3b82f6 100%) !important;
          color: #0f172a !important;
          box-shadow: 0 2px 4px rgba(96, 165, 250, 0.3) !important;
        }

        .light-theme .btn:hover, .light-theme button:hover {
          background: linear-gradient(135deg, #34d399 0%, #10b981 100%) !important;
          transform: translateY(-1px) !important;
          box-shadow: 0 4px 8px rgba(52, 211, 153, 0.4) !important;
        }

        .dark-theme .btn, .dark-theme button {
          background: linear-gradient(135deg, #60a5fa 0%, #3b82f6 100%) !important;
          color: #ffffff !important;
          box-shadow: 0 2px 4px rgba(96, 165, 250, 0.3) !important;
        }

        .dark-theme .btn:hover, .dark-theme button:hover {
          background: linear-gradient(135deg, #34d399 0%, #10b981 100%) !important;
          color: #ffffff !important;
          transform: translateY(-1px) !important;
          box-shadow: 0 4px 8px rgba(52, 211, 153, 0.4) !important;
        }

        /* Table header styling with center alignment and modern colors
           仅命中 DataTables scrollY 的固定表头（.dataTables_scrollHeadInner 内）；
           scrollBody 内的克隆 thead 不再被染色，DataTables 的 inline 0 高样式得以生效，
           从而消除过滤行与数据行之间的淡蓝/深灰空行 */
        .dataTables_scrollHeadInner table.dataTable thead th {
          white-space: nowrap !important;
          text-overflow: ellipsis !important;
          overflow: hidden !important;
          border-right: 1px solid !important;
          text-align: center !important;
          font-weight: 600 !important;
          padding: 12px 8px !important;
        }

        .light-theme .dataTables_scrollHeadInner table.dataTable thead th {
          background: #dbeafe !important;
          color: #1e40af !important;
          border-color: #93c5fd !important;
        }

        .dark-theme .dataTables_scrollHeadInner table.dataTable thead th {
          background: #1e3a8a !important;
          color: #ffffff !important;
          border-color: #3b82f6 !important;
        }

        .dark-theme .dataTables_scrollHeadInner table.dataTable thead th {
          border-right-color: #4a5568;
        }

        /* Ensure table cells don't break */
        table.dataTable tbody td {
          white-space: nowrap !important;
          text-overflow: ellipsis !important;
          overflow: hidden !important;
        }

        /* Modern Typography */
        h1, h2, h3, h4, h5, h6 {
          font-weight: 600 !important;
          margin-bottom: 16px !important;
        }

        /* Icon visibility fix - prevent gradient inheritance */
        h1 .emoji, h2 .emoji, h3 .emoji, h4 .emoji, h5 .emoji, h6 .emoji,
        h1::before, h2::before, h3::before, h4::before, h5::before, h6::before {
          -webkit-text-fill-color: initial !important;
          background: none !important;
          color: inherit !important;
        }

        .light-theme h1, .light-theme h2, .light-theme h3, .light-theme h4, .light-theme h5, .light-theme h6 {
          color: #1e293b !important;
        }

        .dark-theme h1, .dark-theme h2, .dark-theme h3, .dark-theme h4, .dark-theme h5, .dark-theme h6 {
          color: #f1f5f9 !important;
        }

        /* Text-based gradient that distributes across actual text content */
        #main_title, #browser_title {
          background: linear-gradient(90deg, #3762e3 0%, #3762e3 20%, #b33791 80%, #b33791 100%) !important;
          -webkit-background-clip: text !important;
          -webkit-text-fill-color: transparent !important;
          background-clip: text !important;
          background-size: 100% 100% !important;
          background-repeat: no-repeat !important;
          font-weight: 800 !important;
          margin-bottom: 25px !important;
          margin-left: 0 !important;
          padding-left: 0 !important;
          display: inline-block !important;
          width: fit-content !important;
        }

        #main_title {
          font-size: 3.5rem !important;
          text-align: left !important;
        }

        #browser_title {
          font-size: 2rem !important;
          text-align: center !important;
          width: 100% !important;
        }

        /* Ensure gradients work in both themes with enhanced color distribution */
        .light-theme #main_title, .dark-theme #main_title,
        .light-theme #browser_title, .dark-theme #browser_title {
          background: linear-gradient(90deg, #3762e3 0%, #3762e3 20%, #b33791 80%, #b33791 100%) !important;
          -webkit-background-clip: text !important;
          -webkit-text-fill-color: transparent !important;
          background-clip: text !important;
          background-size: 100% 100% !important;
          background-repeat: no-repeat !important;
        }

        /* Row index column styling with consistent header font size */
        .dt-row-index {
          font-weight: bold !important;
          text-align: center !important;
          width: 40px !important;
          min-width: 40px !important;
          max-width: 40px !important;
          padding: 2px !important;
        }

        /* 克隆 thead（DataTables 在 scrollBody 内做列宽测量用）padding/背景归零兜底：
           防止 .dt-row-index 等规则的 !important padding 撑高克隆行，
           在过滤行与数据之间显出细缝。真实表头在 .dataTables_scrollHeadInner 内，不受影响 */
        .dataTables_scrollBody table.dataTable thead th {
          padding: 0 !important;
          background: transparent !important;
        }

        /* Ensure ID column header has same font size as other headers */
        table.dataTable thead th.dt-row-index {
          font-size: inherit !important;
          font-weight: 600 !important;
        }

        /* Table cell tooltips */
        table.dataTable tbody td {
          position: relative;
          cursor: pointer;
        }

        table.dataTable tbody td:hover::after {
          content: attr(title);
          position: absolute;
          bottom: 100%;
          left: 50%;
          transform: translateX(-50%);
          background-color: #333;
          color: white;
          padding: 5px 10px;
          border-radius: 4px;
          white-space: nowrap;
          z-index: 1000;
          font-size: 12px;
          max-width: 300px;
          word-wrap: break-word;
          white-space: normal;
        }

        /* 统一按钮、输入框和record counter样式 */
        .filter-controls .btn, .filter-controls .form-control, .record-counter {
          height: 34px;
          padding: 6px 12px;
          font-size: 14px;
          border-radius: 4px;
          border: 1px solid;
          margin: 0 5px;
        }

        .light-theme .filter-controls .btn,
        .light-theme .filter-controls .form-control,
        .light-theme .record-counter {
          background-color: #ffffff;
          border-color: #ced4da;
          color: #495057;
        }

        .dark-theme .filter-controls .btn,
        .dark-theme .filter-controls .form-control,
        .dark-theme .record-counter {
          background-color: #4a5568;
          border-color: #718096;
          color: #ffffff;
        }

        .dark-theme .filter-controls .btn:hover {
          background-color: #718096;
          border-color: #a0aec0;
        }

        /* 修复分页控件样式 - 两种模式下一致 */
        .dataTables_wrapper .dataTables_paginate .paginate_button {
          padding: 6px 12px !important;
          margin: 0 2px !important;
          border-radius: 4px !important;
          border: 1px solid !important;
        }

        .light-theme .dataTables_wrapper .dataTables_paginate .paginate_button {
          background-color: #ffffff !important;
          border-color: #ced4da !important;
          color: #495057 !important;
        }

        .light-theme .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
          background-color: #e9ecef !important;
          border-color: #adb5bd !important;
          color: #495057 !important;
        }

        .light-theme .dataTables_wrapper .dataTables_paginate .paginate_button.current {
          background-color: #007bff !important;
          border-color: #007bff !important;
          color: #ffffff !important;
        }

        .dark-theme .dataTables_wrapper .dataTables_paginate .paginate_button {
          background-color: #4a5568 !important;
          border-color: #718096 !important;
          color: #ffffff !important;
        }

        .dark-theme .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
          background-color: #718096 !important;
          border-color: #a0aec0 !important;
          color: #ffffff !important;
        }

        .dark-theme .dataTables_wrapper .dataTables_paginate .paginate_button.current {
          background-color: #3182ce !important;
          border-color: #3182ce !important;
          color: #ffffff !important;
        }

        /* Fix dark mode pagination info text */
        .dark-theme .dataTables_wrapper .dataTables_info {
          color: #e9ecef !important;
        }

        .dark-theme .dataTables_wrapper .dataTables_length label,
        .dark-theme .dataTables_wrapper .dataTables_filter label {
          color: #e9ecef !important;
        }

        .dark-theme .dataTables_wrapper .dataTables_length select,
        .dark-theme .dataTables_wrapper .dataTables_filter input {
          background-color: #4a5568 !important;
          border-color: #718096 !important;
          color: #ffffff !important;
        }

        /* Responsive layout for different screen sizes */
        @media (max-width: 1200px) {
          .container-fluid {
            padding: 10px;
          }

          .sidebar-panel, .main-panel {
            padding: 10px;
          }

          #structure_display {
            height: 300px;
          }
        }

        @media (max-width: 768px) {
          .container-fluid {
            padding: 5px;
          }

          .filter-controls {
            flex-direction: column;
            gap: 10px;
          }

          .filter-controls .btn,
          .filter-controls .form-control {
            width: 100%;
            margin: 0;
          }
        }

        /* Ensure full viewport usage */
        html, body {
          min-height: 100vh;
          margin: 0;
          padding: 0;
          overflow-x: hidden;
        }

        .container-fluid {
          min-height: 100vh;
          display: flex;
          flex-direction: column;
        }

        /* DataTables scroll container: inner scrollbar for table only */
        .dataTables_scroll {
          overflow: hidden !important;
        }
        .dataTables_scrollBody {
          overflow-x: auto !important;
          overflow-y: auto !important;
        }

        .dark-theme .form-control {
          background-color: #4a5568;
          border-color: #718096;
          color: #ffffff;
        }

        .dark-theme .btn-default {
          background-color: #4a5568;
          border-color: #718096;
          color: #ffffff;
        }

        .dark-theme .btn-default:hover {
          background-color: #718096;
          border-color: #a0aec0;
        }

        .dark-theme .inchikey-filter-btn {
          background-color: #4a5568;
          border-color: #718096;
          color: #ffffff;
        }

        .dark-theme .inchikey-filter-btn:hover {
          background-color: #718096;
        }

        .dark-theme .inchikey-filter-btn.active {
          background-color: #3182ce;
          border-color: #3182ce;
        }

        /* 化学结构控制按钮 */
        .dark-theme .zoom-controls .btn {
          background-color: #4a5568 !important;
          border-color: #718096 !important;
          color: #ffffff !important;
          font-weight: bold !important;
        }

        .dark-theme .zoom-controls .btn:hover {
          background-color: #718096 !important;
          border-color: #a0aec0 !important;
          color: #ffffff !important;
        }





        /* Dark Theme DataTables Styling - Critical Fix */
        .dark-theme .dataTables_wrapper .dataTables_scrollHeadInner table.dataTable thead th {
          background-color: #2d3748 !important;
          color: #ffffff !important;
          border-bottom: 2px solid #4a5568 !important;
          font-weight: 600 !important;
        }

        .dark-theme .dataTables_wrapper table.dataTable tbody td {
          background-color: #1a202c !important;
          color: #f7fafc !important;
          border-bottom: 1px solid #4a5568 !important;
        }

        .dark-theme .dataTables_wrapper table.dataTable tbody tr:hover td {
          background-color: #2d3748 !important;
          color: #ffffff !important;
        }

        .dark-theme .dataTables_wrapper table.dataTable tbody tr.selected td {
          background-color: #3182ce !important;
          color: #ffffff !important;
        }

        .dark-theme .dataTables_filter input {
          background-color: #4a5568 !important;
          color: #f7fafc !important;
          border: 1px solid #718096 !important;
        }

        /* Dark Theme DataTables Styling - Critical Fix */
        .dark-theme .dataTables_wrapper .dataTables_scrollHeadInner table.dataTable thead th {
          background-color: #2d3748 !important;
          color: #ffffff !important;
          border-bottom: 2px solid #4a5568 !important;
          font-weight: 600 !important;
        }

        .dark-theme .dataTables_wrapper table.dataTable tbody td {
          background-color: #1a202c !important;
          color: #f7fafc !important;
          border-bottom: 1px solid #4a5568 !important;
        }

        .dark-theme .dataTables_wrapper table.dataTable tbody tr:hover td {
          background-color: #2d3748 !important;
          color: #ffffff !important;
        }

        .dark-theme .dataTables_wrapper table.dataTable tbody tr.selected td {
          background-color: #3182ce !important;
          color: #ffffff !important;
        }

        .dark-theme .dataTables_filter input {
          background-color: #4a5568 !important;
          color: #f7fafc !important;
          border: 1px solid #718096 !important;
        }

        .dark-theme .row {
          margin: 0;
        }

        .dark-theme .col-sm-12 {
          padding: 15px;
        }

        /* Smooth transitions */
        * {
          transition: background-color 0.3s ease, color 0.3s ease, border-color 0.3s ease;
        }



        /* Filter controls */
        .filter-controls {
          display: flex;
          gap: 15px;
          align-items: stretch;
          margin-bottom: 20px;
          flex-wrap: nowrap;
        }

        .filter-controls .inchikey-filter-btn {
          height: 34px;
          white-space: nowrap;
          font-size: 13px;
          flex-shrink: 0;
          display: flex;
          align-items: center;
          padding: 6px 12px;
          border-radius: 4px;
          border: 1px solid #ced4da;
          background-color: #f8f9fa;
          color: #495057;
          transition: all 0.2s ease;
        }

        .filter-controls .inchikey-filter-btn:hover {
          background-color: #e9ecef;
          border-color: #adb5bd;
        }

        .filter-controls .inchikey-filter-btn.active {
          background-color: #007bff;
          border-color: #007bff;
          color: white;
        }

        .filter-controls .search-input-wrapper {
          flex: 1;
          min-width: 200px;
        }

        .filter-controls .form-control {
          height: 34px;
          font-size: 13px;
        }

        .record-counter {
          flex-shrink: 0;
          min-width: 180px;
          display: flex;
          align-items: center;
          font-size: 13px;
          font-weight: 500;
          padding: 6px 12px;
          background-color: rgba(0,123,255,0.1);
          border-radius: 4px;
          border: 1px solid rgba(0,123,255,0.2);
        }
      ")),

      # ---- UI 之 JS：DataTables 告警抑制 + 主题切换 + 结构式交互 ----
      # JavaScript for theme switching and functionality
      tags$script(HTML("
        // Suppress DataTables synchronous alert() popups (e.g. 'Non-table
        // node initialisation') and route its errors to the console only.
        // DT 0.34's embedded DataTables calls window.alert() for some
        // warnings, which blocks the rest of the page JS (including the
        // DT init itself) until the user clicks OK, leaving the table stuck
        // in its uninitialised div state.
        window.alert = function() {};
        if (window.jQuery && jQuery.fn && jQuery.fn.dataTable) {
          jQuery.fn.dataTable.ext.errMode = 'none';
        }

        // Initialize theme
        $(document).ready(function() {
          $('body').addClass('light-theme');
        });

        // Handle theme updates from server
        Shiny.addCustomMessageHandler('updateTheme', function(data) {
          $('body').removeClass('light-theme dark-theme');
          $('body').addClass(data.theme + '-theme');
        });

        // Handle record count updates
        Shiny.addCustomMessageHandler('updateRecordCount', function(data) {
          $('#record-counter-text').text(data.text);
        });

        // Handle clearing DataTable filters
        Shiny.addCustomMessageHandler('clearDataTableFilters', function(data) {
          var table = fcmsGetDT();
          if (table) {
            // 清除所有列的搜索
            table.columns().search('').draw();
          }
        });

        // Handle addClass and removeClass
        Shiny.addCustomMessageHandler('addClass', function(data) {
          $('#' + data.id).addClass(data.class);
        });

        Shiny.addCustomMessageHandler('removeClass', function(data) {
          $('#' + data.id).removeClass(data.class);
        });

        // Safely obtain the live DataTable instance for the main table.
        // NEVER call $('#data_table').DataTable() on the widget container:
        // #data_table is the <div> that DT::dataTableOutput renders, not a
        // <table>. DataTables 1.13 refuses to initialise on a div and fires
        // a synchronous alert ('Non-table node initialisation'), then bails
        // out, so the call returns nothing useful. DT 0.34 stores the real
        // instance on the container element via $el.data('datatable', table)
        // (see DT's datatables.js), which is the sanctioned way to reach it.
        // Fallback: locate the inner <table> and ask DataTables for its
        // instance (safe -- that element IS a table).
        function fcmsGetDT() {
          var container = document.getElementById('data_table');
          if (!container) return null;
          var inst = jQuery(container).data('datatable');
          if (inst) return inst;
          var tbl = jQuery(container).find('table.dataTable').first();
          if (tbl.length) {
            try { return tbl.DataTable(); } catch (e) { return null; }
          }
          return null;
        }

        // Table interaction without DataTables API calls
        var currentSelectedRow = 0;

        // Click selection
        $(document).on('click', '#data_table tbody tr', function() {
          $('#data_table tbody tr').removeClass('selected');
          $(this).addClass('selected');
          currentSelectedRow = $(this).index();

          var rowIndex = currentSelectedRow + 1;
          Shiny.setInputValue('data_table_rows_selected', rowIndex, {priority: 'event'});

          // Structure display: extract the SMILES of the clicked row and send
          // it to R as selected_compound_info. This lives on the top-level
          // click handler (not inside a $(document).ready block) so it is
          // registered immediately and always fires on a row click. Two ways
          // to read the SMILES: the DataTables API row data, then a DOM cell
          // read as a fallback.
          try {
            var smilesVal = '';
            var table = fcmsGetDT();
            if (table) {
              var smilesCol = -1;
              var hdrs = table.columns().header();
              for (var i = 0; i < hdrs.length; i++) {
                if ($(hdrs[i]).text().toLowerCase().indexOf('smiles') >= 0) { smilesCol = i; break; }
              }
              if (smilesCol >= 0) {
                var rowData = table.row(this).data();
                if (rowData && rowData.length) {
                  smilesVal = rowData[smilesCol] || '';
                }
                if (!smilesVal) {
                  var cells = $(this).find('td');
                  if (cells.length > smilesCol) smilesVal = cells.eq(smilesCol).text().trim();
                }
              }
            }
            Shiny.setInputValue('selected_compound_info', {
              smiles: smilesVal,
              has_structure: smilesVal !== ''
            }, {priority: 'event'});
          } catch (err) {
            // Silently handle DataTable not ready
          }
        });

        // Keyboard navigation without DataTables API
        $(document).on('keydown', function(e) {
          // Only handle arrow keys when table is visible and has rows
          var tableRows = $('#data_table tbody tr:visible');
          if (tableRows.length === 0) return;

          if (e.which === 38 || e.which === 40) { // Up or Down arrow
            e.preventDefault();

            var totalRows = tableRows.length;

            if (e.which === 38) { // Up arrow
              currentSelectedRow = Math.max(0, currentSelectedRow - 1);
            } else { // Down arrow
              currentSelectedRow = Math.min(totalRows - 1, currentSelectedRow + 1);
            }

            // Update visual selection
            tableRows.removeClass('selected');
            var selectedRow = tableRows.eq(currentSelectedRow);
            selectedRow.addClass('selected');

            // Scroll to selected row
            if (selectedRow.length > 0) {
              selectedRow[0].scrollIntoView({
                behavior: 'smooth',
                block: 'nearest'
              });
            }

            // Update Shiny input
            Shiny.setInputValue('data_table_rows_selected', currentSelectedRow + 1, {priority: 'event'});
          }
        });

        // Reset selection when table content changes
        $(document).on('DOMSubtreeModified', '#data_table tbody', function() {
          currentSelectedRow = 0;
        });


      "))
    ),

    # ---- UI：顶栏（主题 / 语言切换按钮 + 标题） ----
    # Theme and language controls
    div(class = "theme-controls",
      actionButton("theme_toggle", "🌙", class = "theme-btn",
                  title = "Toggle Dark/Light Theme"),
      actionButton("lang_toggle", "中/EN", class = "theme-btn",
                  title = "Toggle Language")
    ),

    titlePanel(textOutput("main_title")),

    # ============================================================
    # 一键操作面板（2026-09-04 新增）
    # 把命令行里一问一答的功能做成网页按钮：
    #   看状态 / 检查手动新清单 / 应用手动清单 / 联网更新全部
    # 只读操作直接执行；写库操作（应用、联网更新）先弹确认框。
    # 日志统一输出到 quick_update_log。
    # ============================================================
    div(class = "quick-update-panel",
      textOutput("quick_actions_title_out"),
      actionButton("db_btn_status", "📋 Status",
                   title = "Show status of all databases / 查看各数据库状态"),
      actionButton("db_btn_check", "🔍 Check new files",
                   title = "Detect manually added files in inst/ / 检查手动新清单"),
      actionButton("db_btn_apply", "📦 Apply manual list",
                   title = "Import manual list files into the database / 应用手动清单"),
      actionButton("db_btn_update", "🚀 Update all (online)",
                   title = "Download latest lists online and update databases / 联网更新全部数据库"),
      actionButton("db_btn_history", "📝 Update log",
                   title = "Show database update history / 显示数据库更新日志"),
      actionButton("db_btn_tables", "📑 Database list",
                   title = "Show all database tables / 显示所有数据库表"),
      actionButton("db_btn_screen", "🧪 Screen substances",
                   title = "Upload a substance list, run regulatory matching + toxicity grading, export report / 上传物质清单，跑法规匹配、毒性定级并导出报告"),
      div(style = "flex-basis: 100%;",
        # 运行状态行：任务进行中显示当前库与序号，"页面还活着"的第一眼信号
        div(id = "quick_progress_line", class = "quick-progress-line", "就绪"),
        # 实时日志：走 fcmLiveLog 自定义消息通道，而不是 renderXxx ——
        # 已实测这条通道在 R 阻塞期间照样送达浏览器，任务跑着就能看到滚动日志。
        # 任务结束时清空，完整正文由下面的 quick_update_log 给出，避免重复。
        # 注意用 tags$pre 而不是 pre()：shiny 没有 re-export htmltools 的 pre()
        shiny::tags$pre(id = "quick_live_log", class = "quick-live-log"),
        shiny::verbatimTextOutput("quick_update_log")
      )
    ),

    # 一键操作的页面侧配合：按钮禁用 + 实时日志显示（纯前端）
    # 服务端只发 fcmSetBusy / fcmLiveLog 两种消息，见 server 里的 set_quick_busy()/push_log()
    tags$script(HTML("
      (function() {
        var QUICK_BTN_IDS = ['db_btn_status', 'db_btn_check', 'db_btn_apply',
                             'db_btn_update', 'db_btn_history', 'db_btn_tables',
                             'db_btn_screen'];

        function setBusy(busy, text) {
          QUICK_BTN_IDS.forEach(function(id) {
            var el = document.getElementById(id);
            if (!el) return;
            el.disabled = !!busy;
            el.style.opacity = busy ? '0.55' : '';
            el.style.cursor = busy ? 'not-allowed' : '';
          });
          var line = document.getElementById('quick_progress_line');
          if (line && text) line.textContent = text;
        }

        function register() {
          if (!(window.Shiny && Shiny.addCustomMessageHandler)) {
            setTimeout(register, 100);   // shiny.js 还没就绪，稍后再试
            return;
          }
          Shiny.addCustomMessageHandler('fcmSetBusy', function(x) {
            setBusy(x.busy, x.text || '');
          });
          Shiny.addCustomMessageHandler('fcmLiveLog', function(x) {
            var box = document.getElementById('quick_live_log');
            if (!box) return;
            if (x.done) {
              box.textContent = '';
              box.style.display = 'none';
              return;
            }
            box.textContent = x.text || '';
            box.style.display = (box.textContent.trim() === '') ? 'none' : 'block';
            box.scrollTop = box.scrollHeight;
          });
        }
        register();
      })();
    ")),

    sidebarLayout(
      sidebarPanel(
        width = 3,
        class = "sidebar-panel",

        h4(textOutput("database_title"), style = "text-align: left;"),
        selectInput("database", NULL,
                   choices = c("Loading..." = ""),
                   selected = ""),
        actionButton("btn_view_db_list", "\U0001F4D1 Database list",
                    class = "btn btn-sm",
                    style = "margin-top: -5px; margin-bottom: 10px; width: 100%; font-size: 13px;"),

        # Database update information panel
        div(class = "database-info-panel",
          style = "margin-top: 15px;",
          h5(textOutput("database_info_title"), style = "margin-bottom: 10px; font-size: 14px;"),
          div(id = "database_info_content",
            htmlOutput("database_update_info")
          )
        ),

        br(),

        h4(textOutput("structure_title")),
        div(id = "structure_panel", class = "structure-panel",
          style = "padding: 10px; border-radius: 8px;",

          div(id = "structure_controls", style = "margin-bottom: 10px;",
            actionButton("zoom_in", "🔍+", class = "btn btn-sm btn-default",
                        style = "margin-right: 5px;", title = "Zoom In / 放大"),
            actionButton("zoom_out", "🔍-", class = "btn btn-sm btn-default",
                        style = "margin-right: 5px;", title = "Zoom Out / 缩小"),
            actionButton("reset_view", textOutput("reset_button_text", inline = TRUE),
                        class = "btn btn-sm btn-default",
                        title = "Reset View / 重置视图")
          ),

          div(id = "structure_display",
            style = "min-height: 300px; text-align: center; display: flex; align-items: center; justify-content: center;",
            p(textOutput("structure_placeholder"), style = "color: #666; font-style: italic;")
          ),


        )
      ),

      mainPanel(
        width = 9,
        class = "main-panel",

        # Data Browser Content with enhanced controls
        fluidRow(
          column(12,
            h3(id = "browser_title", textOutput("browser_title"), style = "text-align: center; margin-bottom: 20px;"),

            # Filter controls and record counter
            div(class = "filter-controls",
              actionButton("inchikey_filter", "InChIKey Only",
                          class = "btn"),
              div(style = "width: 130px; flex-shrink: 0;",
                selectInput("search_column", NULL,
                            choices = c("全部" = "all", "物质名" = "name", "CAS" = "cas",
                                        "InChIKey" = "inchikey", "EC号" = "ec", "备注" = "notes"),
                            selected = "all", width = "100%")
              ),
              div(class = "search-input-wrapper", style = "flex: 1; margin: 0 8px;",
                textInput("search_term", NULL, placeholder = "搜索...")
              ),
              actionButton("clear_search", "\u2715",
                          class = "btn btn-sm btn-default",
                          style = "height: 34px; align-self: center; flex-shrink: 0;"),
              div(class = "record-counter", textOutput("record_counter"))
            ),

            # Enhanced DataTable container with reduced spacing
            div(style = "position: relative; margin-top: 5px;",
              DT::dataTableOutput("data_table")
            )
          )
        ),

        # Enhanced JavaScript for modern UI, DataTable fixes, and functionality
        tags$script(HTML("
          $(document).ready(function() {
            // Initialize theme
            var currentTheme = 'light';
            var currentLang = 'zh';

            // Theme toggle functionality
            $('#theme_toggle').click(function() {
              currentTheme = currentTheme === 'light' ? 'dark' : 'light';
              $('body').removeClass('light-theme dark-theme').addClass(currentTheme + '-theme');
              $(this).text(currentTheme === 'light' ? '🌙' : '☀️');
              Shiny.setInputValue('current_theme', currentTheme, {priority: 'event'});
            });

            // Language toggle functionality
            $('#lang_toggle').click(function() {
              currentLang = currentLang === 'en' ? 'zh' : 'en';
              Shiny.setInputValue('current_lang', currentLang, {priority: 'event'});
            });

            // Initialize with light theme
            $('body').addClass('light-theme');

            // Custom message handler for structure updates
            Shiny.addCustomMessageHandler('updateStructure', function(data) {
              $('#structure_display').html(data.content);
            });

            // Custom message handler for record count updates
            Shiny.addCustomMessageHandler('updateRecordCount', function(data) {
              $('#record_counter').text(data.text);
            });

            // Suppress DataTables alert popups (e.g. 'Non-table node
            // initialisation'). DT 0.34's embedded DataTables still calls
            // window.alert() for some warnings; the errMode='none' switch is
            // set in the main datatable()'s callback (where $.fn.dataTable
            // is guaranteed to exist), not here -- accessing
            // $.fn.dataTable.ext from inside a $(document).ready handler
            // throws `Cannot read properties of undefined (reading 'ext')`
            // because DataTables is still being lazy-loaded. See
            // rstudio/DT#815. We still gate the assignment below with a
            // truthy check so it stays safe in any later ready tick.
            if (window.jQuery && jQuery.fn && jQuery.fn.dataTable && jQuery.fn.dataTable.ext) {
              jQuery.fn.dataTable.ext.errMode = 'none';
            }

            // Enhanced keyboard navigation with proper DataTable handling
            $(document).on('keydown', function(e) {
              // Only handle if focus is not in an input field
              if (!$(e.target).is('input, textarea, select')) {
                try {
                  var table = fcmsGetDT();
                  if (table && table.rows({page: 'current'}).count() > 0) {
                    var selectedRows = table.rows('.selected', {page: 'current'}).indexes();
                    var currentRow = selectedRows.length > 0 ? selectedRows[0] : -1;
                    var visibleRows = table.rows({page: 'current'}).indexes();

                    if (e.which == 38) { // Up arrow
                      e.preventDefault();
                      var newRow = -1;
                      if (currentRow > 0) {
                        for (var i = visibleRows.length - 1; i >= 0; i--) {
                          if (visibleRows[i] < currentRow) {
                            newRow = visibleRows[i];
                            break;
                          }
                        }
                      } else if (currentRow == -1 && visibleRows.length > 0) {
                        newRow = visibleRows[visibleRows.length - 1];
                      }

                      if (newRow >= 0) {
                        table.rows().deselect();
                        table.row(newRow).select();
                        updateStructure(newRow);
                      }
                    } else if (e.which == 40) { // Down arrow
                      e.preventDefault();
                      var newRow = -1;
                      if (currentRow >= 0) {
                        for (var i = 0; i < visibleRows.length; i++) {
                          if (visibleRows[i] > currentRow) {
                            newRow = visibleRows[i];
                            break;
                          }
                        }
                      } else if (currentRow == -1 && visibleRows.length > 0) {
                        newRow = visibleRows[0];
                      }

                      if (newRow >= 0) {
                        table.rows().deselect();
                        table.row(newRow).select();
                        updateStructure(newRow);
                      }
                    }
                  }
                } catch (e) {
                  // Silently handle DataTable not ready
                }
              }
            });

            // Update chemical structure function
            function updateStructure(rowIndex) {
              try {
                var table = fcmsGetDT();
                var rowData = table.row(rowIndex).data();

                if (rowData) {
                  // Find SMILES column
                  var smilesIndex = -1;
                  var headers = table.columns().header();
                  for (var i = 0; i < headers.length; i++) {
                    var headerText = $(headers[i]).text().toLowerCase();
                    if (headerText.includes('smiles')) {
                      smilesIndex = i;
                      break;
                    }
                  }

                  // Always emit a combined signal so R can tell apart
                  // no-row-selected from a-row-without-structure-selected.
                  var smilesVal = (smilesIndex >= 0) ? (rowData[smilesIndex] || '') : '';
                  Shiny.setInputValue('selected_compound_info', {
                    smiles: smilesVal,
                    has_structure: smilesVal !== ''
                  }, {priority: 'event'});
                }
              } catch (e) {
                // Silently handle errors
              }
            }

            // Row click -> structure. Two redundant triggers so the SMILES
            // extraction fires reliably no matter how DataTables surfaces the
            // selection:
            //   1. a delegated 'click' on the row (covers the normal user click;
            //      clicks on a cell bubble up to the <tr>, so this always fires
            //      even after sort/filter/page redraws), and
            //   2. DataTables' own 'select.dt' event (covers selection made via
            //      the select extension or programmatically).
            // Both call updateStructure(rowIndex), which reads the SMILES straight
            // from the displayed row data and pushes it to R as
            // selected_compound_info. Firing twice is harmless (idempotent).
            $(document).on('click', '#data_table tbody tr', function() {
              try {
                var table = fcmsGetDT();
                var rowIndex = table.row(this).index();
                if (rowIndex !== undefined && rowIndex >= 0) {
                  updateStructure(rowIndex);
                }
              } catch (err) {
                // Silently handle DataTable not ready
              }
            });

            $(document).on('select.dt', '#data_table', function(e, dtObj, selType, indexes) {
              try {
                if (selType === 'row' && indexes && indexes.length) {
                  updateStructure(indexes[0]);
                }
              } catch (err) {
                // Silently handle errors
              }
            });

            // Handle search input changes for real-time record counting
            $(document).on('input', '#search_term', function() {
              setTimeout(function() {
                try {
                  var table = fcmsGetDT();
                  if (table) {
                    var info = table.page.info();
                    Shiny.setInputValue('filtered_count', info.recordsDisplay, {priority: 'event'});
                  }
                } catch (e) {
                  // Silently handle errors
                }
              }, 100);
            });
          });
        "))
      )
    )
  )

  # ---- Server 定义 -----------------------------------------------------------
  server <- function(input, output, session) {

    # Custom plot_molecule function using rcdk
    plot_molecule <- function(molecule, name = NULL, sma = NULL, ...) {
      # check if rcdk is installed
      if (!requireNamespace("rcdk", quietly = TRUE)) {
        stop("rcdk is not installed. Please install it first.", call. = FALSE)
      }

      # Image aesthetics
      dep <- rcdk::get.depictor(
        width = 1000,
        height = 1000,
        zoom = 7,
        sma = sma,
        abbr = "off",
        ...
      )
      molecule_sdf <- rcdk::view.image.2d(molecule[[1]], depictor = dep)

      ## Remove extra margins around the molecule
      par(mar = c(0, 0, 0, 0.5))
      plot(NA,
        xlim = c(0, 1),
        ylim = c(0, 1),
        # Remove the black bounding boxes around the molecule
        axes = FALSE,
        type = "n"
      )
      rasterImage(molecule_sdf, 0, 0, 1, 1)
    }

    # ---- 响应式状态（values 是全局唯一的状态容器） ----
    # Reactive values
    values <- reactiveValues(
      db_connection = NULL,
      available_databases = c(),
      current_data = NULL,
      filtered_data = NULL,
      selected_smiles = NULL,
      selected_compound = NULL,
      no_structure_selected = FALSE,  # 当前选中的行是否无结构式（用于区分占位提示与"无单一结构"提示）
      zoom_level = 0.5,
      current_theme = "light",
      current_lang = "zh",
      inchikey_filter_active = FALSE,
      search_term = "",
      search_column = "all",
      quick_busy = FALSE,       # 一键操作是否正在运行（防重复点击）
      quick_lines = character(0), # 本轮任务的日志缓冲（同时实时推给页面）
      last_run_end = NULL,      # 上一轮结束时刻：积压点击守卫用（见 update_run_guard.R）
      report_summary = NULL,    # 最近一次预演/写入的按库汇总表
      report_phase = "dry",     # 报告是预演还是写入结果
      screen_result = NULL      # 最近一次筛查（run_screening）的结果表
    )

    # ---- 主题 / 语言切换 handler ----
    # Theme toggle event handler
    observeEvent(input$theme_toggle, {
      values$current_theme <- if (values$current_theme == "light") "dark" else "light"

      # Update body class for theme switching
      session$sendCustomMessage("updateTheme", list(theme = values$current_theme))
    })

    # Language toggle event handler
    observeEvent(input$lang_toggle, {
      values$current_lang <- if (values$current_lang == "en") "zh" else "en"
    })

    # Legacy handlers for backward compatibility
    observeEvent(input$current_theme, {
      values$current_theme <- input$current_theme
    })

    observeEvent(input$current_lang, {
      values$current_lang <- input$current_lang
    })

    # ---- i18n：文案表 + 界面文案输出 ----
    #
    # texts[[key]][[lang]] 查表，不硬编码。每个 output$ 里都显式写入
    # values$current_lang 来建立响应式依赖 —— 不写就不会随语言切换刷新。
    # 新增文案要同时加到 texts 表和 get_text() 的调用处。
    # Comprehensive language system
    get_text <- function(key) {
      texts <- list(
        # Main Title
        "main_title" = list(en = "🔬 FCMSafety Database", zh = "🔬 FCMSafety 数据库"),

        # UI Elements
        "database_title" = list(en = "📊 Database Selection", zh = "📊 数据库选择"),
        "structure_title" = list(en = "🧪 Chemical Structure", zh = "🧪 化学结构"),
        "structure_placeholder" = list(en = "Select a compound from the table to view its structure",
                                     zh = "从表格中选择化合物以查看其结构"),
        "no_structure_group" = list(en = "This entry is a group restriction (contains multiple substances) without a single chemical structure",
                                    zh = "该条目为组限制（含多个物质），无单一结构式"),
        "no_structure_generic" = list(en = "No chemical structure available for this substance",
                                      zh = "该物质暂无结构式数据"),
        "reset_button" = list(en = "Reset", zh = "重置"),
        "inchikey_filter_show_all" = list(en = "Show All", zh = "显示全部"),
        "inchikey_filter_only" = list(en = "InChIKey Only", zh = "仅显示InChIKey"),
        "search_placeholder" = list(en = "Search...", zh = "搜索..."),
        "search_column_all" = list(en = "All", zh = "全部"),
        "search_column_name" = list(en = "Name", zh = "物质名"),
        "search_column_cas" = list(en = "CAS", zh = "CAS"),
        "search_column_inchikey" = list(en = "InChIKey", zh = "InChIKey"),
        "search_column_ec" = list(en = "EC No.", zh = "EC号"),
        "search_column_notes" = list(en = "Notes", zh = "备注"),
        "zoom_in_tooltip" = list(en = "Zoom In", zh = "放大"),
        "zoom_out_tooltip" = list(en = "Zoom Out", zh = "缩小"),
        "reset_view_tooltip" = list(en = "Reset View", zh = "重置视图"),

        # Database info panel
        "database_info_title" = list(en = "📋 Database Information", zh = "📋 数据库信息"),
        "last_updated" = list(en = "Last Updated", zh = "最后更新"),
        "total_records" = list(en = "Total Records", zh = "总记录数"),
        "database_version" = list(en = "Version", zh = "版本"),
        "no_info_available" = list(en = "No information available", zh = "暂无信息"),

        # Browser titles
        "select_database" = list(en = "Select a database to browse data", zh = "选择数据库以浏览数据"),
        "database_label" = list(en = "Database:", zh = "数据库:"),

        # Record counter
        "showing_records" = list(en = "Showing %d of %d records", zh = "显示 %d 条，共 %d 条记录"),
        "loading" = list(en = "Loading...", zh = "加载中..."),

        # Compound details
        "no_details" = list(en = "No detailed information available for selected compound",
                           zh = "所选化合物无详细信息"),
        "select_compound" = list(en = "Select a compound from the table to view details",
                               zh = "从表格中选择化合物以查看详细信息"),

        # Error messages
        "table_not_found" = list(en = "Table %s not found", zh = "未找到表格 %s"),
        "failed_to_load" = list(en = "Failed to load data: %s", zh = "加载数据失败: %s"),
        "structure_error" = list(en = "Structure display error: %s", zh = "结构显示错误: %s"),
        "smiles_parse_error" = list(en = "Error parsing SMILES: %s", zh = "SMILES解析错误: %s"),
        "invalid_smiles" = list(en = "Invalid SMILES string", zh = "无效的SMILES字符串"),
        "install_rcdk" = list(en = "Install rcdk package for structure visualization",
                             zh = "请安装rcdk包以显示化学结构"),

        # Database status messages
        "db_not_found" = list(en = "Database file not found. Please run migration first.",
                             zh = "未找到数据库文件。请先运行数据迁移。"),
        "connection_failed" = list(en = "Failed to establish database connection",
                                 zh = "无法建立数据库连接"),
        "no_tables" = list(en = "No tables found in database", zh = "数据库中未找到表格"),
        "no_main_tables" = list(en = "No main database tables found", zh = "未找到主数据库表格"),
        "tables_loaded" = list(en = "Successfully loaded %d databases", zh = "成功加载 %d 个数据库"),
        "table_empty" = list(en = "Table %s is empty", zh = "表格 %s 为空"),
        "no_data_matches" = list(en = "No data matches current filters", zh = "没有数据匹配当前过滤条件"),

        # Theme and language
        "theme_toggle_tooltip" = list(en = "Toggle Dark/Light Theme", zh = "切换深色/浅色主题"),
        "lang_toggle_tooltip" = list(en = "Toggle Language", zh = "切换语言"),

        # Database names (for dropdown)
        "svhc_name" = list(en = "SVHC", zh = "高关注物质"),
        "cmr_name" = list(en = "CMR", zh = "致癌致突变生殖毒性"),
        "cmr_suspect_name" = list(en = "CMR SUSPECT", zh = "疑似CMR"),
        "iarc_name" = list(en = "IARC", zh = "国际癌症研究机构"),
        "eu_sml_name" = list(en = "EU SML", zh = "欧盟特定迁移限量"),
        "eu_sml_group_name" = list(en = "EU SML GROUP", zh = "欧盟SML组"),
        "edc_name" = list(en = "EDC", zh = "内分泌干扰物"),
        "china_sml_name" = list(en = "CHINA SML", zh = "中国特定迁移限量"),

        # Quick Actions panel
        "quick_actions_title" = list(en = "⚡ Quick Actions", zh = "⚡ 一键操作"),
        "quick_btn_status" = list(en = "📋 Status", zh = "📋 看状态"),
        "quick_btn_check" = list(en = "🔍 Check new files", zh = "🔍 检查手动新清单"),
        "quick_btn_apply" = list(en = "📦 Apply manual list", zh = "📦 应用手动清单"),
        "quick_btn_update" = list(en = "🚀 Update all (online)", zh = "🚀 更新全部（联网）"),
        "quick_btn_history" = list(en = "📝 Update log", zh = "📝 更新日志"),
        "quick_btn_tables" = list(en = "📑 Database list", zh = "📑 数据库列表")
      )

      if (key %in% names(texts)) {
        lang <- values$current_lang
        return(texts[[key]][[lang]])
      } else {
        return(key)  # Return key if not found
      }
    }

    # Language-dependent text outputs - 响应语言变化
    output$main_title <- renderText({
      values$current_lang  # 添加依赖
      get_text("main_title")
    })

    output$database_title <- renderText({
      values$current_lang  # 添加依赖
      get_text("database_title")
    })

    output$structure_title <- renderText({
      values$current_lang  # 添加依赖
      get_text("structure_title")
    })

    output$structure_placeholder <- renderText({
      values$current_lang  # 添加依赖
      get_text("structure_placeholder")
    })

    output$reset_button_text <- renderText({
      values$current_lang  # 添加依赖
      get_text("reset_button")
    })

    # Database info panel outputs
    output$database_info_title <- renderText({
      values$current_lang  # 添加依赖
      get_text("database_info_title")
    })

    output$database_update_info <- renderUI({
      values$current_lang  # 添加依赖

      if (is.null(input$database) || input$database == "" || is.null(values$db_connection)) {
        return(HTML(paste0("<em>", get_text("no_info_available"), "</em>")))
      }

      tryCatch({
        # Get database metadata
        table_name <- input$database

        # Get record count
        count_query <- sprintf("SELECT COUNT(*) as count FROM %s", table_name)
        record_count <- DBI::dbGetQuery(values$db_connection, count_query)$count

        # Use current date as last updated (since database was created today)
        current_date <- format(Sys.Date(), "%Y-%m-%d")

        # Create simple database info
        if (values$current_lang == "zh") {
          info_html <- sprintf(
            "<strong>%s:</strong> %s<br><strong>%s:</strong> %s<br><strong>新增记录:</strong> 0<br><strong>删除记录:</strong> 0",
            get_text("last_updated"), current_date,
            get_text("total_records"), format(record_count, big.mark = ",")
          )
        } else {
          info_html <- sprintf(
            "<strong>%s:</strong> %s<br><strong>%s:</strong> %s<br><strong>Records Added:</strong> 0<br><strong>Records Removed:</strong> 0",
            get_text("last_updated"), current_date,
            get_text("total_records"), format(record_count, big.mark = ",")
          )
        }

        HTML(info_html)

      }, error = function(e) {
        HTML(paste0("<em>", get_text("no_info_available"), "</em>"))
      })
    })



    # Update search placeholder and column selector when language changes
    observeEvent(values$current_lang, {
      updateTextInput(session, "search_term",
                     placeholder = get_text("search_placeholder"))
      shiny::updateSelectInput(session, "search_column",
                               choices = stats::setNames(
                                 c("all", "name", "cas", "inchikey", "ec", "notes"),
                                 c(get_text("search_column_all"), get_text("search_column_name"),
                                   get_text("search_column_cas"), get_text("search_column_inchikey"),
                                   get_text("search_column_ec"), get_text("search_column_notes"))
                               ),
                               selected = input$search_column)
    })

    # Quick Actions panel: title + button labels follow language
    output$quick_actions_title_out <- renderText({
      values$current_lang  # 添加依赖
      get_text("quick_actions_title")
    })

    observeEvent(values$current_lang, {
      shiny::updateActionButton(session, "db_btn_status", label = get_text("quick_btn_status"))
      shiny::updateActionButton(session, "db_btn_check",  label = get_text("quick_btn_check"))
      shiny::updateActionButton(session, "db_btn_apply",  label = get_text("quick_btn_apply"))
      shiny::updateActionButton(session, "db_btn_update", label = get_text("quick_btn_update"))
      shiny::updateActionButton(session, "db_btn_history", label = get_text("quick_btn_history"))
      shiny::updateActionButton(session, "db_btn_tables", label = get_text("quick_btn_tables"))
      shiny::updateActionButton(session, "btn_view_db_list", label = get_text("quick_btn_tables"))
    })

    # Initialize database connection and populate database choices
    observe({
      tryCatch({
        # Resolve database path with the same dual-mode logic as
        # get_db_connection(): development = inst/, installed = user data dir
        if (dir.exists(file.path(getwd(), "inst"))) {
          db_path <- file.path(getwd(), "inst", "fcmsafety.db")
        } else {
          db_path <- file.path(tools::R_user_dir("fcmsafety", which = "data"), "fcmsafety.db")
        }

        # Check if database file exists first
        if (!file.exists(db_path)) {
          showNotification(get_text("db_not_found"), type = "error")
          updateSelectInput(session, "database", choices = c("Database not found - run migration" = ""))
          return()
        }

        # Establish database connection.
        # Reuse an already-open connection: this observe re-runs when the
        # language switches (choices are rebuilt with localized names), so a
        # fresh connect every time would leak one connection per toggle.
        if (is.null(values$db_connection) || !DBI::dbIsValid(values$db_connection)) {
          values$db_connection <- DBI::dbConnect(RSQLite::SQLite(), db_path)
        }

        # Verify connection is working
        if (is.null(values$db_connection)) {
          showNotification(get_text("connection_failed"), type = "error")
          updateSelectInput(session, "database", choices = c("Connection failed" = ""))
          return()
        }

        # Get available databases
        tables <- DBI::dbListTables(values$db_connection)

        if (length(tables) == 0) {
          showNotification(get_text("no_tables"), type = "error")
          updateSelectInput(session, "database", choices = c("No tables found" = ""))
          return()
        }

        # Filter for main database tables (not raw or metadata tables)
        main_tables <- tables[!grepl("_raw$|metadata|history|change_log|sqlite_sequence|^view_", tables, ignore.case = TRUE)]
        main_tables <- main_tables[main_tables != "chemicals"]

        if (length(main_tables) == 0) {
          showNotification(get_text("no_main_tables"), type = "error")
          updateSelectInput(session, "database", choices = c("No main tables found" = ""))
          return()
        }

        # Get record counts for each table with localized names
        choices <- c()
        for (table in main_tables) {
          tryCatch({
            count_query <- paste("SELECT COUNT(*) as count FROM", table)
            count_result <- DBI::dbGetQuery(values$db_connection, count_query)
            count <- count_result$count[1]

            # Get localized database name
            db_name_key <- paste0(tolower(table), "_name")
            translated <- get_text(db_name_key)
            display_name <- if (identical(translated, db_name_key)) {
              # 字典无此库名（未来新增库表）时保持原 toupper 兜底
              toupper(table)
            } else {
              translated
            }

            # 中文模式下在名称后追加官方缩写
            if (values$current_lang == "zh") {
              display_name <- paste0(display_name, " (", gsub("_", " ", toupper(table)), ")")
            }

            label <- paste0(display_name, " (", count, ")")
            choices[label] <- table
          }, error = function(e) {
            # If count fails, still include the table
            label <- paste0(toupper(table), " (error)")
            choices[label] <- table
          })
        }

        if (length(choices) > 0) {
          # Preserve the user's current selection across re-builds (e.g.
          # language switch). isolate(): reading input here must not make this
          # observe re-run on every dropdown change (only language does).
          current_sel <- shiny::isolate(input$database)
          if (is.null(current_sel) || !(current_sel %in% main_tables)) {
            current_sel <- main_tables[1]
          }
          updateSelectInput(session, "database", choices = choices, selected = current_sel)
          values$available_databases <- main_tables
          # Removed notification for silent language switching
        } else {
          updateSelectInput(session, "database", choices = c("No valid tables" = ""))
        }

      }, error = function(e) {
        error_msg <- paste("Database initialization error:", e$message)
        showNotification(error_msg, type = "error")
        updateSelectInput(session, "database", choices = c("Error loading databases" = ""))
      })
    })

    # Browser title
    output$browser_title <- renderText({
      values$current_lang  # 添加依赖
      if (is.null(input$database) || input$database == "") {
        get_text("select_database")
      } else {
        paste(get_text("database_label"), toupper(input$database))
      }
    })

    # InChIKey filter toggle
    observeEvent(input$inchikey_filter, {
      values$inchikey_filter_active <- !values$inchikey_filter_active

      # Update button text and style
      if (values$inchikey_filter_active) {
        updateActionButton(session, "inchikey_filter",
                          label = if(values$current_lang == "zh") "显示全部" else "Show All")
        # 使用JavaScript添加active类
        session$sendCustomMessage("addClass", list(id = "inchikey_filter", class = "active"))
      } else {
        updateActionButton(session, "inchikey_filter",
                          label = if(values$current_lang == "zh") "仅显示InChIKey" else "InChIKey Only")
        # 使用JavaScript移除active类
        session$sendCustomMessage("removeClass", list(id = "inchikey_filter", class = "active"))
      }

      # Trigger data table refresh
      updateDataTable()
    })

    # Update button text when language changes
    observeEvent(values$current_lang, {
      if (values$inchikey_filter_active) {
        updateActionButton(session, "inchikey_filter",
                          label = if(values$current_lang == "zh") "显示全部" else "Show All")
      } else {
        updateActionButton(session, "inchikey_filter",
                          label = if(values$current_lang == "zh") "仅显示InChIKey" else "InChIKey Only")
      }


    })





    # Search column selector handling
    observeEvent(input$search_column, {
      values$search_column <- input$search_column
      # If search term is non-empty, trigger a refresh with the new column scope
      if (!is.null(values$search_term) && values$search_term != "") {
        updateDataTable()
      }
    })

    # Clear search button
    observeEvent(input$clear_search, {
      shiny::updateTextInput(session, "search_term", value = "")
      values$search_term <- ""
      updateDataTable()
    })

    # Debounced search term to reduce reactivity churn on large tables
    search_term_debounced <- shiny::debounce(reactive(input$search_term), millis = 300)

    # Search term handling
    observeEvent(search_term_debounced(), {
      values$search_term <- search_term_debounced()
      updateDataTable()
    })

    # Record counter
    output$record_counter <- renderText({
      values$current_lang  # 添加依赖
      if (!is.null(values$filtered_data) && !is.null(values$current_data)) {
        total <- nrow(values$current_data)
        filtered <- nrow(values$filtered_data)
        sprintf(get_text("showing_records"), filtered, total)
      } else {
        get_text("loading")
      }
    })

    # Helper: determine which columns to search based on user selection
    resolve_search_columns <- function(data, search_column) {
      all_cols <- names(data)
      switch(search_column,
        "all" = all_cols[sapply(data, function(x) is.character(x) || is.factor(x))],
        "name" = intersect(c("substance_name", "international_chemical_identification", "agent"), all_cols),
        "cas" = intersect(c("cas_no"), all_cols),
        "inchikey" = intersect(c("InChIKey"), all_cols),
        "ec" = intersect(c("ec_no"), all_cols),
        "notes" = intersect(c("notes", "remarks", "additional_information", "description",
                              "restrictions", "restrictions_and_specifications", "decision",
                              "response_to_comments", "reason_for_inclusion"), all_cols),
        all_cols[sapply(data, function(x) is.character(x) || is.factor(x))]
      )
    }

    # Function to update data table and record counter
    updateDataTable <- function() {
      if (!is.null(values$current_data)) {
        data <- values$current_data
        original_count <- nrow(data)

        # Apply InChIKey filter
        if (values$inchikey_filter_active) {
          inchikey_cols <- names(data)[grepl("InChIKey|inchikey", names(data), ignore.case = TRUE)]
          if (length(inchikey_cols) > 0) {
            data <- data[!is.na(data[[inchikey_cols[1]]]) & data[[inchikey_cols[1]]] != "", ]
          }
        }

        # Apply search filter (literal match, case-insensitive)
        if (!is.null(values$search_term) && values$search_term != "") {
          search_cols <- resolve_search_columns(data, values$search_column)
          if (length(search_cols) > 0) {
            matches <- apply(data[, search_cols, drop = FALSE], 1, function(row) {
              any(grepl(values$search_term, row, fixed = TRUE, ignore.case = TRUE))
            })
            data <- data[matches, ]
          }
        }

        values$filtered_data <- data

        # Update record counter with proper filtered vs total count
        filtered_count <- nrow(data)
        counter_text <- sprintf(get_text("showing_records"), filtered_count, original_count)

        session$sendCustomMessage("updateRecordCount", list(text = counter_text))
      }
    }

    # Column name translation map for friendly display
    col_name_map <- list(
      en = list(
        "ID" = "ID",
        "substance_name" = "Substance Name",
        "cas_no" = "CAS No.",
        "InChIKey" = "InChIKey",
        "ec_no" = "EC No.",
        "classification" = "Classification",
        "source" = "Source",
        "notes" = "Notes",
        "id" = "ID",
        "date_of_inclusion" = "Date of Inclusion",
        "reason_for_inclusion" = "Reason for Inclusion",
        "decision" = "Decision",
        "remarks" = "Remarks",
        "response_to_comments" = "Response to Comments",
        "hazard_class_and_category_codes" = "Hazard Class & Category Codes",
        "hazard_statement_codes" = "Hazard Statement Codes",
        "hazard_statement_codes_alt" = "Hazard Statement Codes (Alt)",
        "signal_word_codes" = "Signal Word Codes",
        "suppl_hazard_statement_codes" = "Suppl. Hazard Statement Codes",
        "pictogram" = "Pictogram",
        "m_factors" = "M-Factors",
        "specific_conc_limits" = "Specific Conc. Limits",
        "notes_on_verification" = "Notes on Verification",
        "atp_inserted_updated" = "ATP Inserted/Updated",
        "iuclid_dataset" = "IUCLID Dataset",
        "index_no" = "Index No.",
        "international_chemical_identification" = "International Chemical Identification",
        "agent" = "Agent",
        "group_classification" = "Group Classification",
        "evaluation_year" = "Evaluation Year",
        "volume" = "Volume",
        "volume_publication_year" = "Volume Publication Year",
        "description" = "Description",
        "additional_information" = "Additional Information",
        "regulation_reference" = "Regulation Reference",
        "sml" = "SML",
        "sml_value" = "SML Value",
        "unit" = "Unit",
        "food_type" = "Food Type",
        "restrictions" = "Restrictions",
        "restrictions_and_specifications" = "Restrictions & Specifications",
        "use_as_additive" = "Use as Additive",
        "use_as_monomer" = "Use as Monomer",
        "frf_applicable" = "FRF Applicable",
        "sml_group" = "SML Group",
        "group_no" = "Group No.",
        "fcm_substance_no" = "FCM Substance No.",
        "ref_no" = "Ref. No.",
        "evidence_level" = "Evidence Level",
        "created_at" = "Created At",
        "updated_at" = "Updated At",
        "SMILES" = "SMILES"
      ),
      zh = list(
        "ID" = "序号",
        "substance_name" = "物质名称",
        "cas_no" = "CAS号",
        "InChIKey" = "InChIKey",
        "ec_no" = "EC号",
        "classification" = "分类",
        "source" = "来源",
        "notes" = "备注",
        "id" = "ID",
        "date_of_inclusion" = "列入日期",
        "reason_for_inclusion" = "列入原因",
        "decision" = "决定",
        "remarks" = "备注",
        "response_to_comments" = "评论回复",
        "hazard_class_and_category_codes" = "危害类别代码",
        "hazard_statement_codes" = "危害声明代码",
        "hazard_statement_codes_alt" = "危害声明代码（替代）",
        "signal_word_codes" = "信号词代码",
        "suppl_hazard_statement_codes" = "补充危害声明代码",
        "pictogram" = "象形图",
        "m_factors" = "M系数",
        "specific_conc_limits" = "特定浓度限值",
        "notes_on_verification" = "验证说明",
        "atp_inserted_updated" = "ATP插入/更新",
        "iuclid_dataset" = "IUCLID数据集",
        "index_no" = "索引号",
        "international_chemical_identification" = "国际化学品标识",
        "agent" = "物质/制剂",
        "group_classification" = "组分类",
        "evaluation_year" = "评估年份",
        "volume" = "卷号",
        "volume_publication_year" = "卷号出版年份",
        "description" = "描述",
        "additional_information" = "附加信息",
        "regulation_reference" = "法规参考",
        "sml" = "SML",
        "sml_value" = "SML值",
        "unit" = "单位",
        "food_type" = "食品类型",
        "restrictions" = "限制条件",
        "restrictions_and_specifications" = "限制与规格",
        "use_as_additive" = "用作添加剂",
        "use_as_monomer" = "用作单体",
        "frf_applicable" = "FRF适用",
        "sml_group" = "SML组",
        "group_no" = "组号",
        "fcm_substance_no" = "FCM物质编号",
        "ref_no" = "参考号",
        "evidence_level" = "证据级别",
        "created_at" = "创建时间",
        "updated_at" = "更新时间",
        "SMILES" = "SMILES"
      )
    )

    # ---- 主数据表（查询 + 筛选 + 列名翻译 + DT 配置） ----
    # Data table
    output$data_table <- DT::renderDataTable({
      req(input$database)

      # React to language changes so column headers re-render
      lang <- values$current_lang

      # Return empty table if no database selected
      if (is.null(input$database) || input$database == "") {
        return(DT::datatable(data.frame(Message = "Please select a database"),
                           options = list(dom = 't'), rownames = FALSE))
      }

      # Return error if no connection
      if (is.null(values$db_connection)) {
        return(DT::datatable(data.frame(Error = "Database connection not available"),
                           options = list(dom = 't'), rownames = FALSE))
      }

      tryCatch({
        table_name <- input$database

        # Validate database connection
        if (is.null(values$db_connection)) {
          return(data.frame(Error = "Database connection not available"))
        }

        # Check if table exists
        available_tables <- DBI::dbListTables(values$db_connection)
        if (!table_name %in% available_tables) {
          error_msg <- sprintf(get_text("table_not_found"), table_name)
          return(data.frame(Message = error_msg))
        }

        # Execute query to get all data with error handling.
        # 结构式功能依赖 SMILES 列，但主表本身不存 SMILES（单独在 chemicals 表，
        # 靠 InChIKey 关联）。主表有 InChIKey 列时 LEFT JOIN chemicals 把 SMILES 带进
        # 展示数据；chemicals 的 InChIKey 唯一，不会产生行膨胀。无 InChIKey 列的主表
        # （如 china_sml / eu_sml_group）保持原样，这些库本就没有结构式数据。
        table_cols <- DBI::dbListFields(values$db_connection, table_name)
        has_inchikey <- "InChIKey" %in% table_cols

        # Decide whether to push filtering down to SQLite for large tables
        total_rows <- DBI::dbGetQuery(values$db_connection,
                                       paste("SELECT COUNT(*) AS n FROM", table_name))$n[1]
        use_sql_filter <- total_rows >= 5000 &&
                          (values$inchikey_filter_active ||
                           (!is.null(values$search_term) && values$search_term != ""))

        if (use_sql_filter) {
          # Large table: build WHERE clause and query only matching rows
          where_clauses <- c()
          if (values$inchikey_filter_active && has_inchikey) {
            where_clauses <- c(where_clauses, "\"InChIKey\" IS NOT NULL AND \"InChIKey\" != ''")
          }
          if (!is.null(values$search_term) && values$search_term != "") {
            common_text_cols <- c("substance_name", "international_chemical_identification", "agent",
                                  "cas_no", "InChIKey", "ec_no", "notes", "remarks",
                                  "additional_information", "description", "restrictions",
                                  "restrictions_and_specifications", "decision",
                                  "response_to_comments", "reason_for_inclusion", "source",
                                  "classification")
            search_cols <- switch(values$search_column,
              "all" = intersect(common_text_cols, table_cols),
              "name" = intersect(c("substance_name", "international_chemical_identification", "agent"), table_cols),
              "cas" = intersect(c("cas_no"), table_cols),
              "inchikey" = intersect(c("InChIKey"), table_cols),
              "ec" = intersect(c("ec_no"), table_cols),
              "notes" = intersect(c("notes", "remarks", "additional_information", "description",
                                    "restrictions", "restrictions_and_specifications", "decision",
                                    "response_to_comments", "reason_for_inclusion"), table_cols),
              intersect(common_text_cols, table_cols)
            )
            if (length(search_cols) > 0) {
              safe_term <- gsub("'", "''", values$search_term)
              safe_term <- gsub("%", "\\%", safe_term, fixed = TRUE)
              safe_term <- gsub("_", "\\_", safe_term, fixed = TRUE)
              like_clauses <- vapply(search_cols, function(col) {
                paste0("LOWER(\"", col, "\") LIKE LOWER('%", safe_term, "%')")
              }, character(1))
              where_clauses <- c(where_clauses, paste(like_clauses, collapse = " OR "))
            }
          }

          base_query <- if (has_inchikey) {
            paste0("SELECT m.*, c.SMILES AS SMILES FROM ", table_name, " m ",
                   "LEFT JOIN chemicals c ON m.InChIKey = c.InChIKey")
          } else {
            paste("SELECT * FROM", table_name)
          }
          if (length(where_clauses) > 0) {
            query <- paste(base_query, "WHERE", paste(where_clauses, collapse = " AND "))
          } else {
            query <- base_query
          }
          data <- DBI::dbGetQuery(values$db_connection, query)
          filtered_data <- data
          values$current_data <- NULL  # skip keeping full table in memory for large tables
        } else {
          # Standard path: fetch all then filter in memory
          if (has_inchikey) {
            query <- paste0(
              "SELECT m.*, c.SMILES AS SMILES FROM ", table_name, " m ",
              "LEFT JOIN chemicals c ON m.InChIKey = c.InChIKey"
            )
          } else {
            query <- paste("SELECT * FROM", table_name)
          }
          data <- DBI::dbGetQuery(values$db_connection, query)

          if (nrow(data) == 0) {
            return(data.frame(Message = sprintf(get_text("table_empty"), table_name)))
          }

          values$current_data <- data

          # Apply initial filtering directly without calling updateDataTable()
          filtered_data <- data

          # Apply InChIKey filter
          if (values$inchikey_filter_active) {
            inchikey_cols <- names(filtered_data)[grepl("InChIKey|inchikey", names(filtered_data), ignore.case = TRUE)]
            if (length(inchikey_cols) > 0) {
              filtered_data <- filtered_data[!is.na(filtered_data[[inchikey_cols[1]]]) & filtered_data[[inchikey_cols[1]]] != "", ]
            }
          }

          # Apply search filter (literal match, case-insensitive, scoped by column)
          if (!is.null(values$search_term) && values$search_term != "") {
            search_cols <- resolve_search_columns(filtered_data, values$search_column)
            if (length(search_cols) > 0) {
              matches <- apply(filtered_data[, search_cols, drop = FALSE], 1, function(row) {
                any(grepl(values$search_term, row, fixed = TRUE, ignore.case = TRUE))
              })
              filtered_data <- filtered_data[matches, ]
            }
          }
        }

        values$filtered_data <- filtered_data
        data <- filtered_data

        # Fallback: if substance_name (or equivalent name column) is empty, use CAS No. as display name
        name_col <- grep("substance_name|agent|international_chemical_identification",
                         names(data), ignore.case = TRUE, value = TRUE)[1]
        cas_col <- grep("cas_no|CAS No", names(data), ignore.case = TRUE, value = TRUE)[1]
        if (!is.na(name_col) && !is.na(cas_col) && cas_col %in% names(data)) {
          empty_name <- is.na(data[[name_col]]) | trimws(data[[name_col]]) == ""
          if (any(empty_name)) {
            data[[name_col]][empty_name] <- paste0("[CAS ", data[[cas_col]][empty_name], "]")
          }
        }

        if (nrow(data) == 0) {
          return(DT::datatable(data.frame(Message = get_text("no_data_matches")),
                              options = list(dom = 't'), rownames = FALSE))
        }

        # Add row index column to preserve original database order
        data_with_index <- data.frame(
          `ID` = seq_len(nrow(data)),
          data,
          check.names = FALSE
        )

        # Reorder columns: ID (1), name (2), key identifier (3), then rest
        cols <- names(data_with_index)
        name_col_candidates <- c("substance_name", "international_chemical_identification", "agent")
        name_col <- name_col_candidates[name_col_candidates %in% cols][1]
        if (!is.na(name_col)) {
          pos3_candidates <- c("InChIKey", "id")
          pos3_col <- pos3_candidates[pos3_candidates %in% cols][1]
          if (is.na(pos3_col)) {
            pos3_col <- setdiff(cols, c("ID", name_col))[1]
          }
          other_cols <- setdiff(cols, c("ID", name_col, pos3_col))
          data_with_index <- data_with_index[, c("ID", name_col, pos3_col, other_cols), drop = FALSE]
        }

        # Translate column names for friendly display
        lang_map <- col_name_map[[lang]]
        if (!is.null(lang_map)) {
          new_names <- vapply(names(data_with_index), function(nm) {
            if (nm %in% names(lang_map)) lang_map[[nm]] else nm
          }, character(1))
          names(data_with_index) <- new_names
        }

        # DataTable configuration with dynamic record counter
        dt <- DT::datatable(data_with_index,
                     options = list(
                       pageLength = 25,
                       scrollX = TRUE,
                       scrollY = "calc(100vh - 300px)",
                       dom = 'rtip',
                       selection = 'single',
                       columnDefs = list(
                         list(targets = 0, width = "40px", className = "dt-row-index"),
                         list(targets = "_all", className = "dt-column-stable")
                       ),
                       drawCallback = DT::JS(
                         "function(settings) {",
                         "  var api = this.api();",
                         "  var info = api.page.info();",
                         "  var lang = $('body').hasClass('zh-lang') ? 'zh' : 'en';",
                         "  var text = lang === 'zh' ? ",
                         "    '显示 ' + info.recordsDisplay + ' 条，共 ' + info.recordsTotal + ' 条记录' :",
                         "    'Showing ' + info.recordsDisplay + ' of ' + info.recordsTotal + ' records';",
                         "  Shiny.setInputValue('datatable_info', {",
                         "    recordsDisplay: info.recordsDisplay,",
                         "    recordsTotal: info.recordsTotal,",
                         "    text: text",
                         "  });",
                         "}"
                       )
                     ),
                     filter = 'top',
                     selection = 'single',
                     rownames = FALSE,
                     # DT 0.34 still calls window.alert() for some warnings
                     # (e.g. 'Non-table node initialisation'). Setting
                     # errMode='none' via the datatable() callback runs
                     # AFTER DataTables has been injected into jQuery, so
                     # $.fn.dataTable is guaranteed to exist -- unlike in
                     # a $(document).ready handler where DataTables JS
                     # is still being lazy-loaded and the same line
                     # throws "Cannot read properties of undefined
                     # (reading 'ext')". See rstudio/DT#815.
                     callback = DT::JS("$.fn.dataTable.ext.errMode = 'none';"))

        return(dt)


      }, error = function(e) {
        error_msg <- sprintf(get_text("failed_to_load"), e$message)
        DT::datatable(data.frame(Error = error_msg),
                     options = list(dom = 't'), rownames = FALSE)
      })
    })

    # Dynamic record counter update
    observeEvent(input$datatable_info, {
      if (!is.null(input$datatable_info)) {
        output$record_counter <- renderText({
          input$datatable_info$text
        })
      }
    })

    # ---- 结构式面板（SMILES 由前端 JS 取，R 侧不按列序号反查） ----
    # Structure display is driven by the SMILES string extracted in JS.
    # updateStructure() locates the SMILES column from the rendered table
    # headers (DataTable API), so the value is always the exact one the
    # user clicked -- no need (and no safe way) to reverse-look-up the
    # row by column index on the R side. Earlier versions tried to rebuild
    # the SMILES value from input$selected_row_data plus a hard-coded
    # "ID offset", which silently failed (empty structure panel) whenever
    # the row array layout did not match the assumption.
    observeEvent(input$selected_compound_info, {
      info <- input$selected_compound_info
      try(writeLines(c(sprintf("smiles=%s", deparse(info$smiles)),
                       sprintf("has_structure=%s", deparse(info$has_structure))),
                     ".workbuddy/diag_received.txt"), silent = TRUE)
      sm <- info$smiles
      if (!is.null(sm) && nzchar(sm) && sm != "NULL") {
        values$selected_smiles <- sm
        values$zoom_level <- 0.5
        values$no_structure_selected <- FALSE
      } else {
        # 用户选中了某行，但该行无结构式（混合物/聚合物/组条目）。
        values$selected_smiles <- NULL
        values$no_structure_selected <- TRUE
      }
      updateStructureDisplay()
    }, ignoreInit = TRUE)

    # 切换数据库时重置结构式面板，避免上一个库的提示残留
    observeEvent(input$database, {
      values$selected_smiles <- NULL
      values$no_structure_selected <- FALSE
      updateStructureDisplay()
    }, ignoreInit = TRUE)

    # Zoom controls
    # Enhanced zoom controls with full range support
    observeEvent(input$zoom_in, {
      if (is.null(values$zoom_level)) values$zoom_level <- 0.5
      values$zoom_level <- min(values$zoom_level * 1.4, 4.0)  # Smooth zoom increments
      cat("Zoom in to:", values$zoom_level, "\n")  # Debug info
      updateStructureDisplay()
    })

    observeEvent(input$zoom_out, {
      if (is.null(values$zoom_level)) values$zoom_level <- 0.5
      values$zoom_level <- max(values$zoom_level / 1.4, 0.3)  # Smooth zoom decrements
      cat("Zoom out to:", values$zoom_level, "\n")  # Debug info
      updateStructureDisplay()
    })

    observeEvent(input$reset_view, {
      values$zoom_level <- 0.5
      cat("Reset zoom to:", values$zoom_level, "\n")  # Debug info
      updateStructureDisplay()
    })

    # Function to update structure display using custom rcdk-based plot_molecule
    updateStructureDisplay <- function() {
      if (!is.null(values$selected_smiles) && values$selected_smiles != "") {
        if (requireNamespace("rcdk", quietly = TRUE)) {
          tryCatch({
            # Parse SMILES string to molecule object
            molecule <- rcdk::parse.smiles(values$selected_smiles)

            if (length(molecule) > 0 && !is.null(molecule[[1]])) {
              # Create temporary file for the plot
              temp_file <- tempfile(fileext = ".png")

              # Proper aspect ratio system without CSS transform distortion
              zoom <- if (is.null(values$zoom_level)) 0.5 else values$zoom_level

              # Base size for consistent high quality rendering
              base_size <- 600  # Fixed base size for consistent aspect ratio

              # Calculate actual dimensions based on zoom for proper scaling
              actual_width <- base_size * zoom
              actual_height <- base_size * zoom

              # High resolution rendering with proper dimensions
              png(temp_file, width = actual_width, height = actual_height,
                  bg = "white", res = 200, type = "cairo")

              # Enhanced margins for better breathing room
              par(
                mar = c(1.5, 1.5, 1.5, 1.5),  # Increased margins for better spacing
                xaxs = "i", yaxs = "i",
                oma = c(0.5, 0.5, 0.5, 0.5),  # Outer margins for additional space
                mai = c(0.3, 0.3, 0.3, 0.3)   # Inner margins for structure padding
              )

              plot_molecule(molecule)
              dev.off()

              # Read the image and convert to base64
              tryCatch({
                if (requireNamespace("base64enc", quietly = TRUE)) {
                  img_data <- base64enc::base64encode(temp_file)
                } else {
                  # Use base R method
                  img_raw <- readBin(temp_file, "raw", file.info(temp_file)$size)
                  img_data <- jsonlite::base64_enc(img_raw)
                }
              }, error = function(e) {
                # Simple fallback - just show SMILES text
                img_data <- NULL
              })

              # Clean up temp file
              unlink(temp_file)

              # Create HTML content with wrapper div for proper scroll boundaries
              if (!is.null(img_data)) {
                # Create HTML with wrapper div for symmetric scroll boundaries
                img_html <- paste0(
                  "<div class='image-wrapper'>",
                  "<img src='data:image/png;base64,", img_data, "' ",
                  "width='", actual_width, "px' ",
                  "height='", actual_height, "px' ",
                  "style='display: block; margin: 0; max-width: none; max-height: none; border-radius: 5px;' />",
                  "</div>"
                )
              } else {
                # Fallback to text display
                img_html <- paste0("<div style='text-align: center; padding: 20px; background-color: #f8f9fa; border-radius: 5px;'>",
                                 "<p><strong>SMILES:</strong> ", values$selected_smiles, "</p>",
                                 "<p><em>Structure rendered successfully but display failed</em></p>",
                                 "</div>")
              }

              session$sendCustomMessage("updateStructure", list(
                content = img_html,
                smiles = values$selected_smiles,
                zoom = zoom
              ))
            } else {
              # Invalid molecule
              error_msg <- paste0("<div style='color: #dc3545; text-align: center; padding: 20px;'>",
                                "<p><strong>", sprintf(get_text("smiles_parse_error"), ""), "</strong></p>",
                                "<p>", values$selected_smiles, "</p>",
                                "<p><em>", get_text("invalid_smiles"), "</em></p>",
                                "</div>")

              session$sendCustomMessage("updateStructure", list(
                content = error_msg,
                smiles = values$selected_smiles,
                zoom = 1.0
              ))
            }
          }, error = function(e) {
            error_msg <- paste0("<div style='color: #dc3545; text-align: center; padding: 20px;'>",
                              "<p><strong>", sprintf(get_text("structure_error"), ""), "</strong></p>",
                              "<p>", values$selected_smiles, "</p>",
                              "<p><em>", e$message, "</em></p>",
                              "</div>")

            session$sendCustomMessage("updateStructure", list(
              content = error_msg,
              smiles = values$selected_smiles,
              zoom = 1.0
            ))
          })
        } else {
          install_msg <- paste0("<div style='text-align: center; padding: 20px; background-color: #fff3cd; border: 1px solid #ffeaa7; border-radius: 5px;'>",
                              "<p><strong>SMILES:</strong> ", values$selected_smiles, "</p>",
                              "<p><em>", get_text("install_rcdk"), "</em></p>",
                              "<p><code>install.packages('rcdk')</code></p>",
                              "</div>")

          session$sendCustomMessage("updateStructure", list(
            content = install_msg,
            smiles = values$selected_smiles,
            zoom = 1.0
          ))
        }
      } else {
        # 区分两种情况：从未选中行（占位提示）vs 选中了无结构式的行（无单一结构提示）。
        if (isTRUE(values$no_structure_selected)) {
          if (identical(input$database, "eu_sml_group")) {
            placeholder_msg <- get_text("no_structure_group")
          } else {
            placeholder_msg <- get_text("no_structure_generic")
          }
        } else {
          placeholder_msg <- get_text("structure_placeholder")
        }

        session$sendCustomMessage("updateStructure", list(
          content = paste0("<p style='text-align: center; color: #6c757d; font-style: italic; padding: 20px;'>",
                          placeholder_msg, "</p>"),
          smiles = "",
          zoom = 1.0
        ))
      }
    }





    # ============================================================
    # 一键操作面板 handlers（2026-09-04 新增；2026-09-11 重做防连点 + 两段式）
    #
    # 事故复盘：Shiny 的**出站**消息（进度条 / 日志）在 R 阻塞期间照样送到浏览器，
    # 但**入站**消息（点击）要等 R 空下来才补送。于是全量更新跑着的时候页面
    # 按钮全哑、点击排队；任务一结束，排队的点击立刻又触发一轮 —— 用户永远
    # 等不到"页面恢复"的那一刻。详见 R/update_run_guard.R 顶部与 ADR 0011。
    #
    # 现在三道防线：
    #   1) 任务一开始就禁用按钮（页面侧），跑完再放开；
    #   2) 服务端按"上一轮结束到现在多久"丢弃积压点击（should_drop_run_request）；
    #   3) 写库走两段式：先 auto_apply = FALSE 预演，把 diff 摆出来，
    #      人点确认后再 auto_apply = TRUE 真写。
    # 更新类函数在 interactive = FALSE 下不会 readline 卡住：有移除条目或变更数
    # 超过 max_auto_changes 时安全阀会自动停下不写库，原因写在汇总表的 message 列。
    # ============================================================
    # 更新队列：直接取 ALL_AUTO_DBS（注册表派生），顺序由构造保证与
    # resolve_db_names("all") 一致。逐库调用而不是一次 databases = "all"，
    # 是为了每库给一次 incProgress —— 进度条走真进度。
    UPDATE_DBS <- ALL_AUTO_DBS

    # 页面侧配合：禁用/放开所有一键操作按钮 + 更新状态行
    set_quick_busy <- function(busy, text = NULL) {
      session$sendCustomMessage("fcmSetBusy",
                                list(busy = isTRUE(busy),
                                     text = if (is.null(text)) "" else text))
    }

    # 实时日志：追加一行并推给页面（只送最后 15 行，够看就行）
    push_log <- function(msg) {
      msg <- paste(msg, collapse = "\n")
      values$quick_lines <- c(values$quick_lines, msg)
      session$sendCustomMessage("fcmLiveLog", list(
        text = paste(utils::tail(values$quick_lines, 15), collapse = "\n")))
    }

    # 逐库跑一轮更新。循环本体在 run_db_update_round()（包内函数，有测试覆盖 ——
    # 闭包里的循环没法单独测，而"单库失败不中断""结果能拼成一张表"最容易出错），
    # 这里只负责把日志与进度接到页面上。
    run_update_round <- function(auto_apply, label) {
      out <- NULL
      withProgress(message = label, value = 0, {
        out <- run_db_update_round(
          UPDATE_DBS,
          runner = function(db) {
            update_database_auto(databases = db, auto_apply = auto_apply)
          },
          log = push_log,
          progress = function(i, n, db) {
            incProgress(1 / n, detail = sprintf("%s（%d/%d）", db, i, n))
          })
      })
      out
    }

    # 通用执行器：同步执行 task()，日志实时推到页面，返回 task() 的返回值
    run_quick_task <- function(task, label) {
      if (isTRUE(values$quick_busy)) {
        showNotification("上一个操作还在运行中，请稍候…", type = "warning")
        return(invisible(NULL))
      }
      # 积压点击守卫：阻塞期间的点击会在任务结束后补送，那一刻 quick_busy 早已
      # 复位，只看它挡不住 —— 会一连再跑 N 轮。宽限期内的请求一律丢弃。
      if (should_drop_run_request(values$last_run_end, grace_secs = 10)) {
        showNotification("刚跑完一轮，已忽略这次点击（防止重复执行）。",
                         type = "warning", duration = 5)
        return(invisible(NULL))
      }

      values$quick_busy <- TRUE
      values$quick_lines <- character(0)
      set_quick_busy(TRUE, paste0(label, "：启动中…"))
      # 复位放进 on.exit 而不是顺序执行：R 控制台 Ctrl+C 抛的是 interrupt，
      # tryCatch(error = ) 接不住 —— 顺序执行会让 quick_busy 永久停在 TRUE，
      # 之后四个按钮永远回"上一个操作还在运行中"。
      on.exit({
        values$quick_busy <- FALSE
        values$last_run_end <- Sys.time()
        set_quick_busy(FALSE, paste0(label, "：已完成"))
        session$sendCustomMessage("fcmLiveLog", list(text = "", done = TRUE))
      }, add = TRUE)

      result_val <- NULL
      t0 <- Sys.time()
      withCallingHandlers(
        tryCatch({
          # capture.output 接住 stdout（print/cat 输出，如 update 的汇总表）；
          # message 由下方 calling handler 接住（各函数进度条）。
          # 两路都进同一个 buffer，并且都实时推给页面 —— 不再憋到结束才显示。
          out_txt <- utils::capture.output(result_val <- task())
          if (length(out_txt)) push_log(out_txt)
          NULL
        }, error = function(e) {
          push_log(paste0("❌ 运行出错: ", conditionMessage(e)))
          NULL
        }),
        message = function(m) {
          push_log(sub("\\n$", "", conditionMessage(m)))
          invokeRestart("muffleMessage")
        }
      )
      elapsed <- sprintf("（耗时 %.1f 秒）",
                         as.numeric(difftime(Sys.time(), t0, units = "secs")))
      lines <- values$quick_lines
      if (length(lines) == 0) lines <- "（无输出）"
      output$quick_update_log <- shiny::renderPrint({
        cat(paste(c(paste0("✅ ", label, " 完成 ", elapsed), "",
                    lines), collapse = "\n"))
      })
      showNotification(paste0(label, " 完成 ", elapsed), type = "message")
      invisible(result_val)
    }

    # 展示按库汇总表：预演阶段（dry）告诉你"哪些能写、哪些会被拦"，
    # 写入阶段（write）告诉你"实际结果"。表头用中文，上游 message 已翻译。
    show_update_report <- function(summary_df, phase = c("dry", "write")) {
      phase <- match.arg(phase)
      values$report_summary <- summary_df
      values$report_phase <- phase
      s <- summarise_update_run(summary_df)

      output$update_report_table <- shiny::renderTable({
        d <- summarise_update_run(values$report_summary)$table
        # 预演时 status 恒为 "failed"（上游 done(FALSE, ...)），显示出来只会
        # 让人误判成出错，干脆不显示；写入时它才是有意义的成功/失败信号。
        if (identical(values$report_phase, "dry")) d$status <- NULL
        zh <- c(database = "库", status = "状态", added = "新增",
                removed = "移除", modified = "修改", will_write = "会写入",
                blocked_reason = "被拦下 / 说明", message = "上游返回")
        hit <- names(d) %in% names(zh)
        names(d)[hit] <- unname(zh[names(d)[hit]])
        d
      }, striped = TRUE, hover = TRUE, bordered = TRUE, na = "",
         width = "100%")

      headline <- if (identical(phase, "dry")) {
        s$headline
      } else {
        paste0("写入流程结束：", sum(s$table$status == "ok"), "/",
               nrow(s$table), " 个库处理成功。")
      }
      footer <- if (identical(phase, "dry") && s$n_writable > 0L) {
        shiny::tagList(
          shiny::modalButton("取消（不写库）"),
          actionButton("db_confirm_write",
                       paste0("确认写入 ", s$n_writable, " 条变更"),
                       class = "btn-primary")
        )
      } else {
        shiny::modalButton("关闭")
      }
      shiny::showModal(shiny::modalDialog(
        title = if (identical(phase, "dry")) "预演结果（尚未写库）" else "写入结果",
        size = "l",
        easyClose = identical(phase, "write"),
        HTML(paste0("<p style='margin-bottom:8px;'>", headline, "</p>")),
        shiny::tableOutput("update_report_table"),
        tags$p(style = "color:#6c757d; font-size:12px; margin-top:8px;",
               if (identical(phase, "dry"))
                 paste0("「预演通过（本轮未写库）」= 该库顺利算完差异；",
                        "「安全阀拦下」「超过自动上限」的库不会写入，需到命令行处理。")
               else ""),
        footer = footer
      ))
    }

    # 只读操作：直接执行
    observeEvent(input$db_btn_status, {
      run_quick_task(function() fcmsafety_status(), "查看状态")
    })

    observeEvent(input$db_btn_check, {
      run_quick_task(function() check_manual_lists(ask = FALSE), "检查手动新清单")
    })

    # 写库操作：先弹确认框，确认后再执行
    observeEvent(input$db_btn_apply, {
      shiny::showModal(shiny::modalDialog(
        title = "确认操作",
        HTML("即将把 inst/ 中检测到的<strong>手动放入的清单文件</strong>更新进数据库。<br>是否继续？"),
        footer = shiny::tagList(
          shiny::modalButton("取消"),
          actionButton("db_confirm_apply", "确认更新", class = "btn-primary")
        )
      ))
    })

    observeEvent(input$db_btn_update, {
      shiny::showModal(shiny::modalDialog(
        title = "联网更新全部数据库",
        HTML("流程分<strong>两段</strong>，写库前一定让你先看到差异：<br>",
             "① <strong>预演</strong>：联网下载 SVHC / CMR / IARC / EU SML 最新清单，",
             "只算不写，把每个库的 +新增 / -移除 / ~修改 摆出来；<br>",
             "② <strong>确认写入</strong>：你点确认后才真正写库。<br><br>",
             "需要联网，两段合计可能十几分钟。运行期间页面按钮会临时禁用",
             "（<strong>进度条和日志会实时更新，这是正常现象，不是卡死</strong>）。<br>",
             "检测到官方移除条目或变更数超过自动上限的库，安全阀会拦下，",
             "需到命令行人工确认。"),
        footer = shiny::tagList(
          shiny::modalButton("取消"),
          actionButton("db_confirm_update", "开始预演", class = "btn-primary")
        )
      ))
    })

    observeEvent(input$db_confirm_apply, {
      shiny::removeModal()
      run_quick_task(function() check_manual_lists(auto_apply = TRUE, ask = FALSE),
                     "应用手动清单")
    })

    # 第一段：预演。auto_apply = FALSE，业务表和账本都不写，
    # 拿回来的汇总表直接摆给用户看；只有预计可写入的库才给「确认写入」按钮。
    observeEvent(input$db_confirm_update, {
      shiny::removeModal()
      res <- run_quick_task(function() run_update_round(FALSE, "预演中（未写库）"),
                            "联网更新 · 预演")
      show_update_report(res, phase = "dry")
    })

    # 第二段：真写。max_auto_changes 保持默认（20）—— 浏览器里的"确认"只用来
    # 放行**已经看过的那份 diff**，不放宽自动上限；移除条目依然会被安全阀硬拦。
    # 要动这个语义得先问过（见 ADR 0011 的"未决问题"）。
    observeEvent(input$db_confirm_write, {
      shiny::removeModal()
      res <- run_quick_task(function() run_update_round(TRUE, "写入数据库中"),
                            "联网更新 · 写入")
      show_update_report(res, phase = "write")
    })

    # ============================================================
    # 筛查面板：上传物质清单 → run_screening（补结构 + 匹配 + 定级）→
    # 等级分布预览 + xlsx / csv 报告下载。
    # 与一键操作共用 run_quick_task 的忙碌守卫 / 实时日志 / 防连点。
    # 设计取舍：主数据表是"看库"的地方，筛查结果不往里灌——弹窗给
    # 等级分布与命中概览，完整逐行结果（含 Toxic_level_basis）在导出的报告里。
    # Toxtree 结果文件缺失时会现场自动运行（需 Java）；失败已降级为
    # 仅法规匹配（见 direct_sql_toxicity.R），GUI 不会因此白跑。
    # ============================================================
    observeEvent(input$db_btn_screen, {
      shiny::showModal(shiny::modalDialog(
        title = "物质筛查（法规匹配 + 毒性定级）",
        shiny::fileInput("screen_file", "物质清单文件（xlsx / csv）",
                         accept = c(".xlsx", ".xls", ".csv"),
                         buttonLabel = "选择文件…",
                         placeholder = "尚未选择文件"),
        HTML(paste0(
          "<p style='font-size:12px;color:#6c757d;margin-top:2px;'>",
          "列名自动识别（NAME/名称、SMILES、CAS、InChIKey 等常见写法均可）。<br>",
          "已有 InChIKey 的行直接采用；只有名称 + SMILES 的行用本地 CDK 离线推导。<br>",
          "Toxtree 结果缺失且带 SMILES 列时会现场自动运行（首次需下载约 81MB、需要 Java；",
          "运行失败会自动降级为仅法规匹配，Cramer 列留空）。</p>")),
        shiny::checkboxInput("screen_online", "本地推导失败的行联网查 PubChem 兜底（较慢）", FALSE),
        shiny::checkboxInput("screen_group", "额外做组条目归属判定（较慢，默认关）", FALSE),
        footer = shiny::tagList(
          shiny::modalButton("取消"),
          actionButton("db_confirm_screen", "开始筛查", class = "btn-primary")
        )
      ))
    })

    observeEvent(input$db_confirm_screen, {
      shiny::removeModal()
      uploaded <- if (!is.null(input$screen_file)) input$screen_file$datapath else NULL
      if (is.null(uploaded) || !nzchar(uploaded)) {
        showNotification("请先选择物质清单文件（xlsx / csv）。", type = "error")
        return()
      }
      online <- isTRUE(input$screen_online)
      with_group <- isTRUE(input$screen_group)
      res <- run_quick_task(function() {
        df <- rio::import(uploaded)
        if (!is.data.frame(df) || nrow(df) == 0) {
          stop("文件里没有可读的数据行")
        }
        run_screening(df, online = online, group_membership = with_group)
      }, "物质筛查")
      if (is.null(res)) return()   # 出错信息已在日志里给出
      values$screen_result <- res
      show_screen_result(res)
    })

    # 结果弹窗：等级分布 + 监管清单命中概览
    show_screen_result <- function(res) {
      lv <- if ("Toxic_level" %in% names(res)) res$Toxic_level else rep("-", nrow(res))
      lv <- ifelse(is.na(lv) | lv == "", "-", lv)
      dist <- as.data.frame(table(`Toxic_level` = factor(lv, levels = c("V", "IV", "III", "II", "I", "-"))),
                            responseName = "行数")
      hit_cols <- intersect(c("SVHC", "CMR", "CMR_suspect", "EDC", "IARC"), names(res))
      hits <- do.call(rbind, lapply(hit_cols, function(cc) {
        data.frame(清单 = cc, 命中行数 = sum(res[[cc]] == "Y", na.rm = TRUE))
      }))
      output$screen_dist_table <- shiny::renderTable(dist, striped = TRUE, hover = TRUE,
                                                     bordered = TRUE, na = "", width = "100%")
      output$screen_hit_table <- shiny::renderTable(hits, striped = TRUE, hover = TRUE,
                                                    bordered = TRUE, na = "", width = "100%")
      shiny::showModal(shiny::modalDialog(
        title = "筛查完成",
        size = "l",
        easyClose = TRUE,
        HTML(paste0("<p>共 <b>", nrow(res), "</b> 行物质。毒性等级分布（\"-\" = 无足够证据定级，不代表安全）：</p>")),
        shiny::tableOutput("screen_dist_table"),
        HTML("<p style='margin-top:10px;'>监管清单命中行数：</p>"),
        shiny::tableOutput("screen_hit_table"),
        tags$p(style = "color:#6c757d; font-size:12px; margin-top:8px;",
               "完整逐行结果（含 Toxic_level_basis 定级依据、Unassigned / Issues 表）请下载报告查看。"),
        footer = shiny::tagList(
          shiny::downloadButton("screen_download_xlsx", "下载 xlsx 报告"),
          shiny::downloadButton("screen_download_csv", "下载 CSV"),
          shiny::modalButton("关闭")
        )
      ))
    }

    # 报告下载：xlsx 走 export_toxicity_report（4 张表带样式），csv 走 write.csv。
    # results 保存在 values$screen_result，下载时 isolate 读取。
    output$screen_download_xlsx <- shiny::downloadHandler(
      filename = function() {
        paste0("fcmsafety_screening_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
      },
      content = function(file) {
        res <- shiny::isolate(values$screen_result)
        if (is.null(res)) stop("没有可下载的筛查结果")
        export_toxicity_report(res, path = file)
      },
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    )

    output$screen_download_csv <- shiny::downloadHandler(
      filename = function() {
        paste0("fcmsafety_screening_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        res <- shiny::isolate(values$screen_result)
        if (is.null(res)) stop("没有可下载的筛查结果")
        utils::write.csv(res, file, row.names = FALSE, fileEncoding = "UTF-8")
      },
      contentType = "text/csv"
    )

    # Show database update history in a modal dialog
    observeEvent(input$db_btn_history, {
      shiny::showModal(shiny::modalDialog(
        title = "Database Update History / 数据库更新日志",
        size = "l",
        easyClose = TRUE,
        footer = shiny::modalButton("Close / 关闭"),
        shiny::uiOutput("update_history_container")
      ))

      # Read history from the same connection used by the app
      history_df <- tryCatch({
        conn <- values$db_connection
        if (is.null(conn) || !DBI::dbIsValid(conn)) {
          stop("Database connection is not available / 数据库连接不可用")
        }
        if (!DBI::dbExistsTable(conn, "update_history")) {
          stop("update_history table not found / 找不到 update_history 表")
        }
        DBI::dbGetQuery(conn, "
          SELECT
            database_name AS 'Database / 数据库',
            update_timestamp AS 'Time / 时间',
            update_type AS 'Type / 类型',
            records_added AS 'Added / 新增',
            records_removed AS 'Removed / 删除',
            records_modified AS 'Modified / 修改',
            source_file AS 'Source / 来源',
            user_notes AS 'Notes / 备注',
            CASE WHEN success = 1 THEN 'Success / 成功' ELSE 'Failed / 失败' END AS 'Status / 状态'
          FROM update_history
          ORDER BY update_timestamp DESC
          LIMIT 200
        ")
      }, error = function(e) {
        data.frame(Error = e$message, stringsAsFactors = FALSE)
      })

      output$update_history_container <- shiny::renderUI({
        if (nrow(history_df) == 0) {
          shiny::p("No update history found / 暂无更新记录",
                   style = "color: #666; font-style: italic;")
        } else {
          shiny::tagList(
            shiny::p(paste0("Showing ", nrow(history_df), " records / 显示 ", nrow(history_df), " 条记录"),
                     style = "color: #666; margin-bottom: 10px;"),
            shiny::tableOutput("update_history_table")
          )
        }
      })

      output$update_history_table <- shiny::renderTable({
        history_df
      }, striped = TRUE, hover = TRUE, bordered = TRUE, na = "",
         width = "100%", align = "l")

      # Also mirror a brief summary to the Quick Actions log
      output$quick_update_log <- shiny::renderPrint({
        if (ncol(history_df) == 1 && names(history_df)[1] == "Error") {
          cat("❌ 读取更新日志失败：\n", history_df$Error[1])
        } else if (nrow(history_df) == 0) {
          cat("📭 暂无更新记录")
        } else {
          cat("✅ 更新日志已显示（共 ", nrow(history_df), " 条记录）\n")
          cat("最近一次更新：\n")
          cat("  数据库：", history_df$`Database / 数据库`[1], "\n")
          cat("  时间：", history_df$`Time / 时间`[1], "\n")
          cat("  类型：", history_df$`Type / 类型`[1], "\n")
        }
      })
    })

    # Show all database tables in a modal dialog
    show_database_tables_modal <- function() {
      conn <- values$db_connection
      if (is.null(conn) || !DBI::dbIsValid(conn)) {
        showNotification("Database connection is not available / 数据库连接不可用", type = "error")
        return()
      }

      all_tables <- DBI::dbListTables(conn)
      if (length(all_tables) == 0) {
        showNotification("No tables found in database / 数据库中未找到表", type = "warning")
        return()
      }

      # Categorize tables
      known_internal <- c("chemicals", "sqlite_sequence")
      table_types <- vapply(all_tables, function(t) {
        if (t %in% values$available_databases) {
          "Database / 数据库"
        } else if (t %in% known_internal || grepl("_raw$|metadata|history|change_log", t)) {
          "System / 系统"
        } else {
          "Other / 其他"
        }
      }, character(1))

      # Get record counts
      counts <- vapply(all_tables, function(t) {
        tryCatch(
          DBI::dbGetQuery(conn, paste("SELECT COUNT(*) FROM", t))[[1, 1]],
          error = function(e) NA_integer_
        )
      }, integer(1))

      tables_df <- data.frame(
        `Table / 表名` = all_tables,
        `Type / 类型` = table_types,
        `Records / 记录数` = counts,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )

      shiny::showModal(shiny::modalDialog(
        title = "Database Tables / 数据库表列表",
        size = "l",
        easyClose = TRUE,
        footer = shiny::modalButton("Close / 关闭"),
        shiny::p(paste0("Total ", length(all_tables), " tables / 共 ", length(all_tables), " 张表"),
                 style = "color: #666; margin-bottom: 10px;"),
        shiny::tableOutput("db_tables_table")
      ))

      output$db_tables_table <- shiny::renderTable({
        tables_df
      }, striped = TRUE, hover = TRUE, bordered = TRUE, na = "",
         width = "100%", align = "l")

      output$quick_update_log <- shiny::renderPrint({
        cat("✅ 数据库列表已显示（共 ", length(all_tables), " 张表）\n")
        cat("其中可查询数据库：", sum(table_types == "Database / 数据库"), " 个\n")
      })
    }

    observeEvent(input$db_btn_tables, {
      show_database_tables_modal()
    })

    observeEvent(input$btn_view_db_list, {
      show_database_tables_modal()
    })

    # ---- 会话结束：断开连接 ----
    # Cleanup on session end
    # NOTE: this callback runs OUTSIDE any reactive consumer, so reading
    # reactive values directly errors ("Can't access reactive value ...");
    # wrap the access in shiny::isolate().
    session$onSessionEnded(function() {
      conn <- shiny::isolate(values$db_connection)
      if (!is.null(conn) && DBI::dbIsValid(conn)) {
        DBI::dbDisconnect(conn)
      }
    })
  }

  # ---- 启动 app（自己开浏览器，不依赖 launch.browser） ----
  # Run the app. We DO NOT rely on shiny::runApp's launch.browser here:
  #   - On Windows, launch.browser=TRUE only works when utils::browseURL()
  #     can find a default browser (the R "browser" option). In a VSCode/Git
  #     Bash session this is often empty and the call silently does nothing.
  #   - Newer R versions also deprioritise browseURL in favour of the
  #     "browser" option, which makes this even less reliable.
  # Instead we start the app with launch.browser=FALSE, then open the URL
  # ourselves via shell.exec() (the Windows shell will pick the user's
  # default browser, no "browser" option needed).
  url <- sprintf("http://127.0.0.1:%d", port)

  if (isTRUE(launch_browser)) {
    # Run app in the background and open the browser once the port is
    # accepting connections. If we can't poll the port, fall back to a
    # short delay + shell.exec().
    app_thread <- shiny::runApp(list(ui = ui, server = server),
                                port = port,
                                launch.browser = FALSE,
                                quiet = FALSE)

    # Wait until the port is listening (max ~10s).
    port_ready <- FALSE
    for (i in seq_len(50)) {
      Sys.sleep(0.2)
      con <- try(suppressWarnings(socketConnection("127.0.0.1", port,
                                                   timeout = 0.2,
                                                   blocking = FALSE)),
                 silent = TRUE)
      if (!inherits(con, "try-error") && !is.null(con)) {
        close(con)
        port_ready <- TRUE
        break
      }
    }

    if (!port_ready) {
      # Give Shiny a tiny bit more time even if our probe missed it.
      Sys.sleep(1)
    }

    opened <- try(shell.exec(url), silent = TRUE)
    if (inherits(opened, "try-error")) {
      # Last-resort fallback: utils::browseURL (works when "browser" is set).
      try(utils::browseURL(url), silent = TRUE)
    }
    return(invisible(app_thread))
  }

  shiny::runApp(list(ui = ui, server = server),
                port = port,
                launch.browser = FALSE)
}
