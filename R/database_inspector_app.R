#' FCMSafety Database Inspector Shiny App
#'
#' A comprehensive Shiny web application for inspecting and visualizing
#' the FCMSafety SQLite database contents, including data validation,
#' filtering, and chemical structure visualization.
#'
#' @importFrom shiny fluidPage titlePanel sidebarLayout sidebarPanel mainPanel
#' @importFrom shiny selectInput numericInput checkboxInput actionButton
#' @importFrom shiny tabsetPanel tabPanel dataTableOutput plotOutput
#' @importFrom shiny renderDataTable renderPlot renderText renderUI
#' @importFrom shiny reactive observeEvent req
#' @importFrom DT datatable formatStyle
#' @importFrom DBI dbConnect dbDisconnect dbGetQuery dbListTables
#' @importFrom RSQLite SQLite
#' @importFrom ggplot2 ggplot aes geom_bar geom_col theme_minimal labs
#' @importFrom plotly ggplotly

#' Launch Database Inspector App
#'
#' Launches the Shiny web application for database inspection and visualization.
#'
#' @param port Port number for the Shiny app (default: 3838)
#' @param launch_browser Whether to launch browser automatically (default: TRUE)
#' @export
launch_database_inspector <- function(port = 3838, launch_browser = TRUE) {

  # Check if required packages are available
  required_packages <- c("shiny", "DT", "ggplot2", "plotly")
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

  # Load required libraries
  library(shiny)
  library(DT)
  library(ggplot2)
  library(plotly)

  # Define UI with modern styling and theme support
  ui <- fluidPage(
    # Custom CSS for modern UI and themes
    tags$head(
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

        /* Table header styling with center alignment and modern colors */
        table.dataTable thead th {
          white-space: nowrap !important;
          text-overflow: ellipsis !important;
          overflow: hidden !important;
          border-right: 1px solid !important;
          text-align: center !important;
          font-weight: 600 !important;
          padding: 12px 8px !important;
        }

        .light-theme table.dataTable thead th {
          background: #dbeafe !important;
          color: #1e40af !important;
          border-color: #93c5fd !important;
        }

        .dark-theme table.dataTable thead th {
          background: #1e3a8a !important;
          color: #ffffff !important;
          border-color: #3b82f6 !important;
        }

        .dark-theme table.dataTable thead th {
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
          height: 100vh;
          margin: 0;
          padding: 0;
          overflow-x: hidden;
        }

        .container-fluid {
          min-height: 100vh;
          display: flex;
          flex-direction: column;
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
        .dark-theme .dataTables_wrapper table.dataTable thead th {
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
        .dark-theme .dataTables_wrapper table.dataTable thead th {
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

        .filter-controls > div:nth-child(2) {
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

      # JavaScript for theme switching and functionality
      tags$script(HTML("
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
          var table = $('#data_table').DataTable();
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

        // Table interaction without DataTables API calls
        var currentSelectedRow = 0;

        // Click selection
        $(document).on('click', '#data_table tbody tr', function() {
          $('#data_table tbody tr').removeClass('selected');
          $(this).addClass('selected');
          currentSelectedRow = $(this).index();

          var rowIndex = currentSelectedRow + 1;
          Shiny.setInputValue('data_table_rows_selected', rowIndex, {priority: 'event'});
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

    # Theme and language controls
    div(class = "theme-controls",
      actionButton("theme_toggle", "🌙", class = "theme-btn",
                  title = "Toggle Dark/Light Theme"),
      actionButton("lang_toggle", "中/EN", class = "theme-btn",
                  title = "Toggle Language")
    ),

    titlePanel(textOutput("main_title")),

    sidebarLayout(
      sidebarPanel(
        width = 3,
        class = "sidebar-panel",

        h4(textOutput("database_title"), style = "text-align: left;"),
        selectInput("database", NULL,
                   choices = c("Loading..." = ""),
                   selected = ""),

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
              div(style = "flex: 1; margin: 0;",
                textInput("search_term", NULL, placeholder = "Search...")
              ),
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
            var currentLang = 'en';

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

            // Fix DataTables warnings and errors
            $.fn.dataTable.ext.errMode = 'none';

            // Enhanced keyboard navigation with proper DataTable handling
            $(document).on('keydown', function(e) {
              // Only handle if focus is not in an input field
              if (!$(e.target).is('input, textarea, select')) {
                try {
                  var table = $('#data_table').DataTable();
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
                var table = $('#data_table').DataTable();
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

                  if (smilesIndex >= 0 && rowData[smilesIndex]) {
                    Shiny.setInputValue('selected_smiles', rowData[smilesIndex], {priority: 'event'});
                    Shiny.setInputValue('selected_row_data', rowData, {priority: 'event'});
                  }
                }
              } catch (e) {
                // Silently handle errors
              }
            }

            // Enhanced row click handling with error prevention
            $(document).on('click', '#data_table tbody tr', function() {
              try {
                var table = $('#data_table').DataTable();
                var rowIndex = table.row(this).index();
                if (rowIndex !== undefined && rowIndex >= 0) {
                  table.rows().deselect();
                  table.row(this).select();
                  updateStructure(rowIndex);
                }
              } catch (e) {
                // Silently handle DataTable not ready
              }
            });

            // Handle search input changes for real-time record counting
            $(document).on('input', '#search_term', function() {
              setTimeout(function() {
                try {
                  var table = $('#data_table').DataTable();
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

  # Define Server
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

    # Reactive values
    values <- reactiveValues(
      db_connection = NULL,
      available_databases = c(),
      current_data = NULL,
      filtered_data = NULL,
      selected_smiles = NULL,
      selected_compound = NULL,
      zoom_level = 0.5,
      current_theme = "light",
      current_lang = "en",
      inchikey_filter_active = FALSE,
      search_term = ""
    )

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
        "reset_button" = list(en = "Reset", zh = "重置"),
        "inchikey_filter_show_all" = list(en = "Show All", zh = "显示全部"),
        "inchikey_filter_only" = list(en = "InChIKey Only", zh = "仅显示InChIKey"),
        "search_placeholder" = list(en = "Search...", zh = "搜索..."),
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
        "china_sml_name" = list(en = "CHINA SML", zh = "中国特定迁移限量")
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



    # Update search placeholder when language changes
    observeEvent(values$current_lang, {
      updateTextInput(session, "search_term",
                     placeholder = get_text("search_placeholder"))
    })

    # Initialize database connection and populate database choices
    observe({
      tryCatch({
        # Check if database file exists first
        db_path <- file.path(getwd(), "inst", "fcmsafety.db")
        if (!file.exists(db_path)) {
          showNotification(get_text("db_not_found"), type = "error")
          updateSelectInput(session, "database", choices = c("Database not found - run migration" = ""))
          return()
        }

        # Establish database connection
        db_path <- file.path(getwd(), "inst", "fcmsafety.db")
        values$db_connection <- DBI::dbConnect(RSQLite::SQLite(), db_path)

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
        main_tables <- tables[!grepl("_raw$|metadata|history|change_log", tables)]
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
            display_name <- if (db_name_key %in% names(get_text(""))) {
              get_text(db_name_key)
            } else {
              toupper(table)
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
          updateSelectInput(session, "database", choices = choices, selected = main_tables[1])
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





    # Search term handling
    observeEvent(input$search_term, {
      values$search_term <- input$search_term
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

        # Apply search filter
        if (!is.null(values$search_term) && values$search_term != "") {
          # Search across all text columns
          text_cols <- sapply(data, function(x) is.character(x) || is.factor(x))
          if (any(text_cols)) {
            search_pattern <- paste0("(?i)", values$search_term)
            matches <- apply(data[, text_cols, drop = FALSE], 1, function(row) {
              any(grepl(search_pattern, row, perl = TRUE))
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

    # Data table
    output$data_table <- DT::renderDataTable({
      req(input$database)

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

        # Execute query to get all data with error handling
        query <- paste("SELECT * FROM", table_name)
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

        # Apply search filter
        if (!is.null(values$search_term) && values$search_term != "") {
          text_cols <- sapply(filtered_data, function(x) is.character(x) || is.factor(x))
          if (any(text_cols)) {
            search_pattern <- paste0("(?i)", values$search_term)
            matches <- apply(filtered_data[, text_cols, drop = FALSE], 1, function(row) {
              any(grepl(search_pattern, row, perl = TRUE))
            })
            filtered_data <- filtered_data[matches, ]
          }
        }

        values$filtered_data <- filtered_data
        data <- filtered_data

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

        # DataTable configuration with dynamic record counter
        dt <- DT::datatable(data_with_index,
                     options = list(
                       pageLength = 25,
                       scrollX = TRUE,
                       scrollY = "600px",
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
                     rownames = FALSE)

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

    # Handle table row selection
    observeEvent(input$data_table_rows_selected, {
      if (length(input$data_table_rows_selected) > 0 && !is.null(values$current_data)) {
        selected_row <- input$data_table_rows_selected[1]
        selected_data <- values$current_data[selected_row, ]

        # Find SMILES column
        smiles_cols <- names(selected_data)[grepl("SMILES|smiles", names(selected_data), ignore.case = TRUE)]

        if (length(smiles_cols) > 0) {
          smiles_value <- selected_data[[smiles_cols[1]]]
          if (!is.na(smiles_value) && smiles_value != "" && smiles_value != "NULL") {
            values$selected_smiles <- smiles_value
            values$selected_compound <- selected_data
            values$zoom_level <- 0.5  # Reduced default zoom level for better initial display
            updateStructureDisplay()
          } else {
            values$selected_smiles <- NULL
            values$selected_compound <- selected_data
            updateStructureDisplay()
          }
        } else {
          values$selected_smiles <- NULL
          values$selected_compound <- selected_data
          updateStructureDisplay()
        }
      }
    })

    # Handle SMILES input from JavaScript
    observeEvent(input$selected_smiles, {
      if (!is.null(input$selected_smiles) && input$selected_smiles != "") {
        values$selected_smiles <- input$selected_smiles
        values$zoom_level <- 0.5
        updateStructureDisplay()
      }
    })

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
        placeholder_msg <- if (values$current_lang == "zh") {
          "从表格中选择化合物以查看其结构"
        } else {
          "Select a compound from the table to view its structure"
        }

        session$sendCustomMessage("updateStructure", list(
          content = paste0("<p style='text-align: center; color: #6c757d; font-style: italic; padding: 20px;'>",
                          placeholder_msg, "</p>"),
          smiles = "",
          zoom = 1.0
        ))
      }
    }





    # Cleanup on session end
    session$onSessionEnded(function() {
      if (!is.null(values$db_connection)) {
        DBI::dbDisconnect(values$db_connection)
      }
    })
  }

  # Run the app
  shiny::runApp(list(ui = ui, server = server),
                port = port,
                launch.browser = launch_browser)
}
