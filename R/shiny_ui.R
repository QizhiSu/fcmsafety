# =============================================================================
# Inspector 的 UI 构建（纯静态：CSS + JS + 顶栏 + 快捷按钮 + 侧栏 + 主布局）
#
# 从 database_inspector_app.R 拆出（2026-09-14）。fcm_app_ui() 不依赖任何
# 响应式状态，只拼装 fluidPage 并返回；全部动态行为在 shiny_server.R 的
# fcm_app_server() 里。分区标记：`# ---- UI 之 CSS / 之 JS / ...`。
# =============================================================================

  # ---- UI 定义（主题 CSS + 侧栏 + 选项卡） -----------------------------------
fcm_app_ui <- function() {
  fluidPage(
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
}
