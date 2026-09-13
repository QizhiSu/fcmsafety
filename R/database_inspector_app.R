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

  ui <- fcm_app_ui()
  server <- fcm_app_server
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
