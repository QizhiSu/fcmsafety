# =============================================================================
# Inspector 的 server（全部响应式逻辑）
#
# 从 database_inspector_app.R 拆出（2026-09-14）。分区标记：
#   `# ---- 响应式状态 ----`  values 容器
#   `# ---- i18n ----`        文案表（新增界面文案必须进 texts）
#   `# ---- 主数据表 ----`    查询/筛选/DT
#   `# ---- 结构式面板 ----`
#   `# ---- 一键操作 ----`    run_quick_task + 各按钮 handler
#   `# ---- 筛查面板 ----`    上传清单 -> run_screening -> 报告下载
# ponytail: server 仍是一个函数——再往下拆需要给 handler 做依赖注入
# （values/session/get_text），收益不抵 diff 风险，等真要加第三个大面板再说。
# =============================================================================

  # ---- Server 定义 -----------------------------------------------------------
fcm_app_server <- function(input, output, session) {

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
