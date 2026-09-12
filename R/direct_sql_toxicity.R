# =============================================================================
# 核心：直接查 SQLite 给化合物定毒性等级（assign_toxicity）
#
# 这是用户最常调的一个函数，也是全包的收口处。它做四件事：
#   1) 按 InChIKey 去 SVHC / CMR / CMR_suspect / EDC / IARC / EU SML /
#      China SML 各表匹配（每个源一个 query_*_data()，一表一函数）
#   2) 把多行命中的结果按"取最严"收成一行（summarise_*() 系列）
#   3) 按规则表 inst/toxicity_levels.png 算出 Toxic_level（I–V）与依据
#      （compute_toxicity_levels()，纯函数，无 IO）
#   4) 可选：跑组条目筛查（Group_hits / Group_IARC / Group_review），
#      并把够可信的 IARC 组命中喂进定级
#
# 设计取向（2026-09-10 定案，见 docs/adr/0007、0008）：
#   - "查不到"不等于"安全"。全无证据的行 Toxic_level 留空 "-"，不冒充 I 级。
#     I 级唯一来源是 1.8 < SML <= 60，所以留空只表示"什么都没找到"。
#   - 查询失败要看得见。query_*() 出错不 stop，而是把错误挂在返回值的
#     attr(x, "query_error") 上，由 assign_toxicity() 收成 query_issues
#     并写进 Issues 表；否则一张表查挂了、结果看起来只是"少匹配到一些"，
#     比报错更危险。
#   - group_membership 默认 FALSE。精确 InChIKey 匹配看不见"一大类"条目，
#     但开启后结果会变，为不惊动既有脚本默认关。
# =============================================================================

#' Direct SQL-Based Toxicity Assignment
#'
#' This module provides a clean architecture where assign_toxicity() queries
#' the SQLite database directly without loading data into global variables.
#' This eliminates global variable pollution and provides better performance.
#'
#' Enhanced version of assign_toxicity that queries SQLite database directly
#' without loading data into global variables. Maintains the exact same interface
#' and functionality as the original function while providing better architecture.
#'
#' @importFrom DBI dbConnect dbDisconnect dbGetQuery
#' @importFrom RSQLite SQLite
#' @importFrom dplyr mutate case_when na_if
#' @param data Your data containing at least InChIKey
#' @param toxtree_result Path to the Toxtree result CSV (default:
#'   "toxtree_results.csv"). Cramer classification is optional: if the file
#'   exists it is used as-is (no rerun); if it does not exist and \code{data}
#'   has a SMILES column, \code{\link{run_toxtree}()} is called automatically
#'   to generate it (one-time ~81 MB download + Java 8+ needed on first run);
#'   if it does not exist and \code{data} has no SMILES column, Cramer
#'   classification is skipped with a message and only regulatory-list
#'   matching is performed.
#' @param check_updates Logical, whether to check for available updates (default: FALSE)
#' @param auto_update Logical, whether to automatically apply available updates
#'   (default: FALSE; only relevant when check_updates = TRUE)
#' @param show_update_details Logical, whether to show detailed update information (default: TRUE)
#' @param output_file Optional output path (default: NULL = do not write a file).
#'   A `.xlsx` path produces a styled workbook via
#'   \code{\link{export_toxicity_report}()} (Results / Summary / Unassigned /
#'   Issues sheets); a `.csv` path produces the old flat file.
#' @param db_path Path to an alternative SQLite database (default: NULL =
#'   package default database). Intended for tests and custom deployments.
#' @param group_membership Logical, whether to also look up group-level entries
#'   (default: FALSE). Exact InChIKey matching cannot see entries that describe
#'   a family rather than a structure — "Cadmium and cadmium compounds",
#'   "Chlorinated paraffins", "Nonylphenol and its ethoxylates". With
#'   \code{TRUE}, \code{\link{assign_group_membership_table}()} runs over the
#'   same input and its hits are reported in \code{Group_hits} /
#'   \code{Group_IARC} / \code{Group_review}; IARC group hits that are confident
#'   enough also feed the toxicity tier. Off by default so the numbers produced
#'   by exact matching do not change under existing scripts — see
#'   \code{docs/adr/0008-20260910-report-export-and-failure-visibility.md} for
#'   the measured impact.
#' @return A data.frame or tibble with toxicity assigned (same as original function).
#'   Besides the existing regulatory flags (SVHC / CMR / CMR_suspect / EDC / IARC /
#'   EU_SML / China_SML / Cramer_rules) it also carries \code{CMR_H_codes}: the
#'   CMR evidence codes that drive the toxicity tier, i.e. any of H340/H350/H360
#'   (tier V) and H341/H351/H361 (tier IV) found in the \code{cmr} table, joined
#'   with \code{"; "} and listed tier-V first, or \code{"-"} when there are none.
#'
#'   \code{Toxic_level} grades each compound I–V by the rules in
#'   \code{inst/toxicity_levels.png} (when several rules hit, the strictest wins),
#'   and \code{Toxic_level_basis} names the rule(s) behind that grade. A compound
#'   with no evidence at all gets \code{"-"} rather than level I — the rules award
#'   tier I only for \code{1.8 < SML <= 60}, so a blank means "nothing found",
#'   not "found harmless". For substances in several rows the strictest value
#'   wins: the smallest SML (EU and China compared together) and, for IARC, the
#'   most severe group (1 &gt; 2A &gt; 2B &gt; 3).
#' @export
#' @encoding UTF-8
assign_toxicity <- function(data, toxtree_result = "toxtree_results.csv",
                           check_updates = FALSE, auto_update = FALSE, show_update_details = TRUE,
                           output_file = NULL, db_path = NULL,
                           group_membership = FALSE) {

  message("🧪 FCMSafety Toxicity Assignment with Direct SQL Queries")
  message(paste(rep("=", 60), collapse = ""))

  # Step 1: Check for manually added list files if requested
  # 旧线探测（check_available_updates / update_databases_interactive）已删除，
  # 由 check_manual_lists() 取代：检测 inst/ 下人工放入的新清单并询问是否更新。
  # auto_update = TRUE 时自动应用全部新清单（不逐个询问）。
  if (check_updates) {
    message("🔍 Checking for manually added list files...")
    check_manual_lists(ask = !auto_update, auto_apply = auto_update)
  }

  # Step 2: Verify SQLite database exists and is accessible
  tryCatch({
    db_status <- check_database_status(db_path = db_path)
    if (!db_status$initialized) {
      stop("SQLite database not initialized. Run setup_fcmsafety_database() first.")
    }
  }, error = function(e) {
    stop("Database error: ", e$message, "\nPlease ensure the SQLite database is properly set up.")
  })

  # Step 2b: Validate input shape (before touching toxtree files, so that
  # data without an InChIKey column fails with the right error first)
  if (!"InChIKey" %in% names(data)) {
    stop("Input data must contain an 'InChIKey' column")
  }

  # Step 3: Read and validate toxtree results (optional since P1-③)
  # 方案 A（模块化）：toxtree_result 文件不存在、且输入含 SMILES 列时，
  # 自动调用 run_toxtree() 现场生成（run_toxtree 仍是独立导出的函数，
  # 这里只是调用它，不内联其实现）；文件已存在则直接读取，不重复跑。
  # P1-③ 变更：文件不存在且数据无 SMILES 列时不再 stop——跳过 Cramer
  # 分级（无 SMILES 本就无从计算），仅做法规清单匹配。
  tox <- NULL
  if (file.exists(toxtree_result)) {
    tox <- utils::read.csv(toxtree_result)
  } else if ("SMILES" %in% names(data)) {
    message("🔄 Toxtree result file not found: ", toxtree_result)
    message("   Automatically running Toxtree to generate it...")
    # Toxtree 运行失败（无 Java / jar 下载失败 / 个别环境问题）不拖垮整个筛查：
    # 法规清单匹配照常，Cramer 列留空——与"无 SMILES 跳过分级"（P1-③）同一策略。
    tox <- tryCatch({
      run_toxtree(data, output = toxtree_result)
      utils::read.csv(toxtree_result)
    }, error = function(e) {
      message("⚠️  Toxtree failed: ", conditionMessage(e))
      message("   Continuing with regulatory matching only; Cramer columns stay blank.")
      NULL
    })
  } else {
    message("ℹ️  Toxtree result file not found: ", toxtree_result,
            "\n   Input data has no SMILES column, so Toxtree cannot be run. ",
            "Skipping Cramer classification; regulatory list matching only.")
  }

  if (!"SMILES" %in% names(data)) {
    message("⚠️  No SMILES column found in input data. Cramer rules assignment may be limited.")
  }

  # Step 4: Perform toxicity assignment using direct SQL queries
  message("\n🔬 Performing toxicity assignment with direct SQL queries...")

  # Get database connection
  resolved_db_path <- if (!is.null(db_status$database_path)) {
    db_status$database_path
  } else {
    .resolve_db_path(db_path)
  }
  con <- get_db_connection(db_path = db_path)
  on.exit(DBI::dbDisconnect(con))

  # 每个库查完都登记一条状态，最后由 .print_query_issues() 汇总、挂到返回值上、
  # 并写进 Excel 的 Issues 表。没有这套东西时，任何一个库查询失败都只是几行
  # message，最终表格照常返回一张全是 "-" 的结果，看起来像"这些物质都干净"。
  #
  # 表存在但 0 行，与按 InChIKey 查不到，是两件事：前者说明这个库根本没数据
  # （建库没跑完 / 换了个空库），必须显式提示；后者是正常结果。
  query_issues <- list()
  note_issue <- function(source, status, rows = NA_integer_, message = "") {
    query_issues[[length(query_issues) + 1]] <<- data.frame(
      Source = source, Status = status, Rows = as.integer(rows),
      Message = message, stringsAsFactors = FALSE
    )
  }

  source_tables <- c(svhc = "svhc", cmr = "cmr", cmr_codes = "cmr",
                     cmr_suspect = "cmr_suspect", edc = "edc", iarc = "iarc",
                     eu_sml = "eu_sml", eu_sml_group = "eu_sml_group",
                     china_sml = "china_sml")
  already_noted <- function(source) {
    any(vapply(query_issues, function(d) identical(d$Source, source), logical(1)))
  }
  register_query <- function(source, obj, rows) {
    err <- attr(obj, "query_error")
    if (is.null(err) || already_noted(source)) return(invisible(NULL))
    note_issue(source, "failed", rows, err)
    invisible(NULL)
  }
  table_counts <- db_status$table_counts
  if (!is.null(table_counts)) {
    for (src in names(source_tables)) {
      tbl <- source_tables[[src]]
      n <- suppressWarnings(as.numeric(table_counts[[tbl]]))
      if (is.na(n)) {
        note_issue(src, "missing", NA_integer_,
                   paste0("Table '", tbl,
                          "' does not exist in the database; this source contributes no evidence."))
      } else if (n == 0) {
        note_issue(src, "empty", 0,
                   paste0("Table '", tbl,
                          "' exists but holds no rows; this source contributes no evidence."))
      }
    }
    empty_sources <- vapply(query_issues, function(d) d$Source, character(1))
    if (length(empty_sources)) {
      message("⚠️  ", length(empty_sources),
              " database source(s) are unusable: ",
              paste(unique(empty_sources), collapse = ", "),
              "\n   (table missing or empty - see the Issues sheet of the report / ",
              "attr(result, 'query_issues'))")
    }
  }

  # Extract InChIKeys for SQL queries
  inchikeys <- data$InChIKey[!is.na(data$InChIKey)]

  if (length(inchikeys) == 0) {
    message("⚠️  No valid InChIKeys found in input data")
    return(data)
  }

  # Create SQL IN clause for InChIKeys
  inchikey_list <- paste0("'", inchikeys, "'", collapse = ",")

  # Query each database directly
  message("📊 Querying regulatory databases...")

  # SVHC database
  svhc_matches <- query_database_matches(con, "svhc", inchikey_list, "InChIKey")
  register_query("svhc", svhc_matches, length(svhc_matches))
  message("   SVHC: ", length(svhc_matches), " matches found")

  # CMR database
  cmr_matches <- query_database_matches(con, "cmr", inchikey_list, "InChIKey")
  register_query("cmr", cmr_matches, length(cmr_matches))
  message("   CMR: ", length(cmr_matches), " matches found")

  # CMR 危险说明代码：定级必需的证据列（见 .cmr_h_codes）
  cmr_data <- query_cmr_data(con, inchikey_list)
  register_query("cmr_codes", cmr_data, nrow(cmr_data))

  # CMR Suspect database
  cmr_suspect_matches <- query_database_matches(con, "cmr_suspect", inchikey_list, "InChIKey")
  register_query("cmr_suspect", cmr_suspect_matches, length(cmr_suspect_matches))
  message("   CMR Suspect: ", length(cmr_suspect_matches), " matches found")

  # EDC database
  edc_matches <- query_database_matches(con, "edc", inchikey_list, "InChIKey")
  register_query("edc", edc_matches, length(edc_matches))
  message("   EDC: ", length(edc_matches), " matches found")

  # IARC database with group classification
  # 同一物质可能有多行且分组冲突（实测 11 个键，如 "2B,1" / "1,3" / "2A,3"）。
  # 直接 match() 取首行等于看数据库行序，取到 3 会把整条 IARC 证据抹掉。
  iarc_data <- query_iarc_data(con, inchikey_list)
  register_query("iarc", iarc_data, nrow(iarc_data))
  iarc_summary <- summarise_iarc_groups(iarc_data)
  message("   IARC: ", nrow(iarc_summary), " compounds matched")

  # IARC 的 "(see X)" 交叉引用：源表里这些行的 group_classification 是空的，
  # 分级挂在 X 上（如 Gallium arsenide 的评价挂在 "Arsenic and inorganic
  # arsenic compounds" 组 1 下）。此前没有代码读它，查砷化镓会返回"无证据"。
  # 只填空缺：同键兄弟行已经给出分组的不动，理由见 apply_iarc_see_aliases()。
  iarc_alias <- tryCatch(
    query_iarc_see_alias_map(db_path),
    error = function(e) {
      # iarc 表整体缺失时不必重复登记 —— 上面的 table_counts 已把 iarc 记成
      # missing，这里再报一条只会让 Issues 表出现两条同一原因的记录。
      if (!grepl("no such table", conditionMessage(e), ignore.case = TRUE)) {
        note_issue("iarc_alias", "failed", NA_integer_, conditionMessage(e))
      }
      NULL
    }
  )
  if (!is.null(iarc_alias) && nrow(iarc_alias) > 0) {
    iarc_alias <- iarc_alias[iarc_alias$InChIKey %in% inchikeys, , drop = FALSE]
  }
  iarc_summary <- apply_iarc_see_aliases(iarc_summary, iarc_alias)
  n_alias <- attr(iarc_summary, "see_alias_added")
  if (!is.null(n_alias) && n_alias > 0) {
    message("   IARC (see X) aliases resolved: ", n_alias, " compound(s)")
  }

  # EU SML database
  eu_sml_data <- query_eu_sml_data(con, inchikey_list)
  register_query("eu_sml", eu_sml_data, nrow(eu_sml_data))
  # 组限值表是按 group_no 组织的（38 行的 InChIKey 全是 NULL），必须整表取回
  # 再按组号关联。此前按 InChIKey 过滤，永远返回 0 行，导致 eu_sml 里 126 个
  # 带组号、其中 118 个个体 SML 为空的物质拿不到任何 SML。
  eu_sml_group_all <- query_eu_sml_group_data(con)
  register_query("eu_sml_group", eu_sml_group_all, nrow(eu_sml_group_all))
  eu_sml_summary <- summarise_eu_sml(eu_sml_data, eu_sml_group_all)
  message("   EU SML: ", nrow(eu_sml_summary), " compounds matched")

  # China SML database
  china_sml_data <- query_china_sml_data(con, inchikey_list)
  register_query("china_sml", china_sml_data, nrow(china_sml_data))
  china_sml_summary <- summarise_china_sml(china_sml_data)
  message("   China SML: ", nrow(china_sml_summary), " compounds matched")

  # Step 5: Apply toxicity assignment logic using SQL results
  #
  # 顺序：先按 InChIKey 把多行汇总成一行（取最严），再做列对齐，最后算等级。
  # 汇总与定级函数都在文件末尾，可以脱离数据库单独测试。
  eu_sml_num <- eu_sml_summary$sml[match(data$InChIKey, eu_sml_summary$InChIKey)]
  eu_sml_grp <- eu_sml_summary$from_group[match(data$InChIKey, eu_sml_summary$InChIKey)]
  eu_sml_grp[is.na(eu_sml_grp)] <- FALSE   # match 未命中返回 NA，按 FALSE 处理
  china_sml_num <- china_sml_summary$sml[match(data$InChIKey, china_sml_summary$InChIKey)]
  iarc_group <- iarc_summary$group_classification[match(data$InChIKey, iarc_summary$InChIKey)]

  cramer_vals <- if ("SMILES" %in% names(data) && !is.null(tox)) {
    tox$Cramer.rules[match(data$SMILES, tox$SMILES)]
  } else {
    rep(NA_character_, nrow(data))
  }

  svhc_flag <- data$InChIKey %in% svhc_matches
  cmr_flag <- data$InChIKey %in% cmr_matches
  cmr_suspect_flag <- data$InChIKey %in% cmr_suspect_matches
  edc_flag <- data$InChIKey %in% edc_matches
  cmr_codes <- extract_cmr_h_codes(
    cmr_data$hazard_statement_codes[match(data$InChIKey, cmr_data$InChIKey)]
  )

  # 组级条目（group_membership = TRUE 时）：精确 InChIKey 匹配看不见"镉及镉化合物"
  # "氯化石蜡"这类族条目。IARC 的组条目带分组，可以参与定级；CMR / SVHC 的 UVCB
  # 命中本身不带危险码，只作记录，不擅自升级——升级会引入无法核实的误报。
  group_hits_col <- rep(NA_character_, nrow(data))
  group_iarc_col <- rep(NA_character_, nrow(data))
  group_review_col <- rep(NA_character_, nrow(data))
  if (isTRUE(group_membership)) {
    message("\n🧩 Looking up group-level entries...")
    grp_hits <- tryCatch(
      assign_group_membership_table(data, source = "all", db_path = db_path),
      error = function(e) {
        note_issue("group_membership", "failed", NA_integer_, conditionMessage(e))
        message("Warning: group membership lookup failed: ", conditionMessage(e))
        NULL
      }
    )
    if (!is.null(grp_hits)) {
      grp <- summarise_group_hits(grp_hits)
      idx <- match(seq_len(nrow(data)), grp$input_index)
      group_hits_col <- grp$Group_hits[idx]
      group_iarc_col <- grp$Group_IARC[idx]
      group_review_col <- grp$Group_review[idx]
      message("   group entries matched: ", sum(!is.na(group_hits_col)), " row(s); ",
              sum(!is.na(group_iarc_col)), " with an IARC group")
      n_review <- sum(!is.na(group_review_col))
      if (n_review > 0) {
        note_issue("group_membership", "warn", n_review,
                   paste0(n_review, " row(s) matched a group entry at confidence ",
                          "'manual_review'; recorded in Group_review, not used for grading."))
      }
      grp_errs <- attr(grp_hits, "errors")
      if (!is.null(grp_errs) && nrow(grp_errs) > 0) {
        note_issue("group_membership", "warn", nrow(grp_errs),
                   paste0(nrow(grp_errs),
                          " row(s) could not be resolved to an identity for group lookup."))
      }
    }
  }

  # 毒性等级 I–V（规则表 inst/toxicity_levels.png），多条件命中取最严。
  # 什么都没命中的行留空，不冒充 I 级——见 compute_toxicity_levels()。
  tox_levels <- compute_toxicity_levels(
    svhc = svhc_flag,
    cmr_h_codes = cmr_codes,
    cmr_suspect = cmr_suspect_flag,
    edc = edc_flag,
    iarc = iarc_group,
    sml_eu = eu_sml_num,
    sml_cn = china_sml_num,
    cramer_rules = cramer_vals,
    iarc_extra = group_iarc_col
  )

  result_data <- data %>%
    dplyr::mutate(
      Cramer_rules = cramer_vals,
      # Check if compounds present in any of the databases using SQL results
      SVHC = dplyr::case_when(svhc_flag ~ "Y"),
      CMR = dplyr::case_when(cmr_flag ~ "Y"),
      # CMR 定级相关的 H 码（V 类在前），仅"在 cmr 表中"不足以定级，
      # 需要具体码区分致癌/致突变/生殖毒性的 1 类还是 2 类。
      CMR_H_codes = cmr_codes,
      CMR_suspect = dplyr::case_when(cmr_suspect_flag ~ "Y"),
      EDC = dplyr::case_when(edc_flag ~ "Y"),
      IARC = dplyr::na_if(iarc_group, "3"),
      EU_SML = eu_sml_num,
      China_SML = china_sml_num,
      # 组级条目命中（group_membership = TRUE 时才有内容）
      Group_hits = group_hits_col,
      Group_IARC = group_iarc_col,
      Group_review = group_review_col,
      # 等级与依据放最后：前面是证据，这两列是结论。
      Toxic_level = tox_levels$Toxic_level,
      Toxic_level_basis = tox_levels$Toxic_level_basis
    ) %>%
    dplyr::mutate(
      # 组限值打星号（沿用原约定）。必须先判 !is.na(EU_SML)——原先无条件
      # paste0(EU_SML, "*")，SML 取不到时会写出字面量 "NA*"，且因为它是字符串，
      # 后面的 result_data[is.na(...)] <- "-" 也接不住。
      EU_SML = dplyr::case_when(
        !is.na(EU_SML) & eu_sml_grp ~ paste0(EU_SML, "*"),
        TRUE ~ as.character(EU_SML)
      ),
      China_SML = as.character(China_SML)
    )

  # Replace NA with "-"
  result_data[is.na(result_data)] <- "-"

  # Relocate toxicity columns (same logic as original)
  if ("Flavornet" %in% colnames(result_data)) {
    result_data <- dplyr::relocate(result_data, Cramer_rules:Toxic_level_basis, .after = Flavornet)
  } else if ("CAS_retrieved" %in% colnames(result_data)) {
    result_data <- dplyr::relocate(result_data, Cramer_rules:Toxic_level_basis, .after = CAS_retrieved)
  } else if ("ExactMass" %in% colnames(result_data)) {
    result_data <- dplyr::relocate(result_data, Cramer_rules:Toxic_level_basis, .after = ExactMass)
  }

  # Step 6: Provide summary of results
  message("✅ Toxicity assignment completed!")

  # Count assignments
  svhc_count <- sum(result_data$SVHC == "Y", na.rm = TRUE)
  cmr_count <- sum(result_data$CMR == "Y", na.rm = TRUE)
  cmr_suspect_count <- sum(result_data$CMR_suspect == "Y", na.rm = TRUE)
  edc_count <- sum(result_data$EDC == "Y", na.rm = TRUE)
  iarc_count <- sum(result_data$IARC != "-", na.rm = TRUE)
  eu_sml_count <- sum(result_data$EU_SML != "-", na.rm = TRUE)
  china_sml_count <- sum(result_data$China_SML != "-", na.rm = TRUE)
  group_hit_count <- sum(result_data$Group_hits != "-", na.rm = TRUE)
  group_review_count <- sum(result_data$Group_review != "-", na.rm = TRUE)
  # CMR 定级证据：V 类码 -> 等级 V，IV 类码 -> 等级 IV
  cmr_v_count <- sum(grepl("H340|H350|H360", result_data$CMR_H_codes), na.rm = TRUE)
  cmr_iv_count <- sum(grepl("H341|H351|H361", result_data$CMR_H_codes), na.rm = TRUE)
  # 在 cmr 表里却拿不到任何 V 类码 = 数据可疑，必须显式提示，不能静默
  cmr_odd_count <- sum(result_data$CMR == "Y" &
                         !grepl("H340|H350|H360", result_data$CMR_H_codes))
  # 毒性等级分布。"未定级"单独计数，不并进 I 级——规则表里 I 级唯一的来源是
  # 1.8 < SML <= 60，没有任何证据的行不属于任何等级，混进 I 级等于假装安全。
  tier_counts <- vapply(c("V", "IV", "III", "II", "I"),
                        function(t) sum(result_data$Toxic_level == t, na.rm = TRUE),
                        integer(1))
  unassigned_count <- sum(result_data$Toxic_level == "-", na.rm = TRUE)

  message("\n📊 Toxicity Assignment Summary:")
  message("   Total compounds processed: ", nrow(result_data))
  message("   SVHC matches: ", svhc_count)
  message("   CMR matches: ", cmr_count)
  message("   CMR with H340/H350/H360 (tier V evidence): ", cmr_v_count)
  message("   CMR with H341/H351/H361 (tier IV evidence): ", cmr_iv_count)
  message("   CMR suspect matches: ", cmr_suspect_count)
  message("   EDC matches: ", edc_count)
  message("   IARC matches: ", iarc_count)
  message("   EU SML matches: ", eu_sml_count)
  message("   China SML matches: ", china_sml_count)
  if (isTRUE(group_membership)) {
    message("   Group entries matched: ", group_hit_count,
            " (of which ", group_review_count, " need manual review)")
  }

  message("\n🎯 Toxicity level (I–V):")
  for (t in names(tier_counts)) {
    message("   Level ", t, ": ", tier_counts[[t]])
  }
  message("   Not assigned (no evidence): ", unassigned_count)

  if (cmr_odd_count > 0) {
    message("⚠️  ", cmr_odd_count, " row(s) are listed in 'cmr' but carry none of ",
            "H340/H350/H360; their CMR tier evidence is missing.")
    note_issue("cmr", "warn", cmr_odd_count,
               "Row(s) present in 'cmr' but with none of H340/H350/H360; the CMR tier evidence is missing.")
  }

  total_matches <- svhc_count + cmr_count + cmr_suspect_count + edc_count + iarc_count + eu_sml_count + china_sml_count
  message("   Total database matches: ", total_matches)

  if (total_matches > 0) {
    message("⚠️  ", total_matches, " compounds found in regulatory databases - review carefully!")
  } else {
    message("✅ No compounds found in regulatory databases")
  }

  # 把查询问题摆在显眼位置。这些信息以前只以 message 形式散落在上面几十行输出里，
  # 表格看起来一切正常，实际整列证据可能是空的。
  query_report <- .query_issue_table(query_issues)
  n_failed <- sum(query_report$Status == "failed")
  if (n_failed > 0) {
    message("\n‼️  ", n_failed, " database quer(ies) FAILED - the corresponding ",
            "columns are empty and do not mean 'nothing found':")
    for (i in which(query_report$Status == "failed")) {
      message("     ", query_report$Source[i], ": ", query_report$Message[i])
    }
  }
  if (n_failed == length(source_tables)) {
    warning("Every regulatory query failed; the result table carries no evidence at all.",
            call. = FALSE)
  }

  message(paste(rep("=", 60), collapse = ""))

  run_info <- data.frame(
    Key = c("generated_at", "package_version", "database", "database_mtime",
            "input_rows", "resolved_db_path"),
    Value = c(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
              as.character(utils::packageVersion("fcmsafety")),
              basename(resolved_db_path),
              if (file.exists(resolved_db_path)) {
                format(file.info(resolved_db_path)$mtime, "%Y-%m-%d %H:%M:%S")
              } else {
                "NOT FOUND"
              },
              as.character(nrow(result_data)),
              resolved_db_path),
    stringsAsFactors = FALSE
  )

  # 属性必须在所有 dplyr 操作之后挂：mutate()/relocate() 会把未知属性丢掉。
  attr(result_data, "query_issues") <- query_report
  attr(result_data, "run_info") <- run_info

  # 输出文件：.xlsx 出带样式的多表工作簿，.csv 保持原来的扁平格式。
  if (!is.null(output_file)) {
    message("💾 Saving result to ", output_file)
    if (tolower(tools::file_ext(output_file)) %in% c("xlsx", "xlsm")) {
      export_toxicity_report(
        result_data, output_file,
        summary = .report_summary_table(
          result_data,
          run_info = run_info,
          counts = c(SVHC = svhc_count, CMR = cmr_count,
                     CMR_suspect = cmr_suspect_count, EDC = edc_count,
                     IARC = iarc_count, EU_SML = eu_sml_count,
                     China_SML = china_sml_count)
        ),
        issues = query_report
      )
    } else {
      utils::write.csv(result_data, output_file, row.names = FALSE)
    }
  }

  return(result_data)
}

# 把零散的 issue 记录并成一张表。data.frame() 直接 rbind 空 list 会报错，
# 所以空表要显式构造。
.query_issue_table <- function(issues) {
  empty <- data.frame(Source = character(0), Status = character(0),
                      Rows = integer(0), Message = character(0),
                      stringsAsFactors = FALSE)
  if (length(issues) == 0) return(empty)
  out <- do.call(rbind, issues)
  row.names(out) <- NULL
  out
}

# ---- 查询基础设施：失败可见化 -----------------------------------------------

#' 给查询结果挂上错误信息
#'
#' 各 `query_*()` helper 查询失败时返回空结果并 `message()` 一句警告，调用方
#' 随后把"空"当成"没命中"。结果就是数据库出问题时，用户看到的仍是一张格式
#' 完整、只多了几行 message 的表格，很容易误以为这些物质干净。这里把错误
#' 挂到返回对象上带出去，由 [assign_toxicity()] 汇总进 Issues 表。
#'
#' @param x 查询失败时要返回的空对象
#' @param message 原始错误信息
#' @return x，附带 `query_error` 属性
#' @keywords internal
#' @encoding UTF-8
.attach_query_error <- function(x, message) {
  attr(x, "query_error") <- message
  x
}

# ---- 各法规表查询（一表一函数） ---------------------------------------------
#
# 每个 query_*_data() 只干一件事：拿一批 InChIKey 去某张表捞命中行。
# 出错时不 stop，而是用 .attach_query_error() 把错误信息挂到返回值的 attr 上
# （见文件头"查询失败要看得见"），由 assign_toxicity() 收进 Issues 表。

#' Query Database Matches
#'
#' Helper function to query a database table for InChIKey matches.
#'
#' @param con Database connection
#' @param table_name Name of the table to query
#' @param inchikey_list Comma-separated list of InChIKeys for SQL IN clause
#' @param key_column Name of the InChIKey column in the table
#' @return Vector of matching InChIKeys
#' @encoding UTF-8
query_database_matches <- function(con, table_name, inchikey_list, key_column = "InChIKey") {
  query <- paste0("SELECT DISTINCT ", key_column, " FROM ", table_name,
                  " WHERE ", key_column, " IN (", inchikey_list, ")")

  result <- tryCatch({
    DBI::dbGetQuery(con, query)
  }, error = function(e) {
    message("Warning: Error querying ", table_name, ": ", e$message)
    .attach_query_error(data.frame(), e$message)
  })

  if (nrow(result) > 0) {
    return(result[[key_column]])
  }
  # 查询失败时结果为空，错误信息挂在返回的空向量上带出去（调用方据此登记
  # 状态），否则"查不到"和"查挂了"在调用方看起来一模一样。
  err <- attr(result, "query_error")
  out <- character(0)
  if (!is.null(err)) attr(out, "query_error") <- err
  out
}

#' Query IARC Data
#'
#' Helper function to query IARC database for group classifications.
#'
#' @param con Database connection
#' @param inchikey_list Comma-separated list of InChIKeys for SQL IN clause
#' @return Data frame with InChIKey and group_classification
#' @encoding UTF-8
query_iarc_data <- function(con, inchikey_list) {
  query <- paste0("SELECT InChIKey, group_classification FROM iarc WHERE InChIKey IN (", inchikey_list, ")")

  result <- tryCatch({
    DBI::dbGetQuery(con, query)
  }, error = function(e) {
    message("Warning: Error querying IARC: ", e$message)
    .attach_query_error(
      data.frame(InChIKey = character(0), group_classification = character(0)), e$message)
  })

  return(result)
}

#' Query EU SML Data
#'
#' Helper function to query EU SML database for SML values and groups.
#'
#' @param con Database connection
#' @param inchikey_list Comma-separated list of InChIKeys for SQL IN clause
#' @return Data frame with InChIKey, sml, and sml_group
#' @encoding UTF-8
query_eu_sml_data <- function(con, inchikey_list) {
  query <- paste0("SELECT InChIKey, sml, sml_group FROM eu_sml WHERE InChIKey IN (", inchikey_list, ")")

  result <- tryCatch({
    DBI::dbGetQuery(con, query)
  }, error = function(e) {
    message("Warning: Error querying EU SML: ", e$message)
    .attach_query_error(
      data.frame(InChIKey = character(0), sml = numeric(0), sml_group = character(0)), e$message)
  })

  return(result)
}

#' Query EU SML Group Data
#'
#' Helper function to fetch the whole EU group SML table.
#'
#' The table is keyed by `group_no`, not by substance: all 38 rows have a NULL
#' `InChIKey` (the `substance_name` column holds a list of ref numbers).
#' Filtering it by InChIKey therefore always returned zero rows, which silently
#' dropped the group SML for the substances in `eu_sml` that carry a group
#' number but no individual SML. The table is tiny, so it is read whole and
#' joined on `group_no` by [summarise_eu_sml()].
#'
#' @param con Database connection
#' @return Data frame with group_no and sml
#' @encoding UTF-8
query_eu_sml_group_data <- function(con) {
  query <- "SELECT group_no, sml FROM eu_sml_group"

  result <- tryCatch({
    DBI::dbGetQuery(con, query)
  }, error = function(e) {
    message("Warning: Error querying EU SML Group: ", e$message)
    .attach_query_error(
      data.frame(group_no = character(0), sml = numeric(0)), e$message)
  })

  return(result)
}

#' Query China SML Data
#'
#' Helper function to query China SML database.
#'
#' @param con Database connection
#' @param inchikey_list Comma-separated list of InChIKeys for SQL IN clause
#' @return Data frame with InChIKey and sml_value
#' @encoding UTF-8
query_china_sml_data <- function(con, inchikey_list) {
  # Check if china_sml table has any records first
  count_query <- "SELECT COUNT(*) as count FROM china_sml"
  count_result <- tryCatch({
    DBI::dbGetQuery(con, count_query)$count[1]
  }, error = function(e) {
    0
  })

  if (count_result == 0) {
    # 表不存在或空表：这里不挂错误，因为调用方已按 check_database_status()
    # 的 table_counts 单独登记过 "missing"/"empty"，重复登记只会制造噪音。
    return(data.frame(InChIKey = character(0), sml_value = character(0)))
  }

  query <- paste0("SELECT InChIKey, sml_value FROM china_sml WHERE InChIKey IN (", inchikey_list, ")")

  result <- tryCatch({
    DBI::dbGetQuery(con, query)
  }, error = function(e) {
    message("Warning: Error querying China SML: ", e$message)
    .attach_query_error(
      data.frame(InChIKey = character(0), sml_value = character(0)), e$message)
  })

  return(result)
}

# ---- CMR 定级证据（H 码） ---------------------------------------------------

# 毒性等级规则表（inst/toxicity_levels.png）里与 CMR 有关的 6 个危险说明代码：
#   等级 V ：H340 / H350 / H360（致癌、致突变、生殖毒性 1A 与 1B 类）
#   等级 IV：H341 / H351 / H361（对应 2 类）
# CLP 导出的码带后缀，必须先归并到前 4 位再比对，例如：
#   H350i        吸入途径的致癌 1A/1B
#   H360FD / H360Df / H360F / H360D   生殖毒性 + 发育毒性的组合码
#   H361f *** / H361d ***             "***" 是特定浓度限值标记，不是码的一部分
.cmr_v_h_codes <- c("H340", "H350", "H360")
.cmr_iv_h_codes <- c("H341", "H351", "H361")
.cmr_h_codes <- c(.cmr_v_h_codes, .cmr_iv_h_codes)

#' 从 CLP 危险说明代码文本中提取 CMR 定级相关的 H 码
#'
#' CLP 的 H 码列是换行分隔的多值文本（本库实测分隔符是两个回车符加一个换行符），且同一个码
#' 带不同后缀（见 .cmr_h_codes 注释）。因此先拆成单个码、统一大写、按前 4 位
#' （H + 3 位数字）归并，再与 6 个 CMR 码取交集，其余危险（H302 等）一律忽略。
#'
#' @param x 字符向量，每个元素是 CLP 的一格 H 码文本（可含多个码）
#' @param codes 目标基础码，默认 6 个 CMR 码，且 V 类在 IV 类之前（决定输出顺序）
#' @return 与 x 等长的字符向量：命中的基础码按 codes 顺序用 "; " 连接；
#'   无命中（含 NA、空串、只有非 CMR 码）返回 NA_character_
#' @keywords internal
#' @encoding UTF-8
extract_cmr_h_codes <- function(x, codes = .cmr_h_codes) {
  x <- as.character(x)
  vapply(x, function(one) {
    if (is.na(one) || !nzchar(trimws(one))) return(NA_character_)
    tok <- trimws(strsplit(one, "[\r\n]+")[[1]])
    tok <- toupper(tok[nzchar(tok)])
    base <- sub("^(H[0-9]{3}).*$", "\\1", tok)
    hit <- intersect(codes, base)   # intersect 按 codes 顺序返回，V 类自然在前
    if (length(hit) == 0L) return(NA_character_)
    paste(hit, collapse = "; ")
  }, character(1), USE.NAMES = FALSE)
}

#' 查询 CMR 危险说明代码
#'
#' cmr 表按 (InChIKey, index_no) 存条目，同一 InChIKey 可能对应多行（同族条目，
#' 如 lead powder 与 lead massive）。若直接用 match() 只取第一行会漏码，故这里
#' 按键把各行的 H 码文本合并成一格，交给 extract_cmr_h_codes() 再拆再归并。
#'
#' @param con 数据库连接
#' @param inchikey_list 逗号分隔（各项已带单引号）的 InChIKey 列表，用于 SQL IN
#' @return data.frame(InChIKey, hazard_statement_codes)，每个 InChIKey 一行；
#'   表不存在或查询失败时返回 0 行（调用方据此退化为全部 NA，不中断流程）
#' @keywords internal
#' @encoding UTF-8
query_cmr_data <- function(con, inchikey_list) {
  query <- paste0("SELECT InChIKey, hazard_statement_codes FROM cmr ",
                  "WHERE InChIKey IN (", inchikey_list, ")")

  result <- tryCatch({
    DBI::dbGetQuery(con, query)
  }, error = function(e) {
    message("Warning: Error querying CMR hazard codes: ", e$message)
    .attach_query_error(
      data.frame(InChIKey = character(0), hazard_statement_codes = character(0)), e$message)
  })

  if (nrow(result) == 0) return(result)

  merged <- vapply(split(result$hazard_statement_codes, result$InChIKey),
                   function(v) paste(v[!is.na(v) & nzchar(v)], collapse = "\r\n"),
                   character(1))

  data.frame(InChIKey = names(merged),
             hazard_statement_codes = unname(merged),
             stringsAsFactors = FALSE)
}

# ---- 毒性等级 I–V（规则表 inst/toxicity_levels.png） ------------------------
#
#   等级 V  ：SVHC / CMR(H340, H350, H360) / EDC / IARC 1 组 / SML <= 0.018
#   等级 IV ：CMR(H341, H351, H361) / IARC 2A、2B / 0.018 < SML <= 0.09 / Cramer III
#   等级 III：0.09 < SML <= 0.54 / Cramer II
#   等级 II ：0.54 < SML <= 1.8 / Cramer I
#   等级 I  ：1.8 < SML <= 60
#
# 两点必须说清楚：
#   1) 等级 I 没有"主动证据"。规则表里 I 级唯一来源是 1.8 < SML <= 60，而 Cramer I
#      （最安全的预测）给的是 II 级。所以"什么都没命中"不属于任何等级，留空，
#      不冒充 I 级。
#   2) 多条件命中取最严（等级数字最大）；依据列只列最严那一档命中的规则。证据本身
#      在 SVHC / CMR / EDC / IARC / EU_SML / China_SML / Cramer_rules 各列里都能看到。

# 等级 -> 严重度，数字越大越严
.toxicity_severity <- c(I = 1L, II = 2L, III = 3L, IV = 4L, V = 5L)

# Cramer 等级 -> 毒性等级
.cramer_to_tier <- c(I = "II", II = "III", III = "IV")

# IARC 分组 -> 严重度，数字越小越严。未知/缺失分组给 99：不参与"取最严"的竞争，
# 只在没有任何已知分组时才作为兜底值。
.iarc_severity_rank <- c("1" = 1L, "2A" = 2L, "2B" = 3L, "3" = 4L)

#' IARC 分组的严重度排序
#'
#' @param x 字符向量，如 `c("1", "2B", NA)`
#' @return 与 x 等长的整数；未知或缺失返回 99L
#' @keywords internal
#' @encoding UTF-8
.iarc_rank <- function(x) {
  r <- unname(.iarc_severity_rank[toupper(trimws(as.character(x)))])
  r[is.na(r)] <- 99L
  r
}

#' 解析 Toxtree 的 Cramer 分类字符串
#'
#' Toxtree 的 Cramer.rules 列取值形如 "Low (Class I)" / "Intermediate (Class II)" /
#' "High (Class III)"，这里只取括号里的罗马数字，也接受裸的 "I"/"II"/"III"。
#'
#' @param x 字符向量，如 "High (Class III)"
#' @return 与 x 等长的 "I"/"II"/"III"；无法识别返回 NA_character_
#' @keywords internal
#' @encoding UTF-8
parse_cramer_class <- function(x) {
  x <- trimws(as.character(x))
  out <- rep(NA_character_, length(x))
  hit <- grepl("Class[[:space:]]*(III|II|I)\\b", x, ignore.case = TRUE)
  out[hit] <- toupper(sub(".*Class[[:space:]]*(III|II|I)\\b.*", "\\1",
                          x[hit], ignore.case = TRUE))
  bare <- !hit & toupper(x) %in% names(.cramer_to_tier)
  out[bare] <- toupper(x[bare])
  out
}

#' 由 SML 数值查毒性等级
#'
#' @param sml 数值向量（mg/kg）
#' @return 与 sml 等长的等级字符（"I".."V"），NA 输入返回 NA_character_。
#'   SML > 60 归入 I 级：规则表上界就是 60，且现库中 EU 最大恰为 60、中国最大
#'   48，该分支取不到，写在这里只是不留未定义行为。
#' @keywords internal
#' @encoding UTF-8
toxicity_tier_from_sml <- function(sml) {
  vapply(sml, function(s) {
    if (is.na(s)) return(NA_character_)
    if (s <= 0.018) return("V")
    if (s <= 0.09)  return("IV")
    if (s <= 0.54)  return("III")
    if (s <= 1.8)   return("II")
    "I"
  }, character(1), USE.NAMES = FALSE)
}

#' 取 EU 与 China 两个 SML 中更严的那个
#'
#' @param eu,cn 两个数值（mg/kg），NA 表示该侧没有限值
#' @return list(value = 数值, source = "EU" / "China" / "EU+China" / NA)
#' @keywords internal
#' @encoding UTF-8
strictest_sml <- function(eu, cn) {
  cand <- c(eu, cn)
  src <- c("EU", "China")
  ok <- !is.na(cand)
  if (!any(ok)) return(list(value = NA_real_, source = NA_character_))
  value <- min(cand[ok])
  list(value = value, source = paste(src[ok][cand[ok] == value], collapse = "+"))
}

# "H360; H341" -> c("H360", "H341")；NA / "-" / 空串 -> character(0)
.split_cmr_codes <- function(x) {
  if (is.na(x) || !nzchar(x) || identical(x, "-")) return(character(0))
  trimws(strsplit(x, ";", fixed = TRUE)[[1]])
}

#' 计算毒性等级 I–V 及依据
#'
#' 规则表见 `inst/toxicity_levels.png`。多条件命中取最严；依据列只列最严那一档
#' 命中的规则。完全没有证据的行返回 NA（由调用方统一渲染成 "-"），不冒充 I 级。
#'
#' @param svhc,cmr_suspect,edc 逻辑向量，TRUE 表示命中
#' @param cmr_h_codes 字符向量，[extract_cmr_h_codes()] 的输出（如 "H360; H341"）
#' @param iarc 字符向量，IARC 分组（"1"/"2A"/"2B"；"3" 或 NA 视为无证据）
#' @param sml_eu,sml_cn 数值向量，两侧 SML（mg/kg），NA 表示没有
#' @param cramer_rules 字符向量，Toxtree 的原始输出（如 "High (Class III)"）
#' @param iarc_extra 可选字符向量，来自组级条目匹配的 IARC 分组。与 `iarc`
#'   冲突时取更严的一个，并在依据里标成 `IARC(group):1` 以区分来源。
#' @return data.frame(Toxic_level, Toxic_level_basis)，与输入等长
#' @keywords internal
#' @encoding UTF-8
compute_toxicity_levels <- function(svhc, cmr_h_codes, cmr_suspect, edc, iarc,
                                    sml_eu, sml_cn, cramer_rules,
                                    iarc_extra = NULL) {
  n <- length(svhc)
  iarc <- toupper(trimws(as.character(iarc)))
  if (is.null(iarc_extra)) {
    iarc_extra <- rep(NA_character_, n)
  } else {
    iarc_extra <- toupper(trimws(as.character(iarc_extra)))
    iarc_extra <- rep_len(iarc_extra, n)
  }
  cramer_class <- parse_cramer_class(cramer_rules)

  level <- rep(NA_character_, n)
  basis <- rep(NA_character_, n)

  for (i in seq_len(n)) {
    sev <- integer(0)
    lab <- character(0)
    add <- function(s, l) {
      sev <<- c(sev, s)
      lab <<- c(lab, l)
    }

    codes <- .split_cmr_codes(cmr_h_codes[i])
    v_codes <- codes[codes %in% .cmr_v_h_codes]
    iv_codes <- codes[codes %in% .cmr_iv_h_codes]

    # IARC 证据可能来自两条路：按 InChIKey 精确命中，或按组条目（族）命中。
    # 两者取更严的一档，并在依据里保留来源标签。
    ic <- iarc[i]
    ilab <- "IARC"
    ex <- iarc_extra[i]
    if (!is.na(ex) && nzchar(ex)) {
      if (is.na(ic) || !nzchar(ic) || .iarc_rank(ex) < .iarc_rank(ic)) {
        ic <- ex
        ilab <- "IARC(group)"
      }
    }

    # 等级 V
    if (isTRUE(svhc[i])) add(5L, "SVHC")
    if (length(v_codes)) add(5L, paste0("CMR:", paste(v_codes, collapse = "/")))
    if (isTRUE(edc[i])) add(5L, "EDC")
    if (identical(ic, "1")) add(5L, paste0(ilab, ":1"))

    # 等级 IV
    if (length(iv_codes)) add(4L, paste0("CMR:", paste(iv_codes, collapse = "/")))
    if (isTRUE(cmr_suspect[i])) add(4L, "CMR_suspect")
    if (ic %in% c("2A", "2B")) add(4L, paste0(ilab, ":", ic))
    if (!is.na(cramer_class[i])) {
      tier <- unname(.cramer_to_tier[cramer_class[i]])
      if (!is.na(tier)) {
        add(.toxicity_severity[[tier]], paste0("Cramer:", cramer_class[i]))
      }
    }

    # 等级 III / II / I：全由 SML 决定，EU 与中国取更严
    best <- strictest_sml(sml_eu[i], sml_cn[i])
    sml_tier <- toxicity_tier_from_sml(best$value)
    if (!is.na(sml_tier)) {
      add(.toxicity_severity[[sml_tier]],
          paste0("SML:", format(best$value, trim = TRUE), "(", best$source, ")"))
    }

    if (!length(sev)) next
    top <- max(sev)
    level[i] <- names(.toxicity_severity)[match(top, .toxicity_severity)]
    basis[i] <- paste(unique(lab[sev == top]), collapse = "; ")
  }

  data.frame(Toxic_level = level, Toxic_level_basis = basis,
             stringsAsFactors = FALSE)
}

#' 按 InChIKey 汇总 IARC 分组，取最严的一档
#'
#' `iarc` 表按 (InChIKey, agent) 存条目，同一物质可能既被评过 3 组又被评过 1 组
#' （实测 11 个键冲突，如 "2B,1" / "1,3" / "2A,3"）。直接 `match()` 取首行等于看
#' 数据库行序，取到 3 会把整条 IARC 证据抹掉，故取最严：1 > 2A > 2B > 3。
#' 无法识别的分组排最后，除非没有别的可选。
#'
#' @param iarc_data [query_iarc_data()] 的结果
#' @return data.frame(InChIKey, group_classification)，每个 InChIKey 一行
#' @keywords internal
#' @encoding UTF-8
summarise_iarc_groups <- function(iarc_data) {
  empty <- data.frame(InChIKey = character(0),
                      group_classification = character(0),
                      stringsAsFactors = FALSE)
  if (is.null(iarc_data) || nrow(iarc_data) == 0) return(empty)

  keys <- unique(iarc_data$InChIKey)
  keys <- keys[!is.na(keys)]
  if (!length(keys)) return(empty)

  groups <- vapply(keys, function(k) {
    v <- toupper(trimws(as.character(
      iarc_data$group_classification[iarc_data$InChIKey == k])))
    v <- v[!is.na(v) & nzchar(v)]
    if (!length(v)) return(NA_character_)
    v[which.min(.iarc_rank(v))]
  }, character(1), USE.NAMES = FALSE)

  data.frame(InChIKey = keys, group_classification = groups,
             stringsAsFactors = FALSE)
}

#' 从组号文本里抽出所有组号
#'
#' `eu_sml.sml_group` 存在脏值：源表格两个单元格被读成一格，于是同一格里会出现
#' 两个组号和大量空白（具体样本见 `tests/testthat/test-toxicity-levels.R`）。
#' 这里一律从字符串里抽数字，一个格子可以给出多个组号。
#'
#' @param x 字符向量
#' @return 与 x 等长的 list，每项是组号字符向量（可能为空）
#' @keywords internal
#' @encoding UTF-8
extract_group_nos <- function(x) {
  x <- as.character(x)
  lapply(x, function(one) {
    if (is.na(one) || !nzchar(one)) return(character(0))
    nums <- regmatches(one, gregexpr("[0-9]+", one))[[1]]
    unique(nums[nzchar(nums)])
  })
}

#' 按 InChIKey 汇总 EU SML，取最严
#'
#' `eu_sml` 一个物质可能有多行（实测 9 个键多行，数值相同），且 126 行只有组号、
#' 没有个体值。这里把个体值与所有相关组限值放在一起取最小值。组号取自
#' `sml_group`，该列有脏值（见 [extract_group_nos()]）。
#'
#' @param eu_sml_data [query_eu_sml_data()] 的结果
#' @param eu_sml_group_all [query_eu_sml_group_data()] 取的整表
#' @return data.frame(InChIKey, sml, from_group, groups)；
#'   `from_group` 表示胜出的值是否来自组限值（用于打 `*`）
#' @keywords internal
#' @encoding UTF-8
summarise_eu_sml <- function(eu_sml_data, eu_sml_group_all) {
  empty <- data.frame(InChIKey = character(0), sml = numeric(0),
                      from_group = logical(0), groups = character(0),
                      stringsAsFactors = FALSE)
  if (is.null(eu_sml_data) || nrow(eu_sml_data) == 0) return(empty)

  group_sml <- as.numeric(eu_sml_group_all$sml)
  names(group_sml) <- as.character(eu_sml_group_all$group_no)

  keys <- unique(eu_sml_data$InChIKey)
  keys <- keys[!is.na(keys)]
  if (!length(keys)) return(empty)

  rows <- lapply(keys, function(k) {
    sub <- eu_sml_data[eu_sml_data$InChIKey == k, , drop = FALSE]
    individual <- suppressWarnings(as.numeric(sub$sml))
    individual <- individual[!is.na(individual)]

    grps <- unique(unlist(extract_group_nos(sub$sml_group)))
    gvals <- unname(group_sml[grps])
    gvals <- gvals[!is.na(gvals)]

    cand <- c(individual, gvals)
    if (!length(cand)) {
      value <- NA_real_
      from_group <- FALSE
    } else {
      value <- min(cand)
      # 个体值与组限值相等时算个体值，不打星号
      from_group <- !(value %in% individual)
    }

    data.frame(InChIKey = k, sml = value, from_group = from_group,
               groups = if (length(grps)) paste(grps, collapse = "; ") else NA_character_,
               stringsAsFactors = FALSE)
  })

  do.call(rbind, rows)
}

#' 按 InChIKey 汇总 China SML，取最严
#'
#' `china_sml` 一个物质可能有多行（按食品类别分），实测 4 个键两行数值不同
#' （如 0.05 与 5.0）。取最小值，避免"取数据库首行"这种看行序的结果。
#' `unit` 列实测 1182 行全是 mg/kg，可直接与 EU SML 比较。
#'
#' @param china_sml_data [query_china_sml_data()] 的结果
#' @return data.frame(InChIKey, sml)
#' @keywords internal
#' @encoding UTF-8
summarise_china_sml <- function(china_sml_data) {
  empty <- data.frame(InChIKey = character(0), sml = numeric(0),
                      stringsAsFactors = FALSE)
  if (is.null(china_sml_data) || nrow(china_sml_data) == 0) return(empty)

  keys <- unique(china_sml_data$InChIKey)
  keys <- keys[!is.na(keys)]
  if (!length(keys)) return(empty)

  rows <- lapply(keys, function(k) {
    v <- suppressWarnings(as.numeric(
      china_sml_data$sml_value[china_sml_data$InChIKey == k]))
    v <- v[!is.na(v)]
    data.frame(InChIKey = k,
               sml = if (length(v)) min(v) else NA_real_,
               stringsAsFactors = FALSE)
  })

  do.call(rbind, rows)
}

# ---- 组级条目命中 → 与结果表对齐 --------------------------------------------

# 只有这两档置信度参与定级。"manual_review" 是函数自己标出来的"需人工确认"
# （价态/形态启发式未命中、场景层不可结构判定、含有机骨架又命中金属等），
# 拿它去定级等于把猜测写进结论。
.group_grade_confidences <- c("auto_confirmed", "probable")

#' 把组条目命中汇总到输入行
#'
#' [assign_group_membership_table()] 返回的是长表：一行输入可能命中多个条目，
#' 一行输入也可能一个都不命中。这里按 `input_index` 收成一行，供
#' [assign_toxicity()] 左对齐到结果表。
#'
#' 只有 IARC 的组条目带分组，所以只有它会返回可参与定级的 `Group_IARC`；
#' CMR / SVHC 的 UVCB 命中（如"氯化石蜡"）本身不带危险码，只记进 `Group_hits`。
#' 置信度为 `manual_review` 的命中一律进 `Group_review`，不参与定级。
#'
#' @param hits [assign_group_membership_table()] 的结果
#' @return data.frame(input_index, Group_hits, Group_IARC, Group_review)，
#'   每个出现过的 input_index 一行
#' @keywords internal
#' @encoding UTF-8
summarise_group_hits <- function(hits) {
  empty <- data.frame(input_index = integer(0), Group_hits = character(0),
                      Group_IARC = character(0), Group_review = character(0),
                      stringsAsFactors = FALSE)
  if (is.null(hits) || nrow(hits) == 0) return(empty)
  if (!all(c("input_index", "source_db", "confidence") %in% names(hits))) {
    return(empty)
  }

  idx <- unique(hits$input_index)
  idx <- idx[!is.na(idx)]
  if (!length(idx)) return(empty)

  # 条目名优先用 matched_entry，缺了退回 name
  entry <- as.character(hits$matched_entry)
  fallback <- as.character(hits$name)
  entry[is.na(entry) | !nzchar(entry)] <- fallback[is.na(entry) | !nzchar(entry)]
  label <- paste0(as.character(hits$source_db), ": ", entry)
  label[is.na(entry) | !nzchar(entry)] <- NA_character_
  conf <- as.character(hits$confidence)
  graded <- !is.na(conf) & conf %in% .group_grade_confidences

  rows <- lapply(idx, function(i) {
    sel <- hits$input_index == i
    txt <- unique(label[sel])
    txt <- txt[!is.na(txt) & nzchar(txt)]
    review <- unique(label[sel & !graded])
    review <- review[!is.na(review) & nzchar(review)]

    # IARC 组：复用精确匹配那套"取最严分组"的逻辑，避免两处算法漂移
    grp <- NA_character_
    sel_iarc <- sel & graded & as.character(hits$source_db) == "iarc"
    if (any(sel_iarc)) {
      one <- summarise_iarc_groups(data.frame(
        InChIKey = "input",
        group_classification = as.character(hits$iarc_group[sel_iarc]),
        stringsAsFactors = FALSE))
      if (nrow(one) == 1 && !is.na(one$group_classification[1])) {
        grp <- one$group_classification[1]
      }
    }

    data.frame(
      input_index = as.integer(i),
      Group_hits = if (length(txt)) .paste_capped(txt) else NA_character_,
      Group_IARC = grp,
      Group_review = if (length(review)) .paste_capped(review) else NA_character_,
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}

# 拼接命中条目文本，超过 3 条只列前 3 条并标出剩余数量——整列塞满十几个条目名
# 会让表格没法看，而完整清单在 Group_hits 的原始长表里随时可查。
.paste_capped <- function(x, n = 3L) {
  x <- unique(x)
  if (length(x) <= n) return(paste(x, collapse = "; "))
  paste0(paste(x[seq_len(n)], collapse = "; "),
         " (+", length(x) - n, " more)")
}
