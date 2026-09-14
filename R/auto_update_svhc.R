# =============================================================================
# SVHC 自动更新链路：下载 -> 标准化 -> 补 meta -> diff -> 确认 -> 入库
#
# 背景：
#   ECHA 官网 (echa.europa.eu) 现已启用 Azure WAF，自动化的 POST/静态文件下载
#   一律返回 403，因此旧 download_svhc() 的 Liferay 导出端点基本失效。
#   2026-07 起候选清单 legacy 数据停止维护，官方数据迁至 ECHA CHEM；
#   download_svhc() 已改为直连 ECHA CHEM 的 candidateList/fullExport 端点
#   （纯 HTTP GET + Accept: application/json，无需 cookie/浏览器，2026-09 实测可用）。
#   本文件提供"尽量自动、失败可人工兜底"的流程：
#     source="echa"       -> 直连 ECHA CHEM 官方导出（首选，最新最权威）
#     source="local"      -> 读 inst/ 下人工放置的 candidate_list.xlsx / svhc_new.xlsx
#     source="auto"       -> 依次尝试 echa -> local，全部失败则报错提示
#   （曾有 source="wikipedia" 抓 Wikipedia 镜像列表，2026-09-12 移除：
#     非官方源，且部分网络环境不可达——历史决定见 git log）
#
# 存储模型（2026-09 重建库，蛇形 schema）：
#   化学元数据集中在 chemicals 总表；svhc 业务表只保留"带有效 InChIKey 的行"
#   （InChIKey 外键 -> chemicals），无 InChIKey 的物质（UVCB/聚合物等）不进业务表，
#   全量档案留在 svhc_raw。因此本轨道列名一律为蛇形（substance_name / cas_no ...），
#   diff 前先把无 InChIKey 的新行过滤掉（与业务表语义对齐），写库时先 upsert
#   chemicals 再写 svhc，避免外键失败。
#
# diff 规则：
#   - 主键三级回退：InChIKey 优先；缺失退化为 CAS 号（"CAS:<cas>"）；
#     两者皆无退化为物质名称（"NAME:<name>"）。
#   - 内容比较只针对清单本身的核心字段（名称/EC/CAS/理由/日期/描述/备注），
#     排除 ECHA 文档链接列和 PubChem 化学元数据列，避免误报 modified。
#   - SVHC 清单历史上只增不减：出现 removed 时默认强制人工确认，绝不自动删除。
# =============================================================================

# ---- 库表列定义 + 主键 ------------------------------------------------------

# SVHC 业务表 11 个内容列（蛇形，无系统列/化学列；InChIKey 见 chem_cols）
svhc_db_columns <- c(
  "substance_name", "description", "ec_no", "cas_no",
  "reason_for_inclusion", "date_of_inclusion", "decision", "iuclid_dataset",
  "support_document", "response_to_comments", "remarks"
)

# SVHC 清单核心内容列（diff 时参与内容比较的列）
svhc_content_columns <- c(
  "substance_name", "description", "ec_no", "cas_no",
  "reason_for_inclusion", "date_of_inclusion", "remarks"
)

#' SVHC 行主键（识别"同一个物质"的身份证）
#'
#' 三级回退：InChIKey 优先；缺失退化为 CAS 号（前缀 "CAS:"，先经
#' canonicalize_cas() 归一化，避免 0266309-43-7 与 266309-43-7 这类
#' 前导 0 格式差异导致同一物质被误判为两条）；
#' 两者皆无则退化为物质名称（前缀 "NAME:"，trim 首尾空格）。
#' 全部缺失返回 NA，不参与 diff 配对（保持原样）。
#' @noRd
svhc_key_of <- function(df) {
  ik <- as.character(df[["InChIKey"]])
  cas <- canonicalize_cas(as.character(df[["cas_no"]]))
  nm <- trimws(as.character(df[["substance_name"]]))
  ifelse(!is.na(ik) & nzchar(ik), ik,
         ifelse(!is.na(cas) & nzchar(cas), paste0("CAS:", cas),
                ifelse(!is.na(nm) & nzchar(nm), paste0("NAME:", nm), NA_character_)))
}

# ---- 标准化：源表 -> 库表蛇形列 ---------------------------------------------

#' 解析 SVHC 日期为数据库统一格式 dd/mm/yyyy
#'
#' 兼容多种来源的日期写法：库内既有值（ECHA 原始透传，如 "19/12/2011"）、
#' 新 ECHA 导出 "19/12/2011"、Wikipedia "23 January 2024"、ISO "2024-11-07"
#' 等。无法解析时原样返回。
#'
#' 输出用斜杠 dd/mm/yyyy 而非英文 dd-MMM-yyyy：重建库迁移时 date_of_inclusion
#' 是原样透传（未 normalize），svhc 与 svhc_raw 存量 295+420 行全部是 ECHA
#' 斜杠格式。若新数据写成英文格式会与存量不一致，每次 diff 全表误报 modified。
#' 同时输出不用 format() 的 "%b"（受 locale 影响，中文系统会输出 "6月"），
#' 而是用纯数字段手写，保证任何 locale 下输出稳定。
#'
#' @param x 字符向量
#' @return 字符向量，格式 dd/mm/yyyy
#' @keywords internal
#' @export
normalize_svhc_date <- function(x) {
  fmt <- function(d) sprintf("%02d/%02d/%04d",
                             as.integer(format(d, "%d", tz = "UTC")),
                             as.integer(format(d, "%m", tz = "UTC")),
                             as.integer(format(d, "%Y", tz = "UTC")))
  out <- vapply(x, function(v) {
    if (is.na(v) || v == "" || !nzchar(trimws(v))) return(NA_character_)
    v <- trimws(v)
    d <- suppressWarnings(as.Date(v, format = "%d/%m/%Y"))
    if (is.na(d)) d <- suppressWarnings(as.Date(v, format = "%d-%b-%Y"))
    if (is.na(d)) d <- suppressWarnings(as.Date(v, format = "%d %B %Y"))
    if (is.na(d)) d <- suppressWarnings(as.Date(v, format = "%d %b %Y"))
    if (is.na(d)) d <- suppressWarnings(as.Date(v, format = "%Y-%m-%d"))
    if (is.na(d)) v else fmt(d)
  }, character(1), USE.NAMES = FALSE)
  out
}

#' 标准化 SVHC 数据框（统一到蛇形业务列 + 化学列）
#'
#' 输入任意来源的基础 df（须含 Substance name / EC No. / CAS No. /
#' Date of inclusion / Reason for inclusion），输出与重建库一致的
#' data.frame：11 个蛇形业务内容列 + 6 个化学列（CID/Formula/SMILES/
#' InChIKey/IUPACName/ExactMass，供 enrich 回填与写 chemicals 用），
#' 列名即库表列名。
#'
#' @param df 原始数据框
#' @return 标准化后的 data.frame
#' @keywords internal
#' @export
normalize_svhc_df <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  message("   Detected source columns: ", paste(names(df), collapse = " | "))

  out_cols <- c(svhc_db_columns, c("CID", "Formula", "SMILES", "InChIKey",
                                   "IUPACName", "ExactMass"))
  # 列名别名表：xlsx/ECHA/Wikipedia 变体（驼峰）-> 蛇形库表列名
  aliases <- c(
    "Substance name" = "substance_name",
    "Name" = "substance_name",
    "Substance" = "substance_name",
    "Substance Name" = "substance_name",
    "EC No." = "ec_no",
    "EC No" = "ec_no",
    "EC number" = "ec_no",
    "EC / List no." = "ec_no",
    "EC / List number" = "ec_no",
    "EC list number" = "ec_no",
    "CAS No." = "cas_no",
    "CAS No" = "cas_no",
    "CAS number" = "cas_no",
    "CAS no." = "cas_no",
    "CAS no" = "cas_no",
    "Inclusion date" = "date_of_inclusion",
    "Date of Inclusion" = "date_of_inclusion",
    "Date of inclusion" = "date_of_inclusion",
    "Date" = "date_of_inclusion",
    "Reason for Inclusion" = "reason_for_inclusion",
    "Reason for inclusion" = "reason_for_inclusion",
    "Reason" = "reason_for_inclusion",
    "Decision" = "decision",
    "IUCLID dataset" = "iuclid_dataset",
    "Support document" = "support_document",
    "Response to comments" = "response_to_comments",
    "Remarks" = "remarks",
    "Description" = "description",
    "CID" = "CID",
    "MolecularFormula" = "Formula",
    "Formula" = "Formula",
    "IsomericSMILES" = "SMILES",
    "SMILES" = "SMILES",
    "InChIKey" = "InChIKey",
    "IUPACName" = "IUPACName",
    "ExactMass" = "ExactMass"
  )

  out <- data.frame(matrix(NA_character_, nrow = nrow(df),
                           ncol = length(out_cols)),
                    stringsAsFactors = FALSE)
  names(out) <- out_cols

  for (i in seq_along(names(df))) {
    src <- names(df)[i]
    dst <- unname(aliases[src])
    if (!is.na(dst) && dst %in% names(out)) {
      out[[dst]] <- as.character(df[[src]])
    }
  }

  # 清理：trim + 空串转 NA；日期统一格式；EC/CAS 占位符（-、–、—）转 NA
  placeholder <- c("-", "\u2013", "\u2014", "\u2015", "n/a", "N/A", "NA")
  for (col in names(out)) {
    if (is.character(out[[col]])) {
      out[[col]] <- trimws(out[[col]])
      out[[col]][out[[col]] == ""] <- NA_character_
      if (col %in% c("ec_no", "cas_no")) {
        out[[col]][out[[col]] %in% placeholder] <- NA_character_
      }
    }
  }
  out[["date_of_inclusion"]] <- normalize_svhc_date(out[["date_of_inclusion"]])
  # 去重（同物质多来源重复行），按名称+CAS
  key <- paste0(ifelse(is.na(out[["substance_name"]]), "", out[["substance_name"]]),
                "\x01", ifelse(is.na(out[["cas_no"]]), "", out[["cas_no"]]))
  out <- out[!duplicated(key), , drop = FALSE]
  out
}

#' 补 SVHC 化学元数据（增量：先回填老物质，只查新增）
#'
#' 1) 先按 CAS 从库回填已存在物质的化学元数据（未变动物质零请求）；
#' 2) 再只对仍然缺 InChIKey 的新增物质查 PubChem PUG REST API。
#' 查不到的（UVCB/聚合物等）保持空，由 diff 层过滤（不进业务表）。
#'
#' @param df 标准化后的 SVHC data.frame（蛇形列 + 化学列）
#' @param db_path 自定义数据库路径（NULL 用默认）
#' @param delay 每次请求间隔秒数
#' @param verbose 是否打印进度
#' @return 补全后的 data.frame
#' @keywords internal
#' @export
enrich_svhc_meta <- function(df, db_path = NULL, delay = 0.35, verbose = TRUE) {
  if (is.null(df) || nrow(df) == 0) return(df)
  # 1) 先从库回填老物质（未变动的物质零请求）
  df <- backfill_meta_from_db(df, "svhc", "cas_no", db_path = db_path)
  # 2) 只对仍然缺 InChIKey 的新增物质查 PubChem
  df <- enrich_new_compounds(df, "cas_no", "substance_name",
                             delay = delay, verbose = verbose)
  df
}

# ---- 抓取：本地导出文件 ------------------------------------------------------

#' 读取单个 SVHC 本地导出文件（xlsx / csv）
#'
#' 兼容 ECHA 导出文件（表头可能不在第 1 行）与普通 CSV。读取并标准化为
#' 蛇形列 + 化学列 data.frame；文件不可读或无有效行时返回 NULL。
#'
#' @param path 文件完整路径
#' @return 标准化后的 data.frame，或 NULL
#' @keywords internal
#' @export
read_svhc_source_file <- function(path) {
  raw <- NULL
  if (grepl("\\.csv$", path)) {
    raw <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    # 自适应表头行：ECHA 导出的 xlsx 表头可能在中间行（如第 4 行）
    for (skip in c(0, 1, 2, 3, 4)) {
      tmp <- suppressWarnings(tryCatch(rio::import(path, skip = skip),
                                       error = function(e) NULL))
      if (!is.null(tmp) && any(grepl("Substance name", names(tmp), ignore.case = TRUE))) {
        raw <- tmp
        break
      }
    }
    if (is.null(raw)) {
      # 最后兜底：按第一行表头直接读
      raw <- suppressWarnings(rio::import(path))
    }
  }
  if (is.null(raw) || nrow(raw) == 0) return(NULL)
  normalize_svhc_df(raw)
}

#' 读取本地人工放置的 ECHA 导出文件
#'
#' 依次尝试 inst/candidate_list.xlsx、inst/svhc_new.xlsx、inst/svhc_new.csv。
#'
#' @param inst_dir inst 目录路径
#' @return 标准化后的 data.frame
#' @keywords internal
#' @export
fetch_svhc_local <- function(inst_dir = file.path(getwd(), "inst")) {
  # candidate_list / svhc_new 是 ECHA 导出的候选清单；svhc_meta 是库内全量备份。
  # ECHA 导出可能因 WAF/分页被截断成不完整文件，因此这里读取所有候选文件，
  # 选标准化后行数最多（最完整）的那份，避免误用残缺导出导致大量假 removed。
  cands <- file.path(inst_dir, c("candidate_list.xlsx", "svhc_new.xlsx",
                                  "svhc_new.csv", "svhc_meta.xlsx"))
  found <- cands[file.exists(cands)]
  if (length(found) == 0) {
    stop("No local SVHC file found. Place candidate_list.xlsx, svhc_new.xlsx or svhc_meta.xlsx in ",
         inst_dir, " (or use source='echa')")
  }

  best <- NULL
  best_path <- NULL
  for (path in found) {
    out <- tryCatch(read_svhc_source_file(path), error = function(e) NULL)
    if (is.null(out) || nrow(out) == 0) next
    if (is.null(best) || nrow(out) > nrow(best)) {
      best <- out
      best_path <- path
    }
  }
  if (is.null(best)) {
    stop("Local SVHC file has no readable rows: ", paste(found, collapse = ", "))
  }

  message("Reading local SVHC file: ", best_path)
  message("   Local file rows parsed: ", nrow(best))
  best
}

# ---- 源分发：auto -> echa -> local（依次尝试） ------------------------------

#' SVHC 数据源分发
#'
#' @param source "auto"（默认，echa -> local）、"local"、"echa"
#' @param inst_dir inst 目录
#' @param new_file 显式本地文件路径（优先于 source 分发直接读取；
#'   用于 check_manual_lists() 等场景精确消费某个手动放入的清单文件）
#' @return 标准化后的 data.frame（蛇形列 + 化学列）
#' @keywords internal
#' @export
fetch_svhc_data <- function(source = c("auto", "local", "echa"),
                            inst_dir = file.path(getwd(), "inst"),
                            new_file = NULL) {
  source <- match.arg(source)

  # 显式文件优先：直接读，不触发下载，避免覆盖 inst/ 真实数据
  if (!is.null(new_file)) {
    if (!file.exists(new_file)) {
      stop("Specified SVHC file not found: ", new_file)
    }
    message("Reading specified SVHC file: ", new_file)
    out <- tryCatch(read_svhc_source_file(new_file), error = function(e) NULL)
    if (is.null(out) || nrow(out) == 0) {
      stop("Specified SVHC file has no readable rows: ", new_file)
    }
    message("   File rows parsed: ", nrow(out))
    return(out)
  }

  if (source == "local") return(fetch_svhc_local(inst_dir))

  if (source == "echa") {
    # 直连 ECHA CHEM 官方导出端点（download_svhc 已改为 GET fullExport，
    # 纯 HTTP、无需 cookie/浏览器）。下载结果写入 svhc_new.xlsx，
    # 走 local 的读取/标准化逻辑。
    message("Downloading SVHC candidate list from ECHA CHEM ...")
    download_svhc(out = file.path(inst_dir, "svhc_new.xlsx"))
    return(fetch_svhc_local(inst_dir))
  }

  # auto: echa -> local
  errs <- character(0)
  tryCatch(return(fetch_svhc_data("echa", inst_dir = inst_dir)), error = function(e) {
    errs <<- c(errs, paste("echa:", conditionMessage(e)))
  })
  tryCatch(return(fetch_svhc_local(inst_dir)), error = function(e) {
    errs <<- c(errs, paste("local:", conditionMessage(e)))
  })
  stop("All SVHC fetch sources failed:\n  * ", paste(errs, collapse = "\n  * "))
}

# ---- diff：新数据与库内现状比对（只增不减，removed 强制人工确认） ------------

#' 与库表比对，生成 diff
#'
#' 主键：InChIKey，缺失时退化为 "CAS:<cas>"、再退化为 "NAME:<name>"。
#' 内容比较只针对 svhc_content_columns（清单核心字段），避免 ECHA 文档
#' 链接列与 PubChem 化学列差异造成误报。
#'
#' 存储模型：svhc 业务表只保留带有效 InChIKey 的行（蛇形 schema），
#' 因此入口先过滤掉无 InChIKey 的新行（UVCB/聚合物等查不到 InChIKey 的
#' 物质不进业务表，默认过滤语义，与 cmr 等库一致）。
#'
#' @param new_df 标准化后的新数据（蛇形列 + 化学列）
#' @param db_path 数据库路径（NULL 用默认）
#' @return list(added, removed, modified, total_added, total_removed, total_modified)
#' @keywords internal
#' @export
diff_svhc_data <- function(new_df, db_path = NULL) {
  con <- get_db_connection(db_path)
  on.exit(DBI::dbDisconnect(con))
  current <- DBI::dbGetQuery(con, "SELECT * FROM svhc")

  # 默认过滤：无有效 InChIKey 的新行不参与 diff（业务表不收 UVCB）
  new_df <- new_df[!is_blank_key(new_df[["InChIKey"]]), , drop = FALSE]
  if (nrow(new_df) == 0) {
    message("No new SVHC rows with InChIKey - nothing to compare")
    empty <- new_df[0, , drop = FALSE]
    return(list(total_added = 0L, total_removed = 0L, total_modified = 0L,
                added = empty, removed = empty, modified = empty))
  }

  key_of <- svhc_key_of

  new_all <- key_of(new_df)
  cur_all <- key_of(current)

  new_ok <- !is.na(new_all)
  cur_ok <- !is.na(cur_all)

  new_df <- new_df[new_ok, , drop = FALSE]
  if (nrow(new_df) == 0) stop("New SVHC data has no usable keys (InChIKey, CAS or name)")
  new_keys <- new_all[new_ok]

  # 库里 InChIKey/CAS/名称全空的行无法与任何新数据匹配：不参与分类，保持原样
  cur_df <- current[cur_ok, , drop = FALSE]
  cur_keys <- cur_all[cur_ok]

  added_keys <- setdiff(new_keys, cur_keys)
  removed_keys <- setdiff(cur_keys, new_keys)
  common_keys <- intersect(new_keys, cur_keys)

  # 内容比较（仅核心列；来源整列缺失的列如 Description/Remarks 不参与比较，
  # 避免 Wikipedia 源（无这些列）导致全表误报 modified）
  content_cols <- svhc_content_columns
  miss <- vapply(content_cols, function(col) all(is.na(new_df[[col]])), logical(1))
  content_cols <- content_cols[!miss]

  # 内容比较（仅核心列）
  modified_keys <- character(0)
  if (length(common_keys) > 0 && length(content_cols) > 0) {
    # 单元格归一复用公共线的 canon_cell，排序步显式关掉：remarks 是散文式
    # 多行注释（当前库内即有 4 行含换行），行序有含义——与 GHS/H 码这类
    # "换序无含义"的代码表列不同。见 canon_cell 的 sort_multiline 参数。
    # 行签名：CAS 列先归一化再参与比较，避免 0266309-43-7 vs 266309-43-7
    # 这类前导 0 格式差异被误判为 modified
    row_sig <- function(r) {
      r <- unlist(r)
      nm <- names(r)
      if (!is.null(nm) && "cas_no" %in% nm) {
        r[nm == "cas_no"] <- canonicalize_cas(as.character(r[nm == "cas_no"]))
      }
      paste(canon_cell(r, sort_multiline = FALSE), collapse = "\x01")
    }
    cur_by_key <- split(seq_len(nrow(cur_df)), cur_keys)
    new_by_key <- split(seq_len(nrow(new_df)), new_keys)
    for (k in common_keys) {
      ci <- cur_by_key[[k]]
      ni <- new_by_key[[k]]
      if (is.null(ci) || is.null(ni)) next
      cur_sig <- apply(cur_df[ci, content_cols, drop = FALSE], 1, row_sig)
      new_sig <- apply(new_df[ni, content_cols, drop = FALSE], 1, row_sig)
      if (length(cur_sig) != length(new_sig) ||
          !identical(unname(sort(cur_sig)), unname(sort(new_sig)))) {
        modified_keys <- c(modified_keys, k)
      }
    }
  }

  added <- new_df[new_keys %in% added_keys, , drop = FALSE]
  removed <- cur_df[cur_keys %in% removed_keys, , drop = FALSE]
  modified <- new_df[new_keys %in% modified_keys, , drop = FALSE]

  list(
    total_added = nrow(added), total_removed = nrow(removed),
    total_modified = nrow(modified),
    added = added, removed = removed, modified = modified
  )
}

# ---- 入库：事务写入 + 备份 + 记 update_history -------------------------------

#' 将 diff 结果写入 SQLite（事务 + 备份 + update_history）
#'
#' @param new_df 标准化后的新数据（全量，用于 modified 重插）
#' @param changes diff_svhc_data() 的输出
#' @param db_path 数据库路径
#' @param backup 入库前是否备份数据库到 backups/
#' @return 写入统计
#' @keywords internal
#' @export
write_svhc_to_db <- function(new_df, changes, db_path = NULL, backup = TRUE) {
  con <- get_db_connection(db_path)
  on.exit(DBI::dbDisconnect(con))

  n_added <- changes$total_added
  n_removed <- changes$total_removed
  n_modified <- changes$total_modified
  if (n_added + n_removed + n_modified == 0) {
    message("No changes to apply")
    return(list(records_added = 0, records_removed = 0, records_modified = 0))
  }

  # 备份（路径解析用公共线的 .resolve_db_path，不再维护第三份拷贝）
  if (isTRUE(backup)) {
    backup_db_file(.resolve_db_path(db_path))
  }

  # 修改前给旧表拍快照：仅当存在 modified 时抓取，供 change_log 逐字段对比
  old_snapshot <- NULL
  if (n_modified > 0L) {
    old_snapshot <- DBI::dbGetQuery(con, "SELECT * FROM svhc")
  }

  DBI::dbWithTransaction(con, {
    # 删除 removed / 被 modified 覆盖的旧行（按主键类型区分删除条件，防误删）：
    #   InChIKey 键 -> 精确匹配 InChIKey 删除；
    #   CAS: 键     -> 只删库里 InChIKey 为空的行；
    #   NAME: 键    -> 最保守：只删库里 InChIKey 与 CAS 都为空的行（名称易撞，宁漏不误删）
    del_keys <- unique(c(svhc_key_of(changes$removed), svhc_key_of(changes$modified)))
    del_keys <- del_keys[!is.na(del_keys)]
    for (k in del_keys) {
      if (grepl("^NAME:", k)) {
        nm <- sub("^NAME:", "", k)
        DBI::dbExecute(con,
          'DELETE FROM svhc WHERE "substance_name" = ? AND ("InChIKey" IS NULL OR "InChIKey" = "") AND ("cas_no" IS NULL OR "cas_no" = "")',
          params = list(nm))
      } else if (grepl("^CAS:", k)) {
        # key 里的 CAS 已归一化，库里存的可能是另一种格式（如前导 0），
        # 直接精确匹配会删不到。改为在 R 侧归一化比较后按 rowid 删。
        cas_norm <- sub("^CAS:", "", k)
        # 注意：业务表带 id INTEGER PRIMARY KEY（rowid 别名），SELECT rowid 的
        # 返回列名会被改写为 id，cand$rowid 取不到值导致删除被静默跳过。
        # 显式命名 AS rid 保证任何表结构下都能取到。
        cand <- DBI::dbGetQuery(con,
          'SELECT rowid AS rid, "cas_no" FROM svhc WHERE ("InChIKey" IS NULL OR "InChIKey" = "")')
        if (nrow(cand) > 0) {
          c_norm <- canonicalize_cas(trimws(as.character(cand[["cas_no"]])))
          hit <- which(!is_blank_key(c_norm) & c_norm == cas_norm)
          for (rid in cand$rid[hit]) {
            DBI::dbExecute(con, "DELETE FROM svhc WHERE rowid = ?", params = list(rid))
          }
        }
      } else {
        DBI::dbExecute(con, 'DELETE FROM svhc WHERE "InChIKey" = ?', params = list(k))
      }
    }

    # 插入 added + modified：先保证 chemicals 有对应记录（业务表 InChIKey
    # 外键 -> chemicals，FK 开启时缺失会报错），再写 svhc 业务表。
    to_insert <- rbind(changes$added, changes$modified)
    if (nrow(to_insert) > 0) {
      upsert_chemicals(con, to_insert)
      info <- DBI::dbGetQuery(con, "PRAGMA table_info(svhc)")
      # 系统列（id / created_at / updated_at）交 SQLite 自动维护
      ins_cols <- setdiff(info$name, c("id", "created_at", "updated_at"))
      for (col in ins_cols) {
        if (!col %in% names(to_insert)) to_insert[[col]] <- NA_character_
      }
      to_insert <- to_insert[, ins_cols, drop = FALSE]
      DBI::dbAppendTable(con, "svhc", to_insert)
    }
  })

  # 账本（update_history + change_log）走与公共线共享的 record_update_ledger；
  # SVHC 的键风格差异（key_fn/compare_cols）经 ... 透传
  record_update_ledger(con, "svhc", changes, n_added, n_removed, n_modified,
                       old_df = old_snapshot,
                       source_file = "auto_update_svhc.R",
                       user_notes = "Auto update with diff confirmation",
                       key_fn = svhc_key_of, compare_cols = svhc_content_columns)

  message("   DB write done: +", n_added, " / -", n_removed, " / ~", n_modified)
  list(records_added = n_added, records_removed = n_removed, records_modified = n_modified)
}

# ---- 主入口 -----------------------------------------------------------------

#' SVHC 自动更新（下载 -> diff -> 确认 -> 入库）
#'
#' 在 ECHA 官网被 WAF 拦截的现状下，提供"尽量自动"的 SVHC 候选清单更新：
#' 自动抓取数据源、补齐 PubChem 化学信息、与现有数据库做 diff，
#' 确认后以事务方式写入 SQLite（默认自动备份）。
#'
#' 确认规则：
#'   - interactive = TRUE 时逐项提示 y/n
#'   - interactive = FALSE 且 auto_apply = TRUE 时，仅当无删除且变更总数
#'     不超过 max_auto_changes 才自动入库（适合无人值守）
#'   - 出现 removed（清单移除）时，即使 auto_apply 也强制停下人工确认
#'     （SVHC 清单历史上只增不减，removed 通常意味着数据源不完整）
#'
#' @param source 数据源："auto"（echa -> local）、"local"、"echa"
#' @param interactive 是否交互确认
#' @param auto_apply 非交互模式下是否自动入库（受 max_auto_changes 限制）
#' @param max_auto_changes 自动入库的最大变更条数
#' @param enrich 是否补 PubChem 化学元数据（只查新增物质，通常 1-5 个请求）
#' @param db_path 自定义数据库路径（测试用）
#' @param backup 入库前是否备份数据库
#' @param update_xlsx 是否将合并结果回写 inst/svhc_meta.xlsx（默认关闭）
#' @param new_file 显式指定本地清单文件（xlsx/csv）直接读取并更新，
#'   优先于 source 分发；用于 check_manual_lists() 精确消费手动放入的文件
#' @return list(success, changes, db_write)
#' @export
#' @export
update_svhc_auto <- function(source = c("auto", "local", "echa"),
                             interactive = TRUE,
                             auto_apply = FALSE,
                             max_auto_changes = 20,
                             enrich = TRUE,
                             db_path = NULL,
                             backup = TRUE,
                             update_xlsx = FALSE,
                             new_file = NULL) {
  source <- match.arg(source)

  # ---- 1. 下载 ----
  new_df <- fetch_svhc_data(source = source, new_file = new_file)
  message("   Total rows after normalization: ", nrow(new_df))

  # ---- 2. 补 meta（先回填老物质，只对新增查 PubChem）----
  if (isTRUE(enrich)) {
    message("Enriching missing chemical metadata (incremental)...")
    new_df <- enrich_svhc_meta(new_df, db_path = db_path, verbose = TRUE)
  }

  # ---- 3. diff ----
  changes <- diff_svhc_data(new_df, db_path = db_path)
  message("Diff result:")
  message("   + Added:    ", changes$total_added)
  message("   - Removed:  ", changes$total_removed)
  message("   ~ Modified: ", changes$total_modified)

  if (changes$total_added + changes$total_removed + changes$total_modified == 0) {
    message("SVHC is up to date - no changes needed")
    return(list(success = TRUE, changes = changes, db_write = NULL, message = "No changes"))
  }

  if (changes$total_added > 0) {
    message("   New substances:")
    fmt_row <- function(df, i) {
      nm <- df[["substance_name"]][i]; if (is.na(nm) || !nzchar(nm)) nm <- "?"
      ec <- df[["ec_no"]][i]; if (is.na(ec) || !nzchar(ec)) ec <- "-"
      cas <- df[["cas_no"]][i]; if (is.na(cas) || !nzchar(cas)) cas <- "-"
      dt <- df[["date_of_inclusion"]][i]; if (is.na(dt) || !nzchar(dt)) dt <- "-"
      sprintf("     * %s | EC %s | CAS %s | %s", nm, ec, cas, dt)
    }
    for (i in seq_len(min(nrow(changes$added), 20))) message(fmt_row(changes$added, i))
    if (changes$total_added > 20) message("     ... and ", changes$total_added - 20, " more")
  }
  if (changes$total_removed > 0) {
    message("   !! Removed substances (review carefully - SVHC list rarely removes entries):")
    n_show <- min(nrow(changes$removed), 30)
    for (i in seq_len(n_show)) {
      nm <- changes$removed[["substance_name"]][i]
      if (is.na(nm) || !nzchar(nm)) nm <- "?"
      cas <- changes$removed[["cas_no"]][i]
      if (is.na(cas) || !nzchar(cas)) cas <- "-"
      message(sprintf("     - %s | CAS %s", nm, cas))
    }
    if (nrow(changes$removed) > 30) {
      message("     ... and ", nrow(changes$removed) - 30, " more removed rows")
    }
    # 规模异常提示：库行数远多于新清单条目时，removed 大概率是数据源不完整
    if (changes$total_removed > nrow(new_df)) {
      message("   !! Warning: removed count exceeds total new rows - the source list is",
              " likely incomplete (e.g. truncated export table). Review before applying.")
    }
  }

  # ---- 4. 确认 ----
  if (changes$total_removed > 0) {
    message("!! Removed entries detected - automatic apply is disabled, manual review required")
    proceed <- FALSE
    if (interactive) {
      resp <- readline("SVHC removals detected. Apply anyway? (y/N): ")
      proceed <- tolower(trimws(resp)) == "y"
    }
    if (!proceed) {
      return(list(success = FALSE, changes = changes, db_write = NULL,
                  message = "Cancelled: removed entries require manual review"))
    }
  } else if (interactive) {
    resp <- readline(sprintf("Apply %d changes to svhc table? (y/N): ",
                             changes$total_added + changes$total_modified))
    if (tolower(trimws(resp)) != "y") {
      message("Update cancelled by user")
      return(list(success = FALSE, changes = changes, db_write = NULL,
                  message = "User cancelled"))
    }
  } else if (!isTRUE(auto_apply)) {
    message("Not interactive and auto_apply = FALSE - no changes written")
    return(list(success = FALSE, changes = changes, db_write = NULL,
                message = "Dry run (no apply)"))
  } else if (changes$total_added + changes$total_modified > max_auto_changes) {
    message("Change count exceeds max_auto_changes = ", max_auto_changes,
            " - automatic apply skipped")
    return(list(success = FALSE, changes = changes, db_write = NULL,
                message = "Too many changes for auto_apply"))
  }

  # ---- 5. 入库 ----
  db_write <- write_svhc_to_db(new_df, changes, db_path = db_path, backup = backup)

  # ---- 6. 可选回写 xlsx（svhc_meta.xlsx 格式：驼峰列名，兼容老读取路径） ----
  if (isTRUE(update_xlsx)) {
    tryCatch({
      # snake -> 驼峰反向映射（fetch_svhc_local / normalize_svhc_df 只认驼峰源名）
      xlsx_df <- new_df
      rev <- c(substance_name = "Substance name", description = "Description",
               ec_no = "EC No.", cas_no = "CAS No.",
               reason_for_inclusion = "Reason for inclusion",
               date_of_inclusion = "Date of inclusion",
               decision = "Decision", iuclid_dataset = "IUCLID dataset",
               support_document = "Support document",
               response_to_comments = "Response to comments",
               remarks = "Remarks")
      for (nm in names(rev)) {
        if (nm %in% names(xlsx_df)) {
          names(xlsx_df)[names(xlsx_df) == nm] <- rev[[nm]]
        }
      }
      names(xlsx_df)[names(xlsx_df) == "Formula"] <- "MolecularFormula"
      names(xlsx_df)[names(xlsx_df) == "SMILES"] <- "IsomericSMILES"
      xlsx_df[["IUPACName"]] <- NULL
      xlsx_path <- file.path(dirname(.resolve_db_path(db_path)), "..", "inst", "svhc_meta.xlsx")
      rio::export(xlsx_df, xlsx_path, overwrite = TRUE)
      message("   svhc_meta.xlsx synced: ", xlsx_path)
    }, error = function(e) warning("Could not sync svhc_meta.xlsx: ", e$message))
  }

  message("SVHC auto update completed")
  list(success = TRUE, changes = changes, db_write = db_write,
       message = "Update applied")
}
