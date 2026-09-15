# =============================================================================
# 法规数据库更新链路：CMR / CMR_suspect / IARC / EU_SML / SVHC
#
# 合并后的单一文件，整合了原来的：
#   - update_dbs.R     (入口、DB_SOURCES 注册表、CMR/IARC/EU_SML 标准化与取数)
#   - update_pipeline.R (通用增量更新流水线：diff/enrich/回填/入库/账本)
#   - update_svhc.R    (SVHC 专用链路：三级主键回退、SVHC diff/write)
#
# 各数据源更新链路：
#   CMR / IARC / EU_SML -> run_incremental_update() (公共流水线)
#   SVHC                -> update_svhc_auto() (独立流水线，主键三级回退)
#
# 存储模型（2026-09 重建库，蛇形 schema）：
#   化学元数据集中在 chemicals 总表；业务表只保留 InChIKey 引用。
# =============================================================================

# =============================================================================
# CMR / CMR_suspect / IARC / EU_SML 增量更新链路
#
# 与 update_svhc_auto 同一套思路：下载/读本地 -> 标准化 -> 先 diff（用清单自有
# 主键，如 Index No / Agent / FCM substance No）-> 只对真正新增的物质查 PubChem
# -> 确认 -> 入库。核心逻辑全部复用 R/update_pipeline.R 里的
# run_incremental_update()（对齐库表列 -> 回填老物质 -> 只补新增 -> diff -> 入库）。
#
# 各库主键（与重建后的蛇形 fcmsafety.db 表结构一致）：
#   cmr              -> index_no            （优先；cas_no 兜底）
#   cmr_suspect      -> substance_name      （优先；cas_no 兜底；蛇形表无 index_no）
#   iarc             -> cas_no              （优先；agent 兜底）
#   eu_sml           -> fcm_substance_no    （优先；cas_no 兜底）
# 兜底机制：当主键列缺失/占位时，退化为 CAS/名称作为 diff 身份证，
# 减少因官方编号为空导致的漏配与误报删除。
#
# 说明：新库（蛇形 schema）的化学元数据列集中在 chemicals 总表，业务表只保留
# InChIKey 且仅含带 InChIKey 的行。本文件传的 key_col/cas_col/name_col 必须用
# 蛇形列名——map_to_db_columns() 会把源文件列动态对齐到库表蛇形列（Index No
# 自动命中 index_no、International Chemical Identification 命中 substance_name
# 等），对齐后的 new_df 以蛇形列名为准。
# =============================================================================

# ---- 通用读取 / 清洗小工具 --------------------------------------------------

#' 读取源数据文件（csv / xlsx，自适应表头行）
#'
#' 对于 xlsx，依次尝试 skip 0..6 行，找到包含 expected_col（正则，忽略大小写）
#' 的表头行为止；two_row_header = TRUE 时再合并 CLP 这类两行表头。
#'
#' @param path 文件路径
#' @param sheet xlsx 工作表名（NULL 用第一个）
#' @param expected_col 用于定位表头行的列名片段（正则）
#' @param two_row_header 是否为两行表头（CLP 导出）
#' @return data.frame
#' @keywords internal
#' @export
#' @encoding UTF-8
read_source_table <- function(path, sheet = NULL, expected_col = NULL,
                              two_row_header = FALSE) {
  if (grepl("\\.csv$", path, ignore.case = TRUE)) {
    raw <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
    if (two_row_header) raw <- merge_clp_subheader(raw)
    return(raw)
  }

  raw <- NULL
  for (skip in 0:6) {
    tmp <- suppressWarnings(tryCatch(
      rio::import(path, sheet = sheet, skip = skip), error = function(e) NULL))
    if (is.null(tmp) || nrow(tmp) == 0) next
    if (is.null(expected_col) ||
        any(vapply(names(tmp), function(nm) grepl(expected_col, nm, ignore.case = TRUE),
                   logical(1)))) {
      raw <- tmp
      break
    }
  }
  if (is.null(raw)) {
    raw <- suppressWarnings(rio::import(path, sheet = sheet))
  }
  if (two_row_header) raw <- merge_clp_subheader(raw)
  raw
}

#' 合并 CLP 导出的两行表头
#'
#' CLP Annex VI 导出文件第一行是分组表头（Classification / Labelling），第二行
#' 才是真正的列名（Hazard Class and Category Code(s) 等）。当第一行数据其实是
#' 第二行表头时（Index No 为空但含 Hazard/Pictogram/Signal 等子列名），用子列名
#' 覆盖对应分组列名，并丢弃该表头行。
#'
#' @param df 数据框（表头已是第一行分组名）
#' @return 单行表头的 data.frame
#' @keywords internal
#' @export
#' @encoding UTF-8
merge_clp_subheader <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(df)
  if (!"Index No" %in% names(df)) return(df)

  first <- as.character(df[1, ])
  idx_col <- match("Index No", names(df))
  is_sub <- is_blank_key(first[idx_col]) &&
    any(grepl("Hazard Class|Pictogram|Signal Word|Hazard statement|Suppl",
              first, ignore.case = TRUE))
  if (!is_sub) return(df)

  merged <- names(df)
  for (j in seq_along(merged)) {
    v <- trimws(first[j])
    if (!is.na(v) && nzchar(v)) merged[j] <- v
  }
  df <- df[-1, , drop = FALSE]
  names(df) <- merged
  df
}

#' 按映射表重命名源列（命中的列改名，未命中的保留）
#'
#' @param df 数据框
#' @param mapping 命名字符向量：源列名 -> 目标列名
#' @return 重命名后的 data.frame
#' @keywords internal
#' @export
#' @encoding UTF-8
rename_source_cols <- function(df, mapping) {
  for (src in names(mapping)) {
    idx <- which(names(df) == src)
    if (length(idx) == 1) names(df)[idx] <- unname(mapping[src])
  }
  df
}

#' 清洗键/CAS 列：trim + 占位符转 NA
#'
#' @param df 数据框
#' @param cols 需要清洗的列名
#' @return 清洗后的 data.frame
#' @keywords internal
#' @export
#' @encoding UTF-8
clean_key_cols <- function(df, cols) {
  for (col in cols) {
    if (col %in% names(df)) {
      v <- trimws(as.character(df[[col]]))
      v[v %in% key_placeholder] <- NA_character_
      v[v == ""] <- NA_character_
      df[[col]] <- v
    }
  }
  df
}

# ---- CLP 复合列拆分（Pictogram+Signal Word / SCL+M-factor） -------------------
#
# CLP 官方导出把两个字段塞进一列：
#   "Labelling Pictogram, Signal Word Code(s)"  -> "GHS06\r\nGHS08\r\nDgr"
#   "M, SCL, ATE" / "Specific Conc. Limits, M-factors"
#                                               -> "Repr. 1B; H360FD: C ≥ 3,1 %\r\nM=10\r\n"
# 旧迁移用 safe_get_col 按子串取列，整串同时落进 pictogram 与 signal_word_codes
# （库里 332/332 行两列完全相同，signal_word_codes 里没有一行是纯 Dgr/Wng），
# specific_conc_limits 与 m_factors 同样 332/332 相同。这里把拆法收成一份实现，
# 迁移与增量共用。

# 定位"图示 + 信号词"复合列：新旧表头分别是 "Pictogram, Signal Word Code(s)"
# 与 "Labelling Pictogram, Signal Word Code(s)"。
find_clp_label_col <- function(nms) {
  hit <- grep("Pictogram.*Signal Word", nms, value = TRUE)
  if (length(hit) > 0L) hit[1L] else NA_character_
}

# 定位"限值 + M 系数"复合列：meta 表头 "Specific Conc. Limits, M-factors"，
# 新导出表头 "M, SCL, ATE"（normalize_cmr_df 会把它改名成前者）。
find_clp_limit_col <- function(nms) {
  hit <- grep("^Specific Conc\\. Limits|^M, SCL", trimws(nms), value = TRUE)
  if (length(hit) > 0L) hit[1L] else NA_character_
}

# 取列内容；列名缺失时返回等长的 NA 字符向量（让拆分函数统一处理空值）
col_or_na <- function(df, cn, n) {
  if (!is.na(cn) && !is.null(df) && cn %in% names(df)) {
    return(as.character(df[[cn]]))
  }
  rep(NA_character_, n)
}

#' 拆 CLP 的"图示 + 信号词"复合单元格
#'
#' 按 token 拆而不是按行拆：官方偶发把两个码写在一行（"GHS08 GHS07"）。
#' token 只认 GHS01..GHS09 与 Dgr/Wng；其余（"?" 表示无信息，"****"/"*" 是脚注
#' 标记）不属于这两列中的任何一列，直接丢弃 —— 无信息不冒充有内容。
#'
#' @param x 复合列字符向量
#' @return list(pictogram =, signal_word_codes =)，无内容的元素为 NA
#' @keywords internal
#' @export
#' @encoding UTF-8
split_clp_label_cell <- function(x) {
  x <- as.character(x)
  pic <- rep(NA_character_, length(x))
  sw <- rep(NA_character_, length(x))
  for (i in seq_along(x)) {
    if (is.na(x[i]) || !nzchar(trimws(x[i]))) next
    tok <- trimws(unlist(strsplit(x[i], "[[:space:]]+")))
    tok <- tok[nzchar(tok)]
    g <- unique(tok[grepl("^GHS[0-9]{2}$", tok)])
    s <- unique(ifelse(toupper(tok) %in% c("DGR", "WNG"),
                       ifelse(toupper(tok) == "DGR", "Dgr", "Wng"), NA_character_))
    s <- s[!is.na(s)]
    if (length(g) > 0L) pic[i] <- paste(g, collapse = "\n")
    if (length(s) > 0L) sw[i] <- paste(s, collapse = "\n")
  }
  list(pictogram = pic, signal_word_codes = sw)
}

#' 拆 CLP 的"限值 + M 系数"复合单元格
#'
#' 按行拆（限值行本身带空格，不能按 token 拆）。M 行统一写成 "M = n"：两个源
#' 分别给 "M=1000" 与 "M = 10"，而 canon_cell() 只折换行、不折中间空格，
#' 不归一就会让 diff 永远看到差异。
#'
#' @param x 复合列字符向量
#' @return list(specific_conc_limits =, m_factors =)，无内容的元素为 NA
#' @keywords internal
#' @export
#' @encoding UTF-8
split_clp_limit_cell <- function(x) {
  x <- as.character(x)
  scl <- rep(NA_character_, length(x))
  mf <- rep(NA_character_, length(x))
  for (i in seq_along(x)) {
    if (is.na(x[i]) || !nzchar(trimws(x[i]))) next
    ln <- trimws(unlist(strsplit(x[i], "[\r\n]+")))
    ln <- ln[nzchar(ln)]
    is_m <- grepl("^\\*?\\s*M\\s*=\\s*[0-9]+$", ln)
    if (any(is_m)) {
      mf[i] <- paste(unique(sub("^\\*?\\s*M\\s*=\\s*([0-9]+)$", "M = \\1", ln[is_m])),
                     collapse = "\n")
    }
    if (any(!is_m)) scl[i] <- paste(ln[!is_m], collapse = "\n")
  }
  list(specific_conc_limits = scl, m_factors = mf)
}

#' 从 CLP 源表派生 pictogram / signal_word_codes 两列
#'
#' 只派生**图示 / 信号词**这一对。限值那一对（specific_conc_limits / m_factors）
#' 刻意不在这里派生：新增量源（ATP23）对 91 个 Index No 的 "M, SCL, ATE" 是空的，
#' 老 meta 有值（如 005-008-00-8 的 "Repr. 1B; H360FD: C ≥ 3,1 %"）。一旦派生，
#' 这两列就成了"已映射列"，写库时会把那 91 个限值擦成 NULL 且回不来。
#' 限值列的清洗走另外两条路：迁移侧用 split_clp_limit_cell()，既有库走
#' heal_cmr_split_cols()。
#'
#' @param df 已重命名到库列名的 CMR 数据框
#' @return 加了两列的 data.frame；源里没有复合列时原样返回
#' @keywords internal
#' @export
#' @encoding UTF-8
derive_cmr_label_cols <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(df)
  cn <- find_clp_label_col(names(df))
  if (is.na(cn)) return(df)
  sp <- split_clp_label_cell(df[[cn]])
  df$pictogram <- sp$pictogram
  df$signal_word_codes <- sp$signal_word_codes
  df
}

#' 修 cmr 表里限值列的重复（幂等自愈）
#'
#' 旧迁移把 "Specific Conc. Limits, M-factors" 整串同时写进 specific_conc_limits
#' 与 m_factors。这里只动**两列完全相同**的行：把 M 行搬进 m_factors、其余留在 scl。
#' 两列本来就不同的行一律不碰 —— 既保证幂等，也避免覆盖已经独立的值。
#'
#' @param df 库表数据框，或已回填过库旧值的 new_df
#' @return 修好的 data.frame；缺少这两列时原样返回
#' @keywords internal
#' @export
#' @encoding UTF-8
heal_cmr_split_cols <- function(df) {
  need <- c("specific_conc_limits", "m_factors")
  if (is.null(df) || nrow(df) == 0 || !all(need %in% names(df))) return(df)
  scl <- as.character(df$specific_conc_limits)
  mf <- as.character(df$m_factors)
  same <- !is.na(scl) & !is.na(mf) & canon_cell(scl) == canon_cell(mf)
  if (!any(same)) return(df)
  sp <- split_clp_limit_cell(scl)
  df$specific_conc_limits[same] <- sp$specific_conc_limits[same]
  df$m_factors[same] <- sp$m_factors[same]
  df
}

# ---- 标准化：源列名 -> 库表列名（只做必要重命名 + 清洗，其余交给
#      run_incremental_update() 的 map_to_db_columns() 动态对齐） --------------

#' 标准化 CMR / CMR_suspect 数据框
#'
#' @param df 原始数据框（meta 文件或合并表头后的 CLP 导出）
#' @return 列名对齐后的 data.frame
#' @keywords internal
#' @export
#' @encoding UTF-8
normalize_cmr_df <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(df)
  # 源文件里的别名 -> 库表列名
  #  旧 meta 文件列名：International Chemical Identification 等与库表一致，只有
  #  小写 s 的 "Hazard statement Code(s)" 是备用呈现列；
  #  新 ECHA 官方导出（ATP 版 xlsx）列名不同：Chemical Name / M, SCL, ATE / ATP，
  #  且无备用 H 代码列（由 run_incremental_update 按 key 从库回填，避免假 modified）。
  df <- rename_source_cols(df, c(
    "Hazard statement Code(s)" = "Hazard Statement Code Alternative",
    "Chemical Name" = "International Chemical Identification",
    "M, SCL, ATE" = "Specific Conc. Limits, M-factors",
    "ATP" = "ATP inserted/ATP Updated",
    "MolecularFormula" = "Formula",
    "IsomericSMILES" = "SMILES"
  ))
  clean_key_cols(df, c("Index No", "CAS No", "EC No"))
  # 图示与信号词在源里是同一列，派生回库表的两列（见 derive_cmr_label_cols）
  derive_cmr_label_cols(df)
}

#' 从标准化后的 CLP 全表筛出 CMR / CMR_suspect 子集
#'
#' 按主危险说明代码列（"Hazard Statement Code(s)"）做子串匹配：
#' cmr 取含 H340/H350/H360 的行，cmr_suspect 取含 H341/H351/H361 的行。
#' 与早期全量更新时代（update_databases() 已移除）的筛选规则一致；两者非互斥，独立筛。
#'
#' @param df 标准化后的 CLP 全表（normalize_cmr_df() 输出）
#' @param kind "cmr" 或 "cmr_suspect"
#' @return 筛选后的 data.frame
#' @keywords internal
#' @export
#' @encoding UTF-8
screen_clp <- function(df, kind = c("cmr", "cmr_suspect")) {
  if (is.null(df) || nrow(df) == 0) return(df)
  kind <- match.arg(kind)
  hcol <- find_hazard_code_col(names(df))
  pattern <- if (kind == "cmr") "H340|H350|H360" else "H341|H351|H361"
  keep <- stringr::str_detect(as.character(df[[hcol]]), pattern)
  keep[is.na(keep)] <- FALSE
  df[keep, , drop = FALSE]
}

#' 定位 CLP 主危险说明代码列
#'
#' 优先精确匹配 "Hazard Statement Code(s)"（大写 S）；找不到时按正则
#' "Hazard Statement Code(s)"（大小写敏感）并排除 Alternative/Suppl 备用列兜底；
#' 仍找不到则报错，避免把整张总表误当 CMR 静默入库。
#'
#' @param nms 列名向量
#' @return 主 H 代码列名
#' @keywords internal
#' @export
#' @encoding UTF-8
find_hazard_code_col <- function(nms) {
  if ("Hazard Statement Code(s)" %in% nms) return("Hazard Statement Code(s)")
  cand <- nms[grepl("Hazard Statement Code\\(s\\)", nms) &
                !grepl("Alternative|Suppl", nms)]
  if (length(cand) >= 1L) return(cand[1L])
  stop("CMR screening: cannot locate the 'Hazard Statement Code(s)' column")
}

#' 标准化 IARC 数据框
#'
#' @param df 原始数据框（iarc_meta.xlsx 或 download_iarc() 输出）
#' @return 列名对齐后的 data.frame
#' @keywords internal
#' @export
#' @encoding UTF-8
normalize_iarc_df <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(df)
  df <- rename_source_cols(df, c(
    "Year" = "Volume publication year",
    "MolecularFormula" = "Formula",
    "IsomericSMILES" = "SMILES"
  ))
  clean_key_cols(df, c("Agent", "CAS No."))
}

#' 标准化 EU SML 数据框
#'
#' 业务列名与库表一致（含换行列名由 map_to_db_columns 去空白对齐），这里只处理
#' 化学列别名、键列清洗，以及 SML / SML(T) 两列的数值口径（见
#' [normalize_eu_sml_values()]）。
#'
#' @param df 原始数据框（eu10_2011_meta.xlsx 或 download_eu_sml() 的 SML sheet）
#' @return 列名对齐后的 data.frame
#' @keywords internal
#' @export
#' @encoding UTF-8
normalize_eu_sml_df <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(df)
  df <- rename_source_cols(df, c(
    "MolecularFormula" = "Formula",
    "IsomericSMILES" = "SMILES"
  ))
  df <- clean_key_cols(df, c("FCM substance No", "CAS No"))
  normalize_eu_sml_values(df)
}

#' 按库内既有口径归一 EU SML 的 SML / SML(T) 两列
#'
#' 重建库时写入的 `sml` 已经把 "0,05" 写成 0.05、"ND"（未检出）写成 0.01 并转
#' 成数值；`sml_group` 的括号被剥掉（"(15)" -> "15"）。增量链路若原样传源值，
#' diff 会把这两列整片判成 modified（实测 279 行假变更），而真入库还会把 "ND"
#' 这类文本塞进数值列、把 "(15)" 塞进组号列，破坏下游的数值/组号比较。
#' 这里复刻同一套口径，保证"源 -> 库"的值域一致。
#'
#' @param df 标准化中的 EU SML 数据框
#' @return 处理后的 data.frame
#' @keywords internal
#' @export
#' @encoding UTF-8
normalize_eu_sml_values <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(df)
  sml_col <- map_source_colname(df, "eu_sml", "sml")
  if (!is.na(sml_col)) {
    v <- as.character(df[[sml_col]])
    v <- sub(",", ".", v)        # 源用逗号作小数点
    v <- sub("ND", "0.01", v)    # 未检出 -> 0.01 mg/kg
    v <- sub("\n.*$", "", v)     # 截掉换行后的补充说明
    df[[sml_col]] <- suppressWarnings(as.numeric(trimws(v)))
  }
  grp_col <- map_source_colname(df, "eu_sml", "sml_group")
  if (!is.na(grp_col)) {
    df[[grp_col]] <- gsub("[()]", "", as.character(df[[grp_col]]))
  }
  df
}

# ---- 法规库注册表：每库一条注册项（单一事实源） ------------------------------
#
# 增量更新线的全部逐库差异都是数据不是逻辑，集中登记在这里：
#   label        人类可读名（报错与进度 message 用）
#   line         "incremental"（走 run_incremental_update 公共流水线）或 "svhc"
#                （独立一条线，见 update_svhc.R；code-map 约定不迁公共线）
#   key_col 等   run_incremental_update 的键/列参数
#   fetch        取数配置：下载函数名与落盘名、各分支（显式文件 / 下载 / 本地
#                候选 / meta 兜底）各自的 sheet 与表头层数、期望列、normalize
#                函数、H 码筛选（screen_kind，仅 CLP 派生的两库）
# 新增一个法规库的触点因此收敛为：① schema.sql 建表 ② download_sources.R 加
# 下载函数 ③ 这里加一条注册项。注意 fetch 层是 internal，可自由演进；
# 导出 update_*_auto 保留原签名作为薄壳（测试与用户依赖）。
# 人工新清单探测集（manual_candidates）同样登记在此：manual_list_check.R 的
# file_map 与 database_inspector_app.R 的 UPDATE_DBS 均由此派生。
# 仍独立的登记点：update_pipeline.R 的 db_col_candidates（逐库列名差异）、
# fetch_svhc_local 的本地回退名单（SVHC 独立线，含 svhc_meta 备份语义）。
DB_SOURCES <- list(
  cmr = list(
    label = "CMR", line = "incremental",
    key_col = "index_no", fallback_col = "cas_no",
    cas_col = "cas_no", name_col = "international_chemical_identification",
    manual_candidates = c("clp_new.xlsx", "clp_new.csv", "annex_vi_clp.xlsx"),
    fetch = list(
      download_label = "CLP",
      download_fun = "download_clp", download_file = "clp.xlsx",
      download_fallback = TRUE,
      new_file_sheet = NULL, new_file_two_hdr = TRUE,
      download_sheet = NULL, download_two_hdr = TRUE,
      local_candidates = c("clp_new.xlsx", "clp_new.csv", "annex_vi_clp.xlsx"),
      local_sheet = NULL, local_two_hdr = TRUE,
      meta_file = "clp_cmr_meta.xlsx", meta_sheet = "cmr", meta_two_hdr = FALSE,
      expected_col = "Index No",
      normalize = normalize_cmr_df,
      screen_kind = "cmr"
    )
  ),
  cmr_suspect = list(
    label = "CMR_suspect", line = "incremental",
    key_col = "index_no", fallback_col = "cas_no",
    cas_col = "cas_no", name_col = "substance_name",
    manual_candidates = NULL,  # 与 cmr 共用同一份 CLP 文件，由 cmr 的候选探测覆盖
    fetch = list(
      download_label = "CLP",
      download_fun = "download_clp", download_file = "clp.xlsx",
      download_fallback = TRUE,
      new_file_sheet = NULL, new_file_two_hdr = FALSE,
      download_sheet = NULL, download_two_hdr = TRUE,
      local_candidates = NULL,
      local_sheet = NULL, local_two_hdr = FALSE,
      meta_file = "clp_cmr_meta.xlsx", meta_sheet = "cmr_suspect", meta_two_hdr = FALSE,
      expected_col = "Index No",
      normalize = normalize_cmr_df,
      screen_kind = "cmr_suspect"
    )
  ),
  iarc = list(
    label = "IARC", line = "incremental",
    key_col = "cas_no", fallback_col = "agent",
    cas_col = "cas_no", name_col = "agent",
    manual_candidates = c("iarc_new.xlsx", "iarc_new.csv"),
    fetch = list(
      download_label = "IARC",
      download_fun = "download_iarc", download_file = "iarc.xlsx",
      download_fallback = FALSE,
      new_file_sheet = NULL, new_file_two_hdr = FALSE,
      download_sheet = NULL, download_two_hdr = FALSE,
      local_candidates = c("iarc_new.xlsx", "iarc_new.csv"),
      local_sheet = NULL, local_two_hdr = FALSE,
      meta_file = "iarc_meta.xlsx", meta_sheet = NULL, meta_two_hdr = FALSE,
      expected_col = "Agent",
      normalize = normalize_iarc_df,
      screen_kind = NULL
    )
  ),
  eu_sml = list(
    label = "EU SML", line = "incremental",
    key_col = "fcm_substance_no", fallback_col = "cas_no",
    cas_col = "cas_no", name_col = "substance_name",
    manual_candidates = c("eu10_2011_new.xlsx", "eu10_2011_new.csv"),
    fetch = list(
      download_label = "EU SML",
      download_fun = "download_eu_sml", download_file = "eu10_2011.xlsx",
      download_fallback = FALSE,
      new_file_sheet = "SML", new_file_two_hdr = FALSE,
      download_sheet = "SML", download_two_hdr = FALSE,
      local_candidates = c("eu10_2011_new.xlsx", "eu10_2011_new.csv"),
      local_sheet = "SML", local_two_hdr = FALSE,
      meta_file = "eu10_2011_meta.xlsx", meta_sheet = NULL, meta_two_hdr = FALSE,
      expected_col = "FCM substance No",
      normalize = normalize_eu_sml_df,
      screen_kind = NULL
    )
  ),
  svhc = list(
    label = "SVHC", line = "svhc",
    # svhc_meta.xlsx 刻意不在探测集：它是库内全量备份，不是人工新清单
    # （fetch_svhc_local 的本地回退名单另有它，见 update_svhc.R）
    manual_candidates = c("candidate_list.xlsx", "svhc_new.xlsx", "svhc_new.csv")
  )
)

# ---- 数据获取：读本地新文件 / 回退 meta 文件 / 下载 --------------------------

#' 在 inst/ 中按候选名解析第一个存在的文件
#'
#' @param candidates 候选文件名向量
#' @param inst_dir inst 目录
#' @return 存在的文件路径，找不到返回 NULL
#' @keywords internal
#' @export
#' @encoding UTF-8
resolve_source_file <- function(candidates, inst_dir = file.path(getwd(), "inst")) {
  for (cand in candidates) {
    p <- file.path(inst_dir, cand)
    if (file.exists(p)) return(p)
  }
  NULL
}

#' 注册表驱动的通用取数（internal）
#'
#' 各库取数的唯一实现：显式文件 -> 下载（可配置失败回退本地）->
#' 本地候选名 -> meta 兜底，各分支的 sheet / 表头层数由 DB_SOURCES 注册表的
#' fetch 配置决定，最后统一 normalize（CLP 派生的两库再按 H 码筛）。
#'
#' 一律按 H 码筛的注释（cmr/cmr_suspect）：下载的 CLP 是全表必须筛；已筛过的
#' 文件重复筛是幂等操作（str_detect 只会保留，不会误删），但能挡住"手动放进
#' 来的 clp_new.xlsx 其实是 CLP 全表"这种情况——2026-09-10 实测该路径原先不筛，
#' 会把非 CMR 物质（只要 InChIKey 非空）写进 cmr 表。
#'
#' @param db_name 库名（DB_SOURCES 的名字）
#' @param source "local" 或 "download"
#' @param new_file 显式文件路径（优先于候选名）
#' @param inst_dir inst 目录
#' @return 标准化后的 data.frame
#' @keywords internal
#' @export
#' @encoding UTF-8
fetch_source_data <- function(db_name, source = c("local", "download"),
                              new_file = NULL,
                              inst_dir = file.path(getwd(), "inst")) {
  source <- match.arg(source)
  cfg <- DB_SOURCES[[db_name]]$fetch
  path <- NULL
  sheet <- NULL
  two_hdr <- FALSE

  # 显式文件优先：直接读，不触发下载，避免覆盖 inst/ 真实数据
  if (!is.null(new_file) && file.exists(new_file)) {
    path <- new_file
    sheet <- cfg$new_file_sheet
    two_hdr <- cfg$new_file_two_hdr
  } else if (source == "download") {
    if (isTRUE(cfg$download_fallback)) {
      dl_ok <- tryCatch({
        do.call(cfg$download_fun, list(out = file.path(inst_dir, cfg$download_file)))
        TRUE
      }, error = function(e) {
        message("   ", cfg$download_label, " download failed, falling back to local ",
                cfg$label, " source: ", conditionMessage(e))
        FALSE
      })
      if (dl_ok) {
        path <- file.path(inst_dir, cfg$download_file)
        sheet <- cfg$download_sheet
        two_hdr <- cfg$download_two_hdr
      }
    } else {
      do.call(cfg$download_fun, list(out = file.path(inst_dir, cfg$download_file)))
      path <- file.path(inst_dir, cfg$download_file)
      sheet <- cfg$download_sheet
      two_hdr <- cfg$download_two_hdr
    }
  }

  if (is.null(path)) {
    path <- resolve_source_file(cfg$local_candidates, inst_dir)
    if (is.null(path)) {
      path <- file.path(inst_dir, cfg$meta_file)
      sheet <- cfg$meta_sheet
      two_hdr <- cfg$meta_two_hdr
    } else {
      sheet <- cfg$local_sheet
      two_hdr <- cfg$local_two_hdr
    }
  }
  if (!file.exists(path)) stop("No ", cfg$label, " source file found in ", inst_dir)
  message("Reading ", cfg$label, " source: ", path)
  raw <- read_source_table(path, sheet = sheet, expected_col = cfg$expected_col,
                           two_row_header = two_hdr)
  df <- cfg$normalize(raw)
  if (is.null(cfg$screen_kind)) df else screen_clp(df, cfg$screen_kind)
}

# ---- 各源入口：update_*_auto()（CMR / CMR_suspect / IARC / EU_SML） ----------

#' 注册表驱动的公共更新入口（internal）
#'
#' 四个导出 update_*_auto 薄壳的唯一实现：取数（fetch_source_data）->
#' run_incremental_update 公共流水线，逐库参数全部来自 DB_SOURCES 注册表。
#'
#' @param db_name 库名（DB_SOURCES 的名字）
#' @inheritParams update_cmr_auto
#' @return list(success, changes, db_write)
#' @keywords internal
#' @export
#' @encoding UTF-8
update_source_auto <- function(db_name, source = c("local", "download"),
                               new_file = NULL, interactive = TRUE,
                               auto_apply = FALSE, max_auto_changes = 20,
                               enrich = TRUE, db_path = NULL, backup = TRUE,
                               delay = 0.35) {
  source <- match.arg(source)
  cfg <- DB_SOURCES[[db_name]]
  new_df <- fetch_source_data(db_name, source = source, new_file = new_file)
  message("   ", cfg$label, " rows after normalization: ", nrow(new_df))
  run_incremental_update(
    db_name = db_name, new_df = new_df,
    key_col = cfg$key_col, fallback_col = cfg$fallback_col,
    cas_col = cfg$cas_col, name_col = cfg$name_col,
    content_cols = NULL,
    interactive = interactive, auto_apply = auto_apply,
    max_auto_changes = max_auto_changes, enrich = enrich,
    db_path = db_path, backup = backup, delay = delay
  )
}

#' CMR 自动更新（读源 -> 标准化 -> 只补新增 -> diff -> 确认 -> 入库）
#'
#' 主键 index_no（优先），cas_no 兜底；CAS 列 cas_no。增量策略：先按 CAS 从库回填
#' 老物质化学元数据，只对真正新增的物质查 PubChem，最后按 index_no/cas_no 与库表
#' diff。出现 removed 时强制人工确认。
#'
#' @param source "local"（默认）或 "download"
#' @param new_file 显式文件路径（可选）
#' @param interactive 是否交互确认
#' @param auto_apply 非交互时是否自动入库（受 max_auto_changes 限制）
#' @param max_auto_changes 自动入库最大变更条数
#' @param enrich 是否补 PubChem 化学元数据
#' @param db_path 自定义数据库路径（测试用）
#' @param backup 入库前是否备份
#' @param delay PubChem 请求间隔秒数
#' @return list(success, changes, db_write)
#' @export
#' @export
#' @encoding UTF-8
update_cmr_auto <- function(source = c("local", "download"), new_file = NULL,
                            interactive = TRUE, auto_apply = FALSE,
                            max_auto_changes = 20, enrich = TRUE,
                            db_path = NULL, backup = TRUE, delay = 0.35) {
  update_source_auto("cmr", source = source, new_file = new_file,
                     interactive = interactive, auto_apply = auto_apply,
                     max_auto_changes = max_auto_changes, enrich = enrich,
                     db_path = db_path, backup = backup, delay = delay)
}

#' CMR_suspect 自动更新
#'
#' 与 update_cmr_auto 相同，作用于 cmr_suspect 表（默认读 clp_cmr_meta.xlsx 的
#' "cmr_suspect" 工作表；source = "download" 时下载原始 CLP 并按 H 代码筛）。
#' 主键 index_no（优先；CLP 官方标识），cas_no 兜底。
#'
#' 主键曾是 substance_name，那是错的：物质名是自由文本，上游改一个连字符、逗号
#' 或省略号就会被判成"删一条 + 加一条"。同一份源上，用 index_no 的 cmr 表报 2 行
#' removed，用物质名的这张表报 46 行 —— 而那些行一条都没走。库里老行没有 index_no
#' 时要先跑一次 [migrate_cmr_suspect_index_no()]，否则整张表都没有主键。
#'
#' @param source "local"（默认）或 "download"
#' @param new_file 显式文件路径（可选）
#' @param interactive 是否交互确认
#' @param auto_apply 非交互时是否自动入库
#' @param max_auto_changes 自动入库最大变更条数
#' @param enrich 是否补 PubChem 化学元数据
#' @param db_path 自定义数据库路径
#' @param backup 入库前是否备份
#' @param delay PubChem 请求间隔秒数
#' @return list(success, changes, db_write)
#' @export
#' @export
#' @encoding UTF-8
update_cmr_suspect_auto <- function(source = c("local", "download"),
                                    new_file = NULL, interactive = TRUE,
                                    auto_apply = FALSE, max_auto_changes = 20,
                                    enrich = TRUE, db_path = NULL, backup = TRUE,
                                    delay = 0.35) {
  update_source_auto("cmr_suspect", source = source, new_file = new_file,
                     interactive = interactive, auto_apply = auto_apply,
                     max_auto_changes = max_auto_changes, enrich = enrich,
                     db_path = db_path, backup = backup, delay = delay)
}

#' IARC 自动更新
#'
#' 主键 cas_no（优先），agent 兜底；CAS 列 cas_no。增量策略同上：先回填老物质，
#' 只对新增查 PubChem，按 cas_no/agent 与库表 diff。
#'
#' @param source "local"（默认）或 "download"
#' @param new_file 显式文件路径（可选）
#' @param interactive 是否交互确认
#' @param auto_apply 非交互时是否自动入库
#' @param max_auto_changes 自动入库最大变更条数
#' @param enrich 是否补 PubChem 化学元数据
#' @param db_path 自定义数据库路径
#' @param backup 入库前是否备份
#' @param delay PubChem 请求间隔秒数
#' @return list(success, changes, db_write)
#' @export
#' @export
#' @encoding UTF-8
update_iarc_auto <- function(source = c("local", "download"), new_file = NULL,
                             interactive = TRUE, auto_apply = FALSE,
                             max_auto_changes = 20, enrich = TRUE,
                             db_path = NULL, backup = TRUE, delay = 0.35) {
  update_source_auto("iarc", source = source, new_file = new_file,
                     interactive = interactive, auto_apply = auto_apply,
                     max_auto_changes = max_auto_changes, enrich = enrich,
                     db_path = db_path, backup = backup, delay = delay)
}

#' EU SML 自动更新
#'
#' 主键 fcm_substance_no（优先），cas_no 兜底；CAS 列 cas_no。增量策略同上。
#'
#' `enrich` 默认 TRUE 的理由同 [update_cmr_auto()]：官方 eu_sml.xlsx 不带结构列，
#' 不查 PubChem 则新增行拿不到键、进不了库。
#'
#' @param source "local"（默认）或 "download"
#' @param new_file 显式文件路径（可选）
#' @param interactive 是否交互确认
#' @param auto_apply 非交互时是否自动入库
#' @param max_auto_changes 自动入库最大变更条数
#' @param enrich 是否补 PubChem 化学元数据
#' @param db_path 自定义数据库路径
#' @param backup 入库前是否备份
#' @param delay PubChem 请求间隔秒数
#' @return list(success, changes, db_write)
#' @export
#' @export
#' @encoding UTF-8
update_eu_sml_auto <- function(source = c("local", "download"), new_file = NULL,
                               interactive = TRUE, auto_apply = FALSE,
                               max_auto_changes = 20, enrich = TRUE,
                               db_path = NULL, backup = TRUE, delay = 0.35) {
  update_source_auto("eu_sml", source = source, new_file = new_file,
                     interactive = interactive, auto_apply = auto_apply,
                     max_auto_changes = max_auto_changes, enrich = enrich,
                     db_path = db_path, backup = backup, delay = delay)
}

# =============================================================================
# 总入口：update_database_auto() —— 一键更新全部/指定数据源（薄调度层）
#
# 定位：只做"排程"，不写任何更新逻辑。下载 / diff / 入库全部转发给各源子函数：
# cmr / cmr_suspect / iarc / eu_sml -> update_source_auto()（注册表驱动），
# svhc -> update_svhc_auto()（update_svhc.R）。
# 维护约定：新增数据源 = DB_SOURCES 加一条注册项（名单与分发自动派生）；
# 改某源行为 = 改注册项或对应子函数，这里不动。
# =============================================================================

#' 可一键自动更新的数据源清单
#' @keywords internal
#' @export
#' @encoding UTF-8
ALL_AUTO_DBS <- names(DB_SOURCES)

#' 解析 databases 参数："all" 展开为全部数据源，否则校验名字合法
#' @keywords internal
#' @export
#' @encoding UTF-8
resolve_db_names <- function(databases) {
  if (identical(databases, "all")) return(ALL_AUTO_DBS)
  databases <- as.character(databases)
  bad <- setdiff(databases, ALL_AUTO_DBS)
  if (length(bad) > 0) {
    stop("Unknown database(s): ", paste(bad, collapse = ", "),
         ". Use \"all\" or any of: ", paste(ALL_AUTO_DBS, collapse = ", "))
  }
  databases
}

#' 一键更新数据源（总入口）
#'
#' 薄调度层：一次调用更新全部或指定的数据源。具体下载、diff、入库逻辑全部
#' 转发给各源子函数（update_cmr_auto / update_cmr_suspect_auto /
#' update_iarc_auto / update_eu_sml_auto / update_svhc_auto），本函数只负责
#' 选库、传参、收集结果，不含任何更新逻辑。
#'
#' 默认行为（一键全更新）：
#' - 无人值守：interactive = FALSE 且 auto_apply = TRUE，跑完不逐个询问；
#'   但子函数自带的"出现 removed 强制人工确认"安全阀仍生效——有清单移除的库
#'   不会自动入库，会在汇总表里标 failed 并写明原因。
#' - 联网取数：4 个公共源 source = "download"（重新下载最新清单），SVHC 走
#'   svhc_source = "auto"（ECHA 官网 -> 本地兜底）。注意默认语义与单库函数
#'   相反：各 update_*_auto() 单独调用时默认 source = "local"（读 inst/ 现有
#'   文件、不联网），只有本总入口默认联网下载。
#' - 单库失败不中断：某库抓取失败只在该行记录错误，其余库照常跑完。
#'
#' @param databases 要更新的库："all"（默认）或
#'   c("cmr", "cmr_suspect", "iarc", "eu_sml", "svhc") 的任意子集
#' @param source 4 个公共源（cmr/cmr_suspect/iarc/eu_sml）的取数方式：
#'   "download"（联网下载最新，默认）或 "local"（用 inst/ 现有文件）
#' @param svhc_source SVHC 的取数方式："auto"（默认，ECHA->本地）、
#'   "local"、"echa"
#' @param interactive 是否逐库交互确认（默认 FALSE）
#' @param auto_apply 非交互模式下是否自动入库（默认 TRUE；变更数超过
#'   max_auto_changes 的库不会自动写库）
#' @param max_auto_changes 单库自动入库的最大变更条数（默认 20）
#' @param enrich 是否补 PubChem 化学元数据；NULL（默认）= 不传，尊重各子库
#'   自带默认（五个源现在都是 TRUE）；显式 TRUE/FALSE 则统一所有库
#' @param db_path 自定义数据库路径（测试用）
#' @param backup 入库前是否备份数据库（默认 TRUE）
#' @param delay PubChem 请求间隔秒数（仅 4 个公共源用到；SVHC 无此参数）
#' @return 汇总 data.frame（打印后 invisible 返回）：每个库一行，列为
#'   database / status("ok"|"failed") / added / removed / modified / message
#' @export
#' @export
#' @encoding UTF-8
update_database_auto <- function(databases = "all",
                                 source = c("download", "local"),
                                 svhc_source = c("auto", "local", "echa"),
                                 interactive = FALSE,
                                 auto_apply = TRUE,
                                 max_auto_changes = 20,
                                 enrich = NULL,
                                 db_path = NULL,
                                 backup = TRUE,
                                 delay = 0.35) {
  source <- match.arg(source)
  svhc_source <- match.arg(svhc_source)
  dbs <- resolve_db_names(databases)

  results <- lapply(dbs, function(db) {
    message("\n==== Updating ", db, " ====")
    args <- list(interactive = interactive, auto_apply = auto_apply,
                 max_auto_changes = max_auto_changes,
                 db_path = db_path, backup = backup)
    fn <- update_source_auto
    if (identical(DB_SOURCES[[db]]$line, "svhc")) {
      args$source <- svhc_source              # SVHC 无 delay 参数
      fn <- update_svhc_auto
    } else {
      args$source <- source
      args$delay <- delay
      args <- c(list(db_name = db), args)
    }
    # enrich = NULL 时不传，尊重各子库自带默认
    if (!is.null(enrich)) args$enrich <- enrich
    tryCatch(
      list(db = db, res = do.call(fn, args)),
      error = function(e) list(db = db,
                               res = list(success = FALSE,
                                          message = conditionMessage(e)))
    )
  })

  # 汇总表：计数取不到（失败/未跑完）置 NA，不影响阅读结果
  summary <- data.frame(
    database = vapply(results, function(x) x$db, character(1)),
    status   = vapply(results,
                      function(x) if (isTRUE(x$res$success)) "ok" else "failed",
                      character(1)),
    added    = vapply(results, function(x) {
      v <- x$res$changes$total_added
      if (is.null(v)) NA_integer_ else as.integer(v)
    }, integer(1)),
    removed  = vapply(results, function(x) {
      v <- x$res$changes$total_removed
      if (is.null(v)) NA_integer_ else as.integer(v)
    }, integer(1)),
    modified = vapply(results, function(x) {
      v <- x$res$changes$total_modified
      if (is.null(v)) NA_integer_ else as.integer(v)
    }, integer(1)),
    message  = vapply(results, function(x) {
      if (is.null(x$res$message)) "" else as.character(x$res$message)
    }, character(1)),
    stringsAsFactors = FALSE, row.names = NULL
  )
  print(summary)
  invisible(summary)
}



# ---- 模块常量：列名表 / 系统列 / 占位符 --------------------------------------

# 化学元数据列。新库把它们集中存在 chemicals 总表，
# 各业务表只保留 InChIKey 作为引用（不再内嵌）。
chem_cols <- c("CID", "Formula", "SMILES", "InChIKey", "IUPACName", "ExactMass")

# 新库业务表的"系统列"：id / 时间戳由 SQLite 自动维护（DEFAULT），
# 显式插入 NULL 会覆盖默认值，因此写库时一律排除这些列。
sys_cols <- c("id", "created_at", "updated_at")

# 每库"库列名 -> 源列候选名"：仅登记"库列名（蛇形）与源列名对不上、
# 连超归一匹配（去全部非字母数字 + 小写）也找不到"的列——即列名用词不同
# （如 Group vs group_classification、SML(T) vs sml_group），或源列名在
# 标准化后被改写（如备用 H 代码列被 normalize_cmr_df 改名）。能通过超归一
# 匹配命中的列（如 Index No <-> index_no）无需登记。候选按顺序取第一个命中。
db_col_candidates <- list(
  cmr = list(
    # CLP 官方导出的 H 码列名是"分组名 + 子列名"拼成的长名，与库列名不同词：
    #   Classification Hazard Statement Code(s) -> hazard_statement_codes
    #   Labelling Hazard Statement Code(s)      -> hazard_statement_codes_alt
    # 必须把 Labelling 排在通用词 "Hazard statement Code(s)" 之前——通用词会被
    # first_matching_colname 的"包含"级匹配到 Classification 那列（列序在前），
    # 导致 alt 列取到主 H 码、主 H 码列反而无人认领（两列长期不更新）。
    hazard_statement_codes = c("Classification Hazard Statement Code(s)",
                               "Hazard Statement Code(s)"),
    hazard_statement_codes_alt = c("Labelling Hazard Statement Code(s)",
                                   "Hazard Statement Code Alternative",
                                   "Hazard statement Code(s)"),
    atp_inserted_updated = c("ATP inserted/ATP Updated", "ATP")
  ),
  cmr_suspect = list(
    # index_no 是 CLP 官方标识，也是这张表的 diff 主键（见 update_cmr_suspect_auto）。
    # 显式列出来是为了不落到"包含"级模糊匹配 —— 源里还有 "Index No" 之外的
    # 编号类列时，误命中会让整个主键错位。
    index_no = c("Index No", "index_no"),
    substance_name = c("International Chemical Identification",
                       "Substance name", "Chemical Name"),
    classification = c("Classification Hazard Statement Code(s)",
                       "Hazard Statement Code(s)", "Hazard statement Code(s)")
  ),
  iarc = list(
    group_classification = c("Group"),
    volume_publication_year = c("Year")
  ),
  eu_sml = list(
    use_as_additive = c("Use as additive"),
    use_as_monomer = c("Use as monomer"),
    frf_applicable = c("FRF applicable"),
    sml = c("SML [mg/kg]"),
    sml_group = c("SML(T)")
  )
)

# 键占位符：这些值视为"无有效标识"，不能作为 diff 键或 PubChem 查询条件
key_placeholder <- c("", "-", "\u2013", "\u2014", "\u2015", "n/a", "N/A", "NA")

# ---- 键与 CAS 归一（消化 0266309-43-7 / 266309-43-7 这类格式差异） -----------

#' 判断向量元素是否为"空键/占位符"
#'
#' @param x 字符向量
#' @return 逻辑向量
#' @keywords internal
#' @encoding UTF-8
is_blank_key <- function(x) {
  is.na(x) | x %in% key_placeholder
}

#' 规范化 CAS 字符串（去掉各段前导 0、统一分隔符、剥序号标记）
#'
#' 用于解决不同来源 CAS 格式不一致的问题：有的带前导 0（如
#' 0266309-43-7），有的不带（如 266309-43-7）。规范化后两者相等，
#' 回填和 diff 就不会因为格式差异误判。
#'
#' 多值单元格统一成 `";"` 分隔，单元格里各 CAS 的 `[n]` 序号标记
#' （CLP 官方导出的写法）会被剥掉。注意本函数的职责是**归一化**，不是
#' "挑一个能用的 CAS"——需要后者的场合用 [extract_cas_candidates()]。
#'
#' @param x 字符向量，元素可为单个 CAS，也可为多个 CAS 用 ";"、"\\n"
#'   或 "\\r\\n" 分隔（分隔符由官方导出决定）
#' @return 字符向量；无法规范化的元素返回 NA_character_
#' @keywords internal
#' @encoding UTF-8
canonicalize_cas <- function(x) {
  if (is.null(x)) return(NULL)
  x <- as.character(x)
  # 官方 CLP 导出里是 \r\r\n，逐级归一到单个分隔符
  x <- gsub("\r\n", "\n", x, fixed = TRUE)
  x <- gsub("\r", "\n", x, fixed = TRUE)
  x <- gsub("\n", ";", x, fixed = TRUE)
  # 多 CAS 单元格带序号标记（"10043-35-3 [1]"、"1332-77-0 [12]"），必须在归一
  # **之前**剥掉：否则尾段是 "0 [1]"，剥前导 0 得到 " [1]" 而不是 "0"，CAS 被
  # 削成 "1332-77- [1]" 这种残串。实测 cmr 源 607 行、eu_sml 源 115 行栽在这，
  # 拿残串去 PubChem 必然查不到。
  x <- gsub("\\[[0-9]+\\]", "", x)
  x <- trimws(x)

  canon_seg <- function(s) {
    segs <- strsplit(s, "-", fixed = TRUE)[[1]]
    if (length(segs) != 3) return(s)  # 非标准三段 CAS 不做格式统一
    segs <- vapply(seq_along(segs), function(j) {
      z <- segs[j]
      # 只在纯数字段上剥前导 0：段里混了别的字符时原样保留，
      # 免得再制造出 "50-00-" 这类残缺值
      if (!grepl("^[0-9]+$", z)) return(z)
      z <- gsub("^0+", "", z)
      if (!nzchar(z)) z <- "0"
      # 第二段（倒数第二段）统一补到 2 位，避免 50-0-0 vs 50-00-0 不一致
      if (j == 2 && nchar(z) == 1) z <- paste0("0", z)
      z
    }, character(1), USE.NAMES = FALSE)
    paste(segs, collapse = "-")
  }

  vapply(x, function(s) {
    if (is.na(s) || !nzchar(s) || is_blank_key(s)) return(NA_character_)
    parts <- strsplit(s, ";", fixed = TRUE)[[1]]
    parts <- trimws(parts)
    parts <- parts[nzchar(parts) & parts != "-"]
    parts <- vapply(parts, canon_seg, character(1), USE.NAMES = FALSE)
    parts <- unique(parts[nzchar(parts)])
    if (length(parts) == 0) return(NA_character_)
    paste(parts, collapse = ";")
  }, character(1), USE.NAMES = FALSE)
}

# CAS 号形状：首段 2-7 位、次段 2 位、校验位 1 位
cas_number_pattern <- "\\b[0-9]{2,7}-[0-9]{2}-[0-9]\\b"

#' 从一个单元格里抽出所有可用的 CAS 号
#'
#' 官方导出把多个 CAS 塞进同一格，分隔方式各家不同：CLP 是换行（带 `[n]`
#' 序号标记）、IARC 是 `", "`、EU SML 是换行加一长串缩进空格、手工维护的
#' 清单偶尔用空格。**不按分隔符切分**，直接正则捞出所有 CAS 形状的子串再逐个
#' 归一化——切分法会漏掉空格分隔的写法，也会把 `"n/a"` 当成一个候选。
#'
#' 返回值是列表，每个元素对应该行清洗后的候选序列（按源顺序、已去重）。
#' 调用方按顺序逐个查，命中即停：CLP 的多 index 条目常常是第一个 CAS 在
#' PubChem 里没有条目、后面某一组分才有。
#'
#' @param x 字符向量，元素可为单个 CAS，也可为多值单元格
#' @return list，长度同 `x`；每个元素是字符向量（可能为空）
#' @keywords internal
#' @encoding UTF-8
extract_cas_candidates <- function(x) {
  if (is.null(x)) return(list())
  x <- as.character(x)
  lapply(x, function(s) {
    if (is.na(s) || !nzchar(trimws(s))) return(character(0))
    m <- regmatches(s, gregexpr(cas_number_pattern, s, perl = TRUE))[[1]]
    if (length(m) == 0L) return(character(0))
    m <- canonicalize_cas(m)
    unique(m[!is.na(m)])
  })
}

# ---- PubChem 查询（只对真正新增的物质发请求，带限速） ------------------------

#' 向 PubChem 发一次 GET
#'
#' 单独抽成一层只是为了可测：单测里 mock 这一层就能覆盖重试逻辑，不必联网。
#'
#' @param url 完整 URL
#' @param timeout 超时秒数
#' @return `httr::response`
#' @noRd
pubchem_http_get <- function(url, timeout = 30) {
  httr::GET(url, httr::timeout(timeout))
}

#' 带退避重试的 PubChem GET
#'
#' **为什么必须重试**：PubChem 在持续请求下会间歇性返回 429/5xx 或直接断连，
#' 同一个 CAS 隔一会儿重查就正常。旧实现把这类瞬时故障与"确实没有这个物质"
#' 混为一谈（任何非 200 都返回 NA、且不重试），于是 `update_*_auto()` 报成功、
#' 新增行却大批落进 `unassigned_entries` 账本。2026-09-11 实测：账本里抽 20 个
#' CAS 重查，当场 10 个命中；再查一遍，先前失败的 6 个全部 200。
#'
#' 404 / 400 是 PubChem 明确回答"没有这个标识符"，重试没有意义 —— 直接返回
#' `not_found`，不浪费请求。其余非 200（403 / 429 / 5xx）与连接异常一律算
#' `unavailable`，退避后重试 `retries` 次。
#'
#' @param url 完整 URL
#' @param timeout 单次 HTTP 超时秒数
#' @param retries 首次失败后的重试次数（总请求数 = retries + 1）
#' @param backoff 首次退避秒数，之后按 2 的幂递增
#' @return `list(ok, status, text)`；`status` 为 `"ok"` / `"not_found"` /
#'   `"unavailable"`，`text` 仅在 `ok` 时为响应体
#' @noRd
pubchem_get <- function(url, timeout = 30, retries = 3L, backoff = 1) {
  attempt <- 0L
  repeat {
    attempt <- attempt + 1L
    resp <- tryCatch(pubchem_http_get(url, timeout = timeout),
                     error = function(e) NULL)

    if (is.null(resp)) {
      status <- "unavailable"
    } else if (resp$status_code %in% c(404L, 400L)) {
      return(list(ok = FALSE, status = "not_found", text = NA_character_))
    } else if (resp$status_code == 200L) {
      return(list(ok = TRUE, status = "ok",
                  text = rawToChar(resp$content)))
    } else {
      status <- "unavailable"
    }

    if (attempt > retries) return(list(ok = FALSE, status = status, text = NA_character_))
    if (backoff > 0) Sys.sleep(backoff * 2^(attempt - 1))
  }
}

#' 按 CAS 从 PubChem 查询化学元数据
#'
#' 两步：CAS -> CID，再 CID -> 属性（MolecularFormula、IsomericSMILES、
#' CanonicalSMILES、InChIKey、IUPACName、ExactMass）。任一步失败或查不到都
#' 返回全 NA 的列表，由调用方决定是否跳过。
#'
#' 返回值带 `lookup_status` 属性，取值 `"ok"` / `"not_found"` / `"unavailable"`：
#' 前两者之外，调用方必须能把"PubChem 里没有"与"这次没查成"分开，否则账本
#' 里 `no_structure_found` 会撒谎（见 [enrich_new_compounds()]）。
#'
#' @param cas CAS 号字符串
#' @param timeout 单次 HTTP 超时秒数
#' @param retries 每个请求的重试次数
#' @param backoff 首次退避秒数
#' @return 命名 list，含 CID/Formula/SMILES/InChIKey/IUPACName/ExactMass，
#'   并带 `lookup_status` 属性
#' @keywords internal
#' @encoding UTF-8
pubchem_lookup_cas <- function(cas, timeout = 30, retries = 3L, backoff = 1) {
  empty <- list(
    CID = NA_character_, Formula = NA_character_,
    SMILES = NA_character_, InChIKey = NA_character_,
    IUPACName = NA_character_, ExactMass = NA_character_
  )
  tagged <- function(x, status) {
    attr(x, "lookup_status") <- status
    x
  }
  if (is_blank_key(cas)) return(tagged(empty, "not_found"))

  base <- "https://pubchem.ncbi.nlm.nih.gov/rest/pug"

  r1 <- pubchem_get(sprintf("%s/compound/name/%s/cids/JSON",
                            base, utils::URLencode(cas, reserved = TRUE)),
                    timeout = timeout, retries = retries, backoff = backoff)
  if (!r1$ok) return(tagged(empty, r1$status))

  cid <- tryCatch({
    ids <- jsonlite::fromJSON(r1$text)[["IdentifierList"]][["CID"]]
    if (length(ids) > 0) as.integer(ids[1]) else NA_integer_
  }, error = function(e) NA_integer_)

  # 200 但没给出 CID：PubChem 对无效标识符也会回 200 空列表，算"确实没有"
  if (is.na(cid)) return(tagged(empty, "not_found"))

  r2 <- pubchem_get(sprintf(paste0("%s/compound/cid/%d/property/",
                                   "MolecularFormula,IsomericSMILES,CanonicalSMILES,",
                                   "InChIKey,IUPACName,ExactMass/JSON"),
                            base, cid),
                    timeout = timeout, retries = retries, backoff = backoff)
  # CID 已经拿到了，属性这次没读成属于"没查成"，不是"没有这个物质"
  if (!r2$ok) return(tagged(empty, r2$status))

  out <- tryCatch({
    p <- jsonlite::fromJSON(r2$text)[["PropertyTable"]][["Properties"]]
    if (is.null(p) || length(p) == 0) return(tagged(empty, "unavailable"))

    # 逐字段兜底：PubChem Properties 偶发缺失某字段（返回 NULL/character(0)），
    # 一律转 NA_character_，避免调用方赋值 df[[col]][i] <- NULL 崩溃
    fld <- function(f) {
      v <- p[[f]]
      if (is.null(v) || length(v) == 0) return(NA_character_)
      v <- as.character(v[1])
      if (is.na(v)) NA_character_ else v
    }
    smi <- p[["IsomericSMILES"]]
    if (is.null(smi) || length(smi) == 0 || is.na(smi) || !nzchar(smi)) {
      smi <- p[["CanonicalSMILES"]]
    }
    if (is.null(smi) || length(smi) == 0 || is.na(smi) || !nzchar(smi)) {
      smi <- NA_character_
    }

    list(
      CID = as.character(cid),
      Formula = fld("MolecularFormula"),
      SMILES = smi,
      InChIKey = fld("InChIKey"),
      IUPACName = fld("IUPACName"),
      ExactMass = fld("ExactMass")
    )
  }, error = function(e) NULL)

  # 200 却解析不出属性：是"没查成"，交给下一轮再试，不能说成"没有"
  if (is.null(out)) return(tagged(empty, "unavailable"))
  tagged(out, "ok")
}

#' 只对新增物质补 PubChem 化学元数据
#'
#' 增量核心：仅处理"InChIKey 为空 且 CAS 有效"的行（通常是真正新增的物质），
#' 逐行查 PubChem 并写入化学列。已带 InChIKey 的行（如已从库回填）完全跳过，
#' 不发请求。带限速，避免触发 PubChem 封禁。
#'
#' 一个单元格里可能有多个 CAS（CLP 多 index 条目、IARC 组条目、EU SML 聚合物
#' 族），用 [extract_cas_candidates()] 取出候选后**逐个查、命中即停**。只查
#' 第一个会让"主 CAS 在 PubChem 无单体条目、但某组分有"的行整批落空。
#' 取不到任何 CAS 形状的子串时整行跳过，不发请求。
#'
#' `max_cas_try` 给单行的候选数封顶：CLP 有一行写了 31 个 CAS，无上限时
#' 全失败就要连发 31 次请求。序号靠后的成员与整行物质的关联也越来越弱，
#' 用它查到的结构去代表整行本来就牵强。
#'
#' @param df 标准化后的数据框（含化学列，可为全 NA）
#' @param cas_col CAS 列名
#' @param name_col 名称列名（仅用于进度打印）
#' @param delay 每次请求间隔秒数
#' @param verbose 是否打印进度
#' @param max_cas_try 单行最多试几个候选 CAS
#' @return 补全化学列后的 data.frame
#' @keywords internal
#' @encoding UTF-8
enrich_new_compounds <- function(df, cas_col, name_col, delay = 0.35,
                                 verbose = TRUE, max_cas_try = 5) {
  if (is.null(df) || nrow(df) == 0) return(df)

  for (col in chem_cols) {
    if (!col %in% names(df)) df[[col]] <- NA_character_
  }
  # 每行这次查结构的结论，供 split_unassignable() 分辨账本 reason。
  # 不进业务表：写库只取库表列（见 write_changes_to_db()）。
  df[["structure_lookup"]] <- NA_character_

  need <- which(is_blank_key(df[["InChIKey"]]) & !is_blank_key(df[[cas_col]]))

  if (length(need) == 0) {
    if (verbose) message("   All compounds resolved - no PubChem lookup needed")
    return(df)
  }

  if (verbose) message("   Looking up ", length(need),
                       " new compound(s) on PubChem (incremental)...")

  for (j in seq_along(need)) {
    i <- need[j]
    name <- df[[name_col]][i]
    if (is.na(name) || !nzchar(name)) name <- "?"

    # 一个单元格可能有多个 CAS（CLP 的多 index 条目、IARC 的组条目、EU SML 的
    # 聚合物族）。逐个试、命中即停：第一个 CAS 常常在 PubChem 里没有单体条目，
    # 而后面某一组分有。实测 19 行这类样本，旧逻辑命中 0、逐个试命中 15。
    cands <- extract_cas_candidates(df[[cas_col]][i])[[1]]

    if (length(cands) == 0L) {
      if (verbose) message(sprintf("     [%d/%d] %s -> no usable CAS number",
                                   j, length(need), name))
      next
    }

    meta <- NULL
    saw_unavail <- FALSE
    saw_not_found <- FALSE
    for (cand in utils::head(cands, max_cas_try)) {
      m <- pubchem_lookup_cas(cand)
      Sys.sleep(delay)
      if (!is_blank_key(m$InChIKey)) {
        meta <- m
        break
      }
      # 没带 lookup_status 的（老调用方 / 测试替身）按"确实没有"处理，
      # 与被 mock 前的行为一致
      st <- attr(m, "lookup_status")
      if (is.null(st)) st <- "not_found"
      if (identical(st, "unavailable")) saw_unavail <- TRUE else saw_not_found <- TRUE
    }

    if (!is.null(meta)) {
      df[["CID"]][i] <- meta$CID
      df[["Formula"]][i] <- meta$Formula
      df[["SMILES"]][i] <- meta$SMILES
      df[["InChIKey"]][i] <- meta$InChIKey
      df[["IUPACName"]][i] <- meta$IUPACName
      df[["ExactMass"]][i] <- meta$ExactMass
      df[["structure_lookup"]][i] <- "ok"
      if (verbose) message(sprintf("     [%d/%d] %s (CAS %s) -> CID %s, InChIKey %s",
                                   j, length(need), name,
                                   paste(cands, collapse = ", "),
                                   meta$CID, meta$InChIKey))
    } else {
      # "没查成"优先于"没查到"：只要有一个候选是网络/限流失败，就不能断言
      # PubChem 里没有这个物质 —— 下一轮还会再试，账本也不该说死。
      df[["structure_lookup"]][i] <- if (saw_unavail) "unavailable"
                                     else if (saw_not_found) "not_found"
                                     else NA_character_
      if (verbose) {
        message(sprintf("     [%d/%d] %s (CAS %s) -> %s",
                        j, length(need), name,
                        paste(cands, collapse = ", "),
                        switch(df[["structure_lookup"]][i],
                               unavailable = "lookup unavailable (network/rate limit)",
                               not_found = "not found in PubChem",
                               "no usable CAS number")))
      }
    }
  }
  df
}

# ---- 库内回填（老物质不重查 PubChem，直接从库里搬） --------------------------

#' 从数据库回填已存在物质的化学元数据
#'
#' 按 CAS 在库表里查找已存在的物质，把它的 CID/Formula/SMILES/InChIKey 等
#' 直接填回新清单的对应行。这样"老物质"无需重新查 PubChem，只有真正新增的
#' 物质才会走 enrich_new_compounds()。
#'
#' @param df 标准化后的数据框（含化学列）
#' @param db_name 库表名（svhc/cmr/iarc/eu_sml 等）
#' @param cas_col 库表与新清单共用的 CAS 列名
#' @param db_path 自定义数据库路径（NULL 用默认）
#' @return 回填后的 data.frame
#' @keywords internal
#' @encoding UTF-8
backfill_meta_from_db <- function(df, db_name, cas_col, db_path = NULL) {
  if (is.null(df) || nrow(df) == 0) return(df)

  for (col in chem_cols) {
    if (!col %in% names(df)) df[[col]] <- NA_character_
  }

  con <- get_db_connection(db_path)
  on.exit(DBI::dbDisconnect(con))

  if (!DBI::dbExistsTable(con, db_name)) return(df)

  current <- DBI::dbGetQuery(con, paste("SELECT * FROM", db_name))
  if (nrow(current) == 0) return(df)
  if (!cas_col %in% names(current)) return(df)

  have <- chem_cols[chem_cols %in% names(current)]
  if (length(have) == 0) return(df)

  fill <- which(is_blank_key(df[["InChIKey"]]) & !is_blank_key(df[[cas_col]]))
  if (length(fill) == 0) return(df)

  cur_cas <- canonicalize_cas(trimws(as.character(current[[cas_col]])))
  cur_ik <- as.character(current[["InChIKey"]])

  for (i in fill) {
    cas <- canonicalize_cas(trimws(df[[cas_col]][i]))
    hit <- which(!is_blank_key(cur_cas) & cur_cas == cas)
    if (length(hit) == 0) next
    # 同一 CAS 多条时，优先取有 InChIKey 的那条
    good <- hit[!is_blank_key(cur_ik[hit])]
    if (length(good) > 0) hit <- good[1] else hit <- hit[1]
    for (col in have) {
      val <- current[[col]][hit]
      if (length(val) == 1 && !is.na(val)) df[[col]][i] <- val
    }
  }
  df
}

#' 键值归一：统一换行符 + 连续空白折成单个空格
#'
#' 与 canon_cell() 的分工要分清：canon_cell() 用于内容比对，把多行单元格当
#' 集合处理（按行排序、不折换行），这对 GHS 危险代码列表是对的；键用来判
#' "是不是同一条记录"，换行与空格只是书写差异，必须折成同一种形式。
#'
#' 为什么必须做：官方导出与库内历史快照对同一实体的书写方式不同。实测
#' cmr_suspect 库内 isoproturon 的名字是多行（分号后跟空行再跟 IUPAC 名），
#' 官方 CLP 导出写成单行、用空格分隔。键是 substance_name，两份字符串不相等，
#' 于是一条物质同时被判 removed 和 added，58 行假删除把整库更新挡在安全阀外。
#'
#' 归一力度刻意止步于此：不做大小写、标点、全半角归一，那会把真正不同的
#' 物质合成一条。归一后为空或占位符（同 key_placeholder）的元素返回 NA。
#'
#' @param v 字符向量
#' @return 归一后的字符向量，空键为 NA_character_
#' @keywords internal
#' @encoding UTF-8
canon_key <- function(v) {
  v <- as.character(v)
  v <- gsub("\r\r\n", "\n", v)
  v <- gsub("\r\n", "\n", v)
  v <- gsub("\r", "\n", v)
  v <- gsub("[[:space:]]+", " ", v)
  v <- trimws(v)
  v[is_blank_key(v)] <- NA_character_
  v
}

#' 实体键：主键为空/占位时退化为"兜底列名: 值"
#'
#' diff、回填、变动明细三处必须用同一套键定义。曾经两边不一致：iarc 有 5 行
#' 无 CAS 的组条目（Arecoline / Hexachlorocyclohexanes / Hypochlorite salts）
#' 在 diff 里靠 agent 兜底认了出来，回填却拿空的 cas_no 去找、补不到
#' InChIKey，写库时撞上 InChIKey 非空约束，导致同批 846 行一起回滚。
#'
#' 键值两侧都先过 canon_key()：源与库对同一实体的书写方式可能只差换行或空格。
#'
#' @param df 数据框
#' @param key_col 主键列名
#' @param fallback_col 兜底键列名（可为 NULL 或源/库表里不存在）
#' @return 字符向量，无可用键的行返回 NA
#' @keywords internal
#' @encoding UTF-8
key_of_df <- function(df, key_col, fallback_col = NULL) {
  k <- canon_key(df[[key_col]])
  if (!is.null(fallback_col) && fallback_col %in% names(df)) {
    f <- canon_key(df[[fallback_col]])
    k <- ifelse(is.na(k) & !is.na(f), paste0(fallback_col, ":", f), k)
  }
  k
}

#' 单元格内容归一（diff 与变动明细共用）
#'
#' 把"同一事实的不同写法"归到同一形式，用于判断内容是否真的变了：
#' 统一换行符（xlsx 在不同读取路径下会给出双回车加换行、回车加换行、单个换行
#' 三种写法）、去首尾空白、占位符（"-" 等，同 key_placeholder）视为"无值"、
#' 多行单元格按行排序。
#'
#' 排序一步针对官方导出：同一组危险代码的排列顺序会变
#' （GHS06/GHS08 与 GHS08/GHS06、H335/H372 与 H372/H335），集合相同就不算修改。
#' 代价是"纯换序"的改动不再报为 modified——对这些代码类列而言换序没有实质
#' 含义，可以接受。
#'
#' @param v 字符向量
#' @param sort_multiline 多行单元格是否按行排序后比较。代码表列（GHS/H 码）
#'   保持默认 TRUE：集合相同即不算修改。SVHC 的 remarks 是散文式多行注释
#'   （行序有含义），diff_svhc_data 显式传 FALSE——归一策略在这里显式分叉，
#'   不要再拷贝一份残缺实现（2026-09-12 架构评审第 3 轮）。
#' @return 归一后的字符向量
#' @keywords internal
#' @encoding UTF-8
canon_cell <- function(v, sort_multiline = TRUE) {
  v <- as.character(v)
  v[is.na(v)] <- ""
  Encoding(v) <- "UTF-8"
  v <- gsub("\r\r\n", "\n", v)
  v <- gsub("\r\n", "\n", v)
  v <- gsub("\r", "\n", v)
  v <- gsub("\n+", "\n", v)
  v <- trimws(v)
  v[v %in% key_placeholder] <- ""
  if (isTRUE(sort_multiline)) {
    multi <- grepl("\n", v, fixed = TRUE)
    if (any(multi)) {
      v[multi] <- vapply(v[multi], function(s) {
        paste(sort(strsplit(s, "\n", fixed = TRUE)[[1]]), collapse = "\n")
      }, character(1), USE.NAMES = FALSE)
    }
  }
  v
}

# ---- diff：新旧清单比对（主键 + 兜底键，内容只比核心字段） -------------------

#' 增量 diff：用清单自有键（CAS/编号）在 enrich 之前比对增删改
#'
#' 主键 key_col（如 Index No、FCM substance No、CAS）；key 为空/占位时退化为
#' fallback_col（如名称），并加前缀避免与主键冲突。内容比较只针对 content_cols
#' 指定的核心字段，排除化学元数据列，避免误报 modified。
#'
#' @param new_df 标准化后的新数据（库表同构列）
#' @param current_df 库表当前全部数据
#' @param key_col 主键列名
#' @param fallback_col 兜底键列名（可为 NULL）
#' @param content_cols 参与内容比较的核心字段列
#' @param cas_col CAS 列名；提供时会在比较前做规范化（去掉前导 0），避免
#'   0266309-43-7 与 266309-43-7 这类格式差异被误判为 modified
#' @return list(added/removed/modified 及 total_*)
#' @keywords internal
#' @encoding UTF-8
diff_incremental <- function(new_df, current_df, key_col,
                             fallback_col = NULL, content_cols = NULL,
                             cas_col = NULL) {
  # 键算法只有一份实现（key_of_df -> canon_key）。这里曾内联过一份副本，
  # 与回填函数各改各的，导致 iarc 那 5 行无 CAS 组条目补不到键、整批回滚。
  new_all <- key_of_df(new_df, key_col, fallback_col)
  cur_all <- key_of_df(current_df, key_col, fallback_col)

  new_ok <- !is.na(new_all)
  cur_ok <- !is.na(cur_all)

  new_df <- new_df[new_ok, , drop = FALSE]
  new_keys <- new_all[new_ok]
  cur_df <- current_df[cur_ok, , drop = FALSE]
  cur_keys <- cur_all[cur_ok]

  added_keys <- setdiff(new_keys, cur_keys)
  removed_keys <- setdiff(cur_keys, new_keys)
  common_keys <- intersect(new_keys, cur_keys)

  modified_keys <- character(0)
  if (length(common_keys) > 0) {
    if (is.null(content_cols)) {
      # 默认比较所有业务列（排除化学元数据列与系统列）
      content_cols <- setdiff(intersect(names(new_df), names(cur_df)),
                              c(chem_cols, "id", "created_at", "updated_at"))
    } else {
      content_cols <- content_cols[content_cols %in% names(new_df) &
                                     content_cols %in% names(cur_df)]
    }
    if (length(content_cols) > 0) {
      # CAS 列在构建签名前先整列规范化，避免依赖 apply 传入的行向量 names()
      # 与 cas_col 字面量的编码匹配（曾导致 EU SML 672 个假 modified）
      if (!is.null(cas_col) && cas_col %in% content_cols) {
        new_df[[cas_col]] <- canonicalize_cas(as.character(new_df[[cas_col]]))
        new_df[[cas_col]][is.na(new_df[[cas_col]])] <- ""
        cur_df[[cas_col]] <- canonicalize_cas(as.character(cur_df[[cas_col]]))
        cur_df[[cas_col]][is.na(cur_df[[cas_col]])] <- ""
      }

      canon_row <- function(r) canon_cell(r)
      cur_by <- split(seq_len(nrow(cur_df)), cur_keys)
      new_by <- split(seq_len(nrow(new_df)), new_keys)
      for (k in common_keys) {
        ci <- cur_by[[k]]
        ni <- new_by[[k]]
        if (is.null(ci) || is.null(ni)) next
        cur_sig <- apply(cur_df[ci, content_cols, drop = FALSE], 1,
                         function(r) paste(canon_row(r), collapse = "\x01"))
        new_sig <- apply(new_df[ni, content_cols, drop = FALSE], 1,
                         function(r) paste(canon_row(r), collapse = "\x01"))
        if (length(cur_sig) != length(new_sig) ||
            !identical(unname(sort(cur_sig)), unname(sort(new_sig)))) {
          modified_keys <- c(modified_keys, k)
        }
      }
    }
  }

  added <- new_df[new_keys %in% added_keys, , drop = FALSE]
  removed <- cur_df[cur_keys %in% removed_keys, , drop = FALSE]
  modified <- new_df[new_keys %in% modified_keys, , drop = FALSE]

  list(
    total_added = nrow(added), total_removed = nrow(removed),
    total_modified = nrow(modified),
    added = added, removed = removed, modified = modified,
    added_keys = added_keys, removed_keys = removed_keys,
    modified_keys = modified_keys
  )
}

# ---- 列名映射：源文件列 -> 库表蛇形列（精确 -> 归一 -> 候选表） ---------------

#' 解析某库表列应取哪个源列（逐级兜底）
#'
#' 匹配顺序：源列名精确相等 -> match_col 语义（去空白精确/忽略大小写/包含）
#' -> 超归一精确（去全部非字母数字 + 小写，如 "Index No" 命中 index_no） ->
#' db_col_candidates 登记的候选名（用词不同或标准化后被改名的列，按登记顺序
#' 取第一个命中）。找不到返回 NA_character_。
#'
#' @param raw 源数据框（列名可为源文件原始名）
#' @param db_name 库表名
#' @param col 库表列名（蛇形）
#' @return 命中的源列名，找不到 NA_character_
#' @keywords internal
#' @encoding UTF-8
map_source_colname <- function(raw, db_name, col) {
  nms <- names(raw)
  if (col %in% nms) return(col)
  hit <- first_matching_colname(raw, col)
  if (!is.na(hit)) return(hit)
  # 超归一精确：去掉所有非字母数字后小写比较
  sup <- function(x) tolower(gsub("[^[:alnum:]]", "", x))
  ns <- sup(nms)
  idx <- which(ns == sup(col))
  if (length(idx) > 0) return(nms[idx[1]])
  # 候选名：用 match_col 的模糊语义在源列里找（排除已精确/归一命中的）
  cands <- db_col_candidates[[db_name]][[col]]
  if (length(cands) > 0) {
    for (c in cands) {
      h <- first_matching_colname(raw, c)
      if (!is.na(h)) return(h)
    }
  }
  NA_character_
}

#' 把原始数据框对齐到库表列（动态读库表列名 + 模糊匹配）
#'
#' 读库表的实际列名，用 map_source_colname()（精确 -> 归一 -> 候选表）从 raw
#' 取对应列，构造列名与库表完全一致的 data.frame。这样无需硬编码带换行的列名
#' （如 eu_sml 的 "SML\\n （mg/kg）"），也能正确对齐新旧两代库（蛇形列名如
#' index_no 会自动命中源文件里的 "Index No"）。raw 中缺失的列（如化学列）置 NA。
#' 系统列（id / created_at / updated_at）由 SQLite 自动维护，不参与源列匹配——
#' 否则 db 列名 "id" 会在模糊匹配的"包含"级误命中任意含 "id" 子串的源列名。
#'
#' @param raw 原始数据框（列名可为源文件原始名）
#' @param db_name 库表名
#' @param db_path 自定义数据库路径
#' @return 列名与库表一致的 data.frame
#' @keywords internal
#' @encoding UTF-8
map_to_db_columns <- function(raw, db_name, db_path = NULL) {
  con <- get_db_connection(db_path)
  on.exit(DBI::dbDisconnect(con))
  info <- DBI::dbGetQuery(con, sprintf("PRAGMA table_info(%s)", db_name))
  db_cols <- info$name

  out <- as.data.frame(matrix(NA_character_, nrow = nrow(raw),
                              ncol = length(db_cols)),
                       stringsAsFactors = FALSE)
  names(out) <- db_cols
  # 记录每个库表列命中的源列名：精确/模糊命中 -> 源列名；源完全未提供 -> NA。
  # run_incremental_update 据此识别"新源没有、库里却有旧值"的列（如旧版 CLP
  # 导出的备用 H 代码列），在 diff 前按 key 回填旧值，避免整表误报 modified。
  mapped_from <- stats::setNames(rep(NA_character_, length(db_cols)), db_cols)
  for (col in db_cols) {
    if (col %in% sys_cols) next  # 系统列由 SQLite 维护，不从源数据匹配
    src <- map_source_colname(raw, db_name, col)
    if (!is.na(src)) {
      out[[col]] <- as.character(raw[[src]])
      mapped_from[[col]] <- src
    }
  }
  attr(out, "mapped_from") <- mapped_from
  out
}

#' 反查 match_col 会命中的源列名
#'
#' 与 match_col 相同的优先级（去空白精确 -> 忽略大小写精确 -> 包含），返回
#' 第一个命中的列名；未命中返回 NA_character_。
#'
#' @param data 数据框
#' @param pattern 目标列名
#' @return 源列名字符串或 NA_character_
#' @keywords internal
#' @encoding UTF-8
first_matching_colname <- function(data, pattern) {
  if (!is.data.frame(data) || nrow(data) == 0) return(NA_character_)
  strip <- function(x) gsub("[[:space:]]+", "", x)
  ns <- strip(names(data))
  ps <- strip(pattern)
  if (ps %in% ns) return(names(data)[which(ns == ps)[1]])
  ps_l <- tolower(ps)
  if (ps_l %in% tolower(ns)) return(names(data)[which(tolower(ns) == ps_l)[1]])
  hit <- which(vapply(tolower(ns), function(nm) grepl(ps_l, nm, fixed = TRUE), logical(1)))
  if (length(hit) > 0) return(names(data)[hit[1]])
  NA_character_
}

# ---- 未映射列回填（新源不再提供的列，保住库里的历史值） ----------------------

#' 对"新源未提供的列"按 key 回填库中旧值
#'
#' 场景：某业务列在库里有历史值（如旧版 ECHA 导出的备用 H 代码列），而新
#' 版本源文件已不再提供该列（map_to_db_columns 后整列 NA）。若直接入库会把
#' 该列清空并让 diff 把整表报成 modified。此函数在新清单 diff 之前，把这类
#' 列的空白值按 key 从库表当前数据回填，保留历史快照；源已提供的列不受影响。
#'
#' key 的算法与 diff_incremental() 一致（共用 key_of_df()）：主键为空时用兜底
#' 列，两边必须同键，否则会出现"diff 认得出来、回填找不到"的行。
#'
#' @param new_df map_to_db_columns 后的新数据框（带 mapped_from 属性）
#' @param cur_df 库表当前全部数据
#' @param key_col 主键列名
#' @param fallback_col 兜底键列名（可为 NULL）
#' @return 回填后的 data.frame
#' @keywords internal
#' @encoding UTF-8
backfill_unmapped_cols <- function(new_df, cur_df, key_col, fallback_col = NULL) {
  if (is.null(new_df) || nrow(new_df) == 0) return(new_df)
  if (is.null(cur_df) || nrow(cur_df) == 0) return(new_df)
  mapped_from <- attr(new_df, "mapped_from", exact = TRUE)
  if (is.null(mapped_from)) return(new_df)
  if (!key_col %in% names(new_df) || !key_col %in% names(cur_df)) return(new_df)

  unmapped <- names(mapped_from)[is.na(mapped_from)]
  unmapped <- unmapped[unmapped %in% names(new_df) & unmapped %in% names(cur_df)]
  if (length(unmapped) == 0) return(new_df)

  nk <- key_of_df(new_df, key_col, fallback_col)
  ck <- key_of_df(cur_df, key_col, fallback_col)
  cur_by <- split(seq_len(nrow(cur_df)), ck)  # split 自动丢掉 NA 组

  n_filled <- 0L
  for (col in unmapped) {
    cv <- as.character(cur_df[[col]])
    if (all(is.na(cv) | !nzchar(trimws(cv)))) next  # 库里也没值，无需回填
    nv <- as.character(new_df[[col]])
    blank <- is.na(nv) | !nzchar(trimws(nv))
    if (!any(blank)) next
    for (i in which(blank)) {
      if (is.na(nk[i])) next  # 主键与兜底键都为空，没有对齐依据，不乱回填
      hit <- cur_by[[nk[i]]]
      if (length(hit) == 0) next
      # 同一 key 多行时优先取非空值
      good <- hit[!is.na(cv[hit]) & nzchar(trimws(cv[hit]))]
      if (length(good) > 0) {
        new_df[[col]][i] <- cv[good[1]]
        n_filled <- n_filled + 1L
      }
    }
  }
  if (n_filled > 0) message("Backfilled ", n_filled,
                            " legacy value(s) for source-missing columns")
  attr(new_df, "mapped_from") <- NULL
  new_df
}

# ---- 未分配条目：摘除 + 登记（上游给了、但收不了的行） -----------------------

#' Split Off Added Rows That Cannot Be Stored
#'
#' 上游有一部分条目本体没有结构：IARC 评的感染状态与职业暴露场景
#' （`"Helicobacter pylori (infection with)"`）、A 类组条目
#' （`"salts of hydrazine"`）、UVCB 工业品（`"alcohols, aliphatic, ..., (C4-C22)"`）、
#' 反应产物混合物（`"reaction mass of: ..."`）。实测四个库合计 414 行属于这一类。
#'
#' 它们拿不到 InChIKey，而业务表的 InChIKey 是 `NOT NULL` 外加指向 `chemicals`
#' 的外键 —— 插不进去。写库又是单事务，一行失败连合法行一起回滚（iarc 历史上
#' 因此 846 行全废）。所以这些行在写库前摘出来，交给
#' [record_unassigned()] 登记成账本。
#'
#' 这是"摘出来"，不是"放宽约束"：这些条目不是暂时查不到结构，而是结构这个
#' 概念对它不成立。塞进按 InChIKey 索引的表，只会让下游以为拿到了结构。
#'
#' @param changes [diff_incremental()] 的返回值；只有 `added` 会被改写
#' @param db_name 目标业务表名（写进账本的 `database_name`）
#' @param key_col 主键列名
#' @param fallback_col 主键为空时的兜底列名
#' @param cas_col CAS 列名，用来区分"连 CAS 都没有"和"有 CAS 却查不到结构"
#' @param name_col 名称列名，写进账本给人看
#' @param looked_up 本轮是否真的尝试过补结构（即 `enrich = TRUE`）。为 `FALSE`
#'   时"有 CAS 但没键"只能记成 `not_looked_up` —— 那是本轮没去查，不是查不到。
#'   两者混在一起账本就会撒谎：调用方显式传 `enrich = FALSE` 时新增行一条都拿
#'   不到键，全记成 `no_structure_found` 会让人以为上游没结构。
#' @return `list(changes = , dropped = , n_dropped = )`；`changes$added` 已剔除
#'   无键行，`total_added` 同步改写
#' @keywords internal
#' @encoding UTF-8
split_unassignable <- function(changes, db_name, key_col, fallback_col = NULL,
                               cas_col = NULL, name_col = NULL,
                               looked_up = TRUE) {
  added <- changes$added
  empty <- function() {
    list(changes = changes, dropped = data.frame(), n_dropped = 0L)
  }
  if (is.null(added) || nrow(added) == 0L) return(empty())
  # 表没有化学列（如 svhc_raw）时不适用：那边本来就不靠 InChIKey 入库
  if (!"InChIKey" %in% names(added)) return(empty())

  no_key <- is_blank_key(added[["InChIKey"]])
  if (!any(no_key)) return(empty())

  kept <- added[!no_key, , drop = FALSE]
  bad <- added[no_key, , drop = FALSE]

  cas_vec <- if (!is.null(cas_col) && cas_col %in% names(bad)) {
    bad[[cas_col]]
  } else {
    rep(NA_character_, nrow(bad))
  }
  name_vec <- if (!is.null(name_col) && name_col %in% names(bad)) {
    bad[[name_col]]
  } else {
    rep(NA_character_, nrow(bad))
  }

  # 连 CAS 都没有 -> 结构对它不适用；有 CAS 却没键 -> 再分三种：
  #   lookup_unavailable  这次根本没查成（网络 / 限流），下一轮换个时间能查到
  #   no_structure_found  PubChem 明确回答"没有这个物质"
  #   not_looked_up       本轮没去查（enrich = FALSE）
  # 前两者混在一起，账本就会撒谎：2026-09-11 cmr 那次 764 行被记成
  # no_structure_found，其中大部分（敌草隆、1,4-二氧六环…）重查就命中。
  status <- if ("structure_lookup" %in% names(bad)) {
    as.character(bad[["structure_lookup"]])
  } else {
    rep(NA_character_, nrow(bad))
  }
  reason <- rep("no_structure_found", nrow(bad))
  if (!isTRUE(looked_up)) {
    reason <- rep("not_looked_up", nrow(bad))
  } else {
    reason[!is.na(status) & status == "unavailable"] <- "lookup_unavailable"
  }
  reason[is_blank_key(cas_vec)] <- "no_cas"
  dropped <- data.frame(
    entity_key = key_of_df(bad, key_col, fallback_col),
    substance_name = as.character(name_vec),
    cas_no = as.character(cas_vec),
    reason = reason,
    stringsAsFactors = FALSE
  )

  changes$added <- kept
  changes$total_added <- nrow(kept)
  list(changes = changes, dropped = dropped, n_dropped = nrow(dropped))
}

#' Record Entries That Could Not Be Stored
#'
#' 把 [split_unassignable()] 摘下来的行写进 `unassigned_entries`。同一
#' (库, 键) 重复遇到只累加 `seen_count` 并刷新 `last_seen_at`，不会长出重复行。
#'
#' 人工判断优先于自动流程：`status = 'accepted'` 的条目不因再次遇到而改回
#' `'open'`，`notes` 也一律不覆盖。唯一例外是 `'resolved'` 又变回无键 ——
#' 那说明它现在确实收不了（上游撤了 CAS 之类），退回 `'open'` 让账本不说谎。
#'
#' 表不存在时先按 schema 补建（见 [ensure_schema_table()]），这样早于这张表的
#' 数据库不用整体重建。
#'
#' @param db_name 目标业务表名
#' @param dropped [split_unassignable()] 产出的数据框
#' @param db_path 可选数据库路径
#' @return 实际处理的条目数（整数，隐式返回）
#' @keywords internal
#' @encoding UTF-8
record_unassigned <- function(db_name, dropped, db_path = NULL) {
  if (is.null(dropped) || nrow(dropped) == 0L) return(invisible(0L))

  for (col in c("substance_name", "cas_no", "detail")) {
    if (!col %in% names(dropped)) dropped[[col]] <- NA_character_
  }
  if (!"reason" %in% names(dropped)) dropped$reason <- NA_character_
  if (!"entity_key" %in% names(dropped)) {
    stop("`dropped` must have an `entity_key` column.")
  }

  dropped <- dropped[!is_blank_key(dropped$entity_key), , drop = FALSE]
  # seen_count 记的是"遇到过几次"，不是"几行"：同批次内同键只算一次
  dropped <- dropped[!duplicated(dropped$entity_key), , drop = FALSE]
  if (nrow(dropped) == 0L) return(invisible(0L))

  ensure_schema_table("unassigned_entries", db_path = db_path)

  con <- get_db_connection(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  cur <- DBI::dbGetQuery(con, paste(
    "SELECT entity_key, status FROM unassigned_entries WHERE database_name = ?"),
    params = list(db_name))
  known <- stats::setNames(as.character(cur$status), cur$entity_key)

  DBI::dbWithTransaction(con, {
    for (i in seq_len(nrow(dropped))) {
      k <- dropped$entity_key[i]
      reason <- dropped$reason[i]
      if (is.na(reason) || !nzchar(reason)) reason <- "no_structure_found"
      status_now <- unname(known[k])

      if (!is.na(status_now)) {
        back_to_open <- identical(status_now, "resolved")
        DBI::dbExecute(con, paste(
          "UPDATE unassigned_entries",
          "SET last_seen_at = CURRENT_TIMESTAMP, seen_count = seen_count + 1,",
          "    cas_no = ?, reason = ?, detail = ?",
          if (back_to_open) ", status = 'open'" else "",
          "WHERE database_name = ? AND entity_key = ?"),
          params = list(dropped$cas_no[i], reason, dropped$detail[i], db_name, k))
      } else {
        DBI::dbExecute(con, paste(
          "INSERT INTO unassigned_entries",
          "(database_name, entity_key, substance_name, cas_no, reason, detail)",
          "VALUES (?, ?, ?, ?, ?, ?)"),
          params = list(db_name, k, dropped$substance_name[i],
                        dropped$cas_no[i], reason, dropped$detail[i]))
      }
    }
  })
  invisible(nrow(dropped))
}

#' Mark Registry Entries as Now Stored
#'
#' 入库成功后调用：账本里这些键的条目若还挂着（`'open'` 或 `'accepted'`），
#' 说明它们后来拿到了结构并写进了业务表，标成 `'resolved'`。
#'
#' 先取回未结案的键再求交集，命中通常为空 —— 那样连 UPDATE 都不发。
#'
#' @param db_name 业务表名
#' @param keys 本次成功入库的实体键（[key_of_df()] 的产物）
#' @param db_path 可选数据库路径
#' @return 被标成已解决的条目数（整数，隐式返回）
#' @keywords internal
#' @encoding UTF-8
mark_unassigned_resolved <- function(db_name, keys, db_path = NULL) {
  keys <- as.character(keys)
  keys <- keys[!is_blank_key(keys)]
  if (length(keys) == 0L) return(invisible(0L))

  con <- get_db_connection(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  if (!DBI::dbExistsTable(con, "unassigned_entries")) return(invisible(0L))

  open_keys <- DBI::dbGetQuery(con, paste(
    "SELECT entity_key FROM unassigned_entries",
    "WHERE database_name = ? AND COALESCE(status, 'open') <> 'resolved'"),
    params = list(db_name))$entity_key
  hit <- intersect(unique(keys), open_keys)
  if (length(hit) == 0L) return(invisible(0L))

  DBI::dbWithTransaction(con, {
    for (k in hit) {
      DBI::dbExecute(con, paste(
        "UPDATE unassigned_entries",
        "SET status = 'resolved', last_seen_at = CURRENT_TIMESTAMP",
        "WHERE database_name = ? AND entity_key = ?"),
        params = list(db_name, k))
    }
  })
  invisible(length(hit))
}

# ---- 数据质量标记：已入库但某列不可信的行（旁路登记，不动业务表） -----------

#' 人工核定的 UVCB 结构键冲突（cmr 表，`index_no`）
#'
#' CLP 的 648/649/650 段是 UVCB 章节：石油气、石脑油、干洗溶剂、煤焦油酸馏分。
#' 这类条目没有单一结构，而库里这 9 行各带一个单体 InChIKey。实测
#' `649-200-00-5 Hydrocarbons, C4-5` 被配成丁烷、`649-193-00-9 Alkanes, C1-2`
#' 配成乙烷、`649-345-00-4 stoddard solvent` 配成 `C8H17BrO3`（含溴物）。
#'
#' 来源已定位：旧 `inst/clp_cmr_meta.xlsx` 自带的 `CID / MolecularFormula /
#' SMILES / InChIKey / ExactMass` 五列。该文件 1087 行里 332 行带键，正好等于
#' `cmr` 表的 332 行 —— 是那一次手工"结构补齐"干的。当前管道不会重现：实测
#' `pubchem_lookup_cas()` 对 `68514-31-8` / `8052-41-3` / `64742-49-0` 全部
#' 返回 NA，新流程遇到 UVCB 的 CAS 只会把该行摘进 [record_unassigned()] 账本。
#'
#' 清单逐行人工核定，不做"按段一刀切"的启发式 —— 同段另有 3 行
#' （`650-012-00-0 erionite`、`650-032-00-X cyproconazole`、
#' `650-056-00-0 dibutylbis(pentane-2,4-dionato-O,O')tin`）带的是真实结构。
#'
#' @noRd
.uvcb_structure_conflicts <- c(
  "648-120-00-8",  # Tar acids, methylphenol fraction
  "649-088-00-8",  # Hydrocarbons, C1-4; Petroleum gas
  "649-193-00-9",  # Alkanes, C1-2; Petroleum gas
  "649-194-00-4",  # Alkanes, C2-3; Petroleum gas
  "649-200-00-5",  # Hydrocarbons, C4-5; Petroleum gas
  "649-201-00-0",  # Hydrocarbons, C2-4, C3-rich; Petroleum gas
  "649-328-00-1",  # Naphtha (petroleum), hydrotreated light
  "649-345-00-4",  # stoddard solvent
  "649-402-00-3"   # Hydrocarbons, C5-rich; Low boiling point naphtha
)

#' Record a Data Quality Flag
#'
#' 把"已入库、但某列已知不可信"的行登记进 `data_quality_flags`。与
#' [record_unassigned()] 同一套约定：同一 (库, 键, 问题类型) 重复遇到只累加
#' `seen_count` 并刷新 `last_seen_at`；`notes` 与人工设的 `status` 不被自动流程
#' 覆盖（`'accepted'` 表示人工已判定"就这样，不改"）。
#'
#' **为什么不写进业务表的 `notes` 列**：入库是 DELETE + INSERT，源 df 里没有的
#' 列一律填 `NA`（见 [write_changes_to_db()]）。只要该行日后被判一次 modified，
#' 写在 `notes` 里的标记就被抹掉，且悄无声息 —— 标记本身反而成了不可信的东西。
#' 旁路表不会被写库碰到。
#'
#' 表不存在时先按 schema 补建（见 [ensure_schema_table()]），这样早于这张表的
#' 数据库不用整体重建。
#'
#' @param db_name 业务表名（`"cmr"` 等）
#' @param entries 数据框，须有 `entity_key` 列；可选 `entity_name`、`detail`
#' @param flag 问题类型标识，如 `"unreliable_structure_key"`
#' @param severity `"high"` / `"medium"` / `"low"`
#' @param source 问题出自哪一步，写进表里备查
#' @param db_path 可选数据库路径
#' @return 实际处理的条目数（整数，隐式返回）
#' @keywords internal
#' @encoding UTF-8
record_data_quality_flag <- function(db_name, entries, flag,
                                     severity = "high", source = NULL,
                                     db_path = NULL) {
  if (is.null(entries) || nrow(entries) == 0L) return(invisible(0L))
  if (!"entity_key" %in% names(entries)) {
    stop("`entries` must have an `entity_key` column.")
  }
  # DBI 的具名参数必须长度 1：NULL 会让 "Parameter N does not have length 1"
  if (is.null(source)) source <- NA_character_
  if (is.null(severity)) severity <- NA_character_
  for (col in c("entity_name", "detail")) {
    if (!col %in% names(entries)) entries[[col]] <- NA_character_
  }

  entries <- entries[!is_blank_key(entries$entity_key), , drop = FALSE]
  # seen_count 记"遇到过几次"，不是"几行"：同批次内同键只算一次
  entries <- entries[!duplicated(entries$entity_key), , drop = FALSE]
  if (nrow(entries) == 0L) return(invisible(0L))

  ensure_schema_table("data_quality_flags", db_path = db_path)

  con <- get_db_connection(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  cur <- DBI::dbGetQuery(con, paste(
    "SELECT entity_key FROM data_quality_flags",
    "WHERE database_name = ? AND flag = ?"),
    params = list(db_name, flag))
  known <- as.character(cur$entity_key)

  DBI::dbWithTransaction(con, {
    for (i in seq_len(nrow(entries))) {
      k <- as.character(entries$entity_key[i])
      if (k %in% known) {
        DBI::dbExecute(con, paste(
          "UPDATE data_quality_flags",
          "SET last_seen_at = CURRENT_TIMESTAMP, seen_count = seen_count + 1,",
          "    entity_name = ?, detail = ?, severity = ?, source = ?",
          "WHERE database_name = ? AND entity_key = ? AND flag = ?"),
          params = list(entries$entity_name[i], entries$detail[i], severity,
                        source, db_name, k, flag))
      } else {
        DBI::dbExecute(con, paste(
          "INSERT INTO data_quality_flags",
          "(database_name, entity_key, entity_name, flag, detail, severity, source)",
          "VALUES (?, ?, ?, ?, ?, ?, ?)"),
          params = list(db_name, k, entries$entity_name[i], flag,
                        entries$detail[i], severity, source))
      }
    }
  })
  invisible(nrow(entries))
}

#' Mark Data Quality Flags as Resolved
#'
#' 问题修掉之后调用（该行拿到了正确结构，或条目被上游撤下）。只动
#' `status` 与 `last_seen_at`，`notes` 留着 —— 人工批注是记录，不是状态。
#'
#' 先取回未结案的键再求交集，命中通常为空 —— 那样连 UPDATE 都不发。
#'
#' @param db_name 业务表名
#' @param keys 要标解决的实体键
#' @param flag 只标这一类问题；`NULL`（默认）表示该表所有类型
#' @param db_path 可选数据库路径
#' @return 被标成已解决的条目数（整数，隐式返回）
#' @keywords internal
#' @encoding UTF-8
resolve_data_quality_flag <- function(db_name, keys, flag = NULL, db_path = NULL) {
  keys <- as.character(keys)
  keys <- keys[!is_blank_key(keys)]
  if (length(keys) == 0L) return(invisible(0L))

  con <- get_db_connection(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  if (!DBI::dbExistsTable(con, "data_quality_flags")) return(invisible(0L))

  open_keys <- DBI::dbGetQuery(con, paste(
    "SELECT entity_key FROM data_quality_flags",
    "WHERE database_name = ? AND COALESCE(status, 'open') <> 'resolved'",
    if (is.null(flag)) "" else "AND flag = ?"),
    params = if (is.null(flag)) list(db_name) else list(db_name, flag))$entity_key
  hit <- intersect(unique(keys), open_keys)
  if (length(hit) == 0L) return(invisible(0L))

  DBI::dbWithTransaction(con, {
    for (k in hit) {
      DBI::dbExecute(con, paste(
        "UPDATE data_quality_flags",
        "SET status = 'resolved', last_seen_at = CURRENT_TIMESTAMP",
        "WHERE database_name = ? AND entity_key = ?",
        if (is.null(flag)) "" else "AND flag = ?"),
        params = if (is.null(flag)) list(db_name, k) else list(db_name, k, flag))
    }
  })
  invisible(length(hit))
}

#' Flag the UVCB Entries That Carry a Single-Molecule Structure Key
#'
#' 登记 `.uvcb_structure_conflicts` 列出的 9 行，`detail` 从库内现算 ——
#' 写明这个键实际指向什么分子（如"实际指向 C4H10"），比只记一句"键错了"有用。
#'
#' 只登记，**不改** `cmr.InChIKey`。要不要让这张表允许无结构条目是个架构决策：
#' 该列现在是 `NOT NULL` 加指向 `chemicals` 的外键，改它会影响
#' [assign_toxicity()] 的结构分析与导出，得单独做（`eu_sml_group` 有"键允许空"
#' 的先例，但它不参与结构分析）。
#'
#' @param db_path 可选数据库路径
#' @return 登记的条目数（整数，隐式返回）
#' @keywords internal
#' @encoding UTF-8
flag_uvcb_structure_keys <- function(db_path = NULL) {
  con <- get_db_connection(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  if (!DBI::dbExistsTable(con, "cmr")) return(invisible(0L))

  ph <- paste(rep("?", length(.uvcb_structure_conflicts)), collapse = ", ")
  rows <- DBI::dbGetQuery(con, sprintf("
    SELECT index_no, international_chemical_identification AS nm,
           InChIKey,
           (SELECT Formula FROM chemicals WHERE chemicals.InChIKey = cmr.InChIKey) AS formula
      FROM cmr WHERE index_no IN (%s)", ph),
    params = as.list(.uvcb_structure_conflicts))
  if (nrow(rows) == 0L) return(invisible(0L))

  entries <- data.frame(
    entity_key = rows$index_no,
    entity_name = rows$nm,
    detail = paste0(
      "UVCB 条目（无单一结构），库里却带着单体键 ", rows$InChIKey,
      "，该键实际指向 ",
      ifelse(is.na(rows$formula), "未知分子", rows$formula),
      "；由旧 clp_cmr_meta.xlsx 手工补齐引入，不可用于结构分析"),
    stringsAsFactors = FALSE)

  record_data_quality_flag("cmr", entries, "unreliable_structure_key",
                           severity = "high",
                           source = "inst/clp_cmr_meta.xlsx",
                           db_path = db_path)
}

# ---- 已确认保留的删除项：账本驱动的白名单 ------------------------------------

#' Query the Removal Keys That Were Deliberately Kept
#'
#' 源里已经找不到、但人工裁定要留在库里的行，键登记在 `data_quality_flags`
#' （`flag = "upstream_removed_kept"`）。只有 `status = "accepted"` 进白名单 ——
#' `"open"` 表示还没判断过，不能当已知情放行。
#'
#' **为什么需要这条通道**：这类行删不得。`607-230-00-6` 的 CAS 是 `-`（"其盐类"
#' 组条目），iarc 的 `Progestins` / `Cobalt sulfate` 改名后成了无 CAS 的组条目 ——
#' 三者都拿不到 InChIKey，进不了库。删掉旧行，物质就从库里彻底消失。可留着它们
#' 又会让每一轮更新都报 removed，而 removed > 0 会拦下人工确认闸门（见
#' [run_incremental_update()]），等于这四个库永远跑不动。
#'
#' 白名单存在库里而不是写死在代码里：后人查一次 `data_quality_flags` 就知道
#' 哪几行是被有意留下的、为什么留，不必重做一遍今天的调查。
#'
#' @param db_name 业务表名（`"cmr"` / `"cmr_suspect"` / `"iarc"` / `"eu_sml"`）
#' @param db_path 可选数据库路径
#' @return 键的字符向量；账本不存在或无匹配时返回空向量
#' @keywords internal
#' @encoding UTF-8
query_kept_removals <- function(db_name, db_path = NULL) {
  con <- get_db_connection(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  if (!DBI::dbExistsTable(con, "data_quality_flags")) return(character(0))
  r <- DBI::dbGetQuery(con, paste(
    "SELECT entity_key FROM data_quality_flags",
    "WHERE database_name = ? AND flag = ? AND status = ?"),
    params = list(db_name, "upstream_removed_kept", "accepted"))
  as.character(r$entity_key)
}

#' Drop Deliberately Kept Rows from the Removal List
#'
#' 把白名单里的键从 `changes$removed` 剔除，并同步改写 `total_removed` ——
#' 闸门看的是这个计数，不改它等于没剔。
#'
#' 剔除只影响"要不要删"，这些行本身仍在库里、内容也不参与比对：`removed` 是
#' \"库里有、源里没有\"的那一侧，本来就不写库。
#'
#' @param changes [diff_incremental()] 的返回值
#' @param kept 要保留的键（[query_kept_removals()] 的产物）
#' @param key_col 主键列名
#' @param fallback_col 主键为空时的兜底列（iarc 的组条目没有 CAS）
#' @return 改写过 `removed` 与 `total_removed` 的 `changes` 列表
#' @keywords internal
#' @encoding UTF-8
drop_kept_removals <- function(changes, kept, key_col, fallback_col = NULL) {
  if (length(kept) == 0L) return(changes)
  if (is.null(changes$removed) || nrow(changes$removed) == 0L) return(changes)
  rk <- key_of_df(changes$removed, key_col, fallback_col)
  hit <- !is.na(rk) & rk %in% kept
  if (!any(hit)) return(changes)
  changes$removed <- changes$removed[!hit, , drop = FALSE]
  changes$total_removed <- nrow(changes$removed)
  changes
}

# ---- 入库 ①：写业务表（事务 + 备份 + 记 update_history） ---------------------

#' 数据库文件备份（write_changes_to_db / write_svhc_to_db 共用）
#'
#' @param db_file 数据库文件路径；NULL/空/不存在则静默跳过
#' @keywords internal
#' @encoding UTF-8
backup_db_file <- function(db_file) {
  if (is.null(db_file) || !nzchar(db_file) || !file.exists(db_file)) {
    return(invisible(NULL))
  }
  bak_dir <- file.path(dirname(db_file), "..", "backups")
  dir.create(bak_dir, showWarnings = FALSE, recursive = TRUE)
  bak <- file.path(bak_dir, paste0("fcmsafety_",
                                   format(Sys.time(), "%Y%m%d_%H%M%S"), ".db"))
  if (file.copy(db_file, bak)) message("   Backup saved: ", bak)
  invisible(NULL)
}

#' 账本写入（write_changes_to_db / write_svhc_to_db 共用）
#'
#' update_history 总数 + change_log 明细。两条入库线（公共增量线与 SVHC
#' 独立线）的账本约定必须永远一致，实现只留这一份；线间差异全部走参数：
#' source_file / user_notes 是字符串，明细层的键风格经 ... 透传给
#' log_change_detail（公共线传 key_col/fallback_col，SVHC 传
#' key_fn/compare_cols）。
#'
#' @param con 已打开的数据库连接
#' @param db_name 库名
#' @param changes diff 结果（added/removed/modified）
#' @param n_added,n_removed,n_modified 变更计数
#' @param old_df 修改前快照（仅 modified > 0 时需要）
#' @param source_file 记入 update_history 的来源文件名
#' @param user_notes 记入 update_history 的备注
#' @param ... 透传给 log_change_detail 的键风格参数
#' @keywords internal
#' @encoding UTF-8
record_update_ledger <- function(con, db_name, changes, n_added, n_removed,
                                 n_modified, old_df = NULL, source_file,
                                 user_notes, ...) {
  if (!DBI::dbExistsTable(con, "update_history")) return(invisible(NULL))
  tryCatch({
    DBI::dbExecute(con,
      paste0("INSERT INTO update_history (database_name, update_type, ",
             "records_added, records_removed, records_modified, source_file, ",
             "user_notes, success) VALUES (?, 'incremental_auto', ?, ?, ?, ?, ?, 1)"),
      params = list(db_name, n_added, n_removed, n_modified, source_file, user_notes))
    history_id <- DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id[1]
    # 明细层：把"具体谁变了"写入 change_log（表存在才写；失败仅提示，
    # 不影响数据入库结果——明细是顺手留痕，不是主流程）
    if (!is.na(history_id) && DBI::dbExistsTable(con, "change_log")) {
      tryCatch(
        log_change_detail(con, history_id, db_name, changes,
                          old_df = old_df, ...),
        error = function(e) {
          message("   (change_log detail write skipped: ", conditionMessage(e), ")")
        }
      )
    }
  }, error = function(e) message("   (update_history write skipped: ", e$message, ")"))
}

#' 把 diff 结果写入 SQLite（通用：事务 + 备份 + update_history）
#'
#' 适用于键完全唯一的库（cmr/iarc/eu_sml），删除 removed/modified 旧行、
#' 插入 added/modified 新行。键非单一的 svhc 仍走 write_svhc_to_db()。
#'
#' @param db_name 库表名
#' @param changes diff_incremental() 的输出
#' @param key_col 主键列名（删除/覆盖旧行时用）
#' @param fallback_col 兜底键列名；主键为空时用于删除旧行（可为 NULL）
#' @param db_path 数据库路径
#' @param backup 入库前是否备份数据库
#' @return 写入统计
#' @keywords internal
#' @encoding UTF-8
write_changes_to_db <- function(db_name, changes, key_col, fallback_col = NULL, db_path = NULL, backup = TRUE) {
  con <- get_db_connection(db_path)
  on.exit(DBI::dbDisconnect(con))

  n_added <- changes$total_added
  n_removed <- changes$total_removed
  n_modified <- changes$total_modified
  if (n_added + n_removed + n_modified == 0) {
    message("   No changes to apply")
    return(list(records_added = 0, records_removed = 0, records_modified = 0))
  }

  if (isTRUE(backup)) {
    backup_db_file(con@dbname)
  }

  # 修改前给旧表拍快照：仅当存在 modified 时抓取，供 change_log 逐字段对比
  # （快照必须在 DELETE 之前取，事务外取即可——本连接独占，无并发写入）
  old_snapshot <- NULL
  if (n_modified > 0L) {
    old_snapshot <- DBI::dbGetQuery(con, paste("SELECT * FROM", db_name))
  }

  DBI::dbWithTransaction(con, {
    for_del <- rbind(changes$removed, changes$modified)
    if (nrow(for_del) > 0) {
      for (i in seq_len(nrow(for_del))) {
        k_val <- as.character(for_del[[key_col]][i])
        if (is_blank_key(k_val)) {
          if (!is.null(fallback_col) && fallback_col %in% names(for_del)) {
            fb_val <- as.character(for_del[[fallback_col]][i])
            if (!is_blank_key(fb_val)) {
              DBI::dbExecute(con,
                sprintf('DELETE FROM %s WHERE "%s" = ?', db_name, fallback_col),
                params = list(fb_val))
            }
          }
        } else {
          DBI::dbExecute(con,
            sprintf('DELETE FROM %s WHERE "%s" = ?', db_name, key_col),
            params = list(k_val))
        }
      }
    }

    to_insert <- rbind(changes$added, changes$modified)
    if (nrow(to_insert) > 0) {
      info <- DBI::dbGetQuery(con, sprintf("PRAGMA table_info(%s)", db_name))
      db_cols <- info$name
      # 系统列（id / 时间戳）交 SQLite 自动维护：显式插入 NULL 会覆盖 DEFAULT
      # 导致 created_at/updated_at 变 NULL。只排除目标表里真实存在的系统列，
      # 旧式无这些列的表（及既有测试的临时表）不受影响。
      ins_cols <- setdiff(db_cols, sys_cols)
      # 蛇形重建库的业务表 InChIKey 外键 -> chemicals（连接默认 FK ON），
      # 先保证新物质的化学记录在 chemicals 存在，避免外键报错
      upsert_chemicals(con, to_insert)
      for (col in ins_cols) {
        if (!col %in% names(to_insert)) to_insert[[col]] <- NA_character_
      }
      to_insert <- to_insert[, ins_cols, drop = FALSE]
      DBI::dbAppendTable(con, db_name, to_insert)
    }
  })

  record_update_ledger(con, db_name, changes, n_added, n_removed, n_modified,
                       old_df = old_snapshot,
                       source_file = "update_pipeline.R",
                       user_notes = "Incremental update with diff confirmation",
                       key_col = key_col, fallback_col = fallback_col)

  message("   DB write done: +", n_added, " / -", n_removed, " / ~", n_modified)
  list(records_added = n_added, records_removed = n_removed, records_modified = n_modified)
}

# ---- 入库 ②：并 chemicals 总表（业务表的外键前置） --------------------------

#' 把新物质并入 chemicals 总表（INSERT OR IGNORE）
#'
#' 重建库（蛇形 schema）的业务表通过 InChIKey 外键引用 chemicals；连接默认
#' PRAGMA foreign_keys = ON，业务表插入新 InChIKey 前必须先保证 chemicals
#' 有对应记录，否则外键报错。本函数从待插入的 data.frame 中取出带有效
#' InChIKey 的行，把化学元数据（CID/Formula/SMILES/IUPACName/ExactMass）
#' 以 INSERT OR IGNORE 方式并入 chemicals（已存在则忽略，不覆盖）。
#' 在写业务表的同一事务内调用。
#'
#' @param con 已连接的 SQLite 连接（事务内）
#' @param df 含化学列的 data.frame（可为 map 后/标准化后的新行）
#' @return invisible(新增行数)
#' @keywords internal
#' @encoding UTF-8
upsert_chemicals <- function(con, df) {
  if (is.null(df) || nrow(df) == 0) return(invisible(0L))
  if (!"InChIKey" %in% names(df)) return(invisible(0L))
  d <- df[!is_blank_key(df[["InChIKey"]]), , drop = FALSE]
  if (nrow(d) == 0) return(invisible(0L))
  if (!DBI::dbExistsTable(con, "chemicals")) return(invisible(0L))

  need <- c("InChIKey", "CID", "Formula", "SMILES", "IUPACName", "ExactMass")
  for (col in setdiff(need, names(d))) d[[col]] <- NA
  d <- d[, need, drop = FALSE]
  d[["CID"]] <- suppressWarnings(as.integer(d[["CID"]]))
  d[["ExactMass"]] <- suppressWarnings(as.numeric(d[["ExactMass"]]))
  for (col in c("InChIKey", "Formula", "SMILES", "IUPACName")) {
    d[[col]] <- as.character(d[[col]])
    d[[col]][is.na(d[[col]])] <- NA_character_
  }

  n <- 0L
  for (i in seq_len(nrow(d))) {
    ok <- tryCatch({
      DBI::dbExecute(con,
        "INSERT OR IGNORE INTO chemicals (InChIKey, CID, Formula, SMILES, IUPACName, ExactMass)
         VALUES (?, ?, ?, ?, ?, ?)",
        params = list(d[["InChIKey"]][i], d[["CID"]][i], d[["Formula"]][i],
                      d[["SMILES"]][i], d[["IUPACName"]][i], d[["ExactMass"]][i]))
    }, error = function(e) {
      message("   (chemicals upsert skipped: ", conditionMessage(e), ")")
      0L
    })
    n <- n + ok
  }
  if (n > 0) message("   chemicals: ", n, " new record(s) inserted")
  invisible(n)
}

# ---- 主流程编排（四个库共用这一条流水线） ------------------------------------

#' 通用增量更新编排（跨 cmr / iarc / eu_sml 复用）
#'
#' 统一顺序：对齐库表列 -> 回填老物质 -> 只对新增查 PubChem -> diff ->
#' 确认 -> 入库。确认规则与 update_svhc_auto 一致：removed 强制人工确认；
#' 非交互且 auto_apply 时受 max_auto_changes 限制。
#'
#' @param db_name 库表名
#' @param new_df 标准化后的新数据（列名尽量对齐库表）
#' @param key_col 主键列名
#' @param cas_col CAS 列名
#' @param name_col 名称列名（进度打印用）
#' @param fallback_col 兜底键列名（可为 NULL）
#' @param content_cols 内容比较列（NULL 则比较所有业务列）
#' @param interactive 是否交互确认
#' @param auto_apply 非交互时是否自动入库
#' @param max_auto_changes 自动入库最大变更条数
#' @param enrich 是否补 PubChem 化学元数据
#' @param db_path 自定义数据库路径
#' @param backup 入库前是否备份
#' @param delay PubChem 请求间隔秒数
#' @return list(success, changes, db_write)
#' @keywords internal
#' @encoding UTF-8
run_incremental_update <- function(db_name, new_df, key_col, cas_col, name_col,
                                   fallback_col = NULL, content_cols = NULL,
                                   interactive = TRUE, auto_apply = FALSE,
                                   max_auto_changes = 20, enrich = TRUE,
                                   db_path = NULL, backup = TRUE, delay = 0.35) {
  if (is.null(new_df) || nrow(new_df) == 0) {
    stop(db_name, " source produced no rows; nothing to update")
  }

  # 1. 对齐库表列
  new_df <- map_to_db_columns(new_df, db_name, db_path)

  # 2. 补 meta：先回填老物质，只对新增查 PubChem
  if (isTRUE(enrich)) {
    message("Enriching missing chemical metadata (incremental)...")
    new_df <- backfill_meta_from_db(new_df, db_name, cas_col, db_path = db_path)
    new_df <- enrich_new_compounds(new_df, cas_col, name_col, delay = delay, verbose = TRUE)
  }

  # 3. diff（用完立即断开，避免与入库写连接竞争）
  con <- get_db_connection(db_path)
  current <- DBI::dbGetQuery(con, paste("SELECT * FROM", db_name))
  DBI::dbDisconnect(con)
  # 3b. 新源未提供的业务列（如旧版导出的备用列）按 key 回填库中旧值，
  #     避免整表误报 modified 或把历史列清空。key 用与 diff 相同的定义
  #     （key_of_df：主键为空时退到兜底列），否则会出现"diff 认得出来、
  #     回填补不到"的行 —— iarc 有 5 行无 CAS 的组条目（Arecoline /
  #     Hexachlorocyclohexanes / Hypochlorite salts / MOPP... / Sulfites）
  #     正是这样丢掉 InChIKey，最后撞上非空约束、把同批 846 行一起回滚。
  new_df <- backfill_unmapped_cols(new_df, current, key_col, fallback_col)
  # 3c. cmr 的限值列清洗。specific_conc_limits / m_factors 在增量源里**不能**映射：
  #     新增量源（ATP23）对 91 个 Index No 的 "M, SCL, ATE" 是空的，而老 meta 有值
  #     （如 005-008-00-8 的 "Repr. 1B; H360FD: C ≥ 3,1 %"），映射会把它们擦成 NULL。
  #     所以清洗发生在回填之后：两列曾被整串写成一样的值，把 M 行搬到 m_factors。
  #     幂等，只动两列完全相同的行。
  if (identical(db_name, "cmr")) {
    new_df <- heal_cmr_split_cols(new_df)
  }
  changes <- diff_incremental(new_df, current, key_col, fallback_col, content_cols,
                              cas_col = cas_col)

  # 3c-2. 人工裁定"上游已删、但保留"的行（见 query_kept_removals()）。剔除必须
  #       赶在安全阀之前：闸门看的是剔除后的 total_removed，不剔就等于照拦。
  kept <- query_kept_removals(db_name, db_path)
  if (length(kept) > 0L) {
    n_removed_before <- changes$total_removed
    changes <- drop_kept_removals(changes, kept, key_col, fallback_col)
    if (changes$total_removed < n_removed_before) {
      message("   (retained on purpose: ",
              n_removed_before - changes$total_removed,
              " removal(s) kept by ledger)")
    }
  }

  # 3d. 收不了的行摘出来登记（见 split_unassignable() 的长注释）。必须赶在安全阀
  #     之前：摘完 total_added 才是真正要写的量，闸门该看这个数。
  #     以前这些行一路带到 dbAppendTable，撞 InChIKey NOT NULL 把整批拖回滚。
  split_out <- split_unassignable(changes, db_name, key_col, fallback_col,
                                  cas_col = cas_col, name_col = name_col,
                                  looked_up = isTRUE(enrich))
  changes <- split_out$changes
  if (split_out$n_dropped > 0L) {
    message("   ! Unstorable: ", split_out$n_dropped, " row(s) -> registry",
            if (isTRUE(enrich)) " (structure looked up, not found)"
            else " (enrich = FALSE: structure never looked up)")
  }

  # 摘掉的行必须随返回值交出去：changes$added 已被改写，调用方再也看不到它们。
  # dry-run 分支不写库（所以也不记账），但调用方仍要能拿到这份清单 —— 否则
  # "收不了什么"在预览阶段就丢了。
  done <- function(success, db_write, message) {
    list(success = success, changes = changes, db_write = db_write,
         unassigned = split_out$dropped, n_unassigned = split_out$n_dropped,
         message = message)
  }

  message("Diff result:")
  message("   + Added:    ", changes$total_added)
  message("   - Removed:  ", changes$total_removed)
  message("   ~ Modified: ", changes$total_modified)

  if (changes$total_added + changes$total_removed + changes$total_modified == 0) {
    message(db_name, " is up to date - no changes needed")
    return(done(TRUE, NULL, "No changes"))
  }

  # 4. 确认
  if (changes$total_removed > 0) {
    proceed <- FALSE
    if (interactive) {
      resp <- readline(sprintf("%s: removals detected. Apply anyway? (y/N): ", db_name))
      proceed <- tolower(trimws(resp)) == "y"
    }
    if (!proceed) {
      return(done(FALSE, NULL, "Cancelled: removals require manual review"))
    }
  } else if (interactive) {
    resp <- readline(sprintf("Apply %d changes to %s? (y/N): ",
                             changes$total_added + changes$total_modified, db_name))
    if (tolower(trimws(resp)) != "y") {
      return(done(FALSE, NULL, "User cancelled"))
    }
  } else if (!isTRUE(auto_apply)) {
    return(done(FALSE, NULL, "Dry run (no apply)"))
  } else if (changes$total_added + changes$total_modified > max_auto_changes) {
    return(done(FALSE, NULL, "Too many changes for auto_apply"))
  }

  # 5. 入库
  db_write <- write_changes_to_db(db_name, changes, key_col, fallback_col, db_path, backup)

  # 6. 账本：本次摘下来的登记进去；本次成功入库的键若还挂在账上，标成 resolved。
  #    留痕失败不影响数据入库 —— 与 change_log 同一约定：明细是顺手留痕，不是主流程。
  tryCatch({
    record_unassigned(db_name, split_out$dropped, db_path = db_path)
    mark_unassigned_resolved(db_name,
                             key_of_df(changes$added, key_col, fallback_col),
                             db_path = db_path)
  }, error = function(e) {
    message("   (unassigned registry write skipped: ", conditionMessage(e), ")")
  })

  done(TRUE, db_write, "Update applied")
}

# =============================================================================
# 明细：change_log 逐条写入（"谁变了、什么变成什么"）
#
# 由 write_changes_to_db() / write_svhc_to_db() 在记完 update_history 总数后
# 调用。下面先是几个取行标识的小工具（row_identifier / row_inchikey /
# cell_canon / row_keys），再是主函数 log_change_detail()。
# =============================================================================

# 取每行的"人类可读标识"：名称类列优先，其次 CAS 类列，全空返回 NA
row_identifier <- function(df) {
  if (!is.data.frame(df) || nrow(df) == 0) return(character(0))
  id <- rep(NA_character_, nrow(df))
  # 名称类列优先，其次 CAS 类列，全空返回 NA。同时收新旧两代库的列名
  # （驼峰：Substance name / International Chemical Identification；
  #  蛇形：substance_name / international_chemical_identification ...）。
  cand <- c("Substance name", "Substance Name",
            "International Chemical Identification",
            "Agent", "agent", "Substance", "Name", "name",
            "substance_name", "international_chemical_identification",
            "fcm_substance_no",
            "CAS No.", "CAS No", "CAS", "cas_no")
  for (nm in cand) {
    if (nm %in% names(df)) {
      v <- as.character(df[[nm]])
      fill <- is.na(id) | !nzchar(trimws(id))
      if (any(fill)) id[fill] <- v[fill]
    }
  }
  trimws(id)
}

# 取每行的 InChIKey（无该列则全 NA）
row_inchikey <- function(df) {
  if (!is.data.frame(df) || nrow(df) == 0) return(character(0))
  if ("InChIKey" %in% names(df)) {
    as.character(df[["InChIKey"]])
  } else {
    rep(NA_character_, nrow(df))
  }
}

# 单元格规范化后再比较，避免换行符 / 首尾空白 / 多行顺序差异造成假 modified 明细
cell_canon <- function(v) canon_cell(v)

# 行键：svhc 走 key_fn（svhc_key_of），增量库按 key_col/fallback_col 生成
# （与 key_of_df() 同逻辑，diff / 回填 / 明细三处共用）
row_keys <- function(df, key_fn = NULL, key_col = NULL, fallback_col = NULL) {
  if (!is.null(key_fn)) return(key_fn(df))
  if (!is.data.frame(df) || nrow(df) == 0) return(character(0))
  if (is.null(key_col) || !key_col %in% names(df)) {
    return(rep(NA_character_, nrow(df)))
  }
  key_of_df(df, key_col, fallback_col)
}

#' 把 diff 结果写成 change_log 逐条明细
#'
#' 与 write_changes_to_db() / write_svhc_to_db() 配合：在 update_history 记完
#' 总数之后调用。added / removed 每个物质记一行；modified 逐字段对比（需要
#' old_df 为修改前的旧表快照），每个字段记一行 old_value -> new_value。
#' 明细写入失败不抛出——由调用方 tryCatch 兜底，仅提示，不影响数据入库。
#'
#' @param con 已连接的 SQLite 连接
#' @param history_id 本次 update_history 记录的 id
#' @param db_name 库表名
#' @param changes diff 输出（含 added / removed / modified 三个 data.frame）
#' @param old_df 修改前的旧表全量快照（modified 逐字段对比用，可为 NULL）
#' @param key_fn 行键函数（svhc 用 svhc_key_of；NULL 时按 key_col/fallback_col）
#' @param key_col 主键列名（key_fn 为 NULL 时使用）
#' @param fallback_col 兜底键列名
#' @param compare_cols 参与逐字段对比的列（NULL 时用两表共有业务列，排除
#'   化学元数据 / 系统列；svhc 传 svhc_content_columns）
#' @param exclude_cols 额外排除的列
#' @return invisible(NULL)；成功时 message 汇总条数
#' @keywords internal
#' @encoding UTF-8
log_change_detail <- function(con, history_id, db_name, changes, old_df = NULL,
                              key_fn = NULL, key_col = NULL, fallback_col = NULL,
                              compare_cols = NULL, exclude_cols = NULL) {
  excl <- c(chem_cols, "id", "created_at", "updated_at", "InChIKey")
  if (!is.null(exclude_cols)) excl <- unique(c(excl, exclude_cols))

  # 通用一条记录（added/removed/整行占位 modified 用）
  mk <- function(d, ctype) {
    data.frame(
      update_history_id = history_id,
      database_name = db_name,
      InChIKey = row_inchikey(d),
      substance_identifier = row_identifier(d),
      change_type = ctype,
      field_name = NA_character_,
      old_value = NA_character_,
      new_value = NA_character_,
      stringsAsFactors = FALSE)
  }

  out <- list()
  if (!is.null(changes$added) && nrow(changes$added) > 0) {
    out[[length(out) + 1L]] <- mk(changes$added, "added")
  }
  if (!is.null(changes$removed) && nrow(changes$removed) > 0) {
    out[[length(out) + 1L]] <- mk(changes$removed, "removed")
  }
  if (!is.null(changes$modified) && nrow(changes$modified) > 0) {
    d <- changes$modified
    id_new <- row_identifier(d)
    ik_new <- row_inchikey(d)
    k_new <- row_keys(d, key_fn = key_fn, key_col = key_col,
                      fallback_col = fallback_col)
    k_old <- character(0)
    if (!is.null(old_df) && nrow(old_df) > 0) {
      k_old <- row_keys(old_df, key_fn = key_fn, key_col = key_col,
                        fallback_col = fallback_col)
    }
    for (i in seq_len(nrow(d))) {
      matched <- FALSE
      if (length(k_old) > 0 && !is.na(k_new[i]) && nzchar(k_new[i])) {
        hit <- which(!is.na(k_old) & k_old == k_new[i])
        if (length(hit) > 0) {
          matched <- TRUE
          oi <- hit[1]
          cols <- if (!is.null(compare_cols)) {
            # 新旧数据列名体系可能不同（项目存在新旧 schema 并存），
            # 只比较两边都有的列，避免取到 NULL 产生假记录
            intersect(compare_cols, intersect(names(d), names(old_df)))
          } else {
            setdiff(intersect(names(old_df), names(d)), excl)
          }
          n_rec <- 0L
          for (col in cols) {
            ov <- cell_canon(old_df[[col]][oi])
            nv <- cell_canon(d[[col]][i])
            if (!identical(ov, nv)) {
              out[[length(out) + 1L]] <- data.frame(
                update_history_id = history_id,
                database_name = db_name,
                InChIKey = ik_new[i],
                substance_identifier = id_new[i],
                change_type = "modified",
                field_name = col,
                old_value = ov,
                new_value = nv,
                stringsAsFactors = FALSE)
              n_rec <- n_rec + 1L
            }
          }
          if (n_rec == 0L) next  # 行数增减触发的 modified，无字段级差异：不记噪音
        }
      }
      if (!matched) {
        # 旧行没匹配上：记一条整行占位（无字段级信息）
        out[[length(out) + 1L]] <- mk(d[i, , drop = FALSE], "modified")
      }
    }
  }
  if (length(out) == 0) return(invisible(NULL))
  det <- do.call(rbind, out)
  DBI::dbAppendTable(con, "change_log", det)
  message(sprintf("   change_log: %d detail row(s) recorded", nrow(det)))
  invisible(NULL)
}



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
#' @encoding UTF-8
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
#' @encoding UTF-8
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
#' @encoding UTF-8
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
#' @encoding UTF-8
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
#' @encoding UTF-8
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
#' @encoding UTF-8
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
#' @encoding UTF-8
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
#' @encoding UTF-8
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
                       source_file = "update_svhc.R",
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
#' @encoding UTF-8
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

