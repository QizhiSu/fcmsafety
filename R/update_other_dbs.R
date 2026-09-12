# =============================================================================
# CMR / CMR_suspect / IARC / EU_SML 增量更新链路
#
# 与 update_svhc_auto 同一套思路：下载/读本地 -> 标准化 -> 先 diff（用清单自有
# 主键，如 Index No / Agent / FCM substance No）-> 只对真正新增的物质查 PubChem
# -> 确认 -> 入库。核心逻辑全部复用 R/incremental_update.R 里的
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
#                （独立一条线，见 auto_update_svhc.R；code-map 约定不迁公共线）
#   key_col 等   run_incremental_update 的键/列参数
#   fetch        取数配置：下载函数名与落盘名、各分支（显式文件 / 下载 / 本地
#                候选 / meta 兜底）各自的 sheet 与表头层数、期望列、normalize
#                函数、H 码筛选（screen_kind，仅 CLP 派生的两库）
# 新增一个法规库的触点因此收敛为：① schema.sql 建表 ② download_sources.R 加
# 下载函数 ③ 这里加一条注册项。注意 fetch 层是 internal，可自由演进；四个
# 导出 update_*_auto 与四个 fetch_*_data 保留原签名作为薄壳（测试与用户依赖）。
# manual_list_check.R 的 file_map 与 database_inspector_app.R 的 UPDATE_DBS
# 暂是独立登记点，待并入（见架构评审候选 3）。
DB_SOURCES <- list(
  cmr = list(
    label = "CMR", line = "incremental",
    key_col = "index_no", fallback_col = "cas_no",
    cas_col = "cas_no", name_col = "international_chemical_identification",
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
  svhc = list(label = "SVHC", line = "svhc")
)

# ---- 数据获取：读本地新文件 / 回退 meta 文件 / 下载 --------------------------

#' 在 inst/ 中按候选名解析第一个存在的文件
#'
#' @param candidates 候选文件名向量
#' @param inst_dir inst 目录
#' @return 存在的文件路径，找不到返回 NULL
#' @keywords internal
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
#' 四个 fetch_*_data 孪生体的唯一实现：显式文件 -> 下载（可配置失败回退本地）->
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

#' 获取 CMR 数据（local / download）
#'
#' local 优先读 clp_new.xlsx / clp_new.csv / annex_vi_clp.xlsx，找不到则回退
#' clp_cmr_meta.xlsx 的 "cmr" 工作表；download 调 download_clp() 下载原始导出。
#' 两条路径最后**一律**按 H 码（H340/H350/H360）筛出 CMR 子集后再返回。
#'
#' @param source "local" 或 "download"
#' @param new_file 显式文件路径（优先于候选名）
#' @param inst_dir inst 目录
#' @return 标准化后的 data.frame
#' @keywords internal
#' @encoding UTF-8
fetch_cmr_data <- function(source = c("local", "download"), new_file = NULL,
                           inst_dir = file.path(getwd(), "inst")) {
  fetch_source_data("cmr", source = source, new_file = new_file, inst_dir = inst_dir)
}

#' 获取 CMR_suspect 数据（local / download）
#'
#' local 读 clp_cmr_meta.xlsx 的 "cmr_suspect" 工作表；download 调 download_clp()。
#' 两条路径最后**一律**按 H 码（H341/H351/H361）筛出疑似 CMR 子集后再返回。
#'
#' @param source "local" 或 "download"
#' @param new_file 显式文件路径（可选，默认读 meta 文件的 cmr_suspect 工作表）
#' @param inst_dir inst 目录
#' @return 标准化后的 data.frame
#' @keywords internal
#' @encoding UTF-8
fetch_cmr_suspect_data <- function(source = c("local", "download"),
                                   new_file = NULL,
                                   inst_dir = file.path(getwd(), "inst")) {
  fetch_source_data("cmr_suspect", source = source, new_file = new_file,
                    inst_dir = inst_dir)
}

#' 获取 IARC 数据（local / download）
#'
#' @param source "local" 或 "download"
#' @param new_file 显式文件路径
#' @param inst_dir inst 目录
#' @return 标准化后的 data.frame
#' @keywords internal
#' @encoding UTF-8
fetch_iarc_data <- function(source = c("local", "download"), new_file = NULL,
                            inst_dir = file.path(getwd(), "inst")) {
  fetch_source_data("iarc", source = source, new_file = new_file, inst_dir = inst_dir)
}

#' 获取 EU SML 数据（local / download）
#'
#' @param source "local" 或 "download"
#' @param new_file 显式文件路径
#' @param inst_dir inst 目录
#' @return 标准化后的 data.frame
#' @keywords internal
#' @encoding UTF-8
fetch_eu_sml_data <- function(source = c("local", "download"), new_file = NULL,
                              inst_dir = file.path(getwd(), "inst")) {
  fetch_source_data("eu_sml", source = source, new_file = new_file, inst_dir = inst_dir)
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
# svhc -> update_svhc_auto()（auto_update_svhc.R）。
# 维护约定：新增数据源 = DB_SOURCES 加一条注册项（名单与分发自动派生）；
# 改某源行为 = 改注册项或对应子函数，这里不动。
# =============================================================================

#' 可一键自动更新的数据源清单
#' @keywords internal
#' @encoding UTF-8
ALL_AUTO_DBS <- names(DB_SOURCES)

#' 解析 databases 参数："all" 展开为全部数据源，否则校验名字合法
#' @keywords internal
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
#'   "wikipedia"、"local"、"echa"
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
#' @encoding UTF-8
update_database_auto <- function(databases = "all",
                                 source = c("download", "local"),
                                 svhc_source = c("auto", "wikipedia", "local", "echa"),
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
