# =============================================================================
# 通用增量更新核心（跨 svhc / cmr / cmr_suspect / iarc / eu_sml 复用）
#
# 目标：从 PubChem 提取化学元数据（CID / SMILES / InChIKey / Formula 等）时，
# 只对"真正新增"的物质发请求；库中已存在的物质用"从库回填"直接带过来；
# 未变动的物质零请求。这样把 PubChem 查询量从"全表几百个"降到"新增几个"。
#
# 各数据库的更新链路统一顺序：
#   下载 -> 标准化 -> 回填(backfill) -> 只补新增(enrich) -> diff -> 入库
#
# 说明：旧库（驼峰 schema）把化学元数据直接内嵌在各库表里；重建后的新库
# （蛇形 schema）把化学字段集中存进 chemicals 总表，业务表只留 InChIKey 引用
# （且只保留带 InChIKey 的行，见 process_database_for_migration）。本文件是
# 公共流水线：写库时按"库表实际列名"动态对齐（map_to_db_columns），因此
# 理论上对新旧两代结构都能适配——前提是调用方把 key_col / cas_col / name_col
# 传成与目标库一致的列名（新库为蛇形：index_no / cas_no / substance_name ...）。
# =============================================================================

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
#' @encoding UTF-8
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
#' @encoding UTF-8
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
#' @encoding UTF-8
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
                       source_file = "incremental_update.R",
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
