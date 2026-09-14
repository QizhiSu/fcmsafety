# =============================================================================
# IARC 组别归属判定（物质 → 组条目）
#
# 当用户检测出一种物质 A（CAS / InChIKey / SMILES / 分子式任一），
# 判断它属于哪些 IARC 组别条目（如 "Cadmium and cadmium compounds"、
# "Chromium (VI) compounds" 这类"一大类物质总称"条目）。
#
# 核心策略：先按输入物质的元素组成粗筛候选组（反查，不遍历全表），
# 再对候选组做启发式精确判定（元素层自动 / 价态层启发 / 场景层标人工）。
# =============================================================================

# 常量内联到各函数内部，避免 ::: 访问时环境隔离导致找不到

# ---- 输入归一 ----

#' 将用户输入归一为结构化身份对象
#'
#' 支持 CAS / InChIKey / SMILES / 分子式 四种输入形态，自动识别。
#' 离线优先：先查本地 chemicals 表；查不到且 online=TRUE 时才联网 PubChem。
#'
#' @param input 字符标量：CAS 号、InChIKey、SMILES 或分子式
#' @param online 逻辑值，是否允许联网查询（默认 FALSE）
#' @param db_path 数据库路径，NULL 时用默认路径
#' @return 命名 list：elements（字符向量）、formula、smiles、inchikey、
#'   input_type、source_note
#' @keywords internal
#' @export
#' @encoding UTF-8
normalize_input_identity <- function(input, online = FALSE, db_path = NULL) {
  # P1-②：显式拒绝非标量/整表输入（原实现会对 data.frame 报晦涩的
  # 'length = 6' in coercion to 'logical(1)'）
  if (is.null(input) || length(input) != 1 || is.data.frame(input) ||
      is.na(input) || !nzchar(trimws(as.character(input)))) {
    stop("input 必须为单个非空字符（CAS / InChIKey / SMILES / 分子式）；",
         "整表批量请用 assign_group_membership_table()", call. = FALSE)
  }
  s <- trimws(as.character(input))

  # 1) CAS: \d{2,7}-\d{2}-\d
  if (grepl("^\\d{2,7}-\\d{2}-\\d$", s)) {
    return(.resolve_from_cas(s, online, db_path))
  }

  # 2) InChIKey: 大写 14-连字符-格式（如 ABCDEFGHIJKLMNO-P）
  if (grepl("^[A-Z]{14}-[A-Z]{10}-[A-Z]$", s)) {
    return(.resolve_from_inchikey(s, db_path))
  }

  # 3) SMILES: 含常见 SMILES 特征字符（如 = # [ ] ( ) @ 等），且首字符通常是大写字母或 [
  # 注意：必须用 perl=TRUE —— 默认 TRE 引擎会把字符类开头的 "[=" 当成
  # collating element（等价类）语法，导致整个字符类失效（实测 grepl 恒 FALSE），
  # 任何 SMILES 输入都会被误判为"无法识别输入形态"。
  if (grepl("^[A-Z\\[]", s) && grepl("[=#@\\(\\)\\[\\]]", s, perl = TRUE)) {
    els <- parse_smiles_elements(s)
    return(list(
      elements = els,
      formula = NA_character_,
      smiles = s,
      inchikey = NA_character_,
      input_type = "smiles",
      source_note = "用户提供 SMILES"
    ))
  }

  # 4) 分子式: 仅含字母和数字，且符合 "大写字母+可选小写字母+可选数字" 重复模式
  if (grepl("^[A-Za-z0-9]+$", s) && grepl("[A-Z]", s)) {
    els <- parse_formula_elements(s)
    return(list(
      elements = els,
      formula = s,
      smiles = NA_character_,
      inchikey = NA_character_,
      input_type = "formula",
      source_note = "用户提供分子式"
    ))
  }

  # 5) 名称: offline 时报错; online 时尝试 PubChem 名称搜索
  if (!online) {
    stop("无法识别输入形态（非 CAS / InChIKey / SMILES / 分子式）。",
         "如为物质名称，请设置 online=TRUE 以联网查询。")
  }
  return(.resolve_from_name(s, db_path))
}

# ---- 内部解析器 ----

#' 从 CAS 解析身份（查库 → 可选联网）
#' @keywords internal
#' @export
#' @encoding UTF-8
.resolve_from_cas <- function(cas, online, db_path) {
  cas_canon <- canonicalize_cas(cas)
  if (is.na(cas_canon)) {
    stop("CAS 号格式无效: ", cas)
  }

  # 先查本地 chemicals 表（按 CAS 无法直接查 chemicals，需经 iarc/svhc/cmr 等表反查）
  db <- get_db_connection(db_path)
  on.exit(DBI::dbDisconnect(db), add = TRUE)

  # 从 chemicals 表没有 CAS 列，需从业务表反查 InChIKey
  # 简化：先查 iarc 表的 cas_no（iarc 有 cas_no 列）
  sql <- "SELECT InChIKey FROM iarc WHERE cas_no = ? LIMIT 1"
  res <- DBI::dbGetQuery(db, sql, params = list(cas_canon))
  if (nrow(res) == 0) {
    # 再试 svhc
    sql <- "SELECT InChIKey FROM svhc WHERE cas_no = ? LIMIT 1"
    res <- DBI::dbGetQuery(db, sql, params = list(cas_canon))
  }
  if (nrow(res) == 0) {
    # 再试 cmr
    sql <- "SELECT InChIKey FROM cmr WHERE cas_no = ? LIMIT 1"
    res <- DBI::dbGetQuery(db, sql, params = list(cas_canon))
  }

  if (nrow(res) > 0 && !is.na(res$InChIKey[1]) && nzchar(res$InChIKey[1])) {
    return(.resolve_from_inchikey(res$InChIKey[1], db_path))
  }

  # 本地查不到
  if (!online) {
    stop("CAS ", cas_canon, " 在本地数据库中未找到。",
         "请设置 online=TRUE 以联网查询 PubChem。")
  }

  # 联网 PubChem（复用 pubchem_lookup_cas，但它按 CAS 查，返回 list）
  meta <- pubchem_lookup_cas(cas_canon)
  if (is.na(meta$SMILES) && is.na(meta$Formula)) {
    stop("CAS ", cas_canon, " 在 PubChem 中也未找到。")
  }

  els <- unique(c(
    parse_formula_elements(meta$Formula),
    parse_smiles_elements(meta$SMILES)
  ))
  list(
    elements = els,
    formula = meta$Formula,
    smiles = meta$SMILES,
    inchikey = meta$InChIKey,
    input_type = "cas",
    source_note = paste0("PubChem CAS ", cas_canon)
  )
}

#' 从 InChIKey 解析身份（查 chemicals 表）
#' @keywords internal
#' @export
#' @encoding UTF-8
.resolve_from_inchikey <- function(ik, db_path) {
  db <- get_db_connection(db_path)
  on.exit(DBI::dbDisconnect(db), add = TRUE)

  sql <- "SELECT Formula, SMILES, InChIKey FROM chemicals WHERE InChIKey = ? LIMIT 1"
  res <- DBI::dbGetQuery(db, sql, params = list(ik))

  if (nrow(res) == 0 || is.na(res$Formula[1])) {
    stop("InChIKey ", ik, " 在本地 chemicals 表中未找到。")
  }

  els <- unique(c(
    parse_formula_elements(res$Formula[1]),
    parse_smiles_elements(res$SMILES[1])
  ))
  list(
    elements = els,
    formula = res$Formula[1],
    smiles = res$SMILES[1],
    inchikey = res$InChIKey[1],
    input_type = "inchikey",
    source_note = "本地 chemicals 表"
  )
}

#' 从名称联网解析身份（PubChem 名称搜索）
#' @keywords internal
#' @export
#' @encoding UTF-8
.resolve_from_name <- function(name, db_path) {
  base <- "https://pubchem.ncbi.nlm.nih.gov/rest/pug"
  cid <- tryCatch({
    r <- httr::GET(
      sprintf("%s/compound/name/%s/cids/JSON", base,
              utils::URLencode(name, reserved = TRUE)),
      httr::timeout(30)
    )
    if (r$status_code != 200) return(NA_integer_)
    ids <- jsonlite::fromJSON(rawToChar(r$content))[["IdentifierList"]][["CID"]]
    if (length(ids) > 0) as.integer(ids[1]) else NA_integer_
  }, error = function(e) NA_integer_)

  if (is.na(cid)) {
    stop("名称 '", name, "' 在 PubChem 中未找到。")
  }

  # 用 CID 查属性（复用 pubchem_lookup_cas 的 CID 查询逻辑，但它入口是 CAS）
  # 这里直接内联简化
  r <- httr::GET(
    sprintf(paste0("%s/compound/cid/%d/property/",
                   "MolecularFormula,IsomericSMILES,CanonicalSMILES,",
                   "InChIKey,IUPACName/JSON"),
            base, cid),
    httr::timeout(30)
  )
  if (r$status_code != 200) {
    stop("PubChem CID 查询失败。")
  }
  p <- jsonlite::fromJSON(rawToChar(r$content))[["PropertyTable"]][["Properties"]]
  if (is.null(p) || length(p) == 0) {
    stop("PubChem 无属性数据。")
  }

  formula <- as.character(p[["MolecularFormula"]][1])
  smi <- p[["IsomericSMILES"]][1]
  if (is.null(smi) || is.na(smi) || !nzchar(smi)) {
    smi <- p[["CanonicalSMILES"]][1]
  }
  if (is.null(smi) || is.na(smi)) smi <- NA_character_
  ik <- as.character(p[["InChIKey"]][1])
  if (is.null(ik) || is.na(ik)) ik <- NA_character_

  els <- unique(c(parse_formula_elements(formula), parse_smiles_elements(smi)))
  list(
    elements = els,
    formula = formula,
    smiles = smi,
    inchikey = ik,
    input_type = "name",
    source_note = paste0("PubChem 名称搜索: ", name)
  )
}

# ---- 元素解析 ----

#' 骨架元素与通用元素（小写，用于与 common_els 比较）
#'
#' 这些元素出现在绝大多数有机分子或常见盐里，本身不构成任何条目的判别特征：
#' 碳骨架（C/H/O/N/S/P）、卤素、以及钠钾钙镁这类通用抗衡离子和半金属。
#'
#' 组条目若拿它们当"特征元素"，元素层命中就只说明"这个分子是有机物"或
#' "这是个盐"，没有判别力。screen_iarc_groups() 据此把这类命中压到
#' manual_review，不允许单独构成 auto_confirmed —— 它是一道护栏，防止再往
#' parse_agent_elements() 的映射表里塞类似 `Cyclamate = "C"` 的条目。
#'
#' 注意 As / Se / Cd / Cr / Co / Hg / Ni 等**不在此列**：它们在普通有机物里
#' 不出现，"含砷"对判断砷化合物是有意义的。
#'
#' @noRd
#' @export
.skeletal_elements <- c("c", "h", "o", "n", "s", "p",
                        "f", "cl", "br", "i",
                        "na", "k", "ca", "mg", "si", "b")

#' 出现这些元素时，"含碳骨架"提示可能是有机金属/络合物
#'
#' 与 .skeletal_elements 配合使用：有机砷（如三乙基砷酸酯）和无机砷（如砷酸）
#' 含同样的 As，光看元素区分不了，但含碳骨架是个有效的分界。
#'
#' @noRd
#' @export
.organic_metal_guard <- c("as", "co", "hg", "se", "cd")

#' 从分子式字符串提取元素符号集合
#'
#' 正则匹配 "大写字母 + 可选小写字母"，忽略数字。
#'
#' @param formula 分子式字符串，如 "C6H12O6"、"CdCl2"、"Cr+6"
#' @return 去重后的元素符号字符向量
#' @keywords internal
#' @export
#' @encoding UTF-8
parse_formula_elements <- function(formula) {
  if (is.null(formula) || is.na(formula) || !nzchar(formula)) return(character(0))
  # 去掉电荷标记 + / - 及数字（保留字母）
  f <- gsub("[+\\-]", "", formula)
  m <- gregexpr("[A-Z][a-z]?", f)[[1]]
  if (m[1] == -1) return(character(0))
  els <- regmatches(f, list(m))[[1]]
  unique(els)
}

#' 从 SMILES 字符串提取元素符号集合
#'
#' 优先匹配双字符元素（Br/Cl/Si/Se/As/Co/Cr/Cd/Hg/Be/Pb/Ni等），
#' 再匹配单字符；芳香小写 c/n/o/s/p 归入 C/N/O/S/P；
#' 方括号内如 `[Cd]`、`[Cr+6]` 正常解析。
#'
#' @param smiles SMILES 字符串
#' @return 去重后的元素符号字符向量
#' @keywords internal
#' @export
#' @encoding UTF-8
parse_smiles_elements <- function(smiles) {
  if (is.null(smiles) || is.na(smiles) || !nzchar(smiles)) return(character(0))

  # 双字符元素表（内联，避免 ::: 环境隔离）
  .smiles_two_char <- c(
    "Br", "Cl", "Si", "Se", "As", "Co", "Cr", "Cd", "Hg",
    "Be", "Pb", "Ni", "Ra", "Rn", "Th", "Pu", "Sr", "U", "Na"
  )

  s <- smiles
  els <- character(0)
  i <- 1
  n <- nchar(s)

  while (i <= n) {
    ch <- substr(s, i, i)

    # 跳过数字、括号、键符号、@、. 等
    if (grepl("[0-9=#@%.\\-\\\\/]", ch)) {
      i <- i + 1
      next
    }

    # 跳过普通括号
    if (ch %in% c("(", ")", "[", "]")) {
      i <- i + 1
      next
    }

    # 双字符元素优先
    if (i < n) {
      two <- substr(s, i, i + 1)
      if (two %in% .smiles_two_char) {
        els <- c(els, two)
        i <- i + 2
        next
      }
    }

    # 单字符：大写直接取；小写芳香归入对应大写
    if (grepl("[A-Z]", ch)) {
      els <- c(els, ch)
    } else if (ch %in% c("c", "n", "o", "s", "p")) {
      els <- c(els, toupper(ch))
    }
    # 其他小写（如 b 在 Br 已处理，此处不应出现孤立 b）忽略
    i <- i + 1
  }

  unique(els)
}

#' 从 IARC agent 名称提取元素词
#'
#' 用整词正则匹配元素词表，避免误抽（如 Ethanol 不含元素层组）。
#'
#' @param agent IARC agent 名称
#' @return 匹配到的元素词字符向量
#' @keywords internal
#' @export
#' @encoding UTF-8
parse_agent_elements <- function(agent) {
  if (is.null(agent) || is.na(agent) || !nzchar(agent)) return(character(0))
  # 词 -> 元素符号映射（确保与 parse_formula_elements / parse_smiles_elements 输出一致）
  #
  # 这张表只放"元素本身就是该条目定义"的词。判断标准：凡是该条目所说的物质，
  # 必然含此元素（必要条件），且不含此元素就必然不属于该条目。
  #
  # 两个词被刻意排除在外，理由是它们不满足上面的条件：
  #   Cyclamate -> C  碳是一切有机物的骨架，含碳不能说明是甜蜜素。曾以
  #                   "Cyclamate 以 C 为母体元素" 为由映射到 C，后果是全库
  #                   90% 的行被贴上"Cylamates"标签。
  #   Talc      -> Mg 镁是通用元素（氧化镁、硬脂酸镁、叶绿素都含），滑石的
  #                   特征是层状硅酸镁结构，不是"含镁"。
  # 这两条改由 layer = "manual" 标记为"无自动判据"，见 query_iarc_group_registry()。
  .element_map <- c(
    Arsenic = "As", Beryllium = "Be", Cadmium = "Cd", Chromium = "Cr",
    Cobalt = "Co", Mercury = "Hg", Selenium = "Se", Silica = "Si",
    Nickel = "Ni", Radium = "Ra", Radon = "Rn",
    Thorium = "Th", Lead = "Pb", Plutonium = "Pu", Strontium = "Sr",
    Uranium = "U"
  )
  pat <- paste0("\\b(", paste(names(.element_map), collapse = "|"), ")\\b")
  m <- gregexpr(pat, agent, ignore.case = TRUE)[[1]]
  if (m[1] == -1) return(character(0))
  words <- regmatches(agent, list(m))[[1]]
  idx <- match(tolower(words), tolower(names(.element_map)))
  unique(stats::na.omit(.element_map[idx]))
}

# ---- (see X) 交叉引用 ----

#' 归一化 IARC 条目名，供交叉引用比对
#'
#' 库里同一个条目的写法并不统一 —— `Di(2-ethylhexyl)phthalate` 与
#' `Di(2-ethylhexyl) phthalate` 只差一个空格，按原样比对就会漏。这里把大小写、
#' 空格、连字符、撇号、逗号、句点、括号、分号、冒号一律去掉后再比。
#'
#' @param x 字符向量
#' @return 归一化后的字符向量
#' @keywords internal
#' @export
#' @encoding UTF-8
.norm_iarc_name <- function(x) {
  x <- tolower(as.character(x))
  gsub("[^a-z0-9]+", "", x)
}

#' 从 IARC 条目名里解析 `(see X)` 交叉引用的目标
#'
#' IARC 的 Agents Classified 表用 `X (see Y)` 表示"X 的评价挂在 Y 下面"。
#' 这既是别名（X 与 Y 是同一物质），也是归属声明（X 归入 Y 所指的那一类）。
#' 库里共 36 行这样的记录，此前没有任何代码读它，于是这 24 行的
#' `group_classification` 在源表里为空，查这些物质一律返回"无证据"。
#'
#' 目标名自身可能带括号（`Bis(chloromethyl)ether; chloromethyl methyl ether`），
#' 所以取**最后一个** `(see` / `(See also` 之后、**最外层收尾右括号之前**的内容。
#'
#' @param agent IARC 条目名向量
#' @return 与输入等长的字符向量，解析不出时为 NA
#' @keywords internal
#' @export
#' @encoding UTF-8
parse_iarc_see_target <- function(agent) {
  a <- as.character(agent)
  out <- rep(NA_character_, length(a))
  for (i in seq_along(a)) {
    s <- a[i]
    if (is.na(s) || !nzchar(s)) next
    if (!grepl(")", s, fixed = TRUE)) next
    pos <- gregexpr("\\(\\s*[Ss]ee\\s+(also\\s+)?", s)[[1]]
    if (pos[1] == -1L) next
    k <- length(pos)
    body <- substr(s, pos[k] + attr(pos, "match.length")[k], nchar(s))
    body <- trimws(sub("\\)\\s*$", "", body))
    if (nzchar(body)) out[i] <- body
  }
  out
}

#' 现有组词正则覆盖不到的类条目（人工核定）
#'
#' `query_iarc_group_registry()` 原本只靠一条名称正则筛组条目，于是凡是不含
#' "compounds / salts / metallic / dust" 等词的类条目全部隐形 —— IARC 的
#' `Progestins`（2B）就是这样漏掉的：查 Progesterone 返回"无证据"，
#' 而它属于该组。
#'
#' 这份清单是逐条人工核对 `iarc` 表得出的（判据：条目名指代**一类**物质，
#' 成员各自没有独立条目）。刻意做成**显式人工表**而不是模糊词法启发式 ——
#' 启发式会把 `Dichlorvos`、`Tetrachlorvinphos` 这类以 s 结尾的具体物质
#' 也卷进来。
#'
#' 注意：这些条目大多抽不出特征元素，进注册表后是 `layer = "manual"`，
#' 也就是说**补上它们不改变任何定级结果**，只是让"哪些类条目还没有自动判据"
#' 从隐形变成显式可见 —— 这是后续接成员清单 / 结构骨架层的前提。
#'
#' @noRd
#' @export
.iarc_extra_group_entries <- c(
  "Aflatoxins",                                    # 组 1
  "Bleomycins",                                    # 组 2B
  "Hexachlorocyclohexanes",                        # 组 2B
  "MOPP and other combined chemotherapy including alkylating agents",  # 组 1
  "Nodularins",                                    # 组 3
  "Polybrominated biphenyls",                      # 组 2A
  "Polychlorinated biphenyls",                     # 组 1
  "Polyurethane foams",                            # 组 3
  "Progestins",                                    # 组 2B
  "Silica, amorphous",                             # 组 3
  "Styrene-acrylonitrile copolymers",              # 组 3
  "Styrene-butadiene copolymers",                  # 组 3
  "Sulfites",                                      # 组 3
  "Vinyl chloride-vinyl acetate copolymers",       # 组 3
  "Vinylidene chloride-vinyl chloride copolymers"  # 组 3
)

#' 源数据缺 CAS、但本身是单一物质的条目
#'
#' `cas_no` 为空被当作"类条目"的判据之一，但 `Arecoline`（槟榔碱）是个具体
#' 物质，只是源表里 CAS 列空着。必须排除，否则它会以组条目身份进注册表。
#'
#' @noRd
#' @export
.iarc_force_single <- c("Arecoline")

#' 解析 iarc 表里的 (see X) 交叉引用，给出可补的分组
#'
#' 只对"表里查不到任何分组"的行生效。**同键兄弟行已经给出分组的行一律跳过** ——
#' 一个 InChIKey 上叠着多条不同条目时（如滑石的三种形态、三种碳纳米管共用
#' 碳的键），无法判断来查的物质属于哪一条，把某一行的分级补进去等于给它硬套
#' 一个不属于它的分级，比留空更糟。
#'
#' 目标名解析不出（如 `Monographs on 1,3-Butadiene`，指向的是专著卷而非条目）
#' 或目标条目本身不在库里（`Asbestos`、`Fission products`、`Acid mists` 等）
#' 时，一律不补 —— 宁可不报。
#'
#' @param db_path 数据库路径
#' @return data.frame：InChIKey / group_classification / alias_of / source_agent
#' @keywords internal
#' @export
#' @encoding UTF-8
query_iarc_see_alias_map <- function(db_path = NULL) {
  empty <- data.frame(InChIKey = character(0), group_classification = character(0),
                      alias_of = character(0), source_agent = character(0),
                      stringsAsFactors = FALSE)

  db <- get_db_connection(db_path)
  on.exit(DBI::dbDisconnect(db), add = TRUE)
  raw <- DBI::dbGetQuery(db,
    "SELECT agent, InChIKey, group_classification FROM iarc")
  if (nrow(raw) == 0) return(empty)

  has_cls <- !is.na(raw$group_classification) &
    nzchar(trimws(as.character(raw$group_classification)))
  key_ok <- !is.na(raw$InChIKey) & nzchar(raw$InChIKey)
  covered <- unique(raw$InChIKey[has_cls & key_ok])

  lookup <- stats::setNames(trimws(as.character(raw$group_classification[has_cls])),
                            .norm_iarc_name(raw$agent[has_cls]))
  tgt <- parse_iarc_see_target(raw$agent)

  rows <- list()
  for (i in seq_len(nrow(raw))) {
    if (has_cls[i] || !key_ok[i] || is.na(tgt[i])) next
    if (raw$InChIKey[i] %in% covered) next
    k <- .norm_iarc_name(tgt[i])
    if (!nzchar(k) || !k %in% names(lookup)) next
    rows[[length(rows) + 1]] <- data.frame(
      InChIKey = raw$InChIKey[i],
      group_classification = unname(lookup[[k]]),
      alias_of = tgt[i],
      source_agent = raw$agent[i],
      stringsAsFactors = FALSE
    )
  }
  if (!length(rows)) return(empty)
  unique(do.call(rbind, rows))
}

#' 把 (see X) 别名补进 IARC 汇总结果
#'
#' 只填空缺，不覆盖已有分组。别名与既有证据冲突时以既有证据为准：既有证据来自
#' 物质自己的条目或同键的其它条目，而别名只说明"这一行的评价挂在别处"。
#'
#' @param iarc_summary summarise_iarc_groups() 的返回
#' @param alias_map query_iarc_see_alias_map() 的返回
#' @return iarc_summary，`group_classification` 的空缺已补齐；补了几条记录在
#'   属性 `see_alias_added` 上
#' @keywords internal
#' @export
#' @encoding UTF-8
apply_iarc_see_aliases <- function(iarc_summary, alias_map) {
  if (is.null(iarc_summary)) {
    iarc_summary <- data.frame(InChIKey = character(0),
                               group_classification = character(0),
                               stringsAsFactors = FALSE)
  }
  attr(iarc_summary, "see_alias_added") <- 0L
  if (is.null(alias_map) || nrow(alias_map) == 0) return(iarc_summary)

  idx <- match(iarc_summary$InChIKey, alias_map$InChIKey)
  fill <- is.na(iarc_summary$group_classification) & !is.na(idx)
  if (any(fill)) {
    iarc_summary$group_classification[fill] <- alias_map$group_classification[idx[fill]]
  }
  attr(iarc_summary, "see_alias_added") <- sum(fill)
  iarc_summary
}

# ---- 组条目注册表 ----

#' 构建 IARC 组条目注册表（动态解析 + 可选人工表覆盖）
#'
#' 每次运行时从 iarc 表动态筛出组条目，生成基础注册表；
#' 若存在 inst/extdata/iarc_group_manual.xlsx，则用它覆盖 layer / heuristic 等字段。
#'
#' 组条目的筛选有三个来源，取并集：
#' \enumerate{
#'   \item 名称命中组词正则（`compounds` / `salts` / `metallic` / `dust` 等）；
#'   \item `cas_no` 为空 —— 类条目通常没有唯一 CAS（扣掉
#'     `.iarc_force_single` 里已知的"源表漏填 CAS"的单一物质）；
#'   \item 命中人工核定的 `.iarc_extra_group_entries`。
#' }
#'
#' @param db_path 数据库路径
#' @return data.frame：agent / group_classification / layer / elements /
#'   heuristic_type / smiles_pattern / negative_condition / require_condition /
#'   rationale / representative_formula / representative_smiles
#'
#'   `layer` 取 `element`（按特征元素粗筛）、`valence`（价态，配 SMILES 正则）、
#'   `form`（形态，如金属态、粉尘）、`scenario`（暴露场景，不可结构判定）或
#'   `manual`（拿不出元素判据，元素层直接跳过，见下文）。
#'
#'   `negative_condition` 与 `require_condition` 是元素级的准入条件：前者表示
#'   "含这些元素就不属于本条"（如无机砷条目排除碳），后者表示"必须含这些元素
#'   才属于本条"（如有机砷条目要求碳）。两者都用分号分隔。
#'
#' @keywords internal
#' @export
#' @encoding UTF-8
query_iarc_group_registry <- function(db_path = NULL) {
  db <- get_db_connection(db_path)
  on.exit(DBI::dbDisconnect(db), add = TRUE)

  sql <- paste0(
    "SELECT i.agent, i.group_classification, i.cas_no, ",
    "c.Formula, c.SMILES ",
    "FROM iarc i LEFT JOIN chemicals c ON i.InChIKey = c.InChIKey ",
    "WHERE i.group_classification IS NOT NULL"
  )
  raw <- DBI::dbGetQuery(db, sql)

  # 筛组条目（组词表内联，避免 ::: 环境隔离）。
  #
  # 三个来源取并集 —— 单靠名称正则会漏掉所有不含组词的类条目（Progestins、
  # Aflatoxins、Polychlorinated biphenyls 等，实测漏 15 条，覆盖率只有 24/39）：
  #   1. 名称命中组词正则
  #   2. cas_no 为空（类条目通常没有唯一 CAS），扣掉已知"源表漏填 CAS"的单一物质
  #   3. 人工核定的 .iarc_extra_group_entries
  .group_pattern <- paste0(
    "(compounds|and its (salts|decay products)|metal without|metallic|",
    "fibres|fibers|dust|Cyclamates?|salts\\b)"
  )
  no_cas <- is.na(raw$cas_no) | !nzchar(trimws(as.character(raw$cas_no)))
  is_group <- grepl(.group_pattern, raw$agent, ignore.case = TRUE) |
    no_cas |
    raw$agent %in% .iarc_extra_group_entries
  is_group[raw$agent %in% .iarc_force_single] <- FALSE

  reg <- raw[is_group, , drop = FALSE]
  if (nrow(reg) == 0) {
    stop("IARC 表中未找到任何组条目，请检查数据库。")
  }

  # 基础字段
  reg$layer <- "element"
  reg$heuristic_type <- "none"
  reg$smiles_pattern <- NA_character_
  reg$negative_condition <- NA_character_
  reg$require_condition <- NA_character_
  reg$rationale <- ""

  # 按名称特征自动升级 layer
  for (i in seq_len(nrow(reg))) {
    ag <- reg$agent[i]
    if (grepl("\\([IVX]+\\)", ag)) {
      reg$layer[i] <- "valence"
      reg$heuristic_type[i] <- "smiles_regex"
    } else if (grepl("crystalline|amorphous|metallic|metal|fibres|fibers|dust", ag, ignore.case = TRUE)) {
      reg$layer[i] <- "form"
      reg$heuristic_type[i] <- "none"
    } else if (grepl("decay products|alcoholic beverages|ultraviolet|body powder|perineal", ag, ignore.case = TRUE)) {
      reg$layer[i] <- "scenario"
      reg$heuristic_type[i] <- "none"
    }
  }

  # 从 agent 名称抽元素（供元素层粗筛）
  reg$elements <- vapply(reg$agent, function(a) {
    els <- parse_agent_elements(a)
    paste(els, collapse = ";")
  }, character(1), USE.NAMES = FALSE)

  # 抽不出特征元素的条目 = 元素层对它无判据，标注出来。
  # 这类条目（甜蜜素、糖精、次氮基三乙酸、次氯酸盐）在 screen_iarc_groups()
  # 里会因 elements 为空而整个跳过，不会产生任何命中 —— 也就是说它们是
  # **静默漏报**。标成 layer = "manual" 至少让注册表本身能看出"这些还没有
  # 自动判据"，将来接关键词层时这就是待办清单。宁可不报，也不要靠错误判据
  # 硬报（Cyclamate -> C 就是这么来的）。
  reg$layer[!nzchar(reg$elements)] <- "manual"

  # 预填已知启发式规则（第一版硬编码知识，后续可移入 xlsx）
  for (i in seq_len(nrow(reg))) {
    ag <- reg$agent[i]
    if (ag == "Chromium (VI) compounds") {
      # 注意：R 字符串里写正则，\\[ 表示字面 [, \\- 表示字面 -
      reg$smiles_pattern[i] <- "\\[O\\-\\]\\[Cr\\]\\(=O\\)|Cr\\(=O\\)\\(=O\\)|CrO4|Cr2O7"
      reg$rationale[i] <- "chromate/dichromate pattern"
    } else if (ag == "Chromium (III) compounds") {
      reg$smiles_pattern[i] <- "\\[Cr\\+3\\]|Cr\\(\\)"
      reg$rationale[i] <- "Cr(III) coordination pattern (rough)"
    } else if (ag == "Cobalt metal without tungsten carbide") {
      reg$negative_condition[i] <- "W"
      reg$rationale[i] <- "metallic cobalt, excluding tungsten carbide composite"
    } else if (ag == "Cobalt sulfate and other soluble cobalt(II) salts") {
      reg$smiles_pattern[i] <- "\\[Co\\+2\\]|CoSO4|Co\\(\\)"
      reg$rationale[i] <- "soluble Co(II) salt pattern"
    } else if (ag == "Silica dust, crystalline, in the form of quartz or cristobalite") {
      # 条目说的是石英/方石英**粉尘**，与有机硅无关。硅氧烷、硅烷偶联剂、
      # 含硅农药的 SMILES 里 Si 都连着碳；无机硅（二氧化硅、硅酸盐、氟硅酸盐）
      # 一律不含碳。所以"含碳"是一条干净的排除线。
      # 排除之后剩下的才是无机硅，但 SMILES 无法再区分结晶与无定形（两者写法
      # 相同），故仍停在 manual_review，见 layer = "form"。
      reg$negative_condition[i] <- "C"
      reg$rationale[i] <- "inorganic silica only (carbon excluded); crystalline vs amorphous not structure-detectable"
    } else if (ag == "Arsenic and inorganic arsenic compounds") {
      # 条目名里的 "inorganic" 是实义限定词，元素层看不见它。
      # 有机砷（三乙基砷酸酯、甲基胂酸、二甲基胂酸、砷甜菜碱）全都含碳，
      # 无机砷（砷酸、五氧化二砷、砷酸钙/铅/镍）全都不含碳 —— 同一条排除线。
      # 被排除的行会由 "Arsenobetaine and other organic arsenic compounds"
      # （IARC 3 组）接住，那才是它们该去的地方。
      reg$negative_condition[i] <- "C"
      reg$rationale[i] <- "inorganic arsenic only (carbon excluded); organic arsenic is a separate IARC group 3 entry"
    } else if (ag == "Arsenobetaine and other organic arsenic compounds that are not metabolized in humans") {
      # 与上一条互为正反面：这条的限定词是 "organic"。元素层看到 As 就命中，
      # 而砷单质、砷化镓、砷酸、五氧化二砷这些不含碳的无机砷也会被它收进来，
      # 于是同一批物质同时挂在组 1 和组 3 两条条目下（等级取最严，结果侥幸
      # 不错，但 Group_hits 里会出现明显不相关的条目名）。要求含碳即可分开。
      reg$require_condition[i] <- "C"
      reg$rationale[i] <- "organic arsenic only (carbon required); inorganic arsenic is a separate IARC group 1 entry"
    } else if (ag == "Mercury and inorganic mercury compounds") {
      # 同砷：有机汞（甲基汞等）与无机汞靠含碳与否分开。
      reg$negative_condition[i] <- "C"
      reg$rationale[i] <- "inorganic mercury only (carbon excluded)"
    } else if (grepl("decay products", ag, ignore.case = TRUE)) {
      reg$rationale[i] <- "radioactive decay products, requires nuclide info"
    } else if (grepl("alcoholic beverages", ag, ignore.case = TRUE)) {
      reg$rationale[i] <- "exposure scenario (alcoholic beverages), not structure-detectable"
    } else if (grepl("ultraviolet", ag, ignore.case = TRUE)) {
      reg$rationale[i] <- "PUVA combined exposure, not structure-detectable"
    }
  }

  # 尝试读取人工标注表做覆盖
  manual_path <- system.file("extdata", "iarc_group_manual.xlsx", package = "fcmsafety")
  if (manual_path != "" && file.exists(manual_path)) {
    tryCatch({
      man <- import_xlsx(manual_path)
      if (nrow(man) > 0 && "agent" %in% names(man)) {
        for (i in seq_len(nrow(man))) {
          idx <- which(reg$agent == man$agent[i])
          if (length(idx) == 1) {
            for (col in c("layer", "heuristic_type", "smiles_pattern",
                          "negative_condition", "rationale")) {
              if (col %in% names(man) && !is.na(man[[col]][i]) && nzchar(man[[col]][i])) {
                reg[[col]][idx] <- as.character(man[[col]][i])
              }
            }
          }
        }
      }
    }, error = function(e) {
      warning("读取 iarc_group_manual.xlsx 失败: ", e$message)
    })
  }

  # 列名整理
  names(reg)[names(reg) == "Formula"] <- "representative_formula"
  names(reg)[names(reg) == "SMILES"] <- "representative_smiles"

  reg[, c("agent", "group_classification", "layer", "elements",
          "heuristic_type", "smiles_pattern", "negative_condition",
          "require_condition", "rationale",
          "representative_formula", "representative_smiles")]
}

# ---- 主判定逻辑 ----

#' 对候选组做启发式精确判定
#'
#' @param identity normalize_input_identity() 的返回对象
#' @param registry query_iarc_group_registry() 的返回 data.frame
#' @return data.frame（长表）：命中组列表 + 证据 + 置信
#' @keywords internal
#' @export
#' @encoding UTF-8
screen_iarc_groups <- function(identity, registry) {
  if (nrow(registry) == 0) {
    return(data.frame(
      matched_agent = character(0),
      iarc_group = character(0),
      layer = character(0),
      element_hits = character(0),
      evidence = character(0),
      confidence = character(0),
      source_detail = character(0),
      stringsAsFactors = FALSE
    ))
  }

  hits <- list()
  input_els <- identity$elements

  for (i in seq_len(nrow(registry))) {
    row <- registry[i, ]
    # layer = "manual"：该条目拿不出可用的元素判据（甜蜜素、糖精、次氮基三
    # 乙酸、次氯酸盐、滑石）。元素层直接跳过，不产生命中。宁可不报，也不靠
    # 错误判据硬报 —— Cyclamate -> C 就是这么来的。
    if (identical(row$layer, "manual")) next
    group_els <- strsplit(row$elements, ";", fixed = TRUE)[[1]]
    if (length(group_els) == 0) next

    # 元素层粗筛：输入物质是否含组特征元素
    common_els <- intersect(tolower(input_els), tolower(group_els))
    if (length(common_els) == 0) next

    # 默认置信
    conf <- "auto_confirmed"
    evidence <- paste0("元素命中: ", paste(common_els, collapse = ", "))

    # 价态/形态层启发式
    if (row$layer %in% c("valence", "form")) {
      if (!is.na(row$smiles_pattern) && nzchar(row$smiles_pattern) &&
          !is.na(identity$smiles) && nzchar(identity$smiles)) {
        if (grepl(row$smiles_pattern, identity$smiles)) {
          conf <- "probable"
          evidence <- paste0(evidence, "; SMILES 启发式命中: ", row$rationale)
        } else {
          conf <- "manual_review"
          evidence <- paste0(evidence, "; 价态/形态启发式未命中: ", row$rationale, "，需人工确认")
        }
      } else {
        conf <- "manual_review"
        evidence <- paste0(evidence, "; 缺少 SMILES，无法做价态/形态启发式判定: ", row$rationale)
      }
    }

    # 场景层
    if (row$layer == "scenario") {
      conf <- "manual_review"
      evidence <- paste0(evidence, "; 场景层不可结构判定: ", row$rationale)
    }

    # 负向条件
    if (!is.na(row$negative_condition) && nzchar(row$negative_condition)) {
      neg_els <- strsplit(row$negative_condition, ";", fixed = TRUE)[[1]]
      neg_hit <- intersect(tolower(input_els), tolower(neg_els))
      if (length(neg_hit) > 0) {
        # 触发负向条件 → 剔除该组命中
        next
      }
    }

    # 正向条件：条目要求输入必须含某元素。
    # 负向条件解决"这条不该收我"，正向条件解决"这条只该收某类"。
    # 例："Arsenobetaine and other organic arsenic compounds" 要求含碳，
    # 否则砷单质、砷化镓、砷酸这些无机砷会被它一并收走。
    # 用 row[["..."]] 取值：老版本注册表没有这一列时返回 NULL，直接跳过。
    req <- row[["require_condition"]]
    if (length(req) == 1 && !is.na(req) && nzchar(req)) {
      req_els <- strsplit(req, ";", fixed = TRUE)[[1]]
      if (!any(tolower(input_els) %in% tolower(req_els))) next
    }

    # ---- 置信度护栏 ----
    # 元素层做的是存在性检验（"含不含这个元素"）。这对金属族成立——镉化合物
    # 必然含镉，这是镉的定义。但它只是**必要条件**，不足以单独定论；下面两条
    # 负责把"元素命中但说明不了什么"的情形压到 manual_review。
    # 注：common_els 由 intersect(tolower(...), tolower(...)) 得来，恒为小写。

    # 护栏一：特征元素落在骨架/通用元素里。C/H/O/N/S/P、卤素、钠钾钙镁或硅
    # 被当成特征元素时，"含该元素"几乎不携带信息（含碳 ≈ 这是有机物）。
    skel_hit <- common_els[common_els %in% .skeletal_elements]
    if (length(skel_hit) > 0 && identical(conf, "auto_confirmed")) {
      conf <- "manual_review"
      evidence <- paste0(evidence, "; 特征元素 ", paste(skel_hit, collapse = ", "),
                         " 属骨架或通用元素，不足以独立确认该组，需人工确认")
    }

    # 护栏二：含碳骨架 + 高关注金属，可能是有机金属/络合物，不能按无机盐处理。
    #
    # 这里必须**只看碳**。此前用的是 c("C", "H", "O", "N") 配 any()，而砷酸
    # (O[As](=O)(O)O)、硫酸镉、氧化镍全都含 O 或 H，于是所有含氧或含氢的金属
    # 化合物一律被降级 —— 全库只有砷化镓这类不含 O/H 的侥幸留下，组条目等于
    # 废掉。碳才是有机骨架与无机盐的分界。
    if (identical(conf, "auto_confirmed") &&
        any(toupper(input_els) == "C") &&
        any(common_els %in% .organic_metal_guard)) {
      conf <- "manual_review"
      evidence <- paste0(evidence, "; 输入含碳骨架 + 金属元素，可能为有机金属/络合物，需人工确认")
    }

    hits[[length(hits) + 1]] <- data.frame(
      matched_agent = row$agent,
      iarc_group = row$group_classification,
      layer = row$layer,
      element_hits = paste(common_els, collapse = ";"),
      evidence = evidence,
      confidence = conf,
      source_detail = identity$source_note,
      stringsAsFactors = FALSE
    )
  }

  if (length(hits) == 0) {
    out <- data.frame(
      matched_agent = character(0),
      iarc_group = character(0),
      layer = character(0),
      element_hits = character(0),
      evidence = character(0),
      confidence = character(0),
      source_detail = character(0),
      stringsAsFactors = FALSE
    )
  } else {
    out <- do.call(rbind, hits)
    row.names(out) <- NULL
  }

  out
}

# =============================================================================
# 扩展：CMR / SVHC UVCB 与复杂物质判定
#
# IARC 组条目靠"元素"定义（如 Cadmium and cadmium compounds），
# 而 CMR raw / SVHC raw 中有大量 UVCB / 反应质 / 聚合物 / 异构体混合物
# 等"非单一物质"条目，它们靠"母体结构关键词"定义（如 nonylphenol ethoxylated、
# chlorinated paraffin、siloxane 等）。
#
# 核心策略与 IARC 一致：先粗筛（关键词反查），再精判（匹配质量评估）。
# =============================================================================

#' UVCB / 复杂物质特征词表（用于从名称识别复杂物质）
#' @keywords internal
#' @export
#' @encoding UTF-8
UVCB_INDICATOR_WORDS <- c(
  "UVCB", "reaction mass", "polymer", "oligomer", "homopolymer", "copolymer",
  "branched and linear", "branched", "linear",
  "alkyl", "ethoxylated", "propoxylated",
  "distillate", "fraction", "extract", "residue", "pitch", "sludge",
  "mixture", "composition", "complex combination", "preparation",
  "fatty acids", "alcohols", "paraffin", "wax", "rosin", "resin",
  "siloxane", "silane", "silicone",
  "fluoropolymer", "perfluoro",
  "polyether", "polyol", "polyamine", "polyamide", "polyester",
  "polyurethane", "polyethylene", "polypropylene", "polystyrene", "polyvinyl",
  "anthracene oil", "coal tar", "creosote"
)

#' 母体结构关键词表（用于匹配输入物质与 UVCB 组）
#' @keywords internal
#' @export
#' @encoding UTF-8
BACKBONE_KEYWORDS <- c(
  "phthalate", "phenol", "nonylphenol", "octylphenol", "heptylphenol",
  "paraffin", "chlorinated paraffin", "MCCP", "SCCP",
  "siloxane", "silane", "silicone",
  "formaldehyde",
  "anthracene", "coal tar", "naphthalene", "creosote",
  "benzene", "toluene", "xylene",
  "rosin", "resin", "wax", "fatty acid", "alcohol",
  "polyether", "polyol", "polyamine", "polyamide",
  "polyester", "polyurethane", "polyethylene",
  "polypropylene", "polystyrene", "polyvinyl",
  "perfluoro", "fluoropolymer",
  "chromium", "cobalt", "nickel", "lead", "cadmium", "arsenic",
  "D4", "D5", "D6",   # 环硅氧烷简称
  "isocyanate", "amine", "epoxy", "acrylate", "methacrylate"
)

#' 从名称提取母体关键词
#'
#' 将物质名称与 BACKBONE_KEYWORDS 做整词/子串匹配，返回命中的关键词向量。
#'
#' @param name 物质名称字符串
#' @return 字符向量：命中的母体关键词（去重）
#' @keywords internal
#' @export
#' @encoding UTF-8
extract_backbone_keywords <- function(name) {
  if (is.null(name) || is.na(name) || !nzchar(name)) return(character(0))
  .backbones <- BACKBONE_KEYWORDS
  hits <- character(0)
  for (kw in .backbones) {
    # 整词或边界匹配（避免 "benzene" 误匹配到 "benzenedicarboxylic" 中的子串？
    # 实际上子串匹配对 UVCB 更有用，如 "nonylphenol" 应匹配 "nonylphenol ethoxylated"
    # 所以用 ignore.case 子串匹配
    if (grepl(kw, name, ignore.case = TRUE)) {
      hits <- c(hits, kw)
    }
  }
  unique(hits)
}

#' 从名称推断 UVCB 类别标签
#'
#' 根据名称中的特征词，给出一个或多个类别标签：
#' uvcb / reaction_mass / polymer / oligomer / isomer_mixture / distillate / mixture
#'
#' @param name 物质名称
#' @return 字符向量：类别标签（去重）
#' @keywords internal
#' @export
#' @encoding UTF-8
categorize_uvcb <- function(name) {
  if (is.null(name) || is.na(name) || !nzchar(name)) return(character(0))
  cats <- character(0)
  s <- tolower(name)

  if (grepl("uvcb", s)) cats <- c(cats, "uvcb")
  if (grepl("reaction mass", s)) cats <- c(cats, "reaction_mass")
  if (grepl("polymer", s)) cats <- c(cats, "polymer")
  if (grepl("oligomer", s)) cats <- c(cats, "oligomer")
  if (grepl("branched and linear|branched|linear|isomer", s)) {
    cats <- c(cats, "isomer_mixture")
  }
  if (grepl("distillate|fraction|extract|residue|pitch|sludge", s)) {
    cats <- c(cats, "distillate")
  }
  if (grepl("mixture|composition|complex combination|preparation", s)) {
    cats <- c(cats, "mixture")
  }
  if (grepl("ethoxylated|propoxylated", s)) cats <- c(cats, "derivative_mixture")
  if (grepl("alkyl|C[0-9]+[-–][0-9]+", s)) cats <- c(cats, "homologue_mixture")

  if (length(cats) == 0) cats <- "unspecified_complex"
  unique(cats)
}

#' 判断名称是否为 UVCB / 复杂物质
#'
#' @param name 物质名称
#' @return 逻辑值
#' @keywords internal
#' @export
#' @encoding UTF-8
is_uvcb_name <- function(name) {
  if (is.null(name) || is.na(name) || !nzchar(name)) return(FALSE)
  .indicators <- UVCB_INDICATOR_WORDS
  any(vapply(.indicators, function(kw) grepl(kw, name, ignore.case = TRUE),
             logical(1), USE.NAMES = FALSE))
}

#' 构建 CMR raw UVCB 注册表
#'
#' 从 cmr_raw 表中筛选所有 UVCB / 复杂物质 / 组条目，
#' 提取母体关键词和类别标签，用于后续反查匹配。
#'
#' @param db_path 数据库路径
#' @return data.frame
#' @keywords internal
#' @export
#' @encoding UTF-8
query_cmr_uvcb_registry <- function(db_path = NULL) {
  db <- get_db_connection(db_path)
  on.exit(DBI::dbDisconnect(db), add = TRUE)

  sql <- paste0(
    "SELECT id, international_chemical_identification AS name, ",
    "cas_no, ec_no, hazard_class_and_category_codes, ",
    "notes, Formula, SMILES, InChIKey ",
    "FROM cmr_raw ORDER BY name"
  )
  raw <- DBI::dbGetQuery(db, sql)

  # 筛 UVCB / 复杂物质
  is_uvcb <- vapply(raw$name, is_uvcb_name, logical(1), USE.NAMES = FALSE)
  reg <- raw[is_uvcb, , drop = FALSE]
  if (nrow(reg) == 0) {
    return(data.frame(
      source_db = character(0), entry_id = character(0),
      name = character(0), cas_no = character(0),
      category = character(0), keywords = character(0),
      hazard_codes = character(0), notes = character(0),
      stringsAsFactors = FALSE
    ))
  }

  reg$source_db <- "cmr"
  reg$entry_id <- paste0("cmr_", reg$id)
  reg$category <- vapply(reg$name, function(n) {
    paste(categorize_uvcb(n), collapse = ";")
  }, character(1), USE.NAMES = FALSE)
  reg$keywords <- vapply(reg$name, function(n) {
    els <- extract_backbone_keywords(n)
    if (length(els) == 0) return(NA_character_)
    paste(els, collapse = ";")
  }, character(1), USE.NAMES = FALSE)
  reg$hazard_codes <- ifelse(is.na(reg$hazard_class_and_category_codes),
                              NA_character_, reg$hazard_class_and_category_codes)

  reg[, c("source_db", "entry_id", "name", "cas_no", "category",
          "keywords", "hazard_codes", "notes")]
}

#' 构建 SVHC raw UVCB 注册表
#'
#' 从 svhc_raw 表中筛选 UVCB / 复杂物质条目。
#'
#' @param db_path 数据库路径
#' @return data.frame
#' @keywords internal
#' @export
#' @encoding UTF-8
query_svhc_uvcb_registry <- function(db_path = NULL) {
  db <- get_db_connection(db_path)
  on.exit(DBI::dbDisconnect(db), add = TRUE)

  sql <- paste0(
    "SELECT id, substance_name AS name, cas_no, ec_no, ",
    "reason_for_inclusion, description, remarks, ",
    "Formula, SMILES, InChIKey ",
    "FROM svhc_raw ORDER BY name"
  )
  raw <- DBI::dbGetQuery(db, sql)

  # 名称 + description 联合判断
  is_uvcb <- vapply(seq_len(nrow(raw)), function(i) {
    s <- paste(raw$name[i], raw$description[i], sep = " ")
    is_uvcb_name(s)
  }, logical(1), USE.NAMES = FALSE)

  reg <- raw[is_uvcb, , drop = FALSE]
  if (nrow(reg) == 0) {
    return(data.frame(
      source_db = character(0), entry_id = character(0),
      name = character(0), cas_no = character(0),
      category = character(0), keywords = character(0),
      reason = character(0), notes = character(0),
      stringsAsFactors = FALSE
    ))
  }

  reg$source_db <- "svhc"
  reg$entry_id <- paste0("svhc_", reg$id)
  reg$category <- vapply(reg$name, function(n) {
    paste(categorize_uvcb(n), collapse = ";")
  }, character(1), USE.NAMES = FALSE)
  reg$keywords <- vapply(reg$name, function(n) {
    els <- extract_backbone_keywords(n)
    if (length(els) == 0) return(NA_character_)
    paste(els, collapse = ";")
  }, character(1), USE.NAMES = FALSE)
  reg$reason <- ifelse(is.na(reg$reason_for_inclusion),
                        NA_character_, reg$reason_for_inclusion)

  reg$notes <- ifelse(is.na(reg$remarks), NA_character_, reg$remarks)

  reg[, c("source_db", "entry_id", "name", "cas_no", "category",
          "keywords", "reason", "notes")]
}

#' 从输入物质提取可匹配关键词
#'
#' 优先使用名称，其次从 SMILES 推断（简单子结构规则）。
#'
#' @param identity normalize_input_identity() 返回的列表
#' @param db_path 数据库路径
#' @return 字符向量：输入物质的关键词
#' @keywords internal
#' @export
#' @encoding UTF-8
extract_input_keywords <- function(identity, db_path = NULL) {
  keywords <- character(0)

  # 1) 如果有 inchikey，尝试从 chemicals 表或业务表反查名称
  if (!is.na(identity$inchikey) && nzchar(identity$inchikey)) {
    db <- tryCatch(get_db_connection(db_path), error = function(e) NULL)
    if (!is.null(db)) {
      on.exit(DBI::dbDisconnect(db), add = TRUE)

      # 查 chemicals 表 IUPACName（表可能不存在，包在 tryCatch）
      tryCatch({
        sql <- "SELECT IUPACName FROM chemicals WHERE InChIKey = ? LIMIT 1"
        res <- DBI::dbGetQuery(db, sql, params = list(identity$inchikey))
        if (nrow(res) > 0 && !is.na(res$IUPACName[1]) && nzchar(res$IUPACName[1])) {
          keywords <- c(keywords, extract_backbone_keywords(res$IUPACName[1]))
        }
      }, error = function(e) NULL)

      tryCatch({
        sql <- "SELECT substance_name FROM svhc WHERE InChIKey = ? LIMIT 1"
        res <- DBI::dbGetQuery(db, sql, params = list(identity$inchikey))
        if (nrow(res) > 0 && !is.na(res$substance_name[1]) && nzchar(res$substance_name[1])) {
          keywords <- c(keywords, extract_backbone_keywords(res$substance_name[1]))
        }
      }, error = function(e) NULL)

      tryCatch({
        sql <- "SELECT international_chemical_identification FROM cmr WHERE InChIKey = ? LIMIT 1"
        res <- DBI::dbGetQuery(db, sql, params = list(identity$inchikey))
        if (nrow(res) > 0 && !is.na(res[[1]][1]) && nzchar(res[[1]][1])) {
          keywords <- c(keywords, extract_backbone_keywords(res[[1]][1]))
        }
      }, error = function(e) NULL)
    }
  }

  # 2) 从 SMILES 推断简单母体（仅当没有名称关键词时）
  if (length(keywords) == 0 && !is.na(identity$smiles) && nzchar(identity$smiles)) {
    smi <- tolower(identity$smiles)
    # 简单启发式：含苯环 + 羟基 → phenol；含 Si-O-Si → siloxane 等
    if (grepl("c1ccccc1", smi) && grepl("o", smi)) {
      keywords <- c(keywords, "phenol")
    }
    if (grepl("\\[si\\]", smi) || grepl("si", smi)) {
      keywords <- c(keywords, "siloxane")
    }
    if (grepl("c1ccccc1", smi) && grepl("c\\(=o\\)o", smi)) {
      keywords <- c(keywords, "phthalate")
    }
    if (grepl("c1cccc2ccccc12", smi)) {
      keywords <- c(keywords, "naphthalene")
    }
  }

  unique(keywords)
}

#' UVCB / 复杂物质关键词匹配判定
#'
#' 输入物质的关键词与 UVCB 注册表关键词求交集，返回候选命中列表。
#'
#' @param identity normalize_input_identity() 返回的列表
#' @param registry query_cmr_uvcb_registry() 或 query_svhc_uvcb_registry() 返回的 data.frame
#' @param source_db 字符串，标识数据来源（"cmr" / "svhc"）
#' @return data.frame（长表）
#' @keywords internal
#' @export
#' @encoding UTF-8
screen_uvcb_groups <- function(identity, registry, source_db) {
  if (nrow(registry) == 0) {
    return(data.frame(
      matched_entry = character(0), source_db = character(0),
      name = character(0), category = character(0),
      keyword_hits = character(0), evidence = character(0),
      confidence = character(0), source_detail = character(0),
      stringsAsFactors = FALSE
    ))
  }

  # 提取输入关键词
  input_kws <- extract_input_keywords(identity, db_path = NULL)

  # 若无法提取任何关键词，尝试从输入字符串本身直接提取（用户可能直接输入了名称）
  if (length(input_kws) == 0) {
    # 原始输入可能是名称
    raw_input <- attr(identity, "raw_input")
    if (!is.null(raw_input) && nzchar(raw_input)) {
      input_kws <- extract_backbone_keywords(raw_input)
    }
  }

  if (length(input_kws) == 0) {
    # 完全没有关键词，无法做 UVCB 匹配
    return(data.frame(
      matched_entry = character(0), source_db = character(0),
      name = character(0), category = character(0),
      keyword_hits = character(0), evidence = character(0),
      confidence = character(0), source_detail = character(0),
      stringsAsFactors = FALSE
    ))
  }

  hits <- list()
  for (i in seq_len(nrow(registry))) {
    row <- registry[i, ]
    if (is.na(row$keywords) || !nzchar(row$keywords)) next

    reg_kws <- strsplit(row$keywords, ";", fixed = TRUE)[[1]]
    common <- intersect(tolower(input_kws), tolower(reg_kws))
    if (length(common) == 0) next

    # 置信度：关键词命中即 probable（因为 UVCB 本身就是模糊类别）
    conf <- "probable"
    evidence <- paste0("母体关键词命中: ", paste(common, collapse = ", "))

    # 若输入和注册表条目的名称高度相似（如都含 "nonylphenol ethoxylated"），提升为 auto_confirmed
    if (!is.na(identity$inchikey) && nzchar(identity$inchikey)) {
      # 有结构信息时，匹配更可靠
      # 但仍保持 probable，因为 UVCB 是类别
    }

    hits[[length(hits) + 1]] <- data.frame(
      matched_entry = row$entry_id,
      source_db = row$source_db,
      name = row$name,
      category = row$category,
      keyword_hits = paste(common, collapse = ";"),
      evidence = evidence,
      confidence = conf,
      source_detail = identity$source_note,
      stringsAsFactors = FALSE
    )
  }

  if (length(hits) == 0) {
    out <- data.frame(
      matched_entry = character(0), source_db = character(0),
      name = character(0), category = character(0),
      keyword_hits = character(0), evidence = character(0),
      confidence = character(0), source_detail = character(0),
      stringsAsFactors = FALSE
    )
  } else {
    out <- do.call(rbind, hits)
    row.names(out) <- NULL
  }

  out
}

# ---- 主函数 ----

#' 判定物质是否属于数据库中的组别或 UVCB 复杂物质条目
#'
#' 当检测出一种物质 A（CAS / InChIKey / SMILES / 分子式 / 名称任一），
#' 判断它属于哪些"组条目"或"UVCB 复杂物质条目"。
#'
#' 支持多数据源：
#' - **iarc**：IARC "X and X compounds" 式组条目（靠元素匹配）
#' - **cmr**：CMR raw 中 UVCB / 反应质 / 聚合物 / 混合物等复杂物质（靠关键词匹配）
#' - **svhc**：SVHC raw 中 UVCB / 多 CAS 合并 / 异构体混合物等（靠关键词匹配）
#' - **all**：以上全部
#'
#' 对于 IARC 组，沿用元素层粗筛 + 价态启发式 + 场景层标人工的策略。
#' 对于 CMR / SVHC 的 UVCB 类，采用"母体关键词反查"：从输入物质提取
#' 结构关键词（如 phenol / paraffin / siloxane），反查注册表中含该关键词的
#' UVCB 条目，返回 probable 级别命中（UVCB 本身就是模糊类别，不武断确认）。
#'
#' @param input 字符标量：CAS 号、InChIKey、SMILES、分子式或物质名称。
#'   自动识别输入形态。注意：本函数面向**单一物质**判定；若输入是
#'   data.frame（一批物质整表筛查），请用 [assign_group_membership_table()]。
#' @param source 字符向量，指定要查询的数据源。默认 `"all"`，可选
#'   `"iarc"`、`"cmr"`、`"svhc"` 或它们的组合。
#' @param online 逻辑值。当输入为名称或 CAS 在本地查不到时，是否联网
#'   PubChem 查询（默认 FALSE，离线优先）。
#' @param db_path 数据库路径。NULL 时使用包内默认数据库。
#' @return data.frame（长表），每行一个命中条目。列因数据源略有不同：
#'   - 通用列：`source_db`（来源库）、`name`（条目名称）、`confidence`、
#'     `evidence`、`source_detail`
#'   - IARC 特有：`iarc_group`（1/2A/2B/3）、`layer`（element/valence/form/scenario）、
#'     `element_hits`
#'   - CMR/SVHC 特有：`category`（uvcb/polymer/mixture 等）、`keyword_hits`
#'   若无命中，返回 0 行 data.frame。
#' @export
#' @examples
#' \dontrun{
#' # IARC group lookup (default)
#' assign_group_membership("7440-43-9", source = "iarc")
#'
#' # Extend to SVHC UVCB
#' assign_group_membership("nonylphenol", source = "svhc", online = TRUE)
#'
#' # Query across all databases
#' assign_group_membership("CdCl2", source = "all")
#' }
#' @export
#' @encoding UTF-8
assign_group_membership <- function(input, source = "all",
                                     online = FALSE, db_path = NULL) {
  # 参数校验
  valid_sources <- c("iarc", "cmr", "svhc", "all")
  if (!all(source %in% valid_sources)) {
    stop("source 必须是 'iarc'、'cmr'、'svhc'、'all' 的组合")
  }
  if ("all" %in% source) source <- c("iarc", "cmr", "svhc")

  # P1-②：本函数面向单一物质判定；整表批量筛查请用 table 版
  if (is.data.frame(input)) {
    stop("assign_group_membership() 面向单一物质判定：input 应为单个 ",
         "CAS / InChIKey / SMILES / 分子式字符。整表批量筛查请用 ",
         "assign_group_membership_table()", call. = FALSE)
  }

  # 1. 归一输入
  identity <- normalize_input_identity(input, online = online, db_path = db_path)
  attr(identity, "raw_input") <- input  # 保留原始输入，供 UVCB 名称匹配 fallback

  .screen_identity(identity, source, db_path = db_path)
}

#' 合并若干 data.frame，列集不一致时按并集补齐
#'
#' `do.call(rbind, ...)` 要求各块列名完全一致，而 IARC 分支产
#' `matched_agent / layer / element_hits`、CMR 与 SVHC 分支产
#' `matched_entry / category / keyword_hits`。同一批物质里只要同时命中两类，
#' 整批就会以 \code{names do not match previous names} 失败——命中越多越容易
#' 触发，小样本反而测不出来。这里按列名并集补齐后再合并。
#'
#' @param parts data.frame 组成的 list，空块自动丢弃
#' @return 合并后的 data.frame；全部为空时返回 NULL
#' @noRd
.rbind_fill <- function(parts) {
  parts <- Filter(function(d) !is.null(d) && nrow(d) > 0, parts)
  if (!length(parts)) return(NULL)
  all_cols <- unique(unlist(lapply(parts, names)))
  parts <- lapply(parts, function(d) {
    for (miss in setdiff(all_cols, names(d))) d[[miss]] <- NA
    d[, all_cols, drop = FALSE]
  })
  out <- do.call(rbind, parts)
  row.names(out) <- NULL
  out
}

#' 对单个已归一身份执行 IARC / CMR / SVHC 组条目筛查（内部）
#'
#' assign_group_membership() 与 assign_group_membership_table() 共用的
#' 单物质筛查体。\code{regs} 提供已缓存的 registry（批量场景逐行复用，
#' 避免每行重复查库）；为 NULL 时按需查询。
#' @noRd
.screen_identity <- function(identity, source, db_path = NULL, regs = NULL) {
  results <- list()

  # 2. IARC 组判定
  if ("iarc" %in% source) {
    reg_iarc <- if (!is.null(regs) && !is.null(regs$iarc)) regs$iarc
      else query_iarc_group_registry(db_path = db_path)
    res_iarc <- screen_iarc_groups(identity, reg_iarc)
    if (nrow(res_iarc) > 0) {
      # 统一列名。IARC 分支的条目名列叫 matched_agent（保留不动，有调用方依赖），
      # 同时把它复制到通用的 matched_entry / name，这样整张长表里"条目叫什么"
      # 只有一个取法，不用按来源分支。
      res_iarc$source_db <- "iarc"
      res_iarc$matched_entry <- res_iarc$matched_agent
      res_iarc$name <- res_iarc$matched_agent
      res_iarc$category <- NA_character_
      res_iarc$keyword_hits <- NA_character_
      results[[length(results) + 1]] <- res_iarc
    }
  }

  # 3. CMR UVCB 判定
  if ("cmr" %in% source) {
    reg_cmr <- if (!is.null(regs) && !is.null(regs$cmr)) regs$cmr
      else query_cmr_uvcb_registry(db_path = db_path)
    res_cmr <- screen_uvcb_groups(identity, reg_cmr, "cmr")
    if (nrow(res_cmr) > 0) {
      # 对齐 IARC 输出列
      res_cmr$iarc_group <- NA_character_
      res_cmr$layer <- NA_character_
      res_cmr$element_hits <- NA_character_
      results[[length(results) + 1]] <- res_cmr
    }
  }

  # 4. SVHC UVCB 判定
  if ("svhc" %in% source) {
    reg_svhc <- if (!is.null(regs) && !is.null(regs$svhc)) regs$svhc
      else query_svhc_uvcb_registry(db_path = db_path)
    res_svhc <- screen_uvcb_groups(identity, reg_svhc, "svhc")
    if (nrow(res_svhc) > 0) {
      res_svhc$iarc_group <- NA_character_
      res_svhc$layer <- NA_character_
      res_svhc$element_hits <- NA_character_
      results[[length(results) + 1]] <- res_svhc
    }
  }

  # 5. 合并结果
  if (length(results) == 0) {
    out <- data.frame(
      source_db = character(0), matched_entry = character(0),
      name = character(0), iarc_group = character(0),
      layer = character(0), category = character(0),
      element_hits = character(0), keyword_hits = character(0),
      evidence = character(0), confidence = character(0),
      source_detail = character(0), stringsAsFactors = FALSE
    )
  } else {
    out <- .rbind_fill(results)
  }

  # 6. 附加元信息
  attr(out, "input") <- attr(identity, "raw_input")
  attr(out, "input_type") <- identity$input_type
  attr(out, "input_elements") <- identity$elements
  attr(out, "note") <- ifelse(nrow(out) == 0,
                               "未命中任何组条目或 UVCB 复杂物质",
                               paste0("命中 ", nrow(out), " 个条目"))

  out
}

#' 整表批量筛查：判断 data.frame 中每行物质属于哪些法规"组条目"
#'
#' assign_group_membership() 的批量版。适合"一次筛查检出的一批物质"
#' （如 LIMS 导出的 100 行结果表）。每行取标识的优先级为
#' InChIKey → CAS → SMILES → Formula → NAME（NAME 仅在 online=TRUE 时
#' 尝试，其余形态离线可判）。本地查不到的 CAS/InChIKey 会自动落到
#' SMILES / Formula 离线解析；整行都无法识别时该行不中断，记入返回
#' 对象的 attr(out, "errors")。
#'
#' 注意：与 assign_toxicity() 的"精确 InChIKey 命中"互补——本函数抓的
#' 是"镉及镉化合物 / 壬基酚族 / 氯化石蜡"这类没有单一 InChIKey 的
#' 一组物质条目，精确匹配会系统性漏掉它们。
#'
#' @param data data.frame，每行一种物质。需包含以下至少一列：
#'   InChIKey / CAS / SMILES / Formula / NAME。
#' @param source 要筛查的数据源，'iarc'、'cmr'、'svhc' 或 'all'（默认）。
#' @param online 是否允许联网 PubChem 查询名称（默认 FALSE，离线优先）。
#' @param db_path 数据库路径。NULL 时使用包内默认数据库。
#' @return data.frame（长表），每行一个命中条目。含前导列
#'   \code{input_index}（对应 data 输入行号，便于 merge 回原表）与
#'   \code{input}（该行实际采用的标识）。失败行不产出行，记录在
#'   attr(out, "errors")。
#' @export
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   NAME    = c("Cadmium chloride", "Benzene", "Nonylphenol"),
#'   CAS     = c("10108-64-2", "71-43-2", "25154-52-3"),
#'   SMILES  = c("[Cl-].[Cl-].[Cd+2]", "c1ccccc1", NA),
#'   stringsAsFactors = FALSE
#' )
#' hits <- assign_group_membership_table(data, source = "all")
#' }
#' @export
#' @encoding UTF-8
assign_group_membership_table <- function(data, source = "all",
                                          online = FALSE, db_path = NULL) {
  valid_sources <- c("iarc", "cmr", "svhc", "all")
  if (!all(source %in% valid_sources)) {
    stop("source 必须是 'iarc'、'cmr'、'svhc'、'all' 的组合")
  }
  if ("all" %in% source) source <- c("iarc", "cmr", "svhc")

  if (!is.data.frame(data)) {
    stop("data 必须为 data.frame（每行一种物质）", call. = FALSE)
  }
  cand_cols <- intersect(c("InChIKey", "CAS", "SMILES", "Formula", "NAME"),
                         names(data))
  if (length(cand_cols) == 0) {
    stop("data 需包含 InChIKey / CAS / SMILES / Formula / NAME 中至少一列",
         call. = FALSE)
  }

  # registry 只查一次，供全表逐行复用（性能）
  regs <- list()
  if ("iarc" %in% source) regs$iarc <- query_iarc_group_registry(db_path = db_path)
  if ("cmr"  %in% source) regs$cmr  <- query_cmr_uvcb_registry(db_path = db_path)
  if ("svhc" %in% source) regs$svhc <- query_svhc_uvcb_registry(db_path = db_path)

  priority <- c("InChIKey", "CAS", "SMILES", "Formula")
  if (online) priority <- c(priority, "NAME")

  results <- list()
  errors <- list()

  for (i in seq_len(nrow(data))) {
    row <- data[i, , drop = FALSE]
    identity <- NULL
    ident_chosen <- NA_character_

    # 逐候选形态解析；本地未收录/无法识别则换下一候选
    for (col in priority) {
      if (!col %in% names(row)) next
      v <- row[[col]]
      if (length(v) != 1 || is.na(v) ||
          !nzchar(trimws(as.character(v)))) next
      val <- trimws(as.character(v))
      got <- tryCatch(
        normalize_input_identity(val, online = online, db_path = db_path),
        error = function(e) NULL
      )
      if (!is.null(got)) {
        identity <- got
        ident_chosen <- val
        break
      }
    }

    if (is.null(identity)) {
      errors[[length(errors) + 1]] <- data.frame(
        input_index = i,
        identifier = NA_character_,
        error = "行内无可用标识（所有候选均无法识别或未收录）",
        stringsAsFactors = FALSE
      )
      next
    }

    # raw_input：该行 NAME 文本（供 UVCB 名称 fallback），否则用所选标识
    name_txt <- ident_chosen
    if ("NAME" %in% names(row) && length(row$NAME) == 1 &&
        !is.na(row$NAME) && nzchar(trimws(as.character(row$NAME)))) {
      name_txt <- trimws(as.character(row$NAME))
    }
    attr(identity, "raw_input") <- name_txt

    hits <- tryCatch(
      .screen_identity(identity, source, db_path = db_path, regs = regs),
      error = function(e) {
        errors[[length(errors) + 1]] <<- data.frame(
          input_index = i,
          identifier = ident_chosen,
          error = conditionMessage(e),
          stringsAsFactors = FALSE
        )
        NULL
      }
    )
    if (!is.null(hits) && nrow(hits) > 0) {
      hits$input_index <- i
      hits$input <- ident_chosen
      results[[length(results) + 1]] <- hits
    }
  }

  empty_cols <- c(
    "source_db", "matched_entry", "name", "iarc_group", "layer",
    "category", "element_hits", "keyword_hits", "evidence", "confidence",
    "source_detail", "input_index", "input"
  )
  if (length(results) == 0) {
    out <- data.frame(
      source_db = character(0), matched_entry = character(0),
      name = character(0), iarc_group = character(0),
      layer = character(0), category = character(0),
      element_hits = character(0), keyword_hits = character(0),
      evidence = character(0), confidence = character(0),
      source_detail = character(0), input_index = integer(0),
      input = character(0), stringsAsFactors = FALSE
    )
  } else {
    out <- .rbind_fill(results)
  }

  err_df <- if (length(errors) > 0) {
    do.call(rbind, errors)
  } else {
    data.frame(input_index = integer(0), identifier = character(0),
               error = character(0), stringsAsFactors = FALSE)
  }
  attr(out, "errors") <- err_df
  if (nrow(err_df) > 0) {
    message("assign_group_membership_table: ", nrow(err_df),
            " 行无法处理（详情见 attr(result, 'errors')）：",
            paste(err_df$input_index, collapse = ", "))
  }
  attr(out, "note") <- paste0("命中 ", nrow(out), " 个条目（共 ",
                              nrow(data), " 行输入）")
  out
}
