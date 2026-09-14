# =============================================================================
# IARC 组条目注册表查询 + (see X) 交叉引用解析（自洽精简版）
#
# 2026-09-15 精简：group_membership.R 的组条目判定引擎（元素判定/UVCB/
# screen_* 主逻辑，默认关闭的可选功能）已删除；本文件保留其中被
# assign_toxicity() 无条件使用的部分：iarc 注册表读取 + (see X) 别名解析。
# 只填空缺、绝不覆盖——理由见 docs/adr/0010。
# =============================================================================

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
