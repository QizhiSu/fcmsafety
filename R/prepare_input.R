# =============================================================================
# 输入准备（prepare_input）：把"名称 + SMILES"补全成可直接进入筛查流程的表
#
# 存在的理由（2026-09-10 卡点 3）：
#   assign_toxicity() 以 InChIKey 为匹配主键（法规库各业务表只有 InChIKey，
#   没有任何 SMILES 列），而 Toxtree 又需要 SMILES。用户手上最常见的只有
#   "物质名 + 结构式"，缺 InChIKey 就整条流程跑不动。
#
# 本模块把缺失的那一步自动化，且**默认完全离线**：
#   SMILES --(CDK InChI 模块, 本地)--> InChIKey --(库内一次查询)--> CID/分子式/精确质量
#
# 离线可行性依据（2026-09-10 实测）：
#   - rcdk 3.8.2 自身不导出 InChI 函数，但 rcdklibs 的 cont/ 目录带着
#     cdk-inchi-2.9.jar + jna-inchi-win32-x86-64，可直接用 rJava 调。
#   - 拿 chemicals 表真实数据自校验：重算 InChIKey 与库内存储值
#     80/80、500/500 完全一致，0 不一致 0 失败，27 ms/行。
#
# 立体化学（2026-09-10 实测，务必留意）：
#   - SMILES 归一化必须用 Absolute flavor。用 Canonical 会把 L-丙氨酸、
#     D-丙氨酸、无立体丙氨酸归一成同一个字符串，导致误配。
#   - 输入 SMILES 若不带立体信息，算出的 InChIKey 第二段为 UHFFFAOYSA，
#     与库中带立体信息的完整键对不上。此时按 InChIKey 前 14 位（骨架）
#     降级匹配：实测 2446 条中仅 20 个骨架（0.83%）对应多个立体异构体，
#     故唯一命中时采用，多命中时标记歧义交人工判断。
#
# 设计原则：单行失败不中断整表；每一行都记录 identity_source（这行的
# InChIKey 从哪来），未解决的行单独列在报告里，绝不静默变成 NA。
# =============================================================================

# 模块级缓存：避免同一进程内重复初始化 Java/工厂、重复算同一个 SMILES
.fcm_env <- new.env(parent = emptyenv())

# ---- 列名识别 ------------------------------------------------------------

# 常见列名变体（含中英文）。先精确匹配，再忽略大小写与空白的"超归一"匹配。
.fcm_col_candidates <- list(
  name = c("NAME", "Name", "name", "Chemical Name", "Substance name",
           "Substance Name", "物质名", "物质名称", "名称", "化合物名称",
           "化合物名", "IUPACName"),
  smiles = c("SMILES", "Smiles", "smiles", "SMILES_std", "IsomericSMILES",
             "CanonicalSMILES", "结构式", "SMILES结构式"),
  cas = c("CAS", "cas", "Cas", "CAS No", "CAS No.", "CAS号", "CAS_retrieved",
          "cas_no"),
  inchikey = c("InChIKey", "inchikey", "InchiKey", "InChI Key", "InChIKey_std")
)

# 超归一：只保留字母数字并转小写，用于容忍空格/下划线/全半角差异
.fcm_supernorm <- function(x) {
  tolower(gsub("[^[:alnum:]]", "", as.character(x)))
}

#' 定位输入表中的某一列（内部）
#'
#' 依次尝试：显式给定的列名/列序号 -> 候选名精确匹配 -> 超归一匹配。
#'
#' @param data 输入 data.frame
#' @param explicit 用户显式指定的列（列名或列序号），可为 NULL
#' @param candidates 候选列名向量
#' @param what 用于报错的列含义描述（如 "名称"）
#' @param required 逻辑值，找不到时是否报错
#' @return 命中的列名（字符）或 NULL
#' @keywords internal
#' @export
.fcm_pick_col <- function(data, explicit, candidates, what, required = TRUE) {
  nm <- names(data)

  if (!is.null(explicit)) {
    if (is.numeric(explicit)) {
      if (explicit < 1 || explicit > ncol(data)) {
        stop("指定的 ", what, " 列序号超范围: ", explicit, call. = FALSE)
      }
      return(nm[explicit])
    }
    if (explicit %in% nm) return(explicit)
    hit <- which(.fcm_supernorm(nm) == .fcm_supernorm(explicit))
    if (length(hit) > 0) return(nm[hit[1]])
    stop("指定的 ", what, " 列在输入表中不存在: ", explicit, call. = FALSE)
  }

  hit <- which(nm %in% candidates)
  if (length(hit) > 0) return(nm[hit[1]])

  hit <- which(.fcm_supernorm(nm) %in% .fcm_supernorm(candidates))
  if (length(hit) > 0) return(nm[hit[1]])

  if (required) {
    stop("输入表缺少", what, "列。已尝试的候选列名：",
         paste(candidates, collapse = " / "),
         "\n实际列名：", paste(nm, collapse = ", "),
         "\n可用 name_col / smiles_col 参数显式指定。", call. = FALSE)
  }
  NULL
}

# ---- 本地结构标识计算（CDK InChI 模块） ----------------------------------

#' 初始化 CDK 的 InChI 生成器工厂（内部）
#'
#' rcdk 未导出 InChI 接口，但 rcdklibs 带有 cdk-inchi jar，可直接经 rJava 调用。
#' 仅初始化一次，结果缓存在模块环境里。
#'
#' @return InChIGeneratorFactory 的 Java 对象引用；不可用时返回 NULL
#' @keywords internal
#' @export
.fcm_inchi_factory <- function() {
  if (!is.null(.fcm_env$inchi_factory)) return(.fcm_env$inchi_factory)
  if (isTRUE(.fcm_env$inchi_unavailable)) return(NULL)

  ok <- requireNamespace("rcdk", quietly = TRUE) &&
    requireNamespace("rJava", quietly = TRUE)
  if (!ok) {
    warning("rcdk / rJava 不可用，无法从 SMILES 本地推导 InChIKey。",
            "仅能处理已自带 InChIKey 的行。", call. = FALSE)
    .fcm_env$inchi_unavailable <- TRUE
    return(NULL)
  }

  fac <- tryCatch({
    rJava::.jinit()
    rJava::.jcall("org.openscience.cdk.inchi.InChIGeneratorFactory",
                  "Lorg/openscience/cdk/inchi/InChIGeneratorFactory;",
                  "getInstance")
  }, error = function(e) {
    warning("CDK InChI 模块初始化失败：", conditionMessage(e),
            "\n仅能处理已自带 InChIKey 的行。", call. = FALSE)
    NULL
  })

  if (is.null(fac)) .fcm_env$inchi_unavailable <- TRUE else .fcm_env$inchi_factory <- fac
  fac
}

#' 单个 SMILES -> InChIKey / 归一化 SMILES / 分子式（内部）
#'
#' 全程离线。非法 SMILES 不报错，返回全 NA 的结果并标记 ok = FALSE。
#' 结果按 SMILES 字符串缓存在模块环境里，重复 SMILES 零开销。
#'
#' @param smiles 单个 SMILES 字符串
#' @return 命名 list：ok / inchikey / smiles_canonical / formula / mass / note
#' @keywords internal
#' @export
.fcm_identify_one <- function(smiles) {
  empty <- list(ok = FALSE, inchikey = NA_character_, smiles_canonical = NA_character_,
                formula = NA_character_, mass = NA_real_, note = NA_character_)

  if (is.null(smiles) || is.na(smiles) || !nzchar(trimws(as.character(smiles)))) {
    empty$note <- "SMILES 为空"
    return(empty)
  }
  smi <- trimws(as.character(smiles))

  key <- paste0("id:", smi)
  if (!is.null(.fcm_env[[key]])) return(.fcm_env[[key]])

  fac <- .fcm_inchi_factory()
  if (is.null(fac)) {
    empty$note <- "CDK InChI 模块不可用"
    return(empty)
  }

  mol <- tryCatch({
    # 非法 SMILES 由 rcdk 以 warning 形式提示，这里自行处理失败，
    # 不需要把 warning 抛给调用方（否则每次跑都刷一屏）。
    # parse.smiles 对坏输入返回"长度 1、元素为 NULL"的列表，必须判 m[[1]]。
    m <- suppressWarnings(rcdk::parse.smiles(smi))
    if (is.null(m) || length(m) == 0 || is.null(m[[1]])) NULL else m[[1]]
  }, error = function(e) NULL)

  if (is.null(mol)) {
    empty$note <- "SMILES 无法解析"
    .fcm_env[[key]] <- empty
    return(empty)
  }

  res <- tryCatch({
    gen <- rJava::.jcall(fac, "Lorg/openscience/cdk/inchi/InChIGenerator;",
                         "getInChIGenerator", mol)
    ik <- rJava::.jcall(gen, "S", "getInchiKey")
    # 归一化 SMILES 必须用 Absolute（保留立体化学），Canonical 会丢立体导致误配
    can <- tryCatch(
      rcdk::get.smiles(mol, flavor = rcdk::smiles.flavors("Absolute")),
      error = function(e) NA_character_)
    fml <- NA_character_; mss <- NA_real_
    tryCatch({
      f <- rcdk::get.mol2formula(mol)
      fml <- as.character(f@string)
      mss <- as.numeric(f@mass)
    }, error = function(e) NULL)
    list(ok = TRUE, inchikey = as.character(ik), smiles_canonical = can,
         formula = fml, mass = mss, note = NA_character_)
  }, error = function(e) {
    empty$note <- paste0("InChI 计算失败: ", conditionMessage(e))
    empty
  })

  .fcm_env[[key]] <- res
  res
}

# ---- 库内标识解析 --------------------------------------------------------

#' 从 chemicals 表按 InChIKey 取化学元数据（内部）
#'
#' 一次 IN 查询搞定整表，不逐行查库。返回 data.frame，调用方自行 match。
#'
#' @param con 数据库连接
#' @param keys InChIKey 字符向量
#' @param by_skeleton 逻辑值，TRUE 时按前 14 位骨架匹配（降级路径）
#' @return data.frame：InChIKey / CID / Formula / SMILES / ExactMass / n_key
#' @keywords internal
#' @export
.fcm_chemicals_lookup <- function(con, keys, by_skeleton = FALSE) {
  keys <- unique(keys[!is.na(keys) & nzchar(keys)])
  if (length(keys) == 0) {
    return(data.frame(InChIKey = character(0), CID = character(0),
                      Formula = character(0), SMILES = character(0),
                      ExactMass = character(0), n_key = integer(0),
                      stringsAsFactors = FALSE))
  }

  # 精确路径比较完整键，降级路径比较前 14 位骨架——SQL 列表达式与绑定参数
  # 必须成对切换，否则精确查询会拿骨架去比完整键，永远查不到。
  probe <- if (by_skeleton) substr(keys, 1, 14) else keys
  ph <- paste(rep("?", length(probe)), collapse = ",")
  col_expr <- if (by_skeleton) "substr(InChIKey, 1, 14)" else "InChIKey"
  sql <- paste0("SELECT InChIKey, CID, Formula, SMILES, ExactMass FROM chemicals ",
                "WHERE ", col_expr, " IN (", ph, ")")

  res <- tryCatch(
    DBI::dbGetQuery(con, sql, params = as.list(probe)),
    error = function(e) {
      warning("查询 chemicals 表失败：", conditionMessage(e), call. = FALSE)
      NULL
    })
  if (is.null(res) || nrow(res) == 0) {
    return(data.frame(InChIKey = character(0), CID = character(0),
                      Formula = character(0), SMILES = character(0),
                      ExactMass = character(0), n_key = integer(0),
                      stringsAsFactors = FALSE))
  }
  res$n_key <- substr(res$InChIKey, 1, 14)
  res
}

#' 构建库内名称 -> InChIKey 对照表（内部）
#'
#' 汇总各业务表的名称列，做一次全量拉取后在 R 侧归一化比对（SQLite 的文本
#' 等值比较对大小写不敏感处理不一致，放 R 侧更可靠）。整表约 4000 行，开销可忽略。
#'
#' @param con 数据库连接
#' @return data.frame：name_norm / name_raw / InChIKey / source_table
#' @keywords internal
#' @export
.fcm_name_registry <- function(con) {
  specs <- list(
    list(tbl = "svhc",        col = "substance_name"),
    list(tbl = "cmr",         col = "international_chemical_identification"),
    list(tbl = "cmr_suspect", col = "substance_name"),
    list(tbl = "iarc",        col = "agent"),
    list(tbl = "eu_sml",      col = "substance_name"),
    list(tbl = "china_sml",   col = "substance_name"),
    list(tbl = "edc",         col = "substance_name")
  )

  out <- lapply(specs, function(sp) {
    q <- paste0("SELECT \"", sp$col, "\" AS nm, InChIKey FROM ", sp$tbl,
                " WHERE \"", sp$col, "\" IS NOT NULL AND trim(\"", sp$col, "\") != ''")
    r <- tryCatch(DBI::dbGetQuery(con, q), error = function(e) NULL)
    if (is.null(r) || nrow(r) == 0) return(NULL)
    data.frame(name_raw = as.character(r$nm), InChIKey = as.character(r$InChIKey),
               source_table = sp$tbl, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, out)
  if (is.null(out) || nrow(out) == 0) {
    out <- data.frame(name_raw = character(0), InChIKey = character(0),
                      source_table = character(0), stringsAsFactors = FALSE)
  }
  out$name_norm <- .fcm_supernorm(out$name_raw)
  out <- out[nzchar(out$name_norm) & !is.na(out$InChIKey), , drop = FALSE]
  out
}

# ---- PubChem 兜底（默认关闭） --------------------------------------------

.fcm_pubchem_base <- "https://pubchem.ncbi.nlm.nih.gov/rest/pug"

#' 按 SMILES 查 PubChem 属性（内部，联网）
#'
#' @param smiles SMILES 字符串
#' @param timeout 超时秒数
#' @return 命名 list（CID/Formula/SMILES/InChIKey/ExactMass），失败时为全 NA
#' @keywords internal
#' @export
.fcm_pubchem_by_smiles <- function(smiles, timeout = 30) {
  empty <- list(CID = NA_character_, Formula = NA_character_, SMILES = NA_character_,
                InChIKey = NA_character_, ExactMass = NA_character_)
  if (is.null(smiles) || is.na(smiles) || !nzchar(smiles)) return(empty)

  props <- "MolecularFormula,IsomericSMILES,CanonicalSMILES,InChIKey,ExactMass"
  url <- sprintf("%s/compound/smiles/%s/property/%s/JSON",
                 .fcm_pubchem_base, utils::URLencode(smiles, reserved = TRUE), props)
  tryCatch({
    r <- httr::GET(url, httr::timeout(timeout))
    if (r$status_code != 200) return(empty)
    p <- jsonlite::fromJSON(rawToChar(r$content))[["PropertyTable"]][["Properties"]]
    if (is.null(p) || length(p) == 0 || nrow(p) == 0) return(empty)
    g <- function(f) {
      v <- p[[f]]
      if (is.null(v) || length(v) == 0) return(NA_character_)
      v <- as.character(v[1])
      if (is.na(v)) NA_character_ else v
    }
    smi <- g("IsomericSMILES"); if (is.na(smi)) smi <- g("CanonicalSMILES")
    list(CID = g("CID"), Formula = g("MolecularFormula"), SMILES = smi,
         InChIKey = g("InChIKey"), ExactMass = g("ExactMass"))
  }, error = function(e) empty)
}

#' 按名称查 PubChem 属性（内部，联网）
#'
#' @param name 物质名
#' @param timeout 超时秒数
#' @return 同 .fcm_pubchem_by_smiles()
#' @keywords internal
#' @export
.fcm_pubchem_by_name <- function(name, timeout = 30) {
  empty <- list(CID = NA_character_, Formula = NA_character_, SMILES = NA_character_,
                InChIKey = NA_character_, ExactMass = NA_character_)
  if (is.null(name) || is.na(name) || !nzchar(trimws(name))) return(empty)

  cid <- tryCatch({
    url <- sprintf("%s/compound/name/%s/cids/JSON", .fcm_pubchem_base,
                   utils::URLencode(trimws(name), reserved = TRUE))
    r <- httr::GET(url, httr::timeout(timeout))
    if (r$status_code != 200) return(NA_character_)
    ids <- jsonlite::fromJSON(rawToChar(r$content))[["IdentifierList"]][["CID"]]
    if (length(ids) > 0) as.character(ids[1]) else NA_character_
  }, error = function(e) NA_character_)
  if (is.na(cid)) return(empty)

  props <- "MolecularFormula,IsomericSMILES,CanonicalSMILES,InChIKey,ExactMass"
  tryCatch({
    r <- httr::GET(sprintf("%s/compound/cid/%s/property/%s/JSON",
                           .fcm_pubchem_base, cid, props), httr::timeout(timeout))
    if (r$status_code != 200) return(empty)
    p <- jsonlite::fromJSON(rawToChar(r$content))[["PropertyTable"]][["Properties"]]
    if (is.null(p) || length(p) == 0 || nrow(p) == 0) return(empty)
    g <- function(f) {
      v <- p[[f]]
      if (is.null(v) || length(v) == 0) return(NA_character_)
      v <- as.character(v[1])
      if (is.na(v)) NA_character_ else v
    }
    smi <- g("IsomericSMILES"); if (is.na(smi)) smi <- g("CanonicalSMILES")
    list(CID = cid, Formula = g("MolecularFormula"), SMILES = smi,
         InChIKey = g("InChIKey"), ExactMass = g("ExactMass"))
  }, error = function(e) empty)
}

# ---- 主入口 --------------------------------------------------------------

#' 准备筛查输入：从"名称 + SMILES"补全结构标识
#'
#' 把用户手上的"物质名 + 结构式"表转成可直接喂给
#' \code{\link{assign_toxicity}()} / \code{\link{run_toxtree}()} 的表：
#' 补上 InChIKey、CID、分子式、精确质量，并记录每行的身份来源。
#'
#' 硬性要求两列：\strong{名称}与\strong{SMILES}（列名可用参数显式指定，
#' 常见中英文变体会自动识别）。InChIKey 与 CAS 均为可选输入——已有则直接
#' 采用不会重算。
#'
#' 解析优先级（能离线就不联网）：
#' \enumerate{
#'   \item 输入自带 InChIKey：原样采用；
#'   \item SMILES 经 CDK InChI 模块本地算 InChIKey（实测 27 ms/行，零网络）；
#'   \item 按完整 InChIKey 查本地 \code{chemicals} 表，取回 CID / 分子式 / 精确质量；
#'   \item 完整键未命中且 \code{skeleton_fallback = TRUE} 时，按前 14 位骨架
#'     再查一次；唯一命中则采用并标记，多命中标记为歧义；
#'   \item 仍未解决的，按物质名在本地各业务表中比对；
#'   \item 仍无解且 \code{online = TRUE} 时，才查 PubChem（先 SMILES 后名称）。
#' }
#'
#' 单行失败不会中断整表：未解决的行在返回值的
#' \code{attr(,"prepare_report")} 里单独列出。
#'
#' @param data 输入 data.frame，至少含名称列与 SMILES 列
#' @param name_col 名称列（列名或列序号），NULL 时自动识别
#' @param smiles_col SMILES 列（列名或列序号），NULL 时自动识别
#' @param cas_col CAS 列（可选，列名或列序号），NULL 时自动识别、找不到不报错
#' @param inchikey_col InChIKey 列（可选），NULL 时自动识别、找不到不报错
#' @param skeleton_fallback 逻辑值，完整 InChIKey 未命中时是否降级按骨架匹配
#' @param online 逻辑值，本地全部未命中时是否联网查 PubChem（默认 FALSE）
#' @param delay 联网请求间隔秒数（默认 0.35，避免触发 PubChem 限流）
#' @param db_path 可选的自定义数据库路径，默认用包内数据库
#' @param verbose 逻辑值，是否打印过程信息
#'
#' @return 输入表加上 \code{NAME} / \code{SMILES} / \code{InChIKey} / \code{CID} /
#'   \code{Formula} / \code{ExactMass} / \code{SMILES_canonical} /
#'   \code{identity_source} / \code{identity_method} / \code{identity_note} 列。
#'   \code{identity_method} 是 ASCII 机器可读码（\code{given} /
#'   \code{smiles_cdk} / \code{db_skeleton} / \code{db_skeleton_ambiguous} /
#'   \code{db_name} / \code{pubchem_smiles} / \code{pubchem_name}），
#'   便于程序判断某行的身份可靠度；\code{identity_source} 为对应的中文说明。
#'   \code{attr(,"prepare_report")} 为补全报告。
#' @export
#' @export
prepare_input <- function(data,
                          name_col = NULL, smiles_col = NULL,
                          cas_col = NULL, inchikey_col = NULL,
                          skeleton_fallback = TRUE,
                          online = FALSE, delay = 0.35,
                          db_path = NULL, verbose = TRUE) {

  if (!is.data.frame(data) || nrow(data) == 0) {
    stop("data 必须是非空 data.frame。", call. = FALSE)
  }

  say <- function(...) if (verbose) message(...)
  say("🧬 准备筛查输入（名称 + SMILES -> 结构标识）")
  say(paste(rep("-", 60), collapse = ""))

  nm_col  <- .fcm_pick_col(data, name_col, .fcm_col_candidates$name, "名称")
  smi_col <- .fcm_pick_col(data, smiles_col, .fcm_col_candidates$smiles, "SMILES")
  cas_col <- .fcm_pick_col(data, cas_col, .fcm_col_candidates$cas, "CAS", required = FALSE)
  ik_col  <- .fcm_pick_col(data, inchikey_col, .fcm_col_candidates$inchikey,
                           "InChIKey", required = FALSE)

  say("   名称列   : ", nm_col)
  say("   SMILES 列: ", smi_col)
  say("   CAS 列   : ", if (is.null(cas_col)) "（无，可选）" else cas_col)
  say("   InChIKey : ", if (is.null(ik_col)) "（无，将自动推导）" else ik_col)

  n <- nrow(data)
  name_v  <- as.character(data[[nm_col]])
  smiles_v <- as.character(data[[smi_col]])
  ik_given <- if (is.null(ik_col)) rep(NA_character_, n) else as.character(data[[ik_col]])

  inchikey <- rep(NA_character_, n)
  canon    <- rep(NA_character_, n)
  formula  <- rep(NA_character_, n)
  mass     <- rep(NA_real_, n)
  source   <- rep(NA_character_, n)
  # identity_method：与 identity_source 同义但为 ASCII 机器可读码，便于程序判断
  method   <- rep(NA_character_, n)
  note     <- rep(NA_character_, n)

  # ---- 第 1 步：输入自带的 InChIKey 直接采用 ----
  has_ik <- !is.na(ik_given) & nzchar(trimws(ik_given))
  inchikey[has_ik] <- trimws(ik_given[has_ik])
  source[has_ik] <- "输入自带"
  method[has_ik] <- "given"
  say("\n① 输入自带 InChIKey: ", sum(has_ik), " / ", n)

  # ---- 第 2 步：本地从 SMILES 推导 ----
  need <- which(!has_ik)
  if (length(need) > 0) {
    say("② 本地推导（CDK InChI）: ", length(need), " 行待处理…")
    for (i in need) {
      r <- .fcm_identify_one(smiles_v[i])
      if (isTRUE(r$ok)) {
        inchikey[i] <- r$inchikey
        canon[i]    <- r$smiles_canonical
        formula[i]  <- r$formula
        mass[i]     <- r$mass
        source[i]   <- "本地 SMILES 推导"
        method[i]   <- "smiles_cdk"
      } else {
        note[i] <- r$note
      }
    }
    say("   成功 ", sum(source == "本地 SMILES 推导", na.rm = TRUE), " 行")
  }

  # ---- 第 3 步：查本地 chemicals 表补元数据 ----
  con <- get_db_connection(db_path = db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  cid_v <- rep(NA_character_, n)
  exact <- rep(NA_real_, n)

  resolve_hit <- function(hits, idx, label) {
    if (nrow(hits) == 0) return(0L)
    n_used <- 0L
    for (i in idx) {
      k <- substr(inchikey[i], 1, 14)
      sub <- hits[hits$n_key == k, , drop = FALSE]
      if (nrow(sub) == 0) next
      if (nrow(sub) > 1) {
        note[i] <<- paste0("骨架 ", k, " 对应 ", nrow(sub),
                           " 个立体异构体，结果可能不唯一")
        source[i] <<- paste0(label, "（歧义）")
        method[i] <<- "db_skeleton_ambiguous"
      } else {
        inchikey[i] <<- sub$InChIKey[1]
        source[i] <<- label
        method[i] <<- "db_skeleton"
      }
      cid_v[i]  <<- sub$CID[1]
      formula[i] <<- if (is.na(formula[i]) || !nzchar(formula[i])) sub$Formula[1] else formula[i]
      canon[i]   <<- if (is.na(canon[i])) sub$SMILES[1] else canon[i]
      exact[i]   <<- suppressWarnings(as.numeric(sub$ExactMass[1]))
      n_used <- n_used + 1L
    }
    n_used
  }

  # 3a. 完整 InChIKey 精确命中（只处理还没拿到元数据的行）
  # 用 resolved_exact 而不是"cid 是否为空"来判断命中：库内 CID 可能本身为空，
  # 若以 cid 判定会把已精确命中的行重复丢进骨架降级，贴上错误来源标签。
  resolved_exact <- rep(FALSE, n)
  idx_ik <- which(!is.na(inchikey) & nzchar(inchikey) & !resolved_exact)
  if (length(idx_ik) > 0) {
    hits <- .fcm_chemicals_lookup(con, inchikey[idx_ik], by_skeleton = FALSE)
    if (nrow(hits) > 0) {
      for (i in idx_ik) {
        sub <- hits[hits$InChIKey == inchikey[i], , drop = FALSE]
        if (nrow(sub) == 0) next
        resolved_exact[i] <- TRUE
        cid_v[i]  <- sub$CID[1]
        exact[i]  <- suppressWarnings(as.numeric(sub$ExactMass[1]))
        if (is.na(formula[i]) || !nzchar(formula[i])) formula[i] <- sub$Formula[1]
        if (is.na(canon[i])) canon[i] <- sub$SMILES[1]
      }
    }
    say("③ 本地 chemicals 表精确命中: ", sum(resolved_exact), " / ", n)
  }

  # 3b. 骨架降级：完整键在 chemicals 里查不到时才做（且只对这些行）
  if (isTRUE(skeleton_fallback)) {
    idx_sk <- which(!is.na(inchikey) & nzchar(inchikey) & !resolved_exact)
    if (length(idx_sk) > 0) {
      hits <- .fcm_chemicals_lookup(con, inchikey[idx_sk], by_skeleton = TRUE)
      n_used <- resolve_hit(hits, idx_sk, "骨架匹配")
      if (n_used > 0) say("   骨架降级命中: ", n_used, " 行")
    }
  }

  # ---- 第 4 步：名称在本地库比对 ----
  idx_nm <- which(is.na(inchikey) & !is.na(name_v) & nzchar(name_v))
  if (length(idx_nm) > 0) {
    reg <- .fcm_name_registry(con)
    if (nrow(reg) > 0) {
      key_norm <- .fcm_supernorm(name_v[idx_nm])
      m <- match(key_norm, reg$name_norm)
      hit_rows <- which(!is.na(m))
      if (length(hit_rows) > 0) {
        for (j in hit_rows) {
          i <- idx_nm[j]
          r <- reg[m[j], ]
          inchikey[i] <- r$InChIKey
          source[i]   <- paste0("本地库名称匹配（", r$source_table, "）")
          method[i]   <- "db_name"
        }
        say("④ 本地库名称匹配: ", length(hit_rows), " 行")

        # 名称命中后同样补元数据
        idx2 <- idx_nm[hit_rows]
        hits <- .fcm_chemicals_lookup(con, inchikey[idx2], by_skeleton = FALSE)
        if (nrow(hits) > 0) {
          for (i in idx2) {
            sub <- hits[hits$InChIKey == inchikey[i], , drop = FALSE]
            if (nrow(sub) == 0) next
            cid_v[i] <- sub$CID[1]
            exact[i] <- suppressWarnings(as.numeric(sub$ExactMass[1]))
            if (is.na(formula[i]) || !nzchar(formula[i])) formula[i] <- sub$Formula[1]
            if (is.na(canon[i])) canon[i] <- sub$SMILES[1]
          }
        }
      }
    }
  }

  # ---- 第 5 步：联网兜底（默认关闭）----
  idx_on <- which(is.na(inchikey))
  if (isTRUE(online) && length(idx_on) > 0) {
    say("⑤ 联网 PubChem 兜底: ", length(idx_on), " 行待处理…")
    for (i in idx_on) {
      meta <- .fcm_pubchem_by_smiles(smiles_v[i])
      used <- "PubChem（SMILES）"
      used_code <- "pubchem_smiles"
      if (is.na(meta$InChIKey)) {
        Sys.sleep(delay)
        meta <- .fcm_pubchem_by_name(name_v[i])
        used <- "PubChem（名称）"
        used_code <- "pubchem_name"
      }
      if (!is.na(meta$InChIKey)) {
        inchikey[i] <- meta$InChIKey
        cid_v[i]    <- meta$CID
        formula[i]  <- meta$Formula
        canon[i]    <- meta$SMILES
        exact[i]    <- suppressWarnings(as.numeric(meta$ExactMass))
        source[i]   <- used
        method[i]   <- used_code
      } else {
        note[i] <- "本地与 PubChem 均未解析出 InChIKey"
      }
      Sys.sleep(delay)
    }
    say("   联网补上 ", sum(grepl("^pubchem", method)), " 行")
  } else if (length(idx_on) > 0) {
    say("⑤ 联网兜底已关闭（online = FALSE），", length(idx_on), " 行未解析")
  }

  # ---- 组装输出 ----
  out <- data.frame(data, stringsAsFactors = FALSE)

  # 统一列名：NAME / SMILES / CAS 是下游（run_toxtree / assign_toxicity）认的名字
  if (!identical(nm_col, "NAME")) out[["NAME"]] <- name_v
  out[["SMILES"]] <- smiles_v
  if (!is.null(cas_col) && !identical(cas_col, "CAS")) {
    out[["CAS"]] <- as.character(data[[cas_col]])
  }

  out[["InChIKey"]]         <- inchikey
  out[["CID"]]              <- cid_v
  out[["Formula"]]          <- formula
  out[["ExactMass"]]        <- exact
  out[["SMILES_canonical"]] <- canon
  out[["identity_source"]]  <- source
  out[["identity_method"]]  <- method
  out[["identity_note"]]    <- note

  # ---- 报告 ----
  unresolved <- which(is.na(inchikey) | !nzchar(inchikey))
  cnt <- function(code) sum(!is.na(method) & method == code)
  report <- list(
    total              = n,
    given              = sum(method == "given", na.rm = TRUE),
    from_smiles        = cnt("smiles_cdk"),
    skeleton_hit       = cnt("db_skeleton"),
    skeleton_ambiguous = cnt("db_skeleton_ambiguous"),
    name_hit           = cnt("db_name"),
    online_hit         = cnt("pubchem_smiles") + cnt("pubchem_name"),
    exact_db_hit       = sum(resolved_exact),
    unresolved         = length(unresolved),
    by_method          = as.data.frame(table(method = method, useNA = "ifany"),
                                       stringsAsFactors = FALSE),
    unresolved_rows = if (length(unresolved) > 0) {
      data.frame(row = unresolved, NAME = name_v[unresolved],
                 SMILES = smiles_v[unresolved], reason = note[unresolved],
                 stringsAsFactors = FALSE)
    } else {
      data.frame(row = integer(0), NAME = character(0), SMILES = character(0),
                 reason = character(0), stringsAsFactors = FALSE)
    }
  )
  attr(out, "prepare_report") <- report

  say(paste(rep("-", 60), collapse = ""))
  say("📊 补全结果：共 ", n, " 行")
  say("   输入自带      : ", report$given)
  say("   本地推导      : ", report$from_smiles)
  say("   库内精确命中  : ", report$exact_db_hit)
  say("   骨架降级命中  : ", report$skeleton_hit,
      if (report$skeleton_ambiguous > 0) {
        paste0("（另有 ", report$skeleton_ambiguous, " 行骨架歧义）")
      } else {
        ""
      })
  say("   名称匹配命中  : ", report$name_hit)
  say("   联网补上      : ", report$online_hit)
  say("   ⚠️ 未解析     : ", report$unresolved)
  if (report$unresolved > 0) {
    show_n <- min(5, report$unresolved)
    for (j in seq_len(show_n)) {
      rr <- report$unresolved_rows[j, ]
      say("      · 第 ", rr$row, " 行 ", rr$NAME, "：", rr$reason)
    }
    say("   详见 attr(result, \"prepare_report\")$unresolved_rows")
  }

  out
}

#' 打印 prepare_input 的补全报告
#'
#' @param x prepare_input() 的返回值
#' @return 隐式返回报告对象
#' @export
#' @export
print_prepare_report <- function(x) {
  rep <- attr(x, "prepare_report")
  if (is.null(rep)) {
    message("该对象没有补全报告（不是 prepare_input() 的返回值）。")
    return(invisible(NULL))
  }
  message("共 ", rep$total, " 行：输入自带 ", rep$given,
          "，本地推导 ", rep$from_smiles,
          "，库内精确 ", rep$exact_db_hit,
          "，骨架降级 ", rep$skeleton_hit,
          "，名称匹配 ", rep$name_hit,
          "，联网补上 ", rep$online_hit,
          "，未解析 ", rep$unresolved)
  if (rep$unresolved > 0) print(rep$unresolved_rows)
  invisible(rep)
}
