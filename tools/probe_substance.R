## 单物质组别判定探针
##
## 用途：给定一个物质（名称 + SMILES，或 CAS / InChIKey），用**真实函数**跑一遍
## 组条目判定与全库精确匹配，把过程逐条打印出来，用来回答
## "这个物质属于哪个法规的哪个组别"。
##
## 为什么不能用旁路复刻：元素层判定包含 layer 分层、限定词、两条护栏和置信度
## 降级，任一环节漏掉都会得出相反结论（2026-09-10 已踩过一次，见 ADR 0009）。
##
## 运行（Git Bash，工作目录 = 包根目录）：
##   LC_ALL=zh_CN.UTF-8 "/c/Program Files/R/R-4.6.1/bin/Rscript.exe" \
##     --vanilla tools/probe_substance.R "Progesterone" \
##     "CC(=O)[C@H]1CC[C@H]2[C@@H]3CCC4=CC(=O)CC[C@]4(C)[C@H]3CC[C@]12C"
##
## 第二个参数可省略，此时第一个参数当作 CAS / InChIKey / 分子式用。
## 结果同时写入 .workbuddy/probe_substance_out.txt（进程退出码受沙箱污染，看落盘）。

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  stop("用法: Rscript tools/probe_substance.R <名称或标识> [SMILES]", call. = FALSE)
}
NAME  <- args[1]
SMILES <- if (length(args) >= 2) args[2] else NA_character_

suppressMessages(pkgload::load_all(".", quiet = TRUE))

out <- character(0)
say <- function(...) {
  line <- paste0(...)
  out <<- c(out, line)
  cat(line, "\n", sep = "")
}

say("=== 输入 ===")
say("NAME   : ", NAME)
say("SMILES : ", ifelse(is.na(SMILES), "<未提供>", SMILES))

# ---- 1. 结构标识 ----
say("")
say("=== 1. 结构标识 prepare_input() ===")
df <- if (is.na(SMILES)) {
  data.frame(CAS = NAME, stringsAsFactors = FALSE)
} else {
  data.frame(NAME = NAME, SMILES = SMILES, stringsAsFactors = FALSE)
}
prep <- tryCatch(prepare_input(df, verbose = FALSE), error = function(e) e)
if (inherits(prep, "error")) {
  say("ERROR: ", conditionMessage(prep))
  prep <- NULL
} else {
  print_cols <- intersect(c("NAME", "SMILES", "InChIKey", "Formula",
                            "ExactMass", "identity_source", "identity_method"),
                          names(prep))
  say(paste(utils::capture.output(
    print(as.data.frame(prep)[print_cols], row.names = FALSE)), collapse = "\n"))
}

# ---- 2. IARC 元素层逐条交集 ----
say("")
say("=== 2. IARC 组条目：元素层逐条交集 ===")
id <- tryCatch(
  normalize_input_identity(if (is.na(SMILES)) NAME else SMILES),
  error = function(e) NULL
)
if (is.null(id)) {
  say("输入无法归一，跳过元素层")
} else {
  say("输入元素: ", paste(id$elements, collapse = ", "))
  reg <- query_iarc_group_registry()
  inp_els <- tolower(id$elements)
  n_hit <- 0
  for (i in seq_len(nrow(reg))) {
    els <- if (nzchar(reg$elements[i])) strsplit(reg$elements[i], ";")[[1]] else character(0)
    common <- intersect(inp_els, tolower(els))
    if (length(common)) n_hit <- n_hit + 1
    say(sprintf("[%-3s] %-88s | layer=%-8s | 特征=%-8s | %s",
                reg$group_classification[i], substr(reg$agent[i], 1, 88),
                reg$layer[i],
                ifelse(length(els), paste(els, collapse = ","), "<无>"),
                ifelse(length(common), paste0("命中 ", paste(common, collapse = ",")),
                       "出局")))
  }
  say("--> 交集非空的条目: ", n_hit, " / ", nrow(reg))

  # 元素层看不见的那些类条目（Progestins、Aflatoxins、Polychlorinated biphenyls…）
  # 单列出来，免得"交集 0 命中"被误读成"这个物质确实干净"。
  man <- reg[reg$layer == "manual", , drop = FALSE]
  if (nrow(man) > 0) {
    say("")
    say("--- 无元素判据的类条目 ", nrow(man),
        " 条（元素层对它们完全静默，命中与否都不会出现在上面的表里）---")
    say(paste0("    ", paste(sprintf("[%s] %s", man$group_classification, man$agent),
                             collapse = "\n    ")))
  }
}

# ---- 2b. IARC (see X) 交叉引用 ----
say("")
say("=== 2b. IARC (see X) 交叉引用（别名）===")
am <- tryCatch(query_iarc_see_alias_map(), error = function(e) NULL)
if (is.null(am) || nrow(am) == 0) {
  say("别名表为空（无 (see X) 行，或目标条目不在库中）")
} else {
  say("全表可解析别名 ", nrow(am), " 条；与本次输入相关的：")
  key <- tryCatch(prep$InChIKey[1], error = function(e) NA_character_)
  mine <- am[!is.na(key) & am$InChIKey == key, , drop = FALSE]
  if (nrow(mine) == 0) {
    say("    （无 —— 输入物质的键不在别名表里）")
  } else {
    for (i in seq_len(nrow(mine))) {
      say(sprintf("    [%s] %s  ->  分组继承自 %s",
                  mine$group_classification[i], mine$source_agent[i], mine$alias_of[i]))
    }
  }
}

# ---- 3. 函数实际返回 ----
say("")
say("=== 3. assign_group_membership_table(source = 'all') ===")
hits <- tryCatch(assign_group_membership_table(df, source = "all"),
                 error = function(e) e)
if (inherits(hits, "error")) {
  say("ERROR: ", conditionMessage(hits))
} else {
  say("命中行数: ", nrow(hits))
  if (nrow(hits) > 0) {
    say(paste(utils::capture.output(
      print(as.data.frame(hits), row.names = FALSE)), collapse = "\n"))
  }
  errs <- attr(hits, "errors")
  if (!is.null(errs) && nrow(errs) > 0) {
    say("--- errors ---")
    say(paste(utils::capture.output(print(errs, row.names = FALSE)),
              collapse = "\n"))
  }
  say("--- summarise_group_hits() ---")
  sm <- summarise_group_hits(hits)
  say(paste(utils::capture.output(print(as.data.frame(sm), row.names = FALSE)),
            collapse = "\n"))
}

# ---- 4. UVCB 关键词层（名称类判据）----
say("")
say("=== 4. UVCB 关键词层 ===")
say("is_uvcb_name()              = ", is_uvcb_name(NAME))
say("extract_backbone_keywords() = ",
    paste(extract_backbone_keywords(NAME), collapse = ", "))
say("categorize_uvcb()           = ",
    paste(categorize_uvcb(NAME), collapse = ", "))
if (!is.null(id)) {
  say("extract_input_keywords()    = ",
      paste(extract_input_keywords(id), collapse = ", "))
}

# ---- 5. 全库精确匹配 + 定级 ----
if (!is.null(prep)) {
  say("")
  say("=== 5. assign_toxicity(group_membership = TRUE) ===")
  tox_csv <- file.path(tempdir(), "probe_tox_placeholder.csv")
  utils::write.csv(
    data.frame(SMILES = ifelse(is.na(SMILES), NA_character_, SMILES),
               Cramer.rules = NA_character_, check.names = FALSE),
    tox_csv, row.names = FALSE, fileEncoding = "UTF-8")
  res <- tryCatch(
    assign_toxicity(as.data.frame(prep), toxtree_result = tox_csv,
                    output_file = NULL, group_membership = TRUE),
    error = function(e) e
  )
  if (inherits(res, "error")) {
    say("ERROR: ", conditionMessage(res))
  } else {
    keep <- intersect(c("Cramer_rules", "SVHC", "CMR", "CMR_H_codes",
                        "CMR_suspect", "EDC", "IARC", "EU_SML", "China_SML",
                        "Group_hits", "Group_IARC", "Group_review",
                        "Toxic_level", "Toxic_level_basis"), names(res))
    for (k in keep) {
      say(sprintf("  %-18s : %s", k,
                  paste(as.character(res[[k]]), collapse = " | ")))
    }
    qi <- attr(res, "query_issues")
    if (!is.null(qi) && is.data.frame(qi) && nrow(qi) > 0) {
      say("--- query_issues ---")
      say(paste(utils::capture.output(print(qi, row.names = FALSE)),
                collapse = "\n"))
    }
  }
}

writeLines(out, ".workbuddy/probe_substance_out.txt", useBytes = TRUE)
cat("\n[结果已写入 .workbuddy/probe_substance_out.txt]\n")
