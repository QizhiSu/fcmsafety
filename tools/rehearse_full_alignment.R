# =============================================================================
# 全量对齐演练：修完归一化/列映射/列声明之后，这四个库能不能靠"一次追赶"写到干净？
#
# 五步：
#   R0 副本库按 schema 重建  —— iarc 的 volume 列声明错了（INTEGER），SQLite 改不了
#                            列类型只能建新表搬数据。不先重建，追赶写入的多值文本
#                            会被整列按整数读回而截成前导数字，diff 永远不收敛。
#   R1 修后 dry-run diff     —— 每个库现在还被判多少 added/removed/modified
#   R2 在副本库上追赶        —— 用真实 write_changes_to_db() 真正写一次
#   R3 再 dry-run diff       —— 收敛验证：追赶后是否归零（= 闸门能不能开）
#   R6 读未分配条目账本      —— 收不了的行登记了什么（R4/R5 是列级抽查与真库自证）
#
# 安全约定（重要）：
#   只写副本库 %TEMP%/fcm_auto_test/fcmsafety.db，真库 inst/fcmsafety.db 与
#   inst/*.xlsx 一个字节都不动；末尾对比真库行数 / 关键列非空数 / mtime 自证。
#   入库备份关掉（backup = FALSE）——真库已有整目录备份，副本库不需要。
#
# 两个刻意的取舍，脚本里都记数上报：
#   1) removed 一律"缓议"：不安全删除，只统计不执行（真实链路要求人工确认）。
#      注意：缓议的 removed 行不会被写入，所以它们身上那些旧格式的列值会留在库里
#      （例如 cmr 的 005-011-01-1 / 613-166-00-X 仍是 pictogram==signal_word_codes）。
#      这不是清洗没生效 —— 这两行本来就等着人工确认后删除。
#   2) added 里 InChIKey 为空的行交给真实的 split_unassignable() 摘出来登记成账
#      （见 R6）。脚本不自己复刻这层判断 —— 旁路复刻是这个项目踩过的坑。
#      摘之前先原样试写一次并记录报错，好把"整批回滚"这个现象留在报告里。
#
# 前置：先跑 tools/test_auto_update_line.R 生成 %TEMP%/fcm_auto_test/dl/。
# 用法（Git Bash，cwd = 包根）：
#   LC_ALL=zh_CN.UTF-8 "/c/Program Files/R/R-4.6.1/bin/Rscript.exe" --vanilla tools/rehearse_full_alignment.R
# 结果落盘：.workbuddy/rehearsal_result.txt
# =============================================================================

LOG <- file.path(".workbuddy", "rehearsal_result.txt")
if (file.exists(LOG)) file.remove(LOG)
log_line <- function(...) { m <- paste0(...); cat(m, "\n"); cat(m, "\n", file = LOG, append = TRUE) }
section <- function(t) log_line("\n", strrep("=", 78), "\n## ", t, "\n", strrep("=", 78))

pkgload::load_all(".", quiet = TRUE)

TMP <- file.path(Sys.getenv("TEMP"), "fcm_auto_test")
DL  <- file.path(TMP, "dl")
DB  <- file.path(TMP, "fcmsafety.db")
REAL_DB <- normalizePath("inst/fcmsafety.db")

# 副本库：每次从真库重新拷一份，保证演练基数干净
file.copy(REAL_DB, DB, overwrite = TRUE)
log_line("R: ", R.version.string)
log_line("真库  : ", REAL_DB, " (", file.size(REAL_DB), " bytes)")
log_line("副本库: ", DB, " (", file.size(DB), " bytes)")
log_line("下载目录: ", DL)

# ---- 库规格与检查点 ----------------------------------------------------------

SPECS <- list(
  list(db = "cmr",         f = "clp.xlsx",       key = "index_no",
       fb = "cas_no", cas = "cas_no", nm = "international_chemical_identification",
       cols = c("hazard_statement_codes", "hazard_statement_codes_alt", "pictogram",
                "atp_inserted_updated", "international_chemical_identification")),
  list(db = "cmr_suspect", f = "clp.xlsx",       key = "index_no",
       fb = "cas_no", cas = "cas_no", nm = "substance_name",
       cols = c("cas_no", "ec_no", "classification", "source", "notes")),
  list(db = "iarc",        f = "iarc.xlsx",      key = "cas_no",
       fb = "agent",  cas = "cas_no", nm = "agent",
       cols = c("group_classification", "volume", "volume_publication_year",
                "evaluation_year", "additional_information")),
  list(db = "eu_sml",      f = "eu10_2011.xlsx", key = "fcm_substance_no",
       fb = "cas_no", cas = "cas_no", nm = "substance_name",
       cols = c("sml", "sml_group", "restrictions_and_specifications",
                "notes_on_verification"))
)

row_counts <- function(path) {
  con <- get_db_connection(path); on.exit(DBI::dbDisconnect(con), add = TRUE)
  tabs <- c("cmr", "cmr_suspect", "iarc", "eu_sml", "chemicals", "update_history")
  out <- vapply(tabs, function(t) {
    tryCatch(DBI::dbGetQuery(con, sprintf('select count(*) n from "%s"', t))$n,
             error = function(e) NA_integer_)
  }, integer(1))
  names(out) <- tabs
  out
}

nonblank <- function(path, tab, cols) {
  con <- get_db_connection(path); on.exit(DBI::dbDisconnect(con), add = TRUE)
  out <- vapply(cols, function(cl) {
    tryCatch(DBI::dbGetQuery(con, sprintf(
      'select count(*) n from "%s" where "%s" is not null and trim(cast("%s" as text)) <> \'\'',
      tab, cl, cl))$n, error = function(e) NA_integer_)
  }, integer(1))
  names(out) <- cols
  out
}

real_rows_before <- row_counts(REAL_DB)
real_cols_before <- lapply(SPECS, function(sp) nonblank(REAL_DB, sp$db, sp$cols))
names(real_cols_before) <- vapply(SPECS, function(sp) sp$db, character(1))
real_mtime_before <- file.mtime(REAL_DB)
log_line("\n真库起始行数: ", paste(names(real_rows_before), real_rows_before, sep = "=", collapse = "  "))

# ---- R0 按 schema 重建列声明错的表 -------------------------------------------
# 副本库从真库拷来，iarc 的 volume / volume_publication_year 还是 INTEGER 声明。
# SQLite 改不了列类型，只能建新表搬数据；重建必须在 SQL 层做（经 R 往返时截断
# 已经发生）。不重建的话，追赶写入的多值文本会被整列按整数读回而截成前导数字，
# diff 永远不收敛 —— 这正是 R3 残留 661 行 modified 的成因。

section("R0 副本库按 schema 重建（修列声明）")
n_rebuilt <- tryCatch({
  n <- fcmsafety:::rebuild_table_from_schema("iarc", db_path = DB)
  log_line("iarc 重建完成，行数 ", n)
  con <- get_db_connection(DB)
  types <- DBI::dbGetQuery(con, "PRAGMA table_info(iarc)")
  DBI::dbDisconnect(con)
  log_line("  重建后 volume / volume_publication_year 声明: ",
           types$type[types$name == "volume"], " / ",
           types$type[types$name == "volume_publication_year"])
  n
}, error = function(e) { log_line("重建失败: ", conditionMessage(e)); NA_integer_ })

# cmr_suspect 的 diff 主键从 substance_name 换成 index_no（CLP 官方标识）。
# 老库没有这一列，副本库上先跑一次真实迁移函数 —— 与真库要跑的完全同一个。
section("R0b cmr_suspect 补 index_no 并回填（真实迁移函数）")
mi <- tryCatch(
  fcmsafety:::migrate_cmr_suspect_index_no(db_path = DB),
  error = function(e) { log_line("迁移失败: ", conditionMessage(e)); NULL })
if (!is.null(mi)) {
  log_line(sprintf("  补列: %s   本次回填 %d 行   仍无键 %d 行   共 %d 行",
                   if (mi$added) "是" else "否（已存在）",
                   mi$filled, mi$unmatched, mi$total))
}

# ---- 真实链路调用（dry-run：只算 diff，不写库） ------------------------------

# 演练模式：默认不查 PubChem（快）。此时新增行拿不到 InChIKey，会被
# split_unassignable() 整批摘进账本，R1 的 added 因此恒为 0 —— 那不是
# "没有新增"，是"新增全都没查过结构"。要看真实能写入多少，置
# FCM_REHEARSE_ENRICH=TRUE 再跑（慢：每行一次 PubChem 请求）。
ENRICH <- isTRUE(as.logical(Sys.getenv("FCM_REHEARSE_ENRICH", "FALSE")))

dry_one <- function(sp) {
  switch(sp$db,
    cmr = update_cmr_auto(source = "local", new_file = file.path(DL, sp$f),
                          interactive = FALSE, auto_apply = FALSE, enrich = ENRICH, db_path = DB),
    cmr_suspect = update_cmr_suspect_auto(source = "local", new_file = file.path(DL, sp$f),
                                          interactive = FALSE, auto_apply = FALSE, enrich = ENRICH, db_path = DB),
    iarc = update_iarc_auto(source = "local", new_file = file.path(DL, sp$f),
                            interactive = FALSE, auto_apply = FALSE, enrich = ENRICH, db_path = DB),
    eu_sml = update_eu_sml_auto(source = "local", new_file = file.path(DL, sp$f),
                                interactive = FALSE, auto_apply = FALSE, enrich = ENRICH, db_path = DB))
}

summarise_diff <- function(r) {
  ch <- r$changes
  if (is.null(ch)) return(c(added = NA, removed = NA, modified = NA))
  c(added = ch$total_added, removed = ch$total_removed, modified = ch$total_modified)
}

# ---- R1 修后 dry-run diff ----------------------------------------------------

section("R1 修后 dry-run diff（副本库，只算不写）")
log_line("本轮模式: enrich = ", ENRICH,
         if (ENRICH) "（新增行会查 PubChem 补结构）"
         else "（不查 PubChem：新增行拿不到键，整批被摘进账本，added 因此为 0；见 R6）")
r1 <- list(); s1 <- list()
for (sp in SPECS) {
  t0 <- Sys.time()
  r1[[sp$db]] <- tryCatch(dry_one(sp), error = function(e) list(changes = NULL, message = conditionMessage(e)))
  s1[[sp$db]] <- summarise_diff(r1[[sp$db]])
  log_line(sprintf("[%-12s] added=%4s removed=%4s modified=%4s  (%.0fs)  %s",
                   sp$db, s1[[sp$db]]["added"], s1[[sp$db]]["removed"], s1[[sp$db]]["modified"],
                   as.numeric(difftime(Sys.time(), t0, units = "secs")),
                   substr(if (is.null(r1[[sp$db]]$message)) "" else r1[[sp$db]]$message, 1, 60)))
}

# ---- 自动入库闸门体检（按 R1 数字推算，无需重跑） ----------------------------
section("闸门体检：无人值守调度（auto_apply=TRUE, max_auto_changes=20）会怎么判")
for (sp in SPECS) {
  s <- s1[[sp$db]]
  verdict <- if (is.na(s["added"])) "diff 未产出"
    else if (s["removed"] > 0) "挡下：有 removed，强制人工确认（交互确认优先于条数闸门）"
    else if (s["added"] + s["modified"] > 20) sprintf("挡下：%d 条变更 > 闸门 20", s["added"] + s["modified"])
    else "放行"
  log_line(sprintf("   %-12s %s", sp$db, verdict))
}

# ---- R2 在副本库上追赶 -------------------------------------------------------

section("R2 副本库追赶（真实 write_changes_to_db）")
apply_one <- function(sp, ch, dropped) {
  n_add <- ch$total_added; n_rem <- ch$total_removed; n_mod <- ch$total_modified
  # removed 缓议：不删
  ch2 <- ch
  ch2$removed <- ch2$removed[0, , drop = FALSE]
  ch2$total_removed <- 0L
  if (is.null(dropped)) dropped <- data.frame()

  # 收不了的行在 R1 的真实入口里**已经摘过了**（run_incremental_update() 返回的
  # changes$added 是被改写过的），这里直接用它交出来的清单。再摘一次只会得到 0 行，
  # 账本就永远记不上 —— 第一版就是这么错的。
  reg <- tryCatch({
    record_unassigned(sp$db, dropped, db_path = DB)
    mark_unassigned_resolved(sp$db, key_of_df(ch2$added, sp$key, sp$fb), db_path = DB)
    sprintf("登记 %d 条", nrow(dropped))
  }, error = function(e) paste0("账本写入失败: ", conditionMessage(e)))
  res <- if (nrow(ch2$added) + nrow(ch2$modified) == 0) "无事可写"
    else tryCatch({
      d <- write_changes_to_db(sp$db, ch2, sp$key, sp$fb, DB, backup = FALSE)
      sprintf("写入 +%d / -%d / ~%d", d$records_added, d$records_removed, d$records_modified)
    }, error = function(e) paste0("ERROR: ", conditionMessage(e)))
  nm <- if ("substance_name" %in% names(dropped)) {
    as.character(dropped$substance_name)
  } else {
    character(0)
  }
  nm <- gsub("[\r\n]+", " ", nm)
  list(total_added = n_add, total_removed = n_rem, total_modified = n_mod,
       keyless = nrow(dropped), wrote_added = ch2$total_added,
       wrote_modified = ch2$total_modified, res = res, registry = reg,
       sample = utils::head(nm, 3))
}

applied <- list()
for (sp in SPECS) {
  ch <- r1[[sp$db]]$changes
  if (is.null(ch)) { log_line(sprintf("[%-12s] 跳过（无 diff 结果）", sp$db)); next }
  a <- apply_one(sp, ch, r1[[sp$db]]$unassigned)
  applied[[sp$db]] <- a
  log_line(sprintf("\n[%s] 摘后 diff: added=%d removed=%d modified=%d", sp$db, a$total_added, a$total_removed, a$total_modified))
  log_line("   收不了（已摘出）: ", a$keyless, " 行  -> 本次真正写入 added=", a$wrote_added, " modified=", a$wrote_modified)
  if (length(a$sample) > 0) {
    log_line("     样例: ", paste(substr(a$sample, 1, 46), collapse = " | "))
  }
  log_line("   缓议的 removed: ", a$total_removed)
  log_line("   账本: ", a$registry)
  log_line("   追赶结果: ", a$res)
}

# ---- R3 收敛验证 -------------------------------------------------------------

section("R3 追赶后再 dry-run：是否收敛（这才是'闸门能不能开'的答案）")
r3 <- list(); s3 <- list()
for (sp in SPECS) {
  r3[[sp$db]] <- tryCatch(dry_one(sp), error = function(e) list(changes = NULL, message = conditionMessage(e)))
  s3[[sp$db]] <- summarise_diff(r3[[sp$db]])
  log_line(sprintf("[%-12s] added=%4s removed=%4s modified=%4s",
                   sp$db, s3[[sp$db]]["added"], s3[[sp$db]]["removed"], s3[[sp$db]]["modified"]))
}

log_line("\n收敛对照（R1 -> R3）:")
for (sp in SPECS) {
  log_line(sprintf("   %-12s modified %4s -> %4s   (残留 removed %s)",
                   sp$db, s1[[sp$db]]["modified"], s3[[sp$db]]["modified"], s3[[sp$db]]["removed"]))
}

# ---- R3b 残余成因拆解 --------------------------------------------------------
#
# "追赶后还有残留"必须能自己解释。这里的算法与 run_incremental_update 的步骤
# 顺序一致（fetch -> map -> 取库 -> backfill -> diff，enrich=FALSE），逐个被判
# modified 的 key 比列，按"该列实际值不同的行数"排序。
section("R3b 残余 modified 的列级成因（追赶后仍在变的，是哪些列在顶）")
resid_one <- function(sp) {
  new_df <- fetch_source_data(sp$db, source = "local",
                              new_file = file.path(DL, sp$f))
  mapped <- map_to_db_columns(new_df, sp$db, DB)
  con <- get_db_connection(DB); on.exit(DBI::dbDisconnect(con), add = TRUE)
  cur <- DBI::dbGetQuery(con, paste("SELECT * FROM", sp$db))
  mapped <- backfill_unmapped_cols(mapped, cur, sp$key, sp$fb)
  d <- diff_incremental(mapped, cur, sp$key, sp$fb, NULL, cas_col = sp$cas)
  cols <- setdiff(intersect(names(mapped), names(cur)),
                  c(chem_cols, "id", "created_at", "updated_at"))
  ck <- key_of_df(cur, sp$key, sp$fb); nk <- key_of_df(mapped, sp$key, sp$fb)
  hits <- stats::setNames(integer(length(cols)), cols)
  samples <- character(0)
  for (k in d$modified_keys) {
    ci <- which(ck == k); ni <- which(nk == k)
    if (length(ci) != 1 || length(ni) != 1) next
    for (cc in cols) {
      a <- canon_cell(cur[[cc]][ci]); b <- canon_cell(mapped[[cc]][ni])
      if (!identical(a, b)) {
        hits[cc] <- hits[cc] + 1L
        if (length(samples) < 3) samples <- c(samples, sprintf(
          "      %s | %s: [库] %s -> [源] %s", substr(k, 1, 30), cc,
          substr(gsub("\n", " ; ", a), 1, 38), substr(gsub("\n", " ; ", b), 1, 38)))
      }
    }
  }
  list(n = d$total_modified, hits = sort(hits[hits > 0], decreasing = TRUE), samples = samples)
}
for (sp in SPECS) {
  if (is.na(s3[[sp$db]]["modified"]) || s3[[sp$db]]["modified"] == 0) {
    log_line(sprintf("\n[%-12s] 无残留", sp$db)); next
  }
  rr <- tryCatch(resid_one(sp), error = function(e) NULL)
  if (is.null(rr)) { log_line(sprintf("\n[%-12s] 拆解失败", sp$db)); next }
  log_line(sprintf("\n[%s] 残留 modified = %d，按列:", sp$db, rr$n))
  for (i in seq_along(rr$hits)) {
    log_line(sprintf("   %-42s %5d 行", names(rr$hits)[i], rr$hits[i]))
  }
  for (s in rr$samples) log_line(s)
}

# ---- R4 列级抽查：追赶到底改对了什么 -----------------------------------------

section("R4 列级抽查（真库 -> 副本库追赶后）")
con <- get_db_connection(DB); on.exit(DBI::dbDisconnect(con), add = TRUE)
for (sp in SPECS) {
  log_line(sprintf("\n-- %s", sp$db))
  before <- real_cols_before[[sp$db]]
  after <- nonblank(DB, sp$db, sp$cols)
  for (cl in sp$cols) {
    log_line(sprintf("   %-42s 真库 %4d  ->  追赶后 %4d", cl, before[[cl]], after[[cl]]))
  }
}
log_line("\n-- iarc 列类型抽查（volume 是文本多值列，不该被当整数塞回）")
tv <- DBI::dbGetQuery(con, "select typeof(volume) t, count(*) n from iarc where volume is not null group by 1")
for (i in seq_len(nrow(tv))) log_line("   volume 类型 ", tv$t[i], ": ", tv$n[i], " 行")
# ⚠️ 声明为 INTEGER 的列里存了文本时，RSQLite 走"按声明类型取整数"的通道，
#    会把 "41, Sup 7, 71, 106" 静默截成 41。diff 正是用 SELECT * 读库的，
#    所以不管写进去多少次，它永远认为库里是 41、源里是全文 → 永远判 modified。
star <- DBI::dbGetQuery(con, "SELECT * FROM iarc")
full <- DBI::dbGetQuery(con,
  "select cast(volume as text) v from iarc where typeof(volume) = 'text'")$v
log_line("   SELECT * 读回的 volume 最长长度:      ",
         max(nchar(as.character(star$volume)), na.rm = TRUE))
log_line("   库里 text 型 volume 的最长长度:      ", max(nchar(full), na.rm = TRUE))
log_line("   → 两者不等即证明读取端被截断（列声明类型不对，不是数据没写进去）")
log_line("-- eu_sml.sml 落库类型（应为 real，说明 ND->0.01 的数值口径守住了）")
tv2 <- DBI::dbGetQuery(con, "select typeof(sml) t, count(*) n from eu_sml where sml is not null group by 1")
for (i in seq_len(nrow(tv2))) log_line("   sml 类型 ", tv2$t[i], ": ", tv2$n[i], " 行")
enc <- DBI::dbGetQuery(con, "select count(*) n from eu_sml where sml_group like '%(%'")
log_line("   sml_group 含括号的: ", enc$n)
log_line("-- cmr 主 H 码列与备用 H 码列是否还在完全重复")
ov <- DBI::dbGetQuery(con,
  "select sum(case when hazard_statement_codes = hazard_statement_codes_alt then 1 else 0 end) same, count(*) tot from cmr")
log_line(sprintf("   hazard_statement_codes 与 _alt 完全相同: %d / %d", ov$same, ov$tot))

# ---- R5 真库完整性自证 --------------------------------------------------------

section("R5 真库完整性自证")
real_rows_after <- row_counts(REAL_DB)
real_mtime_after <- file.mtime(REAL_DB)
log_line("真库结束行数: ", paste(names(real_rows_after), real_rows_after, sep = "=", collapse = "  "))
log_line("真库行数是否变动: ", if (identical(as.integer(real_rows_before), as.integer(real_rows_after))) "否（安全）" else "是（异常）")
log_line("真库 mtime: ", format(real_mtime_before), " -> ", format(real_mtime_after),
         "  ", if (identical(real_mtime_before, real_mtime_after)) "未变（安全）" else "变了（异常）")
same_cols <- TRUE
for (sp in SPECS) {
  a <- nonblank(REAL_DB, sp$db, sp$cols)
  if (!identical(as.integer(real_cols_before[[sp$db]]), as.integer(a))) {
    same_cols <- FALSE
    log_line("   注意: ", sp$db, " 真库关键列非空数变了")
  }
}
log_line("真库关键列非空数是否变动: ", if (same_cols) "否（安全）" else "是（异常）")
inst_new <- {
  f <- list.files("inst", recursive = TRUE, full.names = TRUE)
  mt <- file.mtime(f)
  f[!is.na(mt) & mt > as.POSIXct("2026-09-11 00:00:00")]
}
log_line("inst/ 下今天被改动的文件: ",
         if (length(inst_new) == 0) "无（安全）" else paste(inst_new, collapse = ", "))
# ---- R6 未分配条目账本 -------------------------------------------------------

section("R6 未分配条目账本（上游给了、但本体没有结构的）")
con_reg <- DBI::dbConnect(RSQLite::SQLite(), DB)
if (DBI::dbExistsTable(con_reg, "unassigned_entries")) {
  reg <- DBI::dbGetQuery(con_reg, paste(
    "SELECT database_name, reason, COUNT(*) n, SUM(seen_count) seen",
    "FROM unassigned_entries GROUP BY 1, 2 ORDER BY 1, 2"))
  if (nrow(reg) == 0L) {
    log_line("   （空）")
  } else {
    for (i in seq_len(nrow(reg))) {
      log_line(sprintf("   %-12s %-20s %4d 条（累计遇到 %d 次）",
                       reg$database_name[i], reg$reason[i], reg$n[i], reg$seen[i]))
    }
    log_line("   合计 ", sum(reg$n), " 条")
    log_line("   -- 每库样例（名称 | CAS 情况）--")
    for (db in unique(reg$database_name)) {
      ex <- DBI::dbGetQuery(con_reg, paste(
        "SELECT substance_name, cas_no, reason FROM unassigned_entries",
        "WHERE database_name = ? ORDER BY id LIMIT 3"), params = list(db))
      for (j in seq_len(nrow(ex))) {
        nm <- gsub("[\r\n]+", " ", as.character(ex$substance_name[j]))
        log_line(sprintf("     %-12s %-56s | %s", db, substr(nm, 1, 56),
                         if (is.na(ex$cas_no[j])) "无 CAS" else ex$cas_no[j]))
      }
    }
  }
} else {
  log_line("   账本表不存在（未分配条目一条都没登记）")
}
DBI::dbDisconnect(con_reg)

log_line("\n完成: ", format(Sys.time()))
