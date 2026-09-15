# 未能入库条目的登记簿（unassigned_entries）
#
# 背景：上游有四类条目本体没有结构 —— IARC 评的感染状态 / 职业暴露场景
# （"Helicobacter pylori (infection with)"）、A 类组条目（"salts of hydrazine"）、
# UVCB 工业品（"alcohols, aliphatic, monohydric, saturated, linear, primary (C4-C22)"）、
# 反应产物混合物（"reaction mass of: ..."）。实测四库合计 414 行落在这一类。
#
# 它们拿不到 InChIKey，而四张业务表的 InChIKey 是 NOT NULL + 外键指向 chemicals
# —— 插不进去；写库又是单事务，一行失败整批回滚（iarc 历史上因此 846 行全废）。
#
# 处理是"写库前摘出来登记"，不是放宽约束。理由：这些条目不是"暂时查不到结构"，
# 而是"结构这个概念对它不成立" —— 塞进按 InChIKey 索引的表，只会让下游以为拿到了结构。

# 用真实 schema 建一个空库（比走 initialize_database 快，且不带迁移副作用）
make_bare_db <- function() {
  db <- tempfile(fileext = ".db")
  con <- fcmsafety:::get_db_connection(db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  sql <- paste(readLines(fcmsafety:::find_schema_file(), warn = FALSE), collapse = "\n")
  for (s in fcmsafety:::split_sql_statements(sql)) DBI::dbExecute(con, s)
  db
}

make_changes <- function(added) {
  list(added = added,
       modified = added[0, , drop = FALSE],
       removed = added[0, , drop = FALSE],
       total_added = nrow(added),
       total_removed = 0L,
       total_modified = 0L)
}

sample_dropped <- function() {
  data.frame(
    entity_key = c("agent:Acheson process, occupational exposure associated with",
                   "agent:salts of hydrazine"),
    substance_name = c("Acheson process, occupational exposure associated with",
                       "salts of hydrazine"),
    cas_no = c(NA_character_, NA_character_),
    reason = c("no_cas", "no_cas"),
    stringsAsFactors = FALSE
  )
}

test_that("单表设计：InChIKey 为 NULL 的行也能正常写入（完整数据存储）", {
  db <- make_bare_db()
  con <- fcmsafety:::get_db_connection(db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # 有键的行和无键的行都能写入（单一表设计）
  DBI::dbExecute(con, "INSERT INTO chemicals (InChIKey) VALUES ('AAA')")

  batch <- data.frame(
    InChIKey = c("AAA", NA_character_),
    cas_no   = c("50-00-0", NA_character_),
    agent    = c("formaldehyde", "salts of hydrazine"),
    stringsAsFactors = FALSE
  )

  # 现在 NULL InChIKey 允许写入，不会报错
  expect_no_error(DBI::dbWithTransaction(con, DBI::dbAppendTable(con, "iarc", batch)))
  # 两行都进了：合法行 + 无键行（完整数据）
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM iarc")$n, 2L)
})

test_that("split_unassignable 摘掉拿不到键的行，并说清为什么", {
  added <- data.frame(
    cas_no   = c("50-00-0", NA, "121-43-7", NA, "64-17-5"),
    agent    = c("formaldehyde", "Helicobacter pylori (infection with)",
                 "trimethyl borate", "salts of hydrazine", "ethanol"),
    InChIKey = c("AAA", NA, NA, NA, "BBB"),
    stringsAsFactors = FALSE
  )

  out <- fcmsafety:::split_unassignable(make_changes(added), "iarc",
                                        key_col = "cas_no", fallback_col = "agent",
                                        cas_col = "cas_no")

  expect_equal(nrow(out$changes$added), 2L)
  expect_equal(out$changes$total_added, 2L)
  expect_setequal(out$changes$added$agent, c("formaldehyde", "ethanol"))

  expect_equal(nrow(out$dropped), 3L)
  # 有 CAS 却仍拿不到键 -> no_structure_found；连 CAS 都没有 -> no_cas
  expect_equal(out$dropped$reason, c("no_cas", "no_structure_found", "no_cas"))
  # 键沿用与 diff 同一套定义：主键为空时退到兜底列并带前缀
  expect_equal(out$dropped$entity_key,
               c("agent:Helicobacter pylori (infection with)",
                 "121-43-7",
                 "agent:salts of hydrazine"))
})

test_that("本轮跳过补键时记 not_looked_up，不冒充「查不到结构」", {
  # 调用方若显式传 enrich = FALSE，那一轮新增行根本没查过 PubChem，键全空。
  # 若一律记成 no_structure_found，账本会让人以为上游没结构。
  added <- data.frame(
    cas_no   = c(NA, "121-43-7"),
    agent    = c("salts of hydrazine", "trimethyl borate"),
    InChIKey = c(NA, NA),
    stringsAsFactors = FALSE)
  out <- fcmsafety:::split_unassignable(make_changes(added), "iarc",
                                        key_col = "cas_no", fallback_col = "agent",
                                        cas_col = "cas_no", looked_up = FALSE)
  expect_equal(out$dropped$reason, c("no_cas", "not_looked_up"))

  # 查过的那一轮，同一批数据记成 no_structure_found
  out2 <- fcmsafety:::split_unassignable(make_changes(added), "iarc",
                                         key_col = "cas_no", fallback_col = "agent",
                                         cas_col = "cas_no", looked_up = TRUE)
  expect_equal(out2$dropped$reason, c("no_cas", "no_structure_found"))
})

test_that("摘下来的行带着名称和 CAS，账本要给人看得懂", {
  added <- data.frame(
    cas_no   = c(NA, "121-43-7"),
    agent    = c("Helicobacter pylori (infection with)", "trimethyl borate"),
    InChIKey = c(NA, NA),
    stringsAsFactors = FALSE)
  out <- fcmsafety:::split_unassignable(make_changes(added), "iarc",
                                        key_col = "cas_no", fallback_col = "agent",
                                        cas_col = "cas_no", name_col = "agent")
  expect_equal(out$dropped$substance_name,
               c("Helicobacter pylori (infection with)", "trimethyl borate"))
  expect_equal(out$dropped$cas_no, c(NA, "121-43-7"))
})

test_that("全都是可入库行时 split_unassignable 原样返回", {
  added <- data.frame(
    cas_no = "50-00-0", agent = "formaldehyde", InChIKey = "AAA",
    stringsAsFactors = FALSE)
  out <- fcmsafety:::split_unassignable(make_changes(added), "iarc",
                                        key_col = "cas_no", fallback_col = "agent",
                                        cas_col = "cas_no")
  expect_equal(nrow(out$changes$added), 1L)
  expect_equal(nrow(out$dropped), 0L)
})

test_that("record_unassignable 登记成账，重复遇到只累加次数不长新行", {
  db <- make_bare_db()
  d <- sample_dropped()

  fcmsafety:::record_unassigned("iarc", d, db_path = db)
  fcmsafety:::record_unassigned("iarc", d, db_path = db)

  con <- fcmsafety:::get_db_connection(db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  rows <- DBI::dbGetQuery(con, "SELECT * FROM unassigned_entries")

  expect_equal(nrow(rows), 2L)
  expect_equal(sort(rows$seen_count), c(2L, 2L))
  expect_equal(unique(rows$status), "open")
  expect_equal(unique(rows$reason), "no_cas")
  expect_true(all(rows$first_seen_at <= rows$last_seen_at))
})

test_that("同一个键在不同库互不干扰", {
  db <- make_bare_db()
  d <- sample_dropped()
  fcmsafety:::record_unassigned("iarc", d, db_path = db)
  fcmsafety:::record_unassigned("cmr", d, db_path = db)

  con <- fcmsafety:::get_db_connection(db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  rows <- DBI::dbGetQuery(con, "SELECT database_name, COUNT(*) n
                                 FROM unassigned_entries GROUP BY 1 ORDER BY 1")
  expect_equal(rows$database_name, c("cmr", "iarc"))
  expect_equal(rows$n, c(2L, 2L))
})

test_that("后来真入了库的条目，账上标成 resolved", {
  db <- make_bare_db()
  fcmsafety:::record_unassigned("cmr", sample_dropped(), db_path = db)

  fcmsafety:::mark_unassigned_resolved("cmr", c("agent:salts of hydrazine"),
                                       db_path = db)

  con <- fcmsafety:::get_db_connection(db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  rows <- DBI::dbGetQuery(con, "SELECT entity_key, status FROM unassigned_entries")
  got <- setNames(rows$status, rows$entity_key)
  expect_equal(unname(got["agent:salts of hydrazine"]), "resolved")
  # 没入的那条不受影响
  expect_equal(unname(got[["agent:Acheson process, occupational exposure associated with"]]),
               "open")
})

test_that("人工标成 accepted 的条目不会被下一轮覆盖回 open", {
  db <- make_bare_db()
  d <- sample_dropped()
  fcmsafety:::record_unassigned("cmr", d, db_path = db)

  con <- fcmsafety:::get_db_connection(db)
  DBI::dbExecute(con,
    "UPDATE unassigned_entries SET status = 'accepted', notes = '确认结构性收不了'")
  DBI::dbDisconnect(con)

  fcmsafety:::record_unassigned("cmr", d, db_path = db)

  con <- fcmsafety:::get_db_connection(db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  rows <- DBI::dbGetQuery(con, "SELECT status, seen_count, notes FROM unassigned_entries")
  expect_equal(unique(rows$status), "accepted")
  expect_equal(sort(rows$seen_count), c(2L, 2L))       # 次数照涨
  expect_equal(unique(rows$notes), "确认结构性收不了")   # 人工批注不被冲掉
})

test_that("ensure_schema_table 能给老库补上缺的表，且重复调用无害", {
  db <- tempfile(fileext = ".db")
  con <- fcmsafety:::get_db_connection(db)
  DBI::dbExecute(con, "CREATE TABLE chemicals (InChIKey TEXT PRIMARY KEY)")
  DBI::dbDisconnect(con)

  expect_true(fcmsafety:::ensure_schema_table("unassigned_entries", db_path = db))
  expect_false(fcmsafety:::ensure_schema_table("unassigned_entries", db_path = db))

  con <- fcmsafety:::get_db_connection(db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_true("unassigned_entries" %in% DBI::dbListTables(con))
  expect_true("idx_unassigned_entries_key" %in% DBI::dbGetQuery(
    con, "SELECT name FROM sqlite_master WHERE type = 'index'")$name)
})

test_that("ensure_schema_table 对 schema 里没有的表报错，不静默返回", {
  db <- make_bare_db()
  expect_error(fcmsafety:::ensure_schema_table("no_such_table", db_path = db),
               "CREATE TABLE")
})

test_that("摘掉无键行之后，同一批里有键的行能正常写进去", {
  db <- make_bare_db()
  con <- fcmsafety:::get_db_connection(db)
  DBI::dbExecute(con, "INSERT INTO chemicals (InChIKey) VALUES ('AAA')")
  DBI::dbDisconnect(con)

  added <- data.frame(
    cas_no = c("50-00-0", NA_character_),
    agent  = c("formaldehyde", "salts of hydrazine"),
    InChIKey = c("AAA", NA_character_),
    stringsAsFactors = FALSE)

  out <- fcmsafety:::split_unassignable(make_changes(added), "iarc",
                                        key_col = "cas_no", fallback_col = "agent",
                                        cas_col = "cas_no")

  w <- fcmsafety:::write_changes_to_db("iarc", out$changes, "cas_no", "agent",
                                       db_path = db, backup = FALSE)
  expect_equal(w$records_added, 1L)

  con <- fcmsafety:::get_db_connection(db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  rows <- DBI::dbGetQuery(con, "SELECT agent FROM iarc")
  expect_equal(rows$agent, "formaldehyde")
})

# ---- enrich 默认值：四张业务表的 InChIKey 是 NOT NULL，不补键就等于不收 ----
#
# 四库里 cmr / eu_sml 的增量源（官方 clp.xlsx / eu_sml.xlsx）本身不带结构列，
# 键空只能靠查 PubChem 补。默认 enrich = FALSE 时的后果不是"少补点信息"，
# 而是新增行全部无键 -> 全被 split_unassignable 摘掉 -> 一条都进不了库
# （演练实测：cmr 计划新增 820 行，入库 0 行）。所以这里把默认值钉住。

test_that("cmr / eu_sml 的自动更新默认就补结构，否则新增行一条都进不了库", {
  for (fn in list(fcmsafety::update_cmr_auto, fcmsafety::update_eu_sml_auto)) {
    expect_true(isTRUE(formals(fn)$enrich))
  }
})

test_that("四个源的自动更新在 enrich 这一项上行为一致", {
  fns <- list(cmr         = fcmsafety::update_cmr_auto,
              cmr_suspect = fcmsafety::update_cmr_suspect_auto,
              iarc        = fcmsafety::update_iarc_auto,
              eu_sml      = fcmsafety::update_eu_sml_auto)
  defaults <- vapply(fns, function(f) isTRUE(formals(f)$enrich), logical(1))
  expect_true(all(defaults),
              info = paste("未补结构的源：",
                           paste(names(defaults)[!defaults], collapse = ", ")))
})
