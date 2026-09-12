# 已入库但某列不可信的行（data_quality_flags）
#
# 场景：cmr 表 648/649/650 段是 CLP 的 UVCB 章节（石油气、石脑油、干洗溶剂、
# 煤焦油酸馏分）。这类条目没有单一结构，但库里 9 行各带一个单体 InChIKey ——
# "Hydrocarbons, C4-5" 被配成丁烷、"stoddard solvent" 被配成 C8H17BrO3。
#
# 这些行必须留在业务表里（它们是真的法规条目），只是键不可信。所以要有个地方
# 记着"这行的键别用"，而不是删掉或改掉它。

# 用真实 schema 建一个空库（比走 initialize_database 快，且不带迁移副作用）
make_bare_db <- function() {
  db <- tempfile(fileext = ".db")
  con <- fcmsafety:::get_db_connection(db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  sql <- paste(readLines(fcmsafety:::find_schema_file(), warn = FALSE), collapse = "\n")
  for (s in fcmsafety:::split_sql_statements(sql)) DBI::dbExecute(con, s)
  db
}

sample_flags <- function() {
  data.frame(
    entity_key  = c("649-200-00-5", "649-345-00-4"),
    entity_name = c("Hydrocarbons, C4-5; Petroleum gas", "stoddard solvent"),
    detail      = c("该键实际指向 C4H10", "该键实际指向 C8H17BrO3"),
    stringsAsFactors = FALSE)
}

test_that("标记登记进旁路表，重复遇到只累加次数不长新行", {
  db <- make_bare_db()
  d <- sample_flags()

  fcmsafety:::record_data_quality_flag("cmr", d, "unreliable_structure_key",
                                       db_path = db)
  fcmsafety:::record_data_quality_flag("cmr", d, "unreliable_structure_key",
                                       db_path = db)

  con <- fcmsafety:::get_db_connection(db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  rows <- DBI::dbGetQuery(con, "SELECT * FROM data_quality_flags")

  expect_equal(nrow(rows), 2L)
  expect_equal(sort(rows$seen_count), c(2L, 2L))
  expect_equal(unique(rows$status), "open")
  expect_equal(unique(rows$severity), "high")
  expect_true(all(rows$first_seen_at <= rows$last_seen_at))
})

test_that("同一个键的不同问题类型各记一条，互不覆盖", {
  db <- make_bare_db()
  d <- sample_flags()[1, , drop = FALSE]

  fcmsafety:::record_data_quality_flag("cmr", d, "unreliable_structure_key",
                                       db_path = db)
  fcmsafety:::record_data_quality_flag("cmr", d, "missing_cas",
                                       db_path = db)

  con <- fcmsafety:::get_db_connection(db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  flags <- DBI::dbGetQuery(con, "SELECT flag FROM data_quality_flags")$flag
  expect_setequal(flags, c("unreliable_structure_key", "missing_cas"))
})

test_that("同一个键在不同库互不干扰", {
  db <- make_bare_db()
  fcmsafety:::record_data_quality_flag("cmr", sample_flags(),
                                       "unreliable_structure_key", db_path = db)
  fcmsafety:::record_data_quality_flag("iarc", sample_flags(),
                                       "unreliable_structure_key", db_path = db)

  con <- fcmsafety:::get_db_connection(db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  rows <- DBI::dbGetQuery(con, "SELECT database_name, COUNT(*) n
                                 FROM data_quality_flags GROUP BY 1 ORDER BY 1")
  expect_equal(rows$database_name, c("cmr", "iarc"))
  expect_equal(rows$n, c(2L, 2L))
})

test_that("人工标成 accepted 的条目不因再次遇到而改回，批注也不被冲掉", {
  db <- make_bare_db()
  d <- sample_flags()
  fcmsafety:::record_data_quality_flag("cmr", d, "unreliable_structure_key",
                                       db_path = db)

  con <- fcmsafety:::get_db_connection(db)
  DBI::dbExecute(con,
    "UPDATE data_quality_flags SET status = 'accepted', notes = '键留着做对照'")
  DBI::dbDisconnect(con)

  fcmsafety:::record_data_quality_flag("cmr", d, "unreliable_structure_key",
                                       db_path = db)

  con <- fcmsafety:::get_db_connection(db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  rows <- DBI::dbGetQuery(con,
    "SELECT status, seen_count, notes FROM data_quality_flags")
  expect_equal(unique(rows$status), "accepted")
  expect_equal(sort(rows$seen_count), c(2L, 2L))     # 次数照涨
  expect_equal(unique(rows$notes), "键留着做对照")     # 人工批注不被冲掉
})

test_that("问题修掉之后能标成 resolved", {
  db <- make_bare_db()
  fcmsafety:::record_data_quality_flag("cmr", sample_flags(),
                                       "unreliable_structure_key", db_path = db)

  n <- fcmsafety:::resolve_data_quality_flag("cmr", "649-200-00-5",
                                             db_path = db)
  expect_equal(n, 1L)

  con <- fcmsafety:::get_db_connection(db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  rows <- DBI::dbGetQuery(con, "SELECT entity_key, status FROM data_quality_flags")
  got <- stats::setNames(rows$status, rows$entity_key)
  expect_equal(unname(got["649-200-00-5"]), "resolved")
  # 没处理的条目不受影响
  expect_equal(unname(got["649-345-00-4"]), "open")
})

test_that("ensure_schema_table 能给老库补上标记表，且重复调用无害", {
  db <- tempfile(fileext = ".db")
  con <- fcmsafety:::get_db_connection(db)
  DBI::dbExecute(con, "CREATE TABLE chemicals (InChIKey TEXT PRIMARY KEY)")
  DBI::dbDisconnect(con)

  expect_true(fcmsafety:::ensure_schema_table("data_quality_flags", db_path = db))
  expect_false(fcmsafety:::ensure_schema_table("data_quality_flags", db_path = db))

  con <- fcmsafety:::get_db_connection(db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_true("data_quality_flags" %in% DBI::dbListTables(con))
})

# ---- 为什么标记不能写在业务表的 notes 列 -------------------------------------
#
# 这一条是本表存在的全部理由，钉住它，免得后人"顺手"把标记改回 notes 列。

test_that("业务表的 notes 列会被下一次写库抹成 NA，标记放那儿留不住", {
  db <- make_bare_db()
  con <- fcmsafety:::get_db_connection(db)
  DBI::dbExecute(con, "INSERT INTO chemicals (InChIKey) VALUES ('AAA')")
  # 库里已有这一行，notes 带着源数据（CLP 的 ATP 标记）和我们的警告
  DBI::dbExecute(con, "INSERT INTO cmr (InChIKey, index_no, notes)
                       VALUES ('AAA', '649-200-00-5', 'K U / 键不可信')")
  DBI::dbDisconnect(con)

  # 该行被判为变更 -> 写库走 DELETE + INSERT
  changed <- data.frame(
    index_no = "649-200-00-5",
    international_chemical_identification = "Hydrocarbons, C4-5; Petroleum gas",
    InChIKey = "AAA",
    stringsAsFactors = FALSE)
  changes <- list(added = changed[0, , drop = FALSE],
                  modified = changed,
                  removed = changed[0, , drop = FALSE],
                  total_added = 0L, total_removed = 0L, total_modified = 1L)

  fcmsafety:::write_changes_to_db("cmr", changes, "index_no",
                                  db_path = db, backup = FALSE)

  con <- fcmsafety:::get_db_connection(db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  # 旧行被 DELETE、新行 INSERT，所以只有 1 行 —— 且 notes 没了
  notes <- DBI::dbGetQuery(con, "SELECT notes FROM cmr")$notes
  expect_length(notes, 1L)
  # 源 df 里没有 notes 列 -> 被填成 NA。写在里面的标记会静默消失。
  expect_true(is.na(notes))
})

# ---- UVCB 结构键冲突的登记 ----------------------------------------------------

test_that("UVCB 冲突清单是 9 个 CLP Index No，且不含同段的真实物质", {
  keys <- fcmsafety:::.uvcb_structure_conflicts
  expect_length(keys, 9L)
  expect_false(anyDuplicated(keys) > 0)
  expect_true(all(grepl("^6[45][0-9]-[0-9]{3}-00-[0-9X]$", keys)))
  # 同段三行带的是真实结构，按"段"一刀切会误伤 —— 它们必须不在清单里
  expect_false(any(c("650-012-00-0",   # erionite
                     "650-032-00-X",   # cyproconazole
                     "650-056-00-0")   # dibutylbis(pentane-2,4-dionato-O,O')tin
                   %in% keys))
})

test_that("flag_uvcb_structure_keys 登记时说清这个键实际指向什么分子", {
  db <- make_bare_db()
  con <- fcmsafety:::get_db_connection(db)
  DBI::dbExecute(con, "INSERT INTO chemicals (InChIKey, Formula) VALUES ('AAA', 'C4H10')")
  DBI::dbExecute(con, paste(
    "INSERT INTO cmr (InChIKey, index_no, international_chemical_identification)",
    "VALUES ('AAA', '649-200-00-5', 'Hydrocarbons, C4-5; Petroleum gas')"))
  DBI::dbDisconnect(con)

  n <- fcmsafety:::flag_uvcb_structure_keys(db_path = db)
  expect_equal(n, 1L)

  con <- fcmsafety:::get_db_connection(db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  row <- DBI::dbGetQuery(con, "SELECT * FROM data_quality_flags")
  expect_equal(row$database_name, "cmr")
  expect_equal(row$flag, "unreliable_structure_key")
  expect_equal(row$source, "inst/clp_cmr_meta.xlsx")
  expect_match(row$detail, "C4H10")       # 指向哪个分子，写清楚
  expect_match(row$entity_name, "Hydrocarbons")
  # 只登记，不动业务表本身
  expect_equal(DBI::dbGetQuery(con, "SELECT InChIKey FROM cmr")$InChIKey, "AAA")
})

test_that("清单里的键在库里不存在时，登记函数安静返回而不是报错", {
  db <- make_bare_db()
  expect_equal(fcmsafety:::flag_uvcb_structure_keys(db_path = db), 0L)

  con <- fcmsafety:::get_db_connection(db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM data_quality_flags")$n, 0L)
})
