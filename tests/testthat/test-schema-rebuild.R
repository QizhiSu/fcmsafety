# 表重建（按 schema 修列类型）
#
# 背景：iarc.volume 声明成 INTEGER，而 IARC 官方的 Volume 值是多值文本
# （"41, Sup 7, 71, 106"）。线上 859 行里 211 行是整数、648 行是 NULL ——
# 那是旧迁移用 as.integer() 写进去的（多值文本 -> NA）。
#
# 为什么光改写入还不够、还得改列声明：
#   INTEGER 声明列一旦混入文本值，RSQLite 会按**首个非 NA 值的实际类型**决定
#   整列类型。线上 iarc.id = 1 的 volume 正是整数 128，于是整列按整数读，
#   后续任何多值文本都被 sqlite3_column_int64 截成前导数字
#   （"41, Sup 7, 71, 106" -> 41），静默丢值。增量 diff 因此每轮都判 modified，
#   永远收敛不了。声明成 TEXT 后整列恒为文本，读取不再依赖行序。
#
# SQLite 不支持 ALTER TABLE 改列类型，只能建新表搬数据，且必须在 SQL 层做
# （INSERT INTO new SELECT ... FROM old）——经 R 往返时截断就已经发生了。

demo_schema <- function(dir, volume_type = "TEXT") {
  p <- file.path(dir, "schema.sql")
  writeLines(c(
    "CREATE TABLE demo (",
    "    id INTEGER PRIMARY KEY AUTOINCREMENT,",
    "    label TEXT,",
    paste0("    volume ", volume_type),
    ");",
    "",
    "CREATE INDEX idx_demo_label ON demo(label);"
  ), p)
  p
}

# 复刻线上 iarc 的局面：首行 volume 是整数（历史残留），后面才是多值文本。
make_demo_db <- function(path, volume_decl = "INTEGER") {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, paste0(
    "CREATE TABLE demo (id INTEGER PRIMARY KEY AUTOINCREMENT, label TEXT, ",
    "volume ", volume_decl, ")"))
  DBI::dbExecute(con, "CREATE INDEX idx_demo_label ON demo(label)")
  add <- function(label, volume) {
    DBI::dbExecute(con, "INSERT INTO demo (label, volume) VALUES (?, ?)",
                   params = list(label, volume))
  }
  add("boric acid", 41L)
  add("trimethyl borate", "20, Sup 7, 71, 130")
  add("boric acid, crude", "2022 online")
}

test_that("INTEGER 列混入文本后按首行类型整列读取：首行是整数就静默截断", {
  db <- tempfile(fileext = ".db")
  make_demo_db(db, "INTEGER")

  con <- fcmsafety:::get_db_connection(db)
  v <- suppressWarnings(as.character(
    DBI::dbGetQuery(con, "SELECT volume FROM demo ORDER BY id")$volume))
  DBI::dbDisconnect(con)

  # 后两行存进去的是完整文本，读回来只剩前导数字
  expect_equal(v, c("41", "20", "2022"))
})

test_that("旧迁移的 as.integer() 会把多值文本写成 NA —— 线上空值的来历", {
  expect_true(is.na(suppressWarnings(as.integer("41, Sup 7, 71, 106"))))
  expect_true(is.na(suppressWarnings(as.integer("2022 online"))))
  expect_equal(suppressWarnings(as.integer("41")), 41L)
})

test_that("按 schema 重建后，同一批数据读回完整且不再依赖行序", {
  dir <- tempfile("sch"); dir.create(dir)
  sch <- demo_schema(dir, "TEXT")
  db <- tempfile(fileext = ".db")
  make_demo_db(db, "INTEGER")

  n <- fcmsafety:::rebuild_table_from_schema("demo", db_path = db, schema_path = sch)

  con <- fcmsafety:::get_db_connection(db)
  rows <- DBI::dbGetQuery(con, "SELECT * FROM demo ORDER BY id")
  DBI::dbDisconnect(con)

  expect_equal(n, 3L)
  expect_equal(rows$id, c(1L, 2L, 3L))
  expect_equal(rows$label, c("boric acid", "trimethyl borate", "boric acid, crude"))
  expect_equal(rows$volume,
               c("41", "20, Sup 7, 71, 130", "2022 online"))
  expect_equal(class(rows$volume), "character")
})

test_that("重建会保留表上的索引", {
  dir <- tempfile("sch"); dir.create(dir)
  sch <- demo_schema(dir, "TEXT")
  db <- tempfile(fileext = ".db")
  make_demo_db(db, "INTEGER")

  fcmsafety:::rebuild_table_from_schema("demo", db_path = db, schema_path = sch)

  con <- fcmsafety:::get_db_connection(db)
  idx <- DBI::dbGetQuery(con, paste(
    "SELECT name FROM sqlite_master WHERE type = 'index' AND tbl_name = 'demo'"))$name
  DBI::dbDisconnect(con)

  expect_true("idx_demo_label" %in% idx)
})

test_that("重建是幂等的：再跑一次行数与内容不变", {
  dir <- tempfile("sch"); dir.create(dir)
  sch <- demo_schema(dir, "TEXT")
  db <- tempfile(fileext = ".db")
  make_demo_db(db, "INTEGER")

  fcmsafety:::rebuild_table_from_schema("demo", db_path = db, schema_path = sch)
  n2 <- fcmsafety:::rebuild_table_from_schema("demo", db_path = db, schema_path = sch)

  con <- fcmsafety:::get_db_connection(db)
  v <- DBI::dbGetQuery(con, "SELECT volume FROM demo ORDER BY id")$volume
  tabs <- DBI::dbListTables(con)
  DBI::dbDisconnect(con)

  expect_equal(n2, 3L)
  expect_equal(v, c("41", "20, Sup 7, 71, 130", "2022 online"))
  expect_false("demo__rebuild" %in% tabs)
})

test_that("schema 里没有这张表时报错，不静默返回", {
  dir <- tempfile("sch"); dir.create(dir)
  sch <- demo_schema(dir, "TEXT")
  db <- tempfile(fileext = ".db")
  make_demo_db(db, "INTEGER")

  expect_error(
    fcmsafety:::rebuild_table_from_schema("nonexistent", db_path = db, schema_path = sch),
    "CREATE TABLE"
  )
})
