# 建库入口 initialize_database() 的原子替换
#
# 回归对象：`initialize_database(force_recreate = TRUE)` 曾在**连接未断开**时调用
# migrate_xlsx_to_sqlite()。原因：先 get_db_connection() 并注册 on.exit(断连)，
# 再 return(migrate_xlsx_to_sqlite(...)) —— return() 会先求值迁移函数，而 on.exit
# 要等本函数真正退出才执行。Windows 上迁移最后一步的 file.remove(db_path) 会因
# 文件被连接占用而报"拒绝访问"，原子交换必定失败：库不会被破坏，但也永远不会
# 被重建。修法是把重建分支提到建连接之前（结构性避免，不靠"记得先断开"）。

test_that("force_recreate = TRUE 能把已有库原子替换掉", {
  wd <- tempfile("wd"); dir.create(wd)
  # 迁移只在源目录里找 xlsx。空目录 → 各表跳过，但建 schema 与交换步骤照走。
  dir.create(file.path(wd, "inst"))
  # schema 得手工放一份：测试期间工作目录是 tests/testthat，取不到包里的 inst/
  file.copy(fcmsafety:::find_schema_file(), file.path(wd, "inst"))

  db <- file.path(wd, "fcmsafety.db")
  # 先造一个"已存在且非空"的目标库，逼出覆盖已存在文件那条路径
  con <- fcmsafety:::get_db_connection(db)
  DBI::dbExecute(con, "CREATE TABLE probe (x INTEGER)")
  DBI::dbDisconnect(con)

  old <- setwd(wd)
  ok <- suppressMessages(
    fcmsafety::initialize_database(force_recreate = TRUE, db_path = db))
  setwd(old)

  expect_true(ok)

  con <- fcmsafety:::get_db_connection(db)
  tabs <- DBI::dbListTables(con)
  DBI::dbDisconnect(con)

  expect_true("chemicals" %in% tabs)                    # 新 schema 已就位
  expect_false("probe" %in% tabs)                       # 旧库已被换掉
  expect_false("fcmsafety_tmp.db" %in% list.files(wd))  # 临时库没留残骸
})

test_that("库已就绪且 force_recreate = FALSE 时不做任何事", {
  wd <- tempfile("wd"); dir.create(wd)
  dir.create(file.path(wd, "inst"))
  file.copy(fcmsafety:::find_schema_file(), file.path(wd, "inst"))

  db <- file.path(wd, "fcmsafety.db")
  old <- setwd(wd)
  ok <- suppressMessages(fcmsafety::initialize_database(db_path = db))
  setwd(old)
  expect_true(ok)

  # 第二次调用应短路返回 TRUE，且不再产生备份文件
  n_bak_before <- length(list.files(wd, pattern = "\\.bak_"))
  old <- setwd(wd)
  ok2 <- suppressMessages(fcmsafety::initialize_database(db_path = db))
  setwd(old)

  expect_true(ok2)
  expect_equal(length(list.files(wd, pattern = "\\.bak_")), n_bak_before)
})
