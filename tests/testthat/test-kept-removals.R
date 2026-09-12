# 上游已删、但人工裁定保留的行。
#
# 背景：有些行源里确实找不到（按 diff 键），但删了就是真丢 —— 比如
# 607-230-00-6 "2-ethylhexanoic acid and its salts" 的 CAS 是 "-"，
# 新形式进不了库；iarc 的 Progestins / Cobalt sulfate 改名后成了无 CAS
# 的组条目，同样进不来。把这些行删掉，物质就从库里彻底消失。
#
# 保留它们之后，每次更新都会报 removed，而 removed > 0 会拦下整个人工确认
# 闸门 —— 等于这四个库永远跑不动。所以要有一条"已知情"通道：账本里
# flag = 'upstream_removed_kept' 且 status = 'accepted' 的键，从 removed
# 里剔除，既不删、也不拦。

make_changes <- function(removed) {
  list(added = data.frame(index_no = character(0), InChIKey = character(0),
                          stringsAsFactors = FALSE),
       modified = data.frame(index_no = character(0), InChIKey = character(0),
                             stringsAsFactors = FALSE),
       removed = removed,
       total_added = 0L, total_removed = nrow(removed), total_modified = 0L)
}

test_that("空白名单不改变 diff 结果", {
  ch <- make_changes(data.frame(index_no = "A", stringsAsFactors = FALSE))
  out <- fcmsafety:::drop_kept_removals(ch, character(0), "index_no")
  expect_equal(out$total_removed, 1L)
  expect_equal(nrow(out$removed), 1L)
})

test_that("白名单里的键从 removed 剔除，计数同步改写", {
  ch <- make_changes(data.frame(
    index_no = c("607-230-00-6", "022-006-00-2"), stringsAsFactors = FALSE))
  out <- fcmsafety:::drop_kept_removals(ch, "607-230-00-6", "index_no")
  expect_equal(out$total_removed, 1L)
  expect_equal(out$removed$index_no, "022-006-00-2")
})

test_that("白名单不命中时原样返回", {
  ch <- make_changes(data.frame(index_no = "A", stringsAsFactors = FALSE))
  out <- fcmsafety:::drop_kept_removals(ch, "B", "index_no")
  expect_equal(out$total_removed, 1L)
})

test_that("键为空的行靠兜底列匹配 —— iarc 的组条目没有 CAS", {
  ch <- make_changes(data.frame(
    cas_no = c(NA_character_, "022-006-00-2"),
    agent = c("Progestins", "titanium dioxide"),
    stringsAsFactors = FALSE))
  # 兜底键带列名前缀：key_of_df() 把无主键的行的键拼成 "<兜底列>:<值>"，
  # 账本里登记的也必须是这个形式，否则永远匹配不上。
  expect_equal(fcmsafety:::key_of_df(ch$removed, "cas_no", "agent")[1],
               "agent:Progestins")
  out <- fcmsafety:::drop_kept_removals(ch, "agent:Progestins", "cas_no", "agent")
  expect_equal(out$total_removed, 1L)
  expect_equal(out$removed$agent, "titanium dioxide")
})

test_that("剔除全部后 total_removed 归零 —— 闸门才会放行", {
  ch <- make_changes(data.frame(index_no = c("A", "B"), stringsAsFactors = FALSE))
  out <- fcmsafety:::drop_kept_removals(ch, c("A", "B"), "index_no")
  expect_equal(out$total_removed, 0L)
  expect_equal(nrow(out$removed), 0L)
})

test_that("query_kept_removals 只认 status = accepted 且 flag 匹配的行", {
  db <- tempfile(fileext = ".db")
  on.exit(unlink(db), add = TRUE)
  con <- fcmsafety:::get_db_connection(db)
  fcmsafety:::ensure_schema_table("data_quality_flags", db_path = db)
  DBI::dbExecute(con, paste(
    "INSERT INTO data_quality_flags",
    "(database_name, entity_key, flag, status) VALUES",
    "('cmr_suspect', '607-230-00-6', 'upstream_removed_kept', 'accepted'),",
    "('iarc', 'Progestins', 'upstream_removed_kept', 'open'),",
    "('iarc', '1402-68-2', 'unreliable_structure_key', 'accepted')"))
  DBI::dbDisconnect(con)

  expect_equal(fcmsafety:::query_kept_removals("cmr_suspect", db_path = db),
               "607-230-00-6")
  # open 表示"还没判断"，不能当已知情放行
  expect_equal(fcmsafety:::query_kept_removals("iarc", db_path = db), character(0))
  expect_equal(fcmsafety:::query_kept_removals("eu_sml", db_path = db), character(0))
})

test_that("账本表不存在时 query_kept_removals 返回空而不是报错", {
  db <- tempfile(fileext = ".db")
  on.exit(unlink(db), add = TRUE)
  con <- fcmsafety:::get_db_connection(db)
  DBI::dbDisconnect(con)
  expect_equal(fcmsafety:::query_kept_removals("cmr", db_path = db), character(0))
})
