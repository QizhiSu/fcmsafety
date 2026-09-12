# 键归一化：同一实体在两份数据里的书写方式不同，按键匹配时不能算成两条。
#
# 真实案例（2026-09-11 实测）：cmr_suspect 库内 isoproturon 的名字是多行
# （"isoproturon (ISO);" + 空行 + "3-(4-isopropylphenyl)-1,1-dimethylurea"），
# 官方 CLP 导出把同一物质写成单行、用空格分隔。主键是 substance_name，两份
# 字符串不相等 → 同一条物质既判 removed 又判 added，58 行假删除把整库更新
# 挡在安全阀外（removed > 0 时非交互模式一律 Cancelled）。
#
# 键归一化只做两件事：统一换行符、把连续空白折成单个空格。
# 刻意不做大小写 / 标点归一 —— 那会误合真正不同的物质（见最后一组用例）。

test_that("key_of_df 把键值里的换行与连续空白折成单空格", {
  df <- data.frame(
    substance_name = c(
      "isoproturon (ISO);\r\n\r\n3-(4-isopropylphenyl)-1,1-dimethylurea",
      "  boric acid  ",
      "1,4-dioxane"
    ),
    stringsAsFactors = FALSE
  )

  expect_equal(
    fcmsafety:::key_of_df(df, "substance_name"),
    c("isoproturon (ISO); 3-(4-isopropylphenyl)-1,1-dimethylurea",
      "boric acid",
      "1,4-dioxane")
  )
})

test_that("key_of_df 对兜底键做同样的归一", {
  df <- data.frame(
    cas_no = c(NA_character_, NA_character_, "10026-24-1"),
    agent = c("Cobalt sulfate and other\r\nsoluble cobalt(II) salts",
              "Progestins  ",
              "Aflatoxin M1"),
    stringsAsFactors = FALSE
  )

  expect_equal(
    fcmsafety:::key_of_df(df, "cas_no", "agent"),
    c("agent:Cobalt sulfate and other soluble cobalt(II) salts",
      "agent:Progestins",
      "10026-24-1")
  )
})

test_that("纯空白键归一后视为空键", {
  df <- data.frame(cas_no = c("  ", "", "71-43-2"), stringsAsFactors = FALSE)

  expect_equal(fcmsafety:::key_of_df(df, "cas_no"),
               c(NA_character_, NA_character_, "71-43-2"))
})

test_that("键归一化不做大小写与标点归一", {
  df <- data.frame(substance_name = c("Boric acid", "boric acid", "boric-acid"),
                   stringsAsFactors = FALSE)
  k <- fcmsafety:::key_of_df(df, "substance_name")

  expect_false(k[1] == k[2])
  expect_false(k[2] == k[3])
})

test_that("diff_incremental 把多行写法与单行写法认作同一条物质", {
  cur <- data.frame(
    substance_name = "isoproturon (ISO);\r\n\r\n3-(4-isopropylphenyl)-1,1-dimethylurea",
    cas_no = NA_character_,
    stringsAsFactors = FALSE
  )
  new <- data.frame(
    substance_name = "isoproturon (ISO); 3-(4-isopropylphenyl)-1,1-dimethylurea",
    cas_no = NA_character_,
    stringsAsFactors = FALSE
  )

  ch <- fcmsafety:::diff_incremental(
    new, cur,
    key_col = "substance_name", fallback_col = "cas_no",
    content_cols = NULL, cas_col = "cas_no"
  )

  expect_equal(ch$total_added, 0L)
  expect_equal(ch$total_removed, 0L)
  # 键对上了，但名字的书写方式仍然不同：内容比对把多行单元格当集合处理
  # （按行排序），不会把换行折成空格，所以这一轮判 modified。写库会把库内
  # 名字改写成官方写法，下一轮 diff 即干净。
  expect_equal(ch$total_modified, 1L)
})
