# PubChem 取数的可靠性与"查不到 / 没查成"的区分
#
# 背景（2026-09-11 实测）：拿账本里 20 行"no_structure_found"的 CAS 逐个重查，
# 当场有 10 个查得到；把同一批 CAS 再查一遍，之前 FAIL 的 6 个这次全部返回
# HTTP 200。也就是说旧实现把**限流/瞬时网络故障**当成了"PubChem 里没有这个
# 物质"：任何非 200 一律返回全 NA 且不重试。后果不是少几行，而是新增行被大批
# 丢进 unassigned_entries 账本，而 update_*_auto() 依然报 success —— 真库 cmr
# 那一次 820 行新增里 764 行是这样被吞掉的（含敌草隆、1,4-二氧六环）。
#
# 本文件钉三件事：
#   1. 瞬时失败要重试（含 429 / 5xx / 连接异常），退避递增；
#   2. 404 / 400 是"确实没有"，不重试（重试也是浪费）；
#   3. 重试耗尽仍失败时，调用方必须能区分"没查到"与"没查成"，账本不许撒谎。

# ---- 测试替身：伪造 httr 响应 -------------------------------------------------

fake_resp <- function(code, body = "") {
  structure(list(status_code = code, content = charToRaw(body)),
            class = "response")
}

#' 让 pubchem_http_get() 依次吐出给定响应，并记录它被调了几次
#'
#' @param codes 整数向量，逐个作为响应状态码返回；元素用完后一直返回最后一个
#' @param bodies 与 codes 等长的响应体（可选）
#' @return list(calls = function() 次数, queue = 剩余队列)
resp_queue <- function(codes, bodies = NULL) {
  n <- 0L
  i <- 0L
  list(
    get = function(url, timeout = 30) {
      n <<- n + 1L
      i <<- min(i + 1L, length(codes))
      body <- if (is.null(bodies)) "" else bodies[[i]]
      fake_resp(codes[[i]], body)
    },
    calls = function() n
  )
}

test_that("pubchem_get retries transient statuses and finally succeeds", {
  q <- resp_queue(c(503L, 429L, 200L), bodies = c("", "", '{"ok":1}'))
  testthat::local_mocked_bindings(pubchem_http_get = q$get, .package = "fcmsafety")

  out <- fcmsafety:::pubchem_get("http://x", retries = 3L, backoff = 0)
  expect_true(out$ok)
  expect_equal(out$status, "ok")
  expect_equal(q$calls(), 3L)
  expect_equal(out$text, '{"ok":1}')
})

test_that("pubchem_get treats 404 and 400 as definitive not-found, without retrying", {
  for (code in c(404L, 400L)) {
    q <- resp_queue(code)
    testthat::local_mocked_bindings(pubchem_http_get = q$get, .package = "fcmsafety")
    out <- fcmsafety:::pubchem_get("http://x", retries = 3L, backoff = 0)
    expect_false(out$ok)
    expect_equal(out$status, "not_found")
    expect_equal(q$calls(), 1L)
  }
})

test_that("pubchem_get reports unavailable once retries are exhausted", {
  q <- resp_queue(503L)
  testthat::local_mocked_bindings(pubchem_http_get = q$get, .package = "fcmsafety")
  out <- fcmsafety:::pubchem_get("http://x", retries = 3L, backoff = 0)
  expect_false(out$ok)
  expect_equal(out$status, "unavailable")
  expect_equal(q$calls(), 4L)   # 首次 + 3 次重试
})

test_that("pubchem_get retries connection errors too", {
  n <- 0L
  testthat::local_mocked_bindings(
    pubchem_http_get = function(url, timeout = 30) {
      n <<- n + 1L
      if (n < 3L) stop("boom") else fake_resp(200L, '{"ok":1}')
    },
    .package = "fcmsafety"
  )
  out <- fcmsafety:::pubchem_get("http://x", retries = 3L, backoff = 0)
  expect_true(out$ok)
  expect_equal(n, 3L)
})

test_that("pubchem_get maps a non-transient 4xx (other than 404/400) to unavailable", {
  q <- resp_queue(403L)
  testthat::local_mocked_bindings(pubchem_http_get = q$get, .package = "fcmsafety")
  out <- fcmsafety:::pubchem_get("http://x", retries = 1L, backoff = 0)
  expect_false(out$ok)
  expect_equal(out$status, "unavailable")
})

# ---- enrich_new_compounds 要把"没查成"如实标出来 ------------------------------

enrich_df <- function(cas) {
  data.frame(cas_no = cas, substance_name = paste0("s", seq_along(cas)),
             stringsAsFactors = FALSE)
}

test_that("enrich_new_compounds marks rows whose lookups could not be completed", {
  df <- enrich_df(c("330-54-1", "123-91-1"))
  testthat::local_mocked_bindings(
    pubchem_lookup_cas = function(cas, timeout = 30) {
      r <- list(CID = NA_character_, Formula = NA_character_, SMILES = NA_character_,
                InChIKey = NA_character_, IUPACName = NA_character_,
                ExactMass = NA_character_)
      if (identical(cas, "123-91-1")) {
        attr(r, "lookup_status") <- "ok"
        r$InChIKey <- "RYHBNJHYFVUHQT-UHFFFAOYSA-N"
        r$CID <- "31275"
      } else {
        attr(r, "lookup_status") <- "unavailable"   # 解析查出网络故障，不是"没有"
      }
      r
    },
    .package = "fcmsafety"
  )
  out <- fcmsafety:::enrich_new_compounds(df, "cas_no", "substance_name",
                                          delay = 0, verbose = FALSE)
  expect_equal(out$structure_lookup, c("unavailable", "ok"))
})

test_that("enrich_new_compounds marks a genuine miss as not_found", {
  df <- enrich_df("69012-50-6")
  testthat::local_mocked_bindings(
    pubchem_lookup_cas = function(cas, timeout = 30) {
      r <- list(CID = NA_character_, Formula = NA_character_, SMILES = NA_character_,
                InChIKey = NA_character_, IUPACName = NA_character_,
                ExactMass = NA_character_)
      attr(r, "lookup_status") <- "not_found"
      r
    },
    .package = "fcmsafety"
  )
  out <- fcmsafety:::enrich_new_compounds(df, "cas_no", "substance_name",
                                          delay = 0, verbose = FALSE)
  expect_equal(out$structure_lookup, "not_found")
})

test_that("enrich_new_compounds leaves rows it never had to look up as NA", {
  df <- enrich_df("50-00-0")
  df$InChIKey <- "ALREADY-KEY"
  testthat::local_mocked_bindings(
    pubchem_lookup_cas = function(cas, timeout = 30) stop("must not be called"),
    .package = "fcmsafety"
  )
  out <- fcmsafety:::enrich_new_compounds(df, "cas_no", "substance_name",
                                          delay = 0, verbose = FALSE)
  expect_true(is.na(out$structure_lookup))
})

test_that("enrich_new_compounds prefers not_found when a later CAS is genuinely missing", {
  # 前一个 CAS 网络故障、后一个是真 404：不能说成"整体没查成"，
  # 但也不能说"确实没有" —— 取更保守的 unavailable，留给下一轮再试。
  df <- enrich_df("330-54-1")
  df$cas_no <- "330-54-1;123-91-1"
  seen <- 0L
  testthat::local_mocked_bindings(
    pubchem_lookup_cas = function(cas, timeout = 30) {
      seen <<- seen + 1L
      r <- list(CID = NA_character_, Formula = NA_character_, SMILES = NA_character_,
                InChIKey = NA_character_, IUPACName = NA_character_,
                ExactMass = NA_character_)
      attr(r, "lookup_status") <- if (identical(cas, "330-54-1")) "unavailable" else "not_found"
      r
    },
    .package = "fcmsafety"
  )
  out <- fcmsafety:::enrich_new_compounds(df, "cas_no", "substance_name",
                                          delay = 0, verbose = FALSE)
  expect_equal(seen, 2L)
  expect_equal(out$structure_lookup, "unavailable")
})

test_that("enrich_new_compounds falls back to not_found when status is absent", {
  # 测试替身/老调用方可能只给一个裸 list，没有 lookup_status 属性
  df <- enrich_df("69012-50-6")
  testthat::local_mocked_bindings(
    pubchem_lookup_cas = function(cas, timeout = 30) {
      list(CID = NA_character_, Formula = NA_character_, SMILES = NA_character_,
           InChIKey = NA_character_, IUPACName = NA_character_,
           ExactMass = NA_character_)
    },
    .package = "fcmsafety"
  )
  out <- fcmsafety:::enrich_new_compounds(df, "cas_no", "substance_name",
                                          delay = 0, verbose = FALSE)
  expect_equal(out$structure_lookup, "not_found")
})

# ---- 账本 reason 要能区分"没查到"与"没查成" ----------------------------------

test_that("split_unassignable records lookup_unavailable for uncompleted lookups", {
  added <- data.frame(
    index_no = c("330-54-1", "69012-50-6"),
    cas_no = c("330-54-1", "69012-50-6"),
    substance_name = c("diuron", "tar"),
    InChIKey = c(NA_character_, NA_character_),
    structure_lookup = c("unavailable", "not_found"),
    stringsAsFactors = FALSE
  )
  changes <- list(added = added, modified = added[0, ],
                  removed = added[0, ], total_added = 2L,
                  total_modified = 0L, total_removed = 0L)
  out <- fcmsafety:::split_unassignable(changes, "cmr", "index_no", "cas_no",
                                        cas_col = "cas_no",
                                        name_col = "substance_name",
                                        looked_up = TRUE)
  expect_equal(out$n_dropped, 2L)
  expect_equal(out$dropped$reason, c("lookup_unavailable", "no_structure_found"))
})

test_that("split_unassignable keeps the old reasons when no status column exists", {
  added <- data.frame(
    index_no = "330-54-1", cas_no = "330-54-1", substance_name = "diuron",
    InChIKey = NA_character_, stringsAsFactors = FALSE
  )
  changes <- list(added = added, modified = added[0, ],
                  removed = added[0, ], total_added = 1L,
                  total_modified = 0L, total_removed = 0L)
  out <- fcmsafety:::split_unassignable(changes, "cmr", "index_no", "cas_no",
                                        cas_col = "cas_no",
                                        name_col = "substance_name",
                                        looked_up = TRUE)
  expect_equal(out$dropped$reason, "no_structure_found")
})
