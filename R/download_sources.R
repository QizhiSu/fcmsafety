# =============================================================================
# 各法规源的原始数据下载（纯 HTTP，不用浏览器）
#
# 本文件只负责"把官方原始文件抓下来存成 xlsx"，不解析、不入库 ——
# 解析与入库在 update_other_dbs.R / auto_update_svhc.R。
#
# 四个源与各自的下载函数：
#   SVHC 候选清单  download_svhc()      直连 ECHA CHEM 的 fullExport 端点
#   CLP 分类附件   download_clp()       ECHA 静态文件；失败时退到浏览器方案
#   EU 10/2011    download_eu_sml()    EUR-Lex / CELLAR，含版本发现逻辑
#   IARC 分类     download_iarc()      IARC 官网 Monographs 页（JS 里取数组）
#
# ⚠️ 本文件是**最易失效**的一处：ECHA 启用 Azure WAF 后旧端点返回 403，
#    官方页面结构一变解析就废。这里留了多条回退路径（CELAR SPARQL 备用、
#    浏览器下载、inst/ 下手动放文件），改代码前先看对应函数的注释说明
#    哪条路是当前可用的。
# =============================================================================

#' Download raw source data for the fcmsafety databases (pure HTTP, no browser)
#'
#' These functions fetch the authoritative source data behind the databases
#' (SVHC / CLP / IARC / EU 10/2011) and write the raw data to xlsx files. They are
#' pure HTTP (or static-HTML) implementations and do not require a browser.
#'
#' @param out Destination path for the downloaded xlsx file.
#'
#' @return The destination path, invisibly.
#' @name download_sources
#' @encoding UTF-8
NULL

# ---- SVHC 候选清单（ECHA CHEM fullExport 端点） ------------------------------

#' @rdname download_sources
#' @export
#' @encoding UTF-8
download_svhc <- function(out = paste0(getwd(), "/inst/svhc.xlsx")) {
  # SVHC candidate list (ECHA CHEM, official).
  #
  # The legacy Liferay POST endpoint on echa.europa.eu (candidate-list-table
  # exportResults) is blocked by Azure WAF. ECHA has migrated the Candidate
  # List data to ECHA CHEM (the legacy dataset is only maintained until
  # July 2026). The ECHA CHEM endpoint below returns the authoritative xlsx
  # over plain HTTP with NO session cookie required -- but only when the
  # request carries `Accept: application/json` (or `*/*`). Requesting the
  # xlsx MIME type itself triggers a 406 Not Acceptable / empty body.
  url <- "https://chem.echa.europa.eu/api-obligation-list/v1/candidateList/fullExport"
  resp <- httr::GET(
    url,
    httr::add_headers(Accept = "application/json"),
    httr::write_disk(out, overwrite = TRUE),
    httr::timeout(60),
    httr::user_agent("fcmsafety R package")
  )
  # Deletion protection: a 406/error body or an empty file must not be left
  # behind as a "successful" download. xlsx files are zip archives and always
  # start with the "PK" magic bytes.
  ok <- identical(httr::status_code(resp), 200L) &&
    file.exists(out) && file.size(out) > 0 &&
    identical(rawToChar(readBin(file(out, "rb"), "raw", 2L)), "PK")
  if (!ok) {
    unlink(out)
    stop("SVHC: download failed (HTTP ", httr::status_code(resp), ").")
  }
  invisible(out)
}

# ---- CLP 分类附件（ECHA 静态文件，失败退到浏览器方案） -----------------------
#
# 与 SVHC 不同，CLP 附件走的是静态文件夹，可直连；若被 WAF 拦（403）则
# download_clp_via_browser() 用浏览器打开页面让用户手动存，再 check_xlsx() 校验。

#' @rdname download_sources
#' @export
#' @encoding UTF-8
download_clp <- function(out = paste0(getwd(), "/inst/clp.xlsx")) {
  # CLP regulation Table 3 of Annex VI (latest). ECHA publishes a single
  # official xlsx export on the page (annex_vi_clp_table_atpNN_en.xlsx).
  #
  # ECHA has enabled an Azure WAF that returns 403 + a JS challenge to plain
  # HTTP requests. Plain HTTP works only when the WAF is not flagging us, so:
  #   1. try plain HTTP (probe the page, find the xlsx link, download);
  #   2. on any failure, retry through a headless browser
  #      (tools/echacl_download.cjs) which executes the JS challenge, gets the
  #      WAF cookie and downloads the same official xlsx to `out`.
  url <- "https://echa.europa.eu/information-on-chemicals/annex-vi-to-clp"

  http_ok <- tryCatch({
    download_clp_http(url, out)
    TRUE
  }, error = function(e) {
    message("   CLP: plain-HTTP download failed (", conditionMessage(e),
            "); retrying through a headless browser ...")
    FALSE
  })

  if (!http_ok) {
    tryCatch({
      download_clp_via_browser(out)
    }, error = function(e) {
      stop("CLP: ECHA download failed both via HTTP and headless browser. ",
           "Last browser error: ", conditionMessage(e), ". ",
           "Please download the Annex VI table manually and place it at ", out,
           call. = FALSE)
    })
  }
  invisible(out)
}

#' Plain-HTTP half of download_clp() (kept as its own function so the browser
#' fallback can catch its errors cleanly).
#' @noRd
download_clp_http <- function(url, out) {
  # ECHA's Azure WAF returns 403 + JS challenge to automated requests. Probe
  # the page first so we can give a clear error instead of the cryptic
  # "cannot open the connection" from rvest::read_html().
  probe <- httr::GET(url, httr::user_agent("fcmsafety R package"),
                     httr::timeout(60))
  code <- httr::status_code(probe)
  if (!identical(code, 200L)) {
    body <- tryCatch(httr::content(probe, as = "text", encoding = "UTF-8"),
                     error = function(e) "")
    if (grepl("Azure WAF|challenge|appgw_azwaf", body, ignore.case = TRUE)) {
      stop("ECHA page is blocked by Azure WAF (HTTP 403 JS challenge).")
    }
    stop("ECHA page returned HTTP ", code, ".")
  }

  url_list <- url %>%
    rvest::read_html() %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href") %>%
    dplyr::as_tibble() %>%
    dplyr::filter(stringr::str_detect(value, "annex_vi_clp"))

  # Deletion protection: no matching link means the ECHA page structure changed.
  if (nrow(url_list) == 0L) {
    stop("CLP: no annex_vi_clp download link found on the ECHA page; the page structure may have changed.")
  }
  file_url <- paste0("https://echa.europa.eu",
                     dplyr::pull(url_list, value)[nrow(url_list)])

  # Download and verify it is a real xlsx (zip archive, "PK" magic bytes) rather
  # than an ECHA error/HTML page.
  resp <- httr::GET(
    file_url,
    httr::write_disk(out, overwrite = TRUE),
    httr::timeout(60),
    httr::user_agent("fcmsafety R package")
  )
  if (!identical(httr::status_code(resp), 200L)) {
    unlink(out)
    stop("CLP: file download failed (HTTP ", httr::status_code(resp), ").")
  }
  check_xlsx(out, "CLP")
  invisible(out)
}

#' Verify a freshly downloaded file is a real xlsx (zip archive starting with
#' the "PK" magic bytes, non-empty). Used as deletion protection so a WAF
#' challenge/error page is never left behind as a "successful" download.
#' @noRd
check_xlsx <- function(path, what) {
  if (!file.exists(path) || file.size(path) < 100) {
    unlink(path)
    stop(what, ": download produced no usable file.")
  }
  ok <- identical(rawToChar(readBin(file(path, "rb"), "raw", 2L)), "PK")
  if (!ok) {
    unlink(path)
    stop(what, ": downloaded file is not a valid xlsx (WAF/error page?).")
  }
  invisible(path)
}

#' Browser-based fallback for download_clp(): run tools/echacl_download.cjs
#' (Node + playwright-core + a local Chrome) which passes the Azure WAF JS
#' challenge and downloads the official Annex VI xlsx to `out`.
#'
#' Machine-specific locations can be overridden with environment variables:
#'   FCMSAFETY_NODE_BIN   node.exe path
#'   FCMSAFETY_CHROME     Chrome executable path (passed to the node script)
#' @noRd
download_clp_via_browser <- function(out) {
  node <- Sys.getenv(
    "FCMSAFETY_NODE_BIN",
    unset = "C:/Users/13432/.workbuddy/binaries/node/versions/22.22.2-2/node.exe"
  )
  if (!file.exists(node)) {
    stop("node.exe not found at ", node, " (set FCMSAFETY_NODE_BIN to override).")
  }
  script <- file.path(getwd(), "tools", "echacl_download.cjs")
  if (!file.exists(script)) {
    stop("browser download script not found: ", script)
  }
  out_abs <- normalizePath(out, winslash = "/", mustWork = FALSE)
  msg <- suppressWarnings(system2(node, c(script, out_abs), stdout = TRUE,
                                  stderr = TRUE))
  code <- attr(msg, "status")
  if (!is.null(code) && code != 0L) {
    stop("browser download script exited with status ", code, ": ",
         paste(msg, collapse = " | "))
  }
  check_xlsx(out, "CLP")
  invisible(out)
}

# ---- EU 10/2011（EUR-Lex 页面 / CELLAR SPARQL 两条路） ----------------------
#
# 这块最长也最脆：先试 EUR-Lex 的版本列表页；页面改版时退到 CELLAR 的
# SPARQL 接口（fetch_cellar_sparql / fetch_cellar_consolidated）；再不行
# 直接抓 EUR-Lex 的 HTML 表格自己解析（parse_eu_sml_page）。改之前先确认
# 哪条路当前还能通——不是所有分支都还活着，留着是为了哪天前面挂了能顶上。

#' @rdname download_sources
#' @export
#' @encoding UTF-8
download_eu_sml <- function(out = paste0(getwd(), "/inst/eu10_2011.xlsx")) {
  # EU 10/2011 positive list (consolidated version), auto-discovered.
  #
  # Two-step flow:
  #   1. Discover the latest consolidated CELEX (02011R0010-YYYYMMDD); try
  #      versions newest-first.
  #   2. Download the consolidated text and parse its two tables
  #      (SML positive list + group restrictions).
  #
  # Two independent providers are used, Cellar first and EUR-Lex second:
  #   * Cellar (Publications Office, publications.europa.eu) is the machine-
  #     readable backbone behind EUR-Lex. Version discovery goes through its
  #     SPARQL endpoint (cdm:act_consolidated_based_on_resource_legal) and the
  #     consolidated text is fetched as XHTML (Accept: application/xhtml+xml).
  #     This provider has no AWS WAF and keeps working when the EUR-Lex web
  #     front-end is degraded.
  #   * EUR-Lex (eur-lex.europa.eu) is the historical provider: discover the
  #     versions on the base act page and download the HTML. It is protected by
  #     an AWS WAF that intermittently answers with a JS challenge, and its
  #     `?uri=CELEX:` front-end can be temporarily unavailable, so it is only
  #     used when Cellar fails.
  # parse_eu_sml_page() handles both text layouts.
  #
  # Discovery: Cellar SPARQL, falling back to the EUR-Lex base act page.
  # Fetch:      Cellar XHTML, falling back to the EUR-Lex HTML endpoint.

  # ---- 1. Discover the latest consolidated version ----
  versions <- tryCatch(
    discover_eu_sml_versions_cellar("32011R0010"),
    error = function(e) {
      message("   Cellar version discovery failed (", conditionMessage(e),
              "); falling back to the EUR-Lex base act page ...")
      base_url <- "https://eur-lex.europa.eu/legal-content/EN/TXT/?uri=CELEX:32011R0010"
      discover_eu_sml_versions(base_url)
    }
  )

  # ---- 2. Download the newest fetchable consolidated text ----
  parsed <- NULL
  for (ver in versions) {
    message("   Fetching consolidated version ", ver, " ...")
    # Primary provider: Cellar consolidated XHTML.
    page_html <- tryCatch(fetch_cellar_consolidated(ver), error = function(e) {
      message("     Cellar fetch failed: ", conditionMessage(e))
      NULL
    })
    # Fallback provider: EUR-Lex consolidated HTML.
    if (is.null(page_html)) {
      url <- paste0("https://eur-lex.europa.eu/legal-content/EN/TXT/HTML/?uri=CELEX:",
                    ver, "&from=en")
      fetch_err <- NULL
      page_html <- tryCatch(
        fetch_eurlex_page(url, what = paste0("consolidated version ", ver)),
        error = function(e) {
          fetch_err <<- conditionMessage(e)
          message("     ", fetch_err)
          NULL
        }
      )
      # A WAF block is domain-wide: trying older versions would fail the same way.
      if (is.null(page_html) && grepl("AWS WAF|blocked", fetch_err)) break
    }
    if (is.null(page_html)) next
    parsed <- tryCatch(parse_eu_sml_page(page_html), error = function(e) {
      message("     parse failed: ", conditionMessage(e))
      NULL
    })
    if (!is.null(parsed)) {
      message("   Parsed version ", ver, ": ", nrow(parsed$SML), " SML rows, ",
              nrow(parsed$SML_group), " group rows")
      break
    }
  }
  if (is.null(parsed)) {
    stop("EU SML: no fetchable consolidated version found via Cellar or EUR-Lex ",
         "(EUR-Lex may be rate-limiting with its AWS WAF, or temporarily ",
         "unavailable); try again later.")
  }

  # 只在下载成功、即将覆盖旧文件时才备份；下载失败（如 WAF 拦截）不再
  # 留下一份与旧文件完全相同的无意义备份。
  if (file.exists(out)) {
    bak_dir <- file.path(dirname(out), "..", "backups")
    dir.create(bak_dir, showWarnings = FALSE, recursive = TRUE)
    bak <- file.path(bak_dir, paste0("eu10_2011_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"))
    if (file.copy(out, bak)) message("   Backup saved: ", bak)
  }

  rio::export(list(SML = parsed$SML, SML_group = parsed$SML_group), out)
  if (!file.exists(out) || file.size(out) == 0) {
    stop("EU SML: download produced an empty file.")
  }
  invisible(out)
}

#' Extract the consolidated CELEX versions (02011R0010-YYYYMMDD) from the
#' EU 10/2011 base act page HTML.
#'
#' Pure function (no network). Kept separate from the download flow so the
#' version-discovery regex is easy to adjust and offline-testable if EUR-Lex
#' changes its page structure.
#'
#' @param base_html Raw HTML text of the base act page.
#' @return Character vector of consolidated CELEX ids, newest first.
#' @noRd
extract_eu_sml_versions <- function(base_html) {
  versions <- sort(unique(stringr::str_extract_all(
    base_html, "02011R0010-[0-9]{8}"
  )[[1]]), decreasing = TRUE)
  if (length(versions) == 0L) {
    stop("EU SML: no consolidated version (02011R0010-YYYYMMDD) found on the ",
         "base act page; EUR-Lex page structure may have changed.")
  }
  versions
}

#' Discover the latest consolidated EU 10/2011 CELEX version from EUR-Lex.
#'
#' Fetch the base act page and extract every consolidated CELEX listed there;
#' download_eu_sml() tries the versions newest-first.
#'
#' @param base_url URL of the base act page.
#' @return Character vector of consolidated CELEX ids, newest first.
#' @noRd
discover_eu_sml_versions <- function(base_url) {
  base_html <- fetch_eurlex_page(base_url, what = "EU 10/2011 base act page")
  versions <- extract_eu_sml_versions(base_html)
  message("   Latest consolidated version: ", versions[1])
  versions
}

#' Build the Cellar SPARQL query that lists every consolidated version of a
#' base CELEX act, newest first.
#'
#' The base act is matched by its CELEX literal. The literal MUST be typed as
#' `xsd:string`; an untyped literal matches nothing in the Cellar store. The
#' consolidated versions are the works that point back at the base work through
#' `cdm:act_consolidated_based_on_resource_legal`.
#'
#' @param celex Base act CELEX id (e.g. "32011R0010").
#' @return SPARQL query string.
#' @noRd
cellar_eu_sml_query <- function(celex = "32011R0010") {
  sprintf(
    'PREFIX cdm: <http://publications.europa.eu/ontology/cdm#>
SELECT ?celex WHERE {
  ?base cdm:resource_legal_id_celex "%s"^^<http://www.w3.org/2001/XMLSchema#string> .
  ?cons cdm:act_consolidated_based_on_resource_legal ?base .
  ?cons cdm:resource_legal_id_celex ?celex .
} ORDER BY DESC(?celex)',
    celex
  )
}

#' Extract the consolidated CELEX ids from a Cellar SPARQL JSON response.
#'
#' Pure function (no network), kept separate so the response shape is
#' offline-testable.
#'
#' @param json_text Raw JSON text returned by the SPARQL endpoint.
#' @return Character vector of consolidated CELEX ids, newest first.
#' @noRd
parse_cellar_versions <- function(json_text) {
  parsed <- jsonlite::fromJSON(json_text, simplifyVector = FALSE)
  bindings <- parsed$results$bindings
  if (length(bindings) == 0L) return(character(0))
  versions <- vapply(bindings, function(b) b$celex$value, character(1))
  sort(unique(versions), decreasing = TRUE)
}

#' POST a query to the Cellar SPARQL endpoint, retrying with backoff.
#'
#' @param query SPARQL query string.
#' @param max_tries Maximum number of attempts.
#' @param wait Initial seconds to wait; grows x2 on each retry.
#' @return Raw JSON text.
#' @noRd
fetch_cellar_sparql <- function(query, max_tries = 4L, wait = 10) {
  endpoint <- "https://publications.europa.eu/webapi/rdf/sparql"
  code <- "connection error"
  for (i in seq_len(max_tries)) {
    resp <- tryCatch(
      httr::GET(endpoint, query = list(query = query),
                httr::add_headers(Accept = "application/sparql-results+json"),
                httr::user_agent("fcmsafety R package"),
                httr::timeout(90)),
      error = function(e) NULL
    )
    if (!is.null(resp) && identical(httr::status_code(resp), 200L)) {
      txt <- httr::content(resp, as = "text", encoding = "UTF-8")
      if (nchar(txt) > 0L) return(txt)
    }
    code <- if (is.null(resp)) "connection error" else as.character(httr::status_code(resp))
    if (i < max_tries) {
      delay <- wait * 2^(i - 1L)
      message(sprintf("   Cellar SPARQL blocked (HTTP %s); retrying in %ds (attempt %d/%d)...",
                      code, delay, i + 1L, max_tries))
      Sys.sleep(delay)
    }
  }
  stop("Cellar SPARQL endpoint could not be reached after ", max_tries,
       " attempts (last HTTP ", code, ").")
}

#' Discover the consolidated EU 10/2011 CELEX versions from Cellar (SPARQL).
#'
#' @param celex Base act CELEX id.
#' @return Character vector of consolidated CELEX ids, newest first.
#' @noRd
discover_eu_sml_versions_cellar <- function(celex = "32011R0010") {
  json <- fetch_cellar_sparql(cellar_eu_sml_query(celex))
  versions <- parse_cellar_versions(json)
  if (length(versions) == 0L) {
    stop("Cellar: no consolidated version found for CELEX:", celex, ".")
  }
  message("   Latest consolidated version (Cellar): ", versions[1])
  versions
}

#' Fetch a consolidated EU 10/2011 text from Cellar as XHTML.
#'
#' Cellar content-negotiates on the `Accept` header: the consolidated act only
#' resolves to the full XHTML document when `application/xhtml+xml` is
#' requested (otherwise it returns RDF metadata). The `.ENG` suffix pins the
#' English expression.
#'
#' @param celex Consolidated CELEX id (e.g. "02011R0010-20260714").
#' @param max_tries Maximum number of attempts.
#' @param wait Initial seconds to wait; grows x2 on each retry.
#' @return Page XHTML as a character string.
#' @noRd
fetch_cellar_consolidated <- function(celex, max_tries = 3L, wait = 10) {
  url <- paste0("https://publications.europa.eu/resource/celex/", celex, ".ENG")
  code <- "connection error"
  for (i in seq_len(max_tries)) {
    resp <- tryCatch(
      httr::GET(url,
                httr::add_headers(Accept = "application/xhtml+xml",
                                  `Accept-Language` = "en"),
                httr::user_agent("fcmsafety R package"),
                httr::timeout(180)),
      error = function(e) NULL
    )
    if (!is.null(resp) && identical(httr::status_code(resp), 200L)) {
      txt <- httr::content(resp, as = "text", encoding = "UTF-8")
      # Deletion protection: an RDF/meta response has no positive-list table.
      if (nchar(txt) > 1000L && grepl("FCM substance No", txt, fixed = TRUE)) {
        return(txt)
      }
    }
    code <- if (is.null(resp)) "connection error" else as.character(httr::status_code(resp))
    if (i < max_tries) {
      delay <- wait * 2^(i - 1L)
      message(sprintf("   Cellar fetch of %s failed (HTTP %s); retrying in %ds (attempt %d/%d)...",
                      celex, code, delay, i + 1L, max_tries))
      Sys.sleep(delay)
    }
  }
  stop("Cellar: could not fetch consolidated version ", celex,
       " as XHTML (last HTTP ", code, ").")
}

#' Fetch a EUR-Lex page as raw HTML text, retrying with backoff on transient
#' blocks (AWS WAF JS challenge -> HTTP 202, or connection errors).
#'
#' Two hardening measures (added 2026-09 after observing EUR-Lex's AWS WAF is
#' intermittent):
#' 1. Send a full browser-like header set, most importantly
#'    `Accept: text/html, ...`. AWS WAF's challenge rule keys on this header:
#'    non-HTML Accepts get an immediate HTTP 202 (which can never proceed),
#'    while `text/html` requests enter the challenge flow and are often served
#'    straight through when the WAF is not actively flagging our IP.
#' 2. Retry with increasing sleeps so a transient block often clears on its own
#'    (observed: the same URL returns 200 minutes later).
#'
#' @param url Page URL.
#' @param what Human-readable description used in messages.
#' @param max_tries Maximum number of attempts.
#' @param wait Initial seconds to wait; grows x2 on each retry.
#' @return Page HTML as a character string.
#' @noRd
fetch_eurlex_page <- function(url, what = "EUR-Lex page", max_tries = 5L, wait = 30) {
  browser_headers <- c(
    "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8",
    "Accept-Language" = "en-US,en;q=0.9",
    "Accept-Encoding" = "gzip, deflate, br",
    "Upgrade-Insecure-Requests" = "1",
    "Sec-Fetch-Dest" = "document",
    "Sec-Fetch-Mode" = "navigate",
    "Sec-Fetch-Site" = "none"
  )
  ua <- paste("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
              "(KHTML, like Gecko) Chrome/126.0.0.0 Safari/537.36")
  for (i in seq_len(max_tries)) {
    resp <- tryCatch(
      httr::GET(url, httr::add_headers(.headers = browser_headers),
                httr::user_agent(ua), httr::timeout(90)),
      error = function(e) NULL
    )
    if (!is.null(resp) && identical(httr::status_code(resp), 200L)) {
      txt <- httr::content(resp, as = "text", encoding = "UTF-8")
      if (nchar(txt) > 1000L) return(txt)
    }
    code <- if (is.null(resp)) "connection error" else as.character(httr::status_code(resp))
    if (i < max_tries) {
      delay <- wait * 2^(i - 1L)
      message(sprintf("   %s blocked (HTTP %s); retrying in %ds (attempt %d/%d)...",
                      what, code, delay, i + 1L, max_tries))
      Sys.sleep(delay)
    }
  }
  stop(what, " could not be fetched after ", max_tries, " attempts ",
       "(EUR-Lex AWS WAF may be blocking automated requests; try again later).")
}

#' Parse the two positive-list tables out of a consolidated EU 10/2011 HTML page.
#'
#' EUR-Lex has changed the page structure over time (old pages: a title row
#' followed by a header row; new pages: an extra column-number row "(1),(2),..."
#' above the header, plus "▼Mxx —————" amendment-deletion marker rows rendered
#' with colspan that pollute the trailing columns). This parser therefore
#' locates the header row by content ("FCM substance No" / "Group Restriction
#' No") instead of relying on a fixed position, drops marker rows, then drops
#' unnamed empty columns, so it works for both old and new layouts.
#'
#' @param html_text Page HTML as a character string.
#' @return list(SML = <data.frame>, SML_group = <data.frame>).
#' @noRd
parse_eu_sml_page <- function(html_text) {
  page <- rvest::read_html(charToRaw(html_text))
  tbodies <- page %>% rvest::html_elements("div.centered table tbody")
  if (length(tbodies) < 2L) {
    stop("expected at least two tables, found ", length(tbodies))
  }

  # Find the first tbody containing the header marker; use the marker row as
  # column names and everything below as data.  `exclude_pat` lets callers
  # reject tables that also contain a different header marker (e.g. the group
  # table also has an "FCM substance No" column, so the SML search must ignore
  # tables whose header row also says "Group Restriction No").
  find_table <- function(nodes, header_pat, exclude_pat = NULL) {
    for (tb in nodes) {
      tab <- tryCatch(rvest::html_table(tb, header = FALSE),
                      error = function(e) NULL)
      if (is.null(tab) || nrow(tab) < 2L) next
      hit <- which(vapply(seq_len(nrow(tab)), function(i) {
        cells <- as.character(unlist(tab[i, ]))
        has_header <- any(grepl(header_pat, cells, fixed = TRUE))
        has_exclude <- if (is.null(exclude_pat)) FALSE else
          any(grepl(exclude_pat, cells, fixed = TRUE))
        has_header && !has_exclude
      }, logical(1)))
      if (length(hit) == 0L) next
      hdr <- trimws(as.character(unlist(tab[hit[1], ])))
      # Empty cells come back as real NAs (not the string "NA"); turn them
      # into empty names so make.unique() handles them predictably.
      hdr[is.na(hdr)] <- ""
      # Use a plain data.frame: tibble's `names<-` has its own quirks with
      # empty/duplicated names that would break make.unique() below.
      dat <- as.data.frame(tab[(hit[1] + 1L):nrow(tab), , drop = FALSE],
                           stringsAsFactors = FALSE, check.names = FALSE)
      # Align a short header row with the table width, then drop columns that
      # are both unnamed and empty (EUR-Lex header rows sometimes leave blank
      # trailing cells that become "NA"/"NA_1" column names).
      if (length(hdr) < ncol(dat)) hdr <- c(hdr, rep("", ncol(dat) - length(hdr)))
      hdr_empty <- !nzchar(hdr)
      # Drop version-marker rows before checking for empty columns. EUR-Lex
      # renders "▼M16 —————" (amendment deletion markers) as a single cell with
      # colspan=N, so rvest spreads the marker across many columns; the marker
      # rows then pollute the trailing blank columns and keep them from being
      # dropped. No legitimate data row carries the marker in >=2 columns.
      n_v <- vapply(seq_len(nrow(dat)), function(i) {
        sum(grepl("\u25bc", as.character(unlist(dat[i, ]))), na.rm = TRUE)
      }, integer(1))
      dat <- dat[n_v < 2L, , drop = FALSE]
      if (nrow(dat) == 0L) next
      names(dat) <- make.unique(hdr, sep = "_")
      empty_cols <- vapply(seq_len(ncol(dat)), function(j) {
        if (!hdr_empty[j]) return(FALSE)
        vals <- trimws(as.character(dat[[j]]))
        # html_table renders empty cells as the literal string "NA"
        all(is.na(vals) | vals %in% c("", "NA"))
      }, logical(1))
      if (any(empty_cols)) dat <- dat[, !empty_cols, drop = FALSE]
      return(dat)
    }
    NULL
  }

  # Drop header/footnote debris that can leak into the data: column-number
  # rows like "(1)", collapsed rows like "▼M7", and empty substance numbers.
  clean_data <- function(dat, key_col) {
    keep <- !grepl("\u25bc|\\(|^\\s*$", dat[[key_col]])
    dat <- dat[keep, , drop = FALSE]
    dat[!duplicated(dat[[key_col]]), , drop = FALSE]
  }

  eu_sml <- find_table(tbodies, "FCM substance No",
                       exclude_pat = "Group Restriction No")
  if (is.null(eu_sml)) {
    stop("could not locate the SML table ('FCM substance No' header)")
  }
  eu_sml <- clean_data(eu_sml, "FCM substance No")

  eu_sml_group <- find_table(tbodies, "Group Restriction No")
  if (is.null(eu_sml_group)) {
    stop("could not locate the group restrictions table ('Group Restriction No' header)")
  }
  eu_sml_group <- clean_data(eu_sml_group, "FCM substance No") %>%
    dplyr::distinct(`Group Restriction No`, .keep_all = TRUE)

  if (nrow(eu_sml) == 0L || nrow(eu_sml_group) == 0L) {
    stop("parsed empty tables")
  }
  list(SML = eu_sml, SML_group = eu_sml_group)
}

# ---- IARC 分类（官网 Monographs 页，从 JS 数组里取） ------------------------
#
# IARC 没有干净的下载接口，数据嵌在页面的 JavaScript 里。extract_balanced_array()
# 负责按括号配对切出数组字面量（不能简单按行切，字符串里含逗号），
# parse_iarc_agents() 再把它转成表。

#' @rdname download_sources
#' @export
#' @encoding UTF-8
download_iarc <- function(out = paste0(getwd(), "/inst/iarc.xlsx")) {
  # IARC List of Classifications. The data is embedded in the webpack bundle at
  # loc.app.js as an `agents` array (no server-side CSV/Excel endpoint). We fetch
  # the JS and extract the array with a per-field regex parser.
  url <- "https://webapi.iarc.who.int/loc/loc.app.js"
  resp <- httr::GET(url, httr::timeout(60), httr::user_agent("fcmsafety R package"))
  httr::stop_for_status(resp)
  js <- httr::content(resp, as = "text", encoding = "UTF-8")

  tbl <- parse_iarc_agents(js)

  # Guard against a silently-empty parse (deletion protection).
  if (!is.data.frame(tbl) || nrow(tbl) < 500L) {
    stop("IARC: parsed fewer than 500 agents; source structure may have changed, please review.")
  }

  rio::export(tbl, out)
  invisible(out)
}

#' Extract the balanced `[...]` array starting at the bracket at position `pos`.
#'
#' @param text A character string.
#' @param pos Integer position of the opening `[`.
#' @return The substring from `pos` to the matching `]`.
#' @noRd
extract_balanced_array <- function(text, pos) {
  tail <- substr(text, pos, nchar(text))
  chars <- strsplit(tail, "", fixed = TRUE)[[1]]
  depth <- cumsum((chars == "[") - (chars == "]"))
  close_idx <- which(depth == 0L)[1]
  if (is.na(close_idx)) {
    stop("IARC: unbalanced brackets in agents array.")
  }
  end <- pos + close_idx - 1L
  substr(text, pos, end)
}

#' Parse the IARC `agents` array out of the `loc.app.js` bundle text.
#'
#' The data is a JS object literal (unquoted keys, mixed quote styles), not valid
#' JSON, so it is parsed field-by-field with regex instead of `jsonlite`.
#'
#' @param js The raw text of `loc.app.js`.
#' @return A data.frame with columns `CAS No.`, `Agent`, `Group`, `Volume`,
#'   `Volume publication year`, `Evaluation year`, `Additional information`.
#' @noRd
parse_iarc_agents <- function(js) {
  loc <- stringr::str_locate(js, stringr::fixed("agents:["))
  if (is.na(loc[1, 1])) {
    stop("IARC: 'agents:[' marker not found in source.")
  }
  arr <- extract_balanced_array(js, loc[1, 2])
  inner <- substr(arr, 2L, nchar(arr) - 1L)

  # Each agent object starts with the `name` key; split on the object boundary.
  parts <- stringr::str_split(inner, stringr::fixed("},{name:"))[[1]]
  parts <- ifelse(
    stringr::str_starts(parts, stringr::fixed("name:")),
    parts,
    paste0("name:", parts)
  )

  dq <- "\""
  sq <- "'"
  str_val <- paste0("(?:", dq, "([^", dq, "]*)", dq, "|", sq, "([^", sq, "]*)", sq, ")")

  extract_str <- function(txt, key) {
    m <- stringr::str_match(txt, paste0(key, "\\s*:\\s*", str_val))
    if (nrow(m) < 1L || is.na(m[1, 1])) return(NA_character_)
    if (!is.na(m[1, 2])) return(m[1, 2])
    if (!is.na(m[1, 3])) return(m[1, 3])
    NA_character_
  }
  extract_arr <- function(txt, key) {
    m <- stringr::str_match(txt, paste0(key, "\\s*:\\s*\\[([^\\]]*)\\]"))
    if (is.na(m[1, 1])) return(NA_character_)
    vals <- stringr::str_extract_all(m[1, 2], "\"[^\"]*\"")[[1]]
    if (length(vals) == 0L) return(NA_character_)
    paste(stringr::str_remove_all(vals, "^\"|\"$"), collapse = ", ")
  }
  extract_scalar <- function(txt, key) {
    m <- stringr::str_match(txt, paste0(key, "\\s*:\\s*([^,}]+)"))
    if (is.na(m[1, 1])) return(NA_character_)
    val <- stringr::str_trim(stringr::str_remove_all(m[1, 2], "['\"]"))
    # JS numeric literals like `2e3` (== 2000) must be normalized to decimal
    if (grepl("^[-+]?[0-9]+([eE][-+]?[0-9]+)?$", val)) {
      num <- suppressWarnings(as.numeric(val))
      if (!is.na(num)) return(as.character(num))
    }
    val
  }

  # 与 DataTables buttons 默认 stripHtml:true 的导出行为保持一致：
  # 数据源的名称/备注含 <i>...</i> 斜体标签，手动导出 Excel 时会被剥离，
  # 这里同样剥离，避免与库内纯文本名称 diff 时大量误报。
  # 同时把 JS 字符串里的字面 \n 转义替换为空格（与历史库数据格式对齐）。
  clean_source_text <- function(x) {
    if (is.na(x)) return(x)
    x <- gsub("<[^>]+>", "", x)
    gsub("\\n", " ", x, fixed = TRUE)
  }

  rows <- lapply(parts, function(txt) {
    data.frame(
      `CAS No.` = extract_arr(txt, "cas"),
      `Agent` = clean_source_text(extract_str(txt, "name")),
      `Group` = extract_str(txt, "group"),
      `Volume` = extract_arr(txt, "volume"),
      `Volume publication year` = extract_scalar(txt, "year"),
      `Evaluation year` = extract_scalar(txt, "yeareval"),
      `Additional information` = clean_source_text(extract_str(txt, "comment")),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

