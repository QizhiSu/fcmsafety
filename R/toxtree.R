# =============================================================================
# Toxtree 集成（Cramer 分类）
#
# 本文件收纳所有 Toxtree 相关接口：
#   - run_toxtree()       R 内直接调用 Toxtree CLI 完成 Cramer 分类，
#                         产出 data.frame，直接返回给调用方
#   - ensure_toxtree_jar() jar 查找/按需下载（内部函数）
#
# 设计决策见 docs/adr/0001-toxtree-via-local-cli.md 与
# docs/adr/0002-gpl-jar-download-on-demand.md（GPL jar 不随 MIT 包分发）。
#
# 实测要点（2026-09-08 冒烟测试，Toxtree 3.1.0.1851 + Temurin 17）：
#   - CLI 输出列名为 "Cramer rules"（空格），GUI 工作流的 "Cramer.rules"
#     其实是 read.csv(check.names=TRUE) 的自动改名；这里显式归一化。
#   - Toxtree 以【当前工作目录】定位 ext/ 模块目录，而非 jar 所在目录，
#     因此必须 cd 到应用目录再启动 java（system2 不支持设 cwd，用 system()）。
# =============================================================================

# ---- R 内直接调用 Toxtree CLI ----------------------------------------------

#' 判断单个 SMILES 能否被解析（内部）
#'
#' 注意 rcdk::parse.smiles() 对无法解析的输入返回的是"长度为 1、元素为 NULL"
#' 的列表（并伴随 warning），不是长度 0 的空列表——只查 length() 会把坏行
#' 误判成好行。这里同时判 NULL 与 length。
#'
#' @param smiles 字符向量
#' @return 逻辑向量
#' @keywords internal
#' @export
#' @encoding UTF-8
.smiles_is_parsable <- function(smiles) {
  vapply(as.character(smiles), function(s) {
    if (is.na(s) || !nzchar(s)) return(FALSE)
    m <- tryCatch(suppressWarnings(rcdk::parse.smiles(s)),
                  error = function(e) NULL)
    !is.null(m) && length(m) > 0 && !is.null(m[[1]])
  }, logical(1), USE.NAMES = FALSE)
}

# ---- jar 管理：查找 / 下载 / 解压 ------------------------------------------

# 固定版本：Toxtree 3.1.0（SourceForge 最新，2018-05-04 发布）
.toxtree_version <- "3.1.0.1851"
.toxtree_zip_url <- "https://downloads.sourceforge.net/project/toxtree/toxtree/Toxtree-v.3.1.0/Toxtree-v3.1.0.1851.zip"
.toxtree_zip_size <- 81045024  # 期望字节数，下载后校验
.toxtree_app_dirname <- "toxtree_app"  # 缓存内的应用目录名（jar + ext/）

# 空白 NAME / CAS 字段的占位符。
# Toxtree 3.1.0 的已知缺陷（2026-09-10 复现）：输入行 CAS 字段为空时，
# 该行输出会整行左移一列——CAS 字段被整个丢弃，其后所有值前移，导致
# Cramer 分级值落进 CRAMERFLAGS 列，按列名读回时得到 NA。
# 实测给空字段填任意非空占位符即可避免错位（行号 / CID / "N/A" 均可），
# 故这里统一填 "N/A"，且在归一化后回填为输入原值，不让占位符泄进结果。
.toxtree_blank_fill <- "N/A"

#' Ensure the Toxtree application is available (internal)
#'
#' 查找顺序：① 用户显式指定的 jar_path（须为标准安装布局，jar 旁边有
#' ext/ 模块目录）；② 包外缓存目录里已解好的应用目录；③ 从 SourceForge
#' 下载官方 zip 并解出应用目录。
#'
#' Toxtree 以 GPL 2.0 分发，fcmsafety 是 MIT，因此 jar 不随包分发，
#' 只在用户机器上按需下载（用户自取官方源，包只做自动化）。
#'
#' @return 主 jar 的绝对路径（其同级目录含 ext/）。
#' @noRd
ensure_toxtree_jar <- function(jar_path = NULL, download = TRUE) {
  # ① 显式路径（用户自己的 Toxtree 安装）
  if (!is.null(jar_path)) {
    if (!file.exists(jar_path)) {
      stop("jar_path points to a non-existent file: ", jar_path,
           call. = FALSE)
    }
    jar_path <- normalizePath(jar_path)
    if (!dir.exists(file.path(dirname(jar_path), "ext"))) {
      warning("No 'ext' module directory found next to ", jar_path,
              " - Toxtree may fail to load plugins. jar_path should point ",
              "to the main jar of a standard Toxtree installation.")
    }
    return(jar_path)
  }

  # ② 缓存目录
  cache_dir <- tools::R_user_dir("fcmsafety", "cache")
  app_dir <- file.path(cache_dir, .toxtree_app_dirname)
  jar_file <- .find_main_jar(app_dir)
  if (!is.null(jar_file)) {
    return(jar_file)
  }
  if (!download) {
    stop("Toxtree not found in cache. Run run_toxtree() once to download, ",
         "or pass jar_path explicitly.", call. = FALSE)
  }

  # ③ 下载官方 zip 并解出应用目录
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  zip_file <- file.path(cache_dir, paste0("Toxtree-v", .toxtree_version, ".zip"))
  if (!file.exists(zip_file) || file.size(zip_file) != .toxtree_zip_size) {
    message("Downloading Toxtree ", .toxtree_version,
            " from SourceForge (~81 MB, one-time setup)...")
    ok <- tryCatch({
      .download_toxtree_zip(.toxtree_zip_url, zip_file)
      TRUE
    }, error = function(e) {
      message("Download failed: ", conditionMessage(e))
      FALSE
    })
    if (!ok || !file.exists(zip_file) ||
        file.size(zip_file) != .toxtree_zip_size) {
      stop("Failed to download Toxtree zip. Please download manually from:\n  ",
           .toxtree_zip_url, "\nextract it, and pass the main jar path via ",
           "jar_path.", call. = FALSE)
    }
  }
  message("Extracting Toxtree application directory...")
  .extract_toxtree_app(zip_file, cache_dir)
  jar_file <- .find_main_jar(app_dir)
  if (is.null(jar_file)) {
    stop("Toxtree jar could not be extracted from the zip archive.",
         call. = FALSE)
  }
  jar_file
}

#' Locate the main (largest) jar inside the app directory (internal)
#' @noRd
.find_main_jar <- function(app_dir) {
  if (!dir.exists(app_dir)) return(NULL)
  jars <- list.files(app_dir, pattern = "\\.jar$", ignore.case = TRUE,
                     full.names = TRUE)
  if (length(jars) == 0) return(NULL)
  jars[which.max(file.size(jars))]
}

#' Download the Toxtree zip, handling SourceForge's HTML interstitial (internal)
#' @noRd
.download_toxtree_zip <- function(url, dest) {
  tmp <- tempfile(fileext = ".zip")
  resp <- httr::GET(url, httr::user_agent("R (fcmsafety)"),
                    httr::write_disk(tmp, overwrite = TRUE))
  httr::stop_for_status(resp)

  # downloads.sourceforge.net 可能返回带 meta refresh 的 HTML 跳转页而非文件
  ct <- httr::headers(resp)$`content-type`
  if (!is.null(ct) && grepl("text/html", ct, ignore.case = TRUE)) {
    html <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
    m <- regmatches(html, regexpr('content="[0-9]+;\\s*url=([^"]+)"', html))
    if (length(m) == 0 || !nzchar(m)) {
      stop("SourceForge returned an unexpected HTML page (no mirror redirect).")
    }
    redirect <- sub('content="[0-9]+;\\s*url=', "", m)
    redirect <- gsub("&amp;", "&", redirect, fixed = TRUE)
    resp <- httr::GET(redirect, httr::user_agent("R (fcmsafety)"),
                      httr::write_disk(tmp, overwrite = TRUE))
    httr::stop_for_status(resp)
  }
  ok <- file.copy(tmp, dest, overwrite = TRUE)
  if (!ok) stop("Could not write the downloaded zip to: ", dest)
  invisible(dest)
}

#' Extract the Toxtree application directory from the official zip (internal)
#'
#' zip 内布局：Toxtree-v3.1.0.1851/Toxtree/{主jar, ext/*.jar,
#' toxtree-plugins.properties}。只解 Toxtree 应用子目录（跳过 doc/src）。
#' @noRd
.extract_toxtree_app <- function(zip_file, cache_dir) {
  listing <- utils::unzip(zip_file, list = TRUE)
  # 应用子目录 = 主 jar（最大的 .jar 条目）所在的目录
  jar_entries <- listing[grepl("\\.jar$", listing$Name, ignore.case = TRUE), ,
                         drop = FALSE]
  if (nrow(jar_entries) == 0) {
    stop("No jar found inside the Toxtree zip archive.")
  }
  app_entry_dir <- dirname(jar_entries$Name[which.max(jar_entries$Length)])

  exdir <- file.path(cache_dir, "toxtree_extract")
  if (dir.exists(exdir)) unlink(exdir, recursive = TRUE)
  utils::unzip(zip_file,
               files = listing$Name[startsWith(listing$Name, app_entry_dir)],
               exdir = exdir)

  app_src <- file.path(exdir, basename(app_entry_dir))
  app_dst <- file.path(cache_dir, .toxtree_app_dirname)
  if (dir.exists(app_dst)) unlink(app_dst, recursive = TRUE)
  if (!file.rename(app_src, app_dst)) {
    # rename 跨盘失败时退化为复制
    if (!dir.create(app_dst, recursive = TRUE)) {
      stop("Could not create the Toxtree app directory in cache.")
    }
    if (!file.copy(app_src, app_dst, recursive = TRUE)) {
      stop("Could not copy the Toxtree app directory into cache.")
    }
  }
  unlink(exdir, recursive = TRUE)
  invisible(app_dst)
}


# ---- rJava 路径：直接调用 Toxtree Java API ---------------------------------
#
# 背景：Toxtree 的 CLI 每次调用都要启动新的 JVM（约 5-10 秒），通过 rJava
# 直接调用 Toxtree 的 Java API 可以复用同一个 JVM，每次调用只需 ~50-200 ms。
#
# 核心障碍与解法：
#   - jnati (Toxtree 内部用于加载 JNI InChI 本地库) 不支持 macOS ARM64，
#     但 rcdklibs 自带的 JNA InChI (io.github.dan2097.jnainchi) 是纯 Java，
#     天然支持 ARM64。策略：按序加载所有 rcdklibs JAR（rcdklibs CDK 2.9 +
#     JNA InChI）再加载 Toxtree JAR，Java 的单例模式会缓存第一个加载的
#     InChIGeneratorFactory（来自 rcdklibs 的 JNA 版本）。
#   - CDK 2.9 的 AtomContainer2 包级私有（package-private），且覆盖了
#     IAtomContainer.setProperty(String, Object) 为 setProperty(Object, Object)。
#     rJava 的 rJava::.jcall() 按静态声明类型分派，无法从 R 找到 (Object, Object)
#     签名。解法：将 CDKHelper.class 编译到 inst/java/org/openscience/cdk/
#     目录（与 AtomContainer2 同包），由该类代为调用 setProperty()。
#   - rJava 将 jobjRef 识别为自己的类型，不能直接匹配 java.lang.Object。
#     CDKHelper 的所有 public 方法内部 catch 异常并返回 null/false，
#     R 端检查返回值即可判断成功失败。
#
# 调用约定（供 .java_run_cramer 调用）：
#   - JAR 顺序：rcdklibs 所有 JAR → Toxtree 主 jar → ext/*.jar
#   - CDKHelper 位于 inst/java/（通过 .onLoad 部署到缓存目录）
#   - CramerRules 的 initialise(createDecisionResult(), verifyRules(mol, result))
#     三步在 CDKHelper.runCramerRules() 内部完成

#' Check if rJava is available and working (internal)
#'
#' Tests both that the rJava package is installed and that a JVM can be
#' initialised. Returns a message describing why rJava is unavailable.
#'
#' @return NULL if rJava works, a character message describing the failure
#'   if it does not.
#' @noRd
.javainit_check <- function() {
  if (!requireNamespace("rJava", quietly = TRUE)) {
    return("rJava package is not installed.")
  }
  jh <- tryCatch({
    rJava::.jinit()
    TRUE
  }, error = function(e) {
    paste0("JVM initialisation failed: ", conditionMessage(e))
  })
  if (isTRUE(jh)) NULL else jh
}

#' Find paths to all required JARs for the rJava path (internal)
#'
#' @return list with components:
#'   \code{rcdklibs}   absolute paths of rcdklibs JARs
#'   \code{toxtree}    absolute path of Toxtree main jar
#'   \code{ext_jars}   absolute paths of Toxtree ext/ JARs
#'   \code{cdkhelper}  absolute path to the compiled CDKHelper class root
#' @noRd
.java_get_jar_paths <- function() {
  # rcdklibs: installed with the rcdklibs package
  rcdklibs_cont <- system.file("cont", package = "rcdklibs")
  if (!nzchar(rcdklibs_cont)) {
    stop("rcdklibs package not found. Install it with: ",
         "install.packages('rcdklibs')", call. = FALSE)
  }
  rcdklibs <- list.files(rcdklibs_cont, pattern = "\\.jar$",
                          full.names = TRUE)

  # Toxtree: reuse the same cached location as the CLI path
  toxtree_jar <- ensure_toxtree_jar(download = FALSE)
  toxtree_dir <- dirname(toxtree_jar)
  ext_jars <- list.files(file.path(toxtree_dir, "ext"),
                         pattern = "\\.jar$", full.names = TRUE)

  # CDKHelper: deploy from inst/java/ to a persistent cache directory
  # (inst/ is read-only from package, so copy to user cache)
  cdkhelper_root <- .cdkhelper_deploy()

  list(rcdklibs = rcdklibs,
       toxtree = toxtree_jar,
       ext_jars = ext_jars,
       cdkhelper = cdkhelper_root)
}

#' Deploy CDKHelper.class from inst/java/ to user cache (internal)
#'
#' Copies the compiled CDKHelper.class hierarchy from the package's inst/java/
#' directory to a persistent cache directory so it can be used by rJava.
#' Safe to call repeatedly — only copies if the destination doesn't exist.
#'
#' @return Path to the root of the deployed CDKHelper tree (the parent of
#'   the org/ directory).
#' @noRd
.cdkhelper_deploy <- function() {
  src <- system.file("java", package = "fcmsafety")
  if (!nzchar(src) || !dir.exists(src)) {
    stop("inst/java/ not found in fcmsafety. ",
         "CDKHelper.class must be compiled and placed in inst/java/org/openscience/cdk/ ",
         "before loading the rJava path.", call. = FALSE)
  }
  cache_dir <- tools::R_user_dir("fcmsafety", "cache")
  dst <- file.path(cache_dir, "cdkhelper")
  if (!dir.exists(dst)) {
    dir.create(dst, recursive = TRUE)
    files <- list.files(src, full.names = TRUE, all.files = TRUE,
                        include.dirs = TRUE, recursive = TRUE)
    for (f in files) {
      rel <- sub(paste0("^", src, "/?"), "", f)
      dest <- file.path(dst, rel)
      if (dir.exists(f)) {
        dir.create(dest, showWarnings = FALSE, recursive = TRUE)
      } else {
        dir.create(dirname(dest), showWarnings = FALSE, recursive = TRUE)
        file.copy(f, dest)
      }
    }
  }
  dst
}

#' Initialise rJava with all required JARs (internal, cached)
#'
#' Must be called before any other rJava calls. Loads JARs in the correct order
#' (rcdklibs first, then Toxtree) to ensure the JNA-backed InChIGeneratorFactory
#' from rcdklibs is cached before Toxtree's JNI-backed one is loaded.
#'
#' @param force_reinit If TRUE, re-initialise even if already done.
#' @return NULL on success; stop on failure.
#' @noRd
.javainit <- function(force_reinit = FALSE) {
  if (!identical(.javainit_state(), "uninitialised") && !force_reinit) {
    return(invisible(NULL))
  }
  msg <- .javainit_check()
  if (!is.null(msg)) {
    stop("rJava is not available: ", msg, call. = FALSE)
  }
  .javainit_state("loading")
  on.exit(.javainit_state("ready"), add = TRUE)

  jars <- .java_get_jar_paths()

  # Load rcdklibs first (JNA InChI factory is cached here)
  for (jar in jars$rcdklibs) {
    rJava::.jaddClassPath(jar)
  }
  # Load Toxtree after rcdklibs (jnati/JNI InChI factory will NOT override
  # the cached JNA factory because the singleton pattern only uses the first call)
  rJava::.jaddClassPath(jars$toxtree)
  for (jar in jars$ext_jars) {
    rJava::.jaddClassPath(jar)
  }
  # Load CDKHelper from the deployed cache copy
  rJava::.jaddClassPath(jars$cdkhelper)

  invisible(NULL)
}

# Simple state variable (not exported, no locked binding risk)
.javainit_state <- local({
  .state <- "uninitialised"
  function(x) {
    if (missing(x)) .state else .state <<- x
  }
})

#' Run Cramer classification on a single SMILES string via rJava (internal)
#'
#' @param smiles Valid SMILES string
#' @return Named list: \code{verified} (logical), \code{category} (character,
#'   e.g. "Low (Class I)"), \code{inchi} (character or NULL),
#'   \code{inchikey} (character or NULL), \code{error} (character or NULL)
#' @noRd
.java_run_cramer <- function(smiles) {
  # Create builder fresh — DefaultChemObjectBuilder is a factory (cheap to create)
  builder <- rJava::.jcast(
    rJava::.jnew("org.openscience.cdk.DefaultChemObjectBuilder"),
    "org/openscience/cdk/interfaces/IChemObjectBuilder"
  )

  # Parse SMILES
  parser <- rJava::.jnew("org.openscience.cdk.smiles.SmilesParser", builder)
  mol <- rJava::.jcall(parser, "Lorg/openscience/cdk/interfaces/IAtomContainer;",
                 "parseSmiles", smiles)
  if (is.null(mol)) {
    return(list(verified = FALSE, category = NA_character_,
                inchi = NULL, inchikey = NULL,
                error = "SMILES could not be parsed"))
  }

  # Set MolFlags (the blocker that required CDKHelper)
  ok <- rJava::.jcall("org.openscience.cdk.CDKHelper", "Z", "setMolFlags", mol)
  if (!ok) {
    return(list(verified = FALSE, category = NA_character_,
                inchi = NULL, inchikey = NULL,
                error = "CDKHelper.setMolFlags() failed"))
  }

  # Get InChI (optional, used downstream)
  inchi <- rJava::.jcall("org.openscience.cdk.CDKHelper", "S", "getInChI", mol)
  inchikey <- NULL
  if (!is.null(inchi) && !is.na(inchi) && nzchar(inchi)) {
    inchikey <- rJava::.jcall("org.openscience.cdk.CDKHelper", "S",
                         "inchiToInchiKey", inchi)
  }

  # Run Cramer via CDKHelper
  result <- rJava::.jcall("org.openscience.cdk.CDKHelper",
                    "LtoxTree/core/IDecisionResult;",
                    "runCramerRules", mol, builder)
  if (is.null(result)) {
    return(list(verified = FALSE, category = NA_character_,
                inchi = inchi, inchikey = inchikey,
                error = "CDKHelper.runCramerRules() returned NULL"))
  }

  # Extract category
  category <- rJava::.jcall("org.openscience.cdk.CDKHelper", "S",
                      "getCramerCategory", result)
  # verified = the tree reached a category (not excluded by any rule)
  verified <- !is.null(result) && !is.na(category) && nzchar(category)

  list(verified = verified, category = category,
       inchi = inchi, inchikey = inchikey, error = NULL)
}

#' Run Toxtree Cramer classification via rJava (internal)
#'
#' Fast path using the Toxtree Java API directly via rJava, bypassing the
#' JVM cold-start overhead of the CLI path.
#'
#' @param smiles_vec Character vector of SMILES strings
#' @param name_vec   Character vector of names (parallel to smiles_vec, may contain NA)
#' @param cas_vec    Character vector of CAS numbers (parallel, may contain NA)
#' @return data.frame with columns: NAME, CAS, SMILES, Cramer.rules, InChI, InChIKey
#' @noRd
.java_run_cramer_batch <- function(smiles_vec, name_vec, cas_vec) {
  .javainit()

  n <- length(smiles_vec)
  results <- vector("list", n)
  for (i in seq_len(n)) {
    r <- .java_run_cramer(smiles_vec[i])
    results[[i]] <- data.frame(
      NAME = name_vec[i],
      CAS  = cas_vec[i],
      SMILES = smiles_vec[i],
      Cramer.rules = if (isTRUE(r$verified)) r$category else NA_character_,
      InChI = if (!is.null(r$inchi)) r$inchi else NA_character_,
      InChIKey = if (!is.null(r$inchikey)) r$inchikey else NA_character_,
      stringsAsFactors = FALSE, row.names = NULL
    )
  }
  do.call(rbind, results)
}

#' Run Toxtree Cramer classification from R
#'
#' 在 R 内直接调用 Toxtree 的无界面（headless）模式完成 Cramer 分类，
#' 不再需要手动打开 Toxtree GUI 做批处理。
#'
#' 首次运行会自动从 SourceForge 下载 Toxtree（约 81 MB，一次性），
#' 之后复用缓存。也可以用 \code{jar_path} 指向本机已有的 Toxtree 安装
#' （标准布局：主 jar 旁边有 \code{ext/} 模块目录）。需要本机安装 Java 8+
#' （\url{https://adoptium.net}）。
#'
#' 默认优先使用 rJava 路径（直接调用 Toxtree Java API），速度比 CLI 快
#' 约 20-50 倍（JVM 只启动一次，无需每次解析化合物时重新启动）。
#' 如果 rJava 不可用或失败，自动回退到 CLI 路径。设置 \code{use_rjava = FALSE}
#' 可强制使用 CLI 路径。
#'
#' @param data 你的数据表，必须包含 \code{SMILES} 列（通常是
#'   \code{extract_cid()} / \code{extract_meta()} 之后的产物）。
#' @param module Toxtree 插件的完整类名。默认经典 Cramer 规则
#'   \code{"toxTree.tree.cramer.CramerRules"}（与 GUI 默认一致）；
#'   可选 \code{"cramer2.CramerRulesWithExtensions"}（扩展版）、
#'   \code{"toxtree.tree.cramer3.RevisedCramerDecisionTree"}（修订版）。
#'   注意：rJava 路径目前仅支持 \code{"toxTree.tree.cramer.CramerRules"}；
#'   其他模块会回退到 CLI 路径。
#' @param output 结果文件路径（可选），默认 NULL（不写文件）。
#'   设为文件路径可将结果保存到 CSV。
#' @param jar_path 可选，本机已有的 Toxtree 主 jar 路径；缺省时自动
#'   查找缓存并按需下载。
#' @param cas_col CAS 列（列名或列序号），默认自动找 \code{"CAS"}。
#' @param name_col 化学名列（列名或列序号），默认自动找 \code{"NAME"}。
#' @param timeout 单次 CLI 运行的超时秒数，默认 600。
#' @param use_rjava 是否优先使用 rJava 路径（默认 TRUE）。
#'   设为 FALSE 强制使用 CLI 路径。
#'
#' @return 一个 data.frame：输入的 NAME / CAS / SMILES 加上归一化后的
#'   \code{Cramer.rules} 结果列。同时写入 \code{output}（如果指定）。
#'
#' @importFrom dplyr mutate select filter
#' @export
#' @encoding UTF-8
run_toxtree <- function(data,
                        module = "toxTree.tree.cramer.CramerRules",
                        output = NULL,
                        jar_path = NULL,
                        cas_col = "CAS",
                        name_col = "NAME",
                        timeout = 600,
                        use_rjava = TRUE) {
  message("Toxtree CLI runner (module: ", module, ")")
  message(paste(rep("-", 60), collapse = ""))

  # --- 输入校验 ---
  if (!"SMILES" %in% names(data)) {
    stop("Input data must contain a 'SMILES' column (needed for Cramer ",
         "classification). Regulatory matching without it: use assign_toxicity().",
         call. = FALSE)
  }
  java_bin <- Sys.which("java")
  if (!nzchar(java_bin)) {
    stop("Java not found on this machine. Install Java 8+ from ",
         "https://adoptium.net and retry.", call. = FALSE)
  }

  # --- 定位 NAME / CAS 列（列名或序号均可） ---
  resolve_col <- function(col, fallback_search) {
    if (is.numeric(col)) {
      if (col < 1 || col > ncol(data)) {
        stop("Column index out of range: ", col, call. = FALSE)
      }
      return(col)
    }
    if (col %in% names(data)) return(match(col, names(data)))
    hit <- match(fallback_search, names(data))
    hit <- hit[!is.na(hit)]
    if (length(hit) > 0) return(hit[1])
    NA_integer_
  }
  name_idx <- resolve_col(name_col, c("NAME", "name", "Chemical.Name"))
  cas_idx <- resolve_col(cas_col, c("CAS", "cas", "CAS_retrieved"))

  # --- rJava vs CLI 路径选择 ---
  use_rjava_path <- isTRUE(use_rjava) &&
    identical(module, "toxTree.tree.cramer.CramerRules") &&
    requireNamespace("rJava", quietly = TRUE) &&
    is.null(.javainit_check())

  if (use_rjava_path) {
    message("Using rJava path (direct Toxtree Java API, fast)")
    message(paste(rep("-", 60), collapse = ""))
    return(.run_toxtree_rjava(data, name_idx, cas_idx, output))
  }

  message("Using CLI path (java -jar Toxtree, fallback)")

  # --- jar 准备 ---
  jar <- ensure_toxtree_jar(jar_path)
  app_dir <- dirname(jar)
  message("Toxtree jar: ", jar)

  # --- 组装 Toxtree 输入（NAME / CAS / SMILES，SMILES 缺失的行丢弃） ---
  tox_input <- data.frame(
    NAME = if (is.na(name_idx)) NA_character_ else as.character(data[[name_idx]]),
    CAS = if (is.na(cas_idx)) NA_character_ else as.character(data[[cas_idx]]),
    SMILES = as.character(data$SMILES),
    stringsAsFactors = FALSE
  )
  n_before <- nrow(tox_input)
  tox_input <- tox_input[!is.na(tox_input$SMILES) & nzchar(tox_input$SMILES), ,
                         drop = FALSE]
  message("Compounds to classify: ", nrow(tox_input),
          " (dropped ", n_before - nrow(tox_input), " rows without SMILES)")
  if (nrow(tox_input) == 0) {
    stop("No compounds with a valid SMILES to classify.", call. = FALSE)
  }

  # --- 剔除无法解析的 SMILES ---
  # Toxtree 遇到解析不了的 SMILES 会静默丢掉该行，导致输出行数少于输入，
  # 按行序对齐随即失败 —— 一行坏数据就能让整批 Cramer 结果全丢。
  # 这里先用 CDK 在本地下同样的判断，把不能解析的行挡在批处理之外，
  # 跑完再按原顺序把它们的各列置 NA 合并回去（行数、行序都与输入一致）。
  # rcdk 不可用时跳过这一步，此时仍由 .normalize_toxtree_output 的行数校验兜底。
  n_all <- nrow(tox_input)
  parsable <- rep(TRUE, n_all)
  if (requireNamespace("rcdk", quietly = TRUE)) {
    parsable <- .smiles_is_parsable(tox_input$SMILES)
    n_bad <- sum(!parsable)
    if (n_bad > 0) {
      message("Skipping ", n_bad, " row(s) whose SMILES cannot be parsed; ",
              "their Cramer result will be NA.")
    }
  }
  tox_run <- tox_input[parsable, , drop = FALSE]
  if (nrow(tox_run) == 0) {
    stop("No compounds with a parsable SMILES to classify.", call. = FALSE)
  }

  # --- 空白 NAME/CAS 占位符（规避 Toxtree 输出错位，见 .toxtree_blank_fill）---
  # 归一化后会用 orig_identity 把这两列还原成输入原值，占位符不会出现在结果里。
  orig_identity <- tox_run[, c("NAME", "CAS"), drop = FALSE]
  n_filled <- 0L
  for (col in c("NAME", "CAS")) {
    blank <- is.na(tox_run[[col]]) |
      !nzchar(trimws(as.character(tox_run[[col]])))
    blank[is.na(blank)] <- TRUE
    if (any(blank)) {
      tox_run[[col]] <- as.character(tox_run[[col]])
      tox_run[[col]][blank] <- .toxtree_blank_fill
      n_filled <- n_filled + sum(blank)
    }
  }
  if (n_filled > 0L) {
    message("Filled ", n_filled, " blank NAME/CAS field(s) with \"",
            .toxtree_blank_fill, "\" to avoid Toxtree's output column shift.")
  }

  # --- 调用 CLI ---
  # 注意：Toxtree 以【当前工作目录】定位 ext/ 模块目录，必须让 java 的
  # 工作目录 = 应用目录。system2() 不支持设 cwd、Windows 的 system()
  # 又不认 cd 内建命令，故用临时 setwd()（子进程继承 cwd），helper 的
  # on.exit 保证必然还原。
  in_csv <- tempfile(fileext = ".csv")
  out_csv <- tempfile(fileext = ".csv")
  utils::write.csv(tox_run, in_csv, row.names = FALSE, na = "")

  message("Running Toxtree headless (this may take a while)...")
  cli_out <- .run_toxtree_cli(java_bin, jar, app_dir, in_csv, out_csv,
                              module, timeout)
  status <- attr(cli_out, "status")
  if (is.null(status)) status <- 0
  if (!file.exists(out_csv) || status != 0) {
    stop("Toxtree CLI failed (exit status ", status, ").\n",
         "CLI output (last 30 lines):\n",
         paste(utils::tail(cli_out, 30), collapse = "\n"),
         call. = FALSE)
  }

  # --- 读取并归一化输出 ---
  result <- .normalize_toxtree_output(out_csv, module, tox_run)

  # 占位符还原：把 NAME / CAS 写回输入原值（含 NA），不让 "N/A" 泄进结果。
  # .normalize_toxtree_output 保证输出行序与 tox_run 一致，可直接按位置回填。
  result$NAME <- orig_identity$NAME
  result$CAS  <- orig_identity$CAS

  # --- 把被剔除的非法 SMILES 行按原位置合并回来（结果列 NA）---
  # 目的是让返回值的行数、行序与输入（非空 SMILES 的行）完全一致，
  # 调用方既能按位置对齐、也能按 SMILES 匹配，不会因个别坏行错位。
  if (nrow(tox_run) < n_all) {
    extra_cols <- setdiff(names(result), names(tox_input))
    full <- tox_input
    for (cn in extra_cols) full[[cn]] <- result[[cn]][NA_integer_]
    full[parsable, names(result)] <- result
    result <- full[, c(names(tox_input), extra_cols), drop = FALSE]
  }

  # --- 写出结果（可选）---
  if (!is.null(output)) {
    utils::write.csv(result, output, row.names = FALSE)
    message("Results written to: ", normalizePath(output))
  }
  message("Cramer classification complete.")
  message(paste(rep("-", 60), collapse = ""))

  invisible(result)
}

#' Run Toxtree via rJava (fast path), return normalized data.frame (internal)
#'
#' Called by run_toxtree() when rJava is available and module == CramerRules.
#' @noRd
.run_toxtree_rjava <- function(data, name_idx, cas_idx, output) {
  # --- input normalization (mirrors CLI path) ---
  tox_input <- data.frame(
    NAME   = if (is.na(name_idx)) NA_character_ else as.character(data[[name_idx]]),
    CAS    = if (is.na(cas_idx))  NA_character_ else as.character(data[[cas_idx]]),
    SMILES = as.character(data$SMILES),
    stringsAsFactors = FALSE
  )
  n_before <- nrow(tox_input)
  tox_input <- tox_input[!is.na(tox_input$SMILES) & nzchar(tox_input$SMILES), ,
                          drop = FALSE]
  message("Compounds to classify: ", nrow(tox_input),
          " (dropped ", n_before - nrow(tox_input), " rows without SMILES)")
  if (nrow(tox_input) == 0) {
    stop("No compounds with a valid SMILES to classify.", call. = FALSE)
  }

  # --- run via rJava ---
  message("Running Cramer classification via rJava...")
  result <- .java_run_cramer_batch(
    smiles_vec = tox_input$SMILES,
    name_vec   = tox_input$NAME,
    cas_vec    = tox_input$CAS
  )

  # --- write output (optional) ---
  if (!is.null(output)) {
    utils::write.csv(result, output, row.names = FALSE)
    message("Results written to: ", normalizePath(output))
  }
  message("Cramer classification complete.")
  message(paste(rep("-", 60), collapse = ""))
  invisible(result)
}

#' Invoke the Toxtree headless CLI with cwd set to the app dir (internal)
#' @noRd
.run_toxtree_cli <- function(java_bin, jar, app_dir, in_csv, out_csv,
                             module, timeout) {
  old_wd <- getwd()
  on.exit(if (!is.null(old_wd)) setwd(old_wd), add = TRUE)
  setwd(app_dir)
  suppressWarnings(system2(
    java_bin,
    args = c("-jar", shQuote(jar), "-n",
             "-i", shQuote(in_csv),
             "-o", shQuote(out_csv),
             "-m", module),
    stdout = TRUE, stderr = TRUE, timeout = timeout
  ))
}

#' Normalize Toxtree CLI output to Cramer.rules convention (internal)
#'
#' CLI 输出的结果列名为 "Cramer rules"（空格）；GUI 工作流里的
#' "Cramer.rules" 其实是 read.csv(check.names = TRUE) 的自动改名。
#' 这里显式重命名为 Cramer.rules，保证 assign_toxicity() 的
#' tox$Cramer.rules 在任何读取方式下都能命中。
#'
#' 防错位（P1-①，2026-09-09 实测）：当输入行 CAS 为空时，Toxtree CLI
#' 会把该行整行左移一列（NAME 顶进 CAS 位、SMILES 顶进 NAME 位…），
#' Cramer 分级值落进 CRAMERFLAGS 列。此处不信任输出身份列——行数校验后
#' 一律按输入行序重建 NAME/CAS/SMILES，并对检测到错位的行把结果列置 NA，
#' 避免分级值被静默错配到别的物质。
#' @noRd
.normalize_toxtree_output <- function(out_csv, module, tox_input) {
  out <- utils::read.csv(out_csv, check.names = FALSE, stringsAsFactors = FALSE)

  # 行数硬校验：Toxtree 按输入行序逐行输出（SMILES 过滤后），
  # 行数不符即无法按行对齐，直接拒绝而非静默错配。
  if (nrow(out) != nrow(tox_input)) {
    stop("Cannot align Toxtree output with input: output has ", nrow(out),
         " rows but input (after dropping rows without SMILES) has ",
         nrow(tox_input), " rows. Please check the input CSV or rerun ",
         "run_toxtree().", call. = FALSE)
  }

  # SMILES 列兜底：CLI 正常应原样保留输入列；若没有则按行序回填
  if (!"SMILES" %in% names(out)) {
    warning("Toxtree output has no SMILES column; falling back to input order.")
    out$SMILES <- tox_input$SMILES
  }

  # 结果列定位（实测列名 "Cramer rules"；排除 FLAGS 与决策路径列）
  result_col <- NULL
  candidates <- grep("cramer", names(out), ignore.case = TRUE, value = TRUE)
  candidates <- candidates[!grepl("flags", candidates, ignore.case = TRUE)]
  candidates <- candidates[!grepl("treeresult", candidates, ignore.case = TRUE)]
  # 优先与 module 类名尾段对应的列（CramerRules -> "Cramer rules"），
  # 否则取剩下的第一个
  if (module %in% names(out)) {
    result_col <- module
  } else if (length(candidates) > 0) {
    result_col <- candidates[1]
  }
  if (is.null(result_col)) {
    stop("Could not locate the Cramer result column in Toxtree output. ",
         "Columns found: ", paste(names(out), collapse = ", "),
         call. = FALSE)
  }

  # 身份列错位检测：在重建之前比对输出与输入共有的身份列。
  # NA/空值不参与比较；任一列不一致即视为该行错位。
  id_cols <- intersect(c("NAME", "CAS", "SMILES"), names(out))
  id_ok <- rep(TRUE, nrow(out))
  if (length(id_cols) > 0) {
    for (col in id_cols) {
      a <- out[[col]]
      b <- tox_input[[col]]
      a_miss <- is.na(a) | !nzchar(as.character(a))
      b_miss <- is.na(b) | !nzchar(as.character(b))
      both_present <- !a_miss & !b_miss
      diff <- both_present & as.character(a) != as.character(b)
      id_ok <- id_ok & !diff
    }
  }
  n_shift <- sum(!id_ok)
  if (n_shift > 0) {
    warning("Detected ", n_shift, " Toxtree output row(s) whose identity ",
            "columns do not match the input (likely caused by empty CAS ",
            "values shifting output columns). Identity columns were rebuilt ",
            "from the input in row order; Cramer results on those rows were ",
            "set to NA to avoid silent mis-assignment. For a full Cramer ",
            "result, provide CAS values or rerun with corrected input.")
  }

  # 行序对齐：身份列一律以输入为准重建（覆盖或补建 NAME/CAS/SMILES），
  # 不信任 CLI 输出的身份列。
  for (col in c("NAME", "CAS", "SMILES")) {
    out[[col]] <- tox_input[[col]]
  }

  # 保守：错位行的结果/FLAGS/决策路径列置 NA，宁缺毋滥
  if (n_shift > 0) {
    flag_cols <- grep("flags", names(out), ignore.case = TRUE, value = TRUE)
    tree_cols <- grep("treeresult", names(out), ignore.case = TRUE, value = TRUE)
    for (col in unique(c(result_col, flag_cols, tree_cols))) {
      out[[col]][!id_ok] <- NA_character_
    }
  }

  keep <- setdiff(names(out), "cdk:Title")  # cdk:Title 恒为空，丢弃
  out <- out[, keep, drop = FALSE]
  names(out)[names(out) == result_col] <- "Cramer.rules"

  out
}
