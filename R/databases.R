#' Update SVHC, CMR, IARC, and EU_SML database
#'
#' \code{update_databases()} provides a way to update the Substances of Very High
#' Concern (SVHC), Carcinogenic, Mutagenic, and Reprotoxic (CMR) compounds
#' from the Classification, Labelling, and Packagine (CLP) regulation Table 3 of
#' Annex VI, Carcinogenic substances from the International Agency for Research
#' on Cancer (IARC), as well as the positive list of the EU 10/2011 regulation on
#' plastic food contact materials. It then extracts CID and meta data, e.g.,
#' IsomericSMILES, InChIKey. It will first check if there is a \strong{inst} folder
#' in the working directory. If no, it will create one and generate
#' some *.xlsx files that can be read in later on with the \code{load_databases()}
#' function. If yes, it will update existed *.xlsx files. This process can be
#' time-consuming (up to several hours to retrieve information from Pubchem) and
#' It is not necessary to update all databases every time you want to use them
#' because these databases will not be frequently updated by the holders. Moreover,
#' Chrome brower is required and it will prompt up a Chrome windows for few seconds.
#' Please do not close it. It will disappear after downloading the required database.
#'
#' @param svhc A logical value to decide whether or not to update this database.
#' @param cmr A logical value to decide whether or not to update this database.
#' @param iarc A logical value to decide whether or not to update this database.
#' @param eu_sml A logical value to decide whether or not to update this database.
#'
#' @return It will return updated databases with CID and meta data assigned.
#'
#' @export
#'
#' @import webchem
#' @import dplyr
#' @import stringr
#' @import rvest
#' @import httr
#' @import RSelenium
#' @importFrom binman list_versions
#' @importFrom netstat free_port
#' @importFrom fs dir_ls
#' @importFrom janitor row_to_names
#' @importFrom magrittr extract
#' @importFrom utils download.file
update_databases <-
  function(svhc = TRUE, cmr = TRUE, iarc = TRUE, eu_sml = TRUE) {
    if (!dir.exists(paste0(getwd(), "/inst"))) {
      dir.create(paste0(getwd(), "/inst"))
    }

    if (svhc == TRUE) {
      # SVHC https://echa.europa.eu/candidate-list-table
      # the url is available via checking the Network tab in the Inspect menu in Chrome
      url <-
        "https://echa.europa.eu/candidate-list-table?p_p_id=disslists_WAR_disslistsportlet&p_p_lifecycle=2&p_p_state=normal&p_p_mode=view&p_p_resource_id=exportResults&p_p_cacheability=cacheLevelPage"
      httr::POST(
        url,
        body = list(
          "_disslists_WAR_disslistsportlet_formDate" = as.numeric(as.POSIXct(Sys.time())) * 1000,
          "_disslists_WAR_disslistsportlet_exportColumns" = "name,ecNumber,casNumber,haz_detailed_concern,dte_inclusion,doc_cat_decision,doc_cat_iuclid_dossier,doc_cat_supdoc,doc_cat_rcom,prc_external_remarks",
          "_disslists_WAR_disslistsportlet_orderByCol" = "dte_inclusion",
          "_disslists_WAR_disslistsportlet_orderByType" = "desc",
          "_disslists_WAR_disslistsportlet_searchFormColumns" = "haz_detailed_concern,dte_inclusion",
          "_disslists_WAR_disslistsportlet_searchFormElements" = "DROP_DOWN,DATE_PICKER",
          "_disslists_WAR_disslistsportlet_substance_identifier_field_key" = "",
          "_disslists_WAR_disslistsportlet_haz_detailed_concern" = "",
          "_disslists_WAR_disslistsportlet_dte_inclusionFrom" = "",
          "_disslists_WAR_disslistsportlet_dte_inclusionTo" = "",
          "_disslists_WAR_disslistsportlet_total" = "219",
          "_disslists_WAR_disslistsportlet_exportType" = "xls"
        ),
        httr::write_disk(paste0(getwd(), "/inst/svhc.xlsx"), overwrite = TRUE)
      )

      # read in the downloaded xlsx file
      svhc <- rio::import(paste0(getwd(), "/inst/svhc.xlsx"), skip = 3)
      svhc <- svhc %>%
        # this compound cannot be extract by CAS neither name (weird name)
        mutate(
          `Substance name` =
            str_replace(
              .$`Substance name`,
              "^Trixylyl phosphate$",
              "Trixylenyl phosphate"
            )
        ) %>%
        extract_cid(cas_col = 4, name_col = 1)
      svhc_meta <- extract_meta(svhc)
      # export the table with meta data
      rio::export(svhc_meta, paste0(getwd(), "/inst/svhc.xlsx"))
    }


    if (cmr == TRUE) {
      # extract CMR compounds from CLP regulation Table 3 of Annex VI (latest one)
      url <- "https://echa.europa.eu/information-on-chemicals/annex-vi-to-clp"
      # extract href from all a tags and then fileter on the base of "documents/10162.."
      url_list <- url %>%
        read_html() %>%
        html_nodes("a") %>%
        html_attr("href") %>%
        as_tibble() %>%
        filter(str_detect(value, "documents/10162/17218/annex_vi_clp"))
      # have to add https//echa.europa.eu at the beginning
      url <- paste0("https://echa.europa.eu", url_list[nrow(url_list), ])
      # download the latest clp list
      download.file(url, paste0(getwd(), "/inst/clp.xlsx"), quiet = TRUE, mode = "wb")

      # import the download latest clp list file
      clp <- suppressMessages(rio::import(paste0(getwd(), "/inst/clp.xlsx"), skip = 3))
      # rename some columns
      for (i in 1:ncol(clp)) {
        if (!is.na(clp[1, i])) {
          colnames(clp)[i] <- clp[1, i]
        }
      }
      clp <- clp[-1, ] # remove first row

      # subset for CMR and suspect CMR
      cmr <- clp %>%
        filter(str_detect(`Hazard Statement Code(s)`, "H340|H350|H360")) %>%
        extract_cid(cas_col = 4, name_col = 2)
      cmr_meta <- extract_meta(cmr)
      cmr_suspect <- clp %>%
        filter(str_detect(`Hazard Statement Code(s)`, "H341|H351|H361")) %>%
        extract_cid(cas_col = 4, name_col = 2)
      cmr_suspect_meta <- extract_meta(cmr_suspect)
      # export cmr and cmr_suspect to a single file but different sheet
      rio::export(
        list(cmr = cmr_meta, cmr_suspect = cmr_suspect_meta),
        paste0(getwd(), "/inst/clp_cmr_meta.xlsx")
      )
    }


    if (iarc == TRUE) {
      # the IARC database, https://monographs.iarc.who.int/agents-classified-by-the-iarc/
      url <- "https://monographs.iarc.who.int/list-of-classifications"
      path <- gsub("/", "\\\\", paste0(getwd(), "/inst")) # set the working directory for downloading
      # set up extra capabilities
      ecaps <- list(chromeOptions = list(
        prefs = list(
          "profile.default_content_settings.popups" = 0L,
          "download.prompt_for_download" = FALSE,
          "download.default_directory" = path
        ),
        args = c("--disable-gpu", "--window-size=600,800")
      ))
      # lauch a chrome driver
      driver <- RSelenium::rsDriver(
        browser = "chrome",
        # the long chromever avoid imcompatible chrome version
        # https://thatdatatho.com/tutorial-web-scraping-rselenium/
        chromever =
          system2(
            command = "wmic",
            args = 'datafile where name="C:\\\\Program Files (x86)\\\\Google\\\\Chrome\\\\Application\\\\chrome.exe" get Version /value',
            stdout = TRUE,
            stderr = TRUE
          ) %>%
            stringr::str_extract(pattern = "(?<=Version=)\\d+\\.\\d+\\.\\d+\\.") %>%
            magrittr::extract(!is.na(.)) %>%
            stringr::str_replace_all(
              pattern = "\\.",
              replacement = "\\\\."
            ) %>%
            paste0("^", .) %>%
            stringr::str_subset(
              string =
                binman::list_versions(appname = "chromedriver") %>%
                  dplyr::last()
            ) %>%
            as.numeric_version() %>%
            max() %>%
            as.character(),
        # use free port to allowed being repeatedly executed
        port = netstat::free_port(),
        extraCapabilities = ecaps
      )

      remote_driver <- driver[["client"]]
      remote_driver$navigate(url)
      buttom_element <-
        remote_driver$findElement(
          using = "xpath",
          value = '//*[@id="table_wrapper"]/div[1]/button[3]'
        )
      buttom_element$clickElement()
      Sys.sleep(2)
      remote_driver$close()

      # get the downloaded file name
      all_files <- fs::dir_ls(paste0(getwd(), "/inst"), glob = "*.xlsx") %>%
        str_remove_all(paste0(getwd(), "/inst/"))
      file_name <- all_files[str_which(
        all_files,
        "Agents Classified by the IARC Monographs"
      )]
      # move the file to the desired directory
      file.rename(
        file.path(paste0(getwd(), "/inst/"), file_name),
        file.path(paste0(getwd(), "/inst/iarc.xlsx"))
      )

      # read in the file and extract meta
      iarc <- rio::import(paste0(getwd(), "/inst/iarc.xlsx"), skip = 1)
      iarc <- iarc %>% extract_cid(cas_col = 1, name_col = 2)
      iarc_meta <- extract_meta(iarc)
      rio::export(iarc_meta, paste0(getwd(), "/inst/iarc_meta.xlsx"))
    }


    if (eu_sml == TRUE) {
      # EU 10/2011, no up to date
      url <- "https://eur-lex.europa.eu/legal-content/EN/TXT/HTML/?uri=CELEX:02011R0010-20200923&qid=1636402301680&from=en"
      # nodes of all tables
      eu_nodes <- read_html(url) %>% html_elements("div.centered table tbody")
      eu_sml <- eu_nodes %>%
        .[[1]] %>%
        html_table(header = TRUE) %>%
        suppressWarnings() %>%
        janitor::row_to_names(row_number = 1) %>%
        filter(!str_detect(`FCM substance No`, "\u25bc|\\(")) %>%
        distinct(`FCM substance No`, .keep_all = TRUE)
      # the group SML
      eu_sml_group <- eu_nodes %>%
        .[[2]] %>%
        html_table(header = TRUE) %>%
        suppressWarnings() %>%
        janitor::row_to_names(row_number = 1) %>%
        filter(!str_detect(`FCM substance No`, "\u25bc")) %>%
        distinct(`Group Restriction No`, .keep_all = TRUE)
      # export the extacted table into a single excel file
      rio::export(
        list(SML = eu_sml, SML_group = eu_sml_group),
        paste0(getwd(), "/inst/eu10_2011.xlsx")
      )

      # clean CAS and extract meta
      eu_sml <- eu_sml %>%
        mutate(`CAS No` = str_remove(.$`CAS No`, "^0*")) %>%
        mutate(`CAS No` = str_remove(.$`CAS No`, "\n.*")) %>%
        as.data.frame() %>% # don't know why as_data_frame or tibble do not work
        extract_cid(cas_col = 3, name_col = 4)
      eu_sml_meta <- extract_meta(eu_sml)
      rio::export(eu_sml_meta, paste0(getwd(), "/inst/eu10_2011_meta.xlsx")) # export complete list
    }
  }


#' Load all databases with meta data
#'
#' \code{load_databases()} provides a way to load all databases with meta data
#' into the global environment. It requires no argument but cleaned up databases
#' files in *.xlsx format in the \strong{inst} folder. Pre-prepared databases
#' (updated on 2021/11/11) already exist in the \strong{inst} folder. If you
#' \code{update_databases()}, theses databases will be updated to the latest one.
#' At the moment, the EDC and China SML (GB 9685) databases can not be updated.
#'
#' @param use_default A logical value. If use_default = TRUE, it will load
#' the pre-prepared databases. Otherwise, it will load the use-updated databases
#' using the \code{update_databases()} function. In the latter case, a \strong{inst}
#' folder containing all the databases (*.xlsx) is required.
#'
#' @return data.frame of all databases.
#'
#' @export
#'
#' @importFrom rio import export
load_databases <- function(use_default = TRUE) {
  if (use_default == FALSE) {
    svhc_meta <- rio::import(paste0(getwd(), "/inst/svhc_meta.xlsx"))
    cmr_meta <- rio::import(paste0(getwd(), "/inst/clp_cmr_meta.xlsx"),
      sheet = "cmr"
    )
    cmr_suspect_meta <- rio::import(paste0(getwd(), "/inst/clp_cmr_meta.xlsx"),
      sheet = "cmr_suspect"
    )
    iarc_meta <- rio::import(paste0(getwd(), "/inst/iarc_meta.xlsx"))
    eu_sml_meta <- rio::import(paste0(getwd(), "/inst/eu10_2011_meta.xlsx"))
    eu_sml_group <- rio::import(paste0(getwd(), "/inst/eu10_2011.xlsx"),
      sheet = "SML_group"
    )
    edc_meta <- rio::import(paste0(getwd(), "/inst/edc_meta.xlsx"))
    china_sml_meta <- rio::import(paste0(getwd(), "/inst/china_sml_meta_cleaned.xlsx"))
  } else {
    svhc_meta <- rio::import(system.file("svhc_meta.xlsx", package = "fcmsafety"))
    cmr_meta <- rio::import(system.file("clp_cmr_meta.xlsx", package = "fcmsafety"),
      sheet = "cmr"
    )
    cmr_suspect_meta <- rio::import(
      system.file("clp_cmr_meta.xlsx",
        package = "fcmsafety"
      ),
      sheet = "cmr_suspect"
    )
    iarc_meta <- rio::import(system.file("iarc_meta.xlsx", package = "fcmsafety"))
    eu_sml_meta <- rio::import(system.file("eu10_2011_meta.xlsx", package = "fcmsafety"))
    eu_sml_group <- rio::import(system.file("eu10_2011.xlsx", package = "fcmsafety"),
      sheet = "SML_group"
    )
    edc_meta <- rio::import(system.file("edc_meta.xlsx", package = "fcmsafety"))
    china_sml_meta <- rio::import(system.file("china_sml_meta_cleaned.xlsx",
      package = "fcmsafety"
    )) %>%
      suppressWarnings()
  }

  # read in the glogal environment and clean them up
  svhc_meta <<- svhc_meta %>% filter(!is.na(InChIKey))
  cmr_meta <<- cmr_meta %>% filter(!is.na(InChIKey))
  cmr_suspect_meta <<- cmr_suspect_meta %>% filter(!is.na(InChIKey))
  iarc_meta <<- iarc_meta %>% filter(!is.na(InChIKey))
  eu_sml_meta <<- eu_sml_meta %>%
    filter(!is.na(InChIKey)) %>%
    rename(
      SML = `SML\r\n                     [mg/kg]`,
      SML_group = `SML(T)\r\n                     [mg/kg]\r\n                     (Group restriction No)`
    ) %>%
    mutate(
      SML = SML %>%
        str_replace(",", ".") %>%
        str_replace("ND", "0.01") %>%
        # soybean oil expoxidized have 2 SML, keep only the first one
        str_remove_all("\n.*$") %>%
        trimws() %>%
        as.numeric(),
      SML_group = str_remove_all(SML_group, "\\(|\\)")
    )
  eu_sml_group <<- eu_sml_group %>%
    rename(SML = `SML (T)\r\n                     [mg/kg]`) %>%
    mutate(SML = SML %>%
      str_replace(",", ".") %>%
      str_replace("ND", "0.01") %>%
      trimws() %>%
      as.numeric())
  edc_meta <<- edc_meta %>% filter(!is.na(InChIKey))
  china_sml_meta <<- china_sml_meta
}
