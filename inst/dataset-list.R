#' @title Get list of dataset name required for replicate ADNIMERGE2 package
#'
#' @description
#'  This function is shows dataset names that are required and to be downloaded
#'  # from data-sharing platform in order to replicate ADNIMERGE2 R data package.
#'
#' @param use_type Usage of dataset either \code{article} or \code{prep_script}
#'  \item \code{article} To list raw dataset names that are used in \code{ADNIMERGE2} vignettes
#'  \item \code{prep_script} To list raw dataset names that are used during data preparation prior to the \code{ADNIMERGE2} R package build
#'
#' @param add_url_link A Boolean value to add an URL link of corresponding source file
#'
#' @return A data.frame with the following columns:
#'  \item \code{data_code} Dataset code
#'  \item \code{label} Dataset label/description
#'  \item \code{use_prep_script} Indicator if the raw dataset is used in the data-wrangling prior to the package build
#'  \item \code{script_list} List of data wrangling script names
#'  \item \code{use_article} Indicator if the raw dataset is used in article/vignette
#'  \item \code{article_list} List of articles/vignettes
#'  \item \code{source_derived_data} Derived dataset name
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' # To get list of dataset that are used during data wrangling prior to package build
#' get_required_dataset_list(
#'   use_type = "prep_script",
#'   add_url_link = FALSE
#' )
#'
#' # To get list of dataset that are required to
#' # reproduce ADNIMERGE2 R package including its vignettes
#' get_required_dataset_list(
#'   use_type = "article",
#'   add_url_link = FALSE
#' )
#' }
#'
#' @rdname get_required_dataset_list
#' @keywords adni_utils
#' @family ADNIMERGE2 required datasets
#' @export
#' @importFrom rlang arg_match0
#' @importFrom dplyr bind_rows select filter if_all
#' @importFrom tibble as_tibble
#' @importFrom tidyselect all_of

get_required_dataset_list <- function(use_type, add_url_link = FALSE) {
  rlang::arg_match0(arg = use_type, values = c("prep_script", "article"))
  check_object_type(add_url_link, "logical")
  pkg_url <- paste0("https://atri-biostats.github.io/ADNIMERGE2")

  if (add_url_link) {
    derived_data_artc <- paste0("<a href='", paste0(pkg_url, "/articles/ADNIMERGE2-Derived-Data.html"), "' target='_blank'>ADNIMERGE2 Derived Data</a>")
    prep_url <- paste0("<a href='", paste0(pkg_url, "/tree/main/data-raw/data_prep.R"), "' target='_blank'>data-prep.R</a>")
    prep_recode_url <- paste0("<a href='", paste0(pkg_url, "/tree/main/data-raw/data-prep-recode.R"), "' target='_blank'>data-prep-recode.R</a>")
    document_url <- paste0("<a href='", paste0(pkg_url, "/tree/main/tools/document.R"), "' target='_blank'>document.R</a>")
    pacc_input_url <- paste0("<a href='", paste0(pkg_url, "/tree/main/tools/generate-pacc-input-data.R"), "' target='_blank'>generate-pacc-input-data.R</a>")
  } else {
    derived_data_artc <- "ADNIMERGE2-Derived-Data"
    prep_url <- "data_prep.R"
    prep_recode_url <- "data-prep-recode.R"
    document_url <- "document.R"
    pacc_input_url <- "generate-pacc-input-data.R"
  }

  order_cols <- c(
    "data_code", "label", "use_prep_script", "script_list",
    "use_article", "article_list", "source_derived_data"
  )
  pkg_data_list <- bind_rows(
    c(
      data_code = "REGISTRY",
      label = "Registry",
      article_list = derived_data_artc,
      source_derived_data = "DM, NV",
      use_article = TRUE
    ),
    c(
      data_code = "STUDYSUM",
      label = "Study Visits Summary in ADNI3 & ADNI4 Study Phases",
      article_list = derived_data_artc,
      use_article = TRUE,
      source_derived_data = "DM"
    ),
    c(
      data_code = "DATADIC",
      label = "Data dictionary",
      article_list = derived_data_artc,
      script_list = paste0(c(prep_url, prep_recode_url, document_url), collapse = ", "),
      source_derived_data = "SC",
      use_prep_script = TRUE,
      use_article = TRUE
    ),
    c(
      data_code = "VISITS",
      label = "Study Level Visit Data Dictionary",
      article_list = derived_data_artc,
      use_article = TRUE
    ),
    c(
      data_code = "ROSTER",
      label = "Roster",
      article_list = derived_data_artc,
      source_derived_data = "DM",
      use_article = TRUE
    ),
    c(
      data_code = "PTDEMOG",
      label = "Participant Demographic Information",
      article_list = derived_data_artc,
      source_derived_data = "DM, SC",
      use_article = TRUE
    ),
    c(
      data_code = "RECADV",
      label = "Adverse Events Log Information in ADNI1, ADNI-GO and ADNI2 Study Phases",
      article_list = derived_data_artc,
      source_derived_data = "DM, AE",
      use_article = TRUE
    ),
    c(
      data_code = "ADVERSE",
      label = "Adverse Events Log Information in ADNI3 & ADNI4 Study Phases",
      article_list = derived_data_artc,
      source_derived_data = "DM, AE",
      use_article = TRUE
    ),
    c(
      data_code = "ADI",
      label = "Area Deprivation Index Score",
      article_list = derived_data_artc,
      source_derived_data = "SC",
      use_article = TRUE
    ),
    c(
      data_code = "RURALITY",
      label = "Rurality – RUCA & RUCC",
      article_list = derived_data_artc,
      source_derived_data = "SC",
      use_article = TRUE
    ),
    c(
      data_code = "ADAS",
      label = "ADAS-Cognitive Behavior",
      article_list = derived_data_artc,
      source_derived_data = "QS", # Since ADAS contains only total scores
      script_list = pacc_input_url,
      use_article = TRUE,
      use_prep_script = TRUE
    ),
    c(
      data_code = "CDR",
      label = "Clinical Dementia Rating",
      article_list = derived_data_artc,
      source_derived_data = "QS",
      use_article = TRUE
    ),
    c(
      data_code = "ECOGPT",
      label = "Everyday Cognition - Participant Self-Report",
      article_list = derived_data_artc,
      source_derived_data = "QS",
      use_article = TRUE
    ),
    c(
      data_code = "ECOGSP",
      label = "Everyday Cognition - Study Partner Report",
      article_list = derived_data_artc,
      source_derived_data = "QS",
      use_article = TRUE
    ),
    c(
      data_code = "FCI",
      label = "Financial Capacity Instrument Short Form (FCI-SF)",
      article_list = derived_data_artc,
      source_derived_data = "QS",
      use_article = TRUE
    ),
    c(
      data_code = "FCI",
      label = "Financial Capacity Instrument Short Form (FCI-SF)",
      article_list = derived_data_artc,
      source_derived_data = "QS",
      use_article = TRUE
    ),
    c(
      data_code = "FCI",
      label = "Financial Capacity Instrument Short Form (FCI-SF)",
      article_list = derived_data_artc,
      source_derived_data = "QS",
      use_article = TRUE
    ),
    c(
      data_code = "FAQ",
      label = "Functional Assessment Questionnaire",
      article_list = derived_data_artc,
      source_derived_data = "QS",
      use_article = TRUE
    ),
    c(
      data_code = "GDSCALE",
      label = "Geriatric Depression Scale",
      article_list = derived_data_artc,
      source_derived_data = "QS",
      use_article = TRUE
    ),
    c(
      data_code = "MMSE",
      label = "Mini-Mental State Exam",
      article_list = derived_data_artc,
      source_derived_data = "QS, PACC",
      script_list = pacc_input_url,
      use_article = TRUE,
      use_prep_script = TRUE
    ),
    c(
      data_code = "MOCA",
      label = "Montreal Cognitive Assessment",
      article_list = derived_data_artc,
      source_derived_data = "QS",
      use_article = TRUE
    ),
    c(
      data_code = "NPI",
      label = "Neuropsychiatric Inventory",
      article_list = derived_data_artc,
      source_derived_data = "QS",
      use_article = TRUE
    ),
    c(
      data_code = "NPIQ",
      label = "Neuropsychiatric Inventory Q",
      article_list = derived_data_artc,
      source_derived_data = "QS",
      use_article = TRUE
    ),
    c(
      data_code = "NEUROBAT",
      label = "Neuropsychological Battery",
      article_list = derived_data_artc,
      source_derived_data = "QS, PACC",
      script_list = pacc_input_url,
      use_article = TRUE,
      use_prep_script = TRUE
    ),
    c(
      data_code = "DXSUM",
      label = "Diagnostic Summary",
      article_list = derived_data_artc,
      source_derived_data = "RS",
      use_article = TRUE
    ),
    c(
      data_code = "UCBERKELEYFDG_8mm",
      label = "UC Berkeley - FDG analysis",
      article_list = derived_data_artc,
      source_derived_data = "NV",
      use_article = TRUE
    ),
    c(
      data_code = "PIBPETSUVR",
      label = "U Pitt PIB PET Analysis",
      article_list = derived_data_artc,
      source_derived_data = "NV",
      use_article = TRUE
    ),
    c(
      data_code = "AMYREAD",
      label = "Amyloid PET Read",
      article_list = derived_data_artc,
      source_derived_data = "NV",
      use_article = TRUE
    ),
    c(
      data_code = "AMYQC",
      label = "Amyloid PET QC",
      article_list = derived_data_artc,
      source_derived_data = "NV",
      use_article = TRUE
    ),
    c(
      data_code = "AV45QC",
      label = "AV-45 PET QC Tracking",
      article_list = derived_data_artc,
      source_derived_data = "NV",
      use_article = TRUE
    ),
    c(
      data_code = "PETQC",
      label = "PET QC Tracking",
      article_list = derived_data_artc,
      source_derived_data = "NV",
      use_article = TRUE
    ),
    c(
      data_code = "TAUQC",
      label = "Tau AV-1451 PET QC",
      article_list = derived_data_artc,
      source_derived_data = "NV",
      use_article = TRUE
    ),
    c(
      data_code = "PIBQC",
      label = "PIB QC Tracking",
      article_list = derived_data_artc,
      source_derived_data = "NV",
      use_article = TRUE
    ),
    c(
      data_code = "UCBERKELEY_AMY_6MM",
      label = "UC Berkeley - Amyloid PET 6mm Res analysis",
      article_list = derived_data_artc,
      source_derived_data = "NV",
      use_article = TRUE
    ),
    c(
      data_code = "UCBERKELEY_TAU_6MM",
      label = "UC Berkeley - Tau PET 6mm Res analysis",
      article_list = derived_data_artc,
      source_derived_data = "NV",
      use_article = TRUE
    ),
    c(
      data_code = "UCBERKELEY_TAUPVC_6MM",
      label = "UC Berkeley - Tau PET PVC 6mm Res analysis",
      article_list = derived_data_artc,
      source_derived_data = "NV",
      use_article = TRUE
    ),
    c(
      data_code = "UCBERKELEY_TAUPVC_6MM",
      label = "UC Berkeley - Tau PET PVC 6mm Res analysis",
      article_list = derived_data_artc,
      source_derived_data = "NV",
      use_article = TRUE
    ),
    c(
      data_code = "LABDATA",
      label = "Laboratory Data for ADNI1, ADNI-G0, and ADNI2 Study Phases",
      article_list = derived_data_artc,
      source_derived_data = "LB",
      use_article = TRUE
    ),
    c(
      data_code = "URMC_LABDATA",
      label = "Laboratory Data for ADNI3, and ADNI4 Study Phases",
      article_list = derived_data_artc,
      source_derived_data = "LB",
      use_article = TRUE
    ),
    c(
      data_code = "BIOMARK",
      label = "Biomarker Samples",
      article_list = derived_data_artc,
      source_derived_data = "LB",
      use_article = TRUE
    ),
    c(
      data_code = "UPENNBIOMK_ROCHE_ELECSYS",
      label = "UPENN CSF Biomarkers Elecsys [ADNI1,GO,2,3]",
      article_list = derived_data_artc,
      source_derived_data = "LB",
      use_article = TRUE
    ),
    c(
      data_code = "UPENNBIOMK_MASTER",
      label = "UPENN CSF Biomarker Master Alzbio3 (BATCH1-8)",
      article_list = derived_data_artc,
      source_derived_data = "LB",
      use_article = TRUE
    ),
    c(
      data_code = "UPENN_PLASMA_FUJIREBIO_QUANTERIX",
      label = "UPENN - Plasma Biomarkers measured by Fujirebio & Quanterix",
      article_list = derived_data_artc,
      source_derived_data = "LB",
      use_article = TRUE
    ),
    c(
      data_code = "C2N_PRECIVITYAD2_PLASMA",
      label = "C2N Diagnostics - PrecivityAD2 Blood Test Results",
      article_list = derived_data_artc,
      source_derived_data = "LB",
      use_article = TRUE
    ),
    c(
      data_code = "APOERES",
      label = "ApoE Genotyping - Results",
      article_list = derived_data_artc,
      source_derived_data = "GF",
      use_article = TRUE
    ),
    c(
      data_code = "VITALS",
      label = "AV-45 Pre and Post Injection Vitals",
      article_list = derived_data_artc,
      source_derived_data = "VS",
      use_article = TRUE
    )
  ) %>%
    select(all_of(order_cols))

  pkg_data_list <- pkg_data_list %>%
    as_tibble() %>%
    filter(if_all(all_of(paste0("use_", use_type)), ~ .x == TRUE)) %>%
    {
      if (nrow(.) > 0) {
        assert_non_missing(., all_of(paste0(gsub("prep", "", use_type), "_list")))
      } else {
        (.)
      }
    }

  return(pkg_data_list)
}

#' @title Concatenate Dataset URL Link
#' @param .data A data.frame
#' @param var_name Variable names, Default: 'data_code'
#' @return A data.frame with appended URL link
#' @examples
#' \dontrun{
#' library(dplyr)
#' test_data <- get_required_dataset_list(
#'   use_type = "article",
#'   add_url_link = TRUE
#' )
#' test_data <- concat_dataset_url(
#'   .data = test_data,
#'   var_name = "data_code"
#' )
#' }
#' @rdname concat_dataset_url
#' @keywords adni_utils
#' @family ADNIMERGE2 required datasets
#' @importFrom dplyr mutate  across
#' @importFrom tidyselect all_of
#' @export

concat_dataset_url <- function(.data, var_name = "data_code") {
  pkg_ref_url <- "https://atri-biostats.github.io/ADNIMERGE2/reference/"
  .data <- .data %>%
    mutate(across(all_of(var_name), ~ paste0("<a href='", pkg_ref_url, "' target='_blank'>", .x, "</a>")))
  return(.data)
}
