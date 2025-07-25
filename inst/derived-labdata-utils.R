# Lab Result Adjustment -----
#' @title Adjust Completion Status of Lab Test - URMC
#' @description
#'   This function is used to adjust the completion status of laboratory test and
#'   to flag for lab test that were considered as `NOT DONE`.
#' @param lab_data A data.frame of lab test result data
#' @return
#'  A data.frame the same as the input dataset with the following appended columns:
#' \itemize{
#'  \item LBSTAT: Lab result completion status: `NOT DONE` for non-completed lab test or invalid test result
#'  \item LBREASND: reasons of not completed lab test, mainly based on comment text field
#'  \item ResultValue_translated: Adjust result of `ResultValue` after accounting for completion status
#'  \item ResultValueConv_translated: Adjust result of `ResultValueConv` after accounting for completion status
#'  \item ResultValueSI_translated: Adjust result of `ResultValueSI` after accounting for completion status
#' }
#' @examples
#' \dontrun{
#' library(tidyverse)
#' LAB_DATA <- adjust_lab_status(lab_data = ADNIMERGE2::URMC_LABDATA)
#' }
#' @details
#'  Completion status is determined based on the reported text value either in
#'  the comment section (`Comments`) or result value (`ResultValue`).
#'  The corresponding result value will be considered as missing/blank.
#' @rdname adjust_lab_status
#' @importFrom dplyr mutate case_when across
#' @importFrom stringr str_c str_detect
#' @importFrom tidyselect all_of
#' @export
adjust_lab_status <- function(lab_data) {
  require(stringr)
  require(dplyr)
  require(tidyselect)
  LBSTAT <- LBREASND <- impute_value <- NULL
  cols <- c("ResultValue", "ResultValueConv", "ResultValueSI", "Comments")
  check_colnames(
    .data = lab_data,
    col_names = cols,
    strict = TRUE,
    stop_message = TRUE
  )
  # `NOT DONE` based on result value
  not_done_value <- c(
    "CANCELLED", "Not Performed", "Sample was Not Received",
    "Result Removed", "Quantity Not Sufficient", "Unable to perform"
  )
  not_done_value <- tolower(not_done_value)
  not_done_value_ptrn <- str_c(not_done_value, collapse = "|")
  # Based on text comments
  not_done_cmt <- c(
    "Unable", "Test not", "Suspected specimen", "specimen was not",
    "Sample was Not Received", "Sample received out of stability",
    "Sample inadequate for testing", "Quantity not sufficient",
    "Not Performed", "Not calculated", "Hemolyzed", "Contaminated",
    "past stability time window", "Adequate", "incorrect sample type"
  )
  not_done_cmt <- tolower(not_done_cmt)
  not_done_cmt_ptrn <- str_c(not_done_cmt, collapse = "|")
  not_done_char <- "NOT DONE"

  lab_data <- lab_data %>%
    mutate(
      LBSTAT = case_when(
        str_detect(tolower(get(cols[1])), not_done_value_ptrn) == TRUE ~ not_done_char
      ),
      LBSTAT = case_when(
        str_detect(tolower(get(cols[4])), not_done_cmt_ptrn) == TRUE ~ not_done_char,
        TRUE ~ LBSTAT
      ),
      LBREASND = case_when(
        str_detect(tolower(get(cols[1])), not_done_value_ptrn) == TRUE ~ paste0(get(cols[1]), " ", get(cols[4]))
      ),
      LBREASND = case_when(
        str_detect(tolower(get(cols[4])), not_done_cmt_ptrn) == TRUE ~ get(cols[4]),
        TRUE ~ LBREASND
      )
    ) %>%
    mutate(across(all_of(cols[1:3]), ~ {
      impute_value <- ifelse(str_detect(cur_column(), "SI"), not_done_char, NA_character_)
      case_when(
        LBSTAT %in% not_done_char ~ impute_value,
        TRUE ~ as.character(.x)
      )
    },
    .names = "{col}_translated"
    ))

  return(lab_data)
}

#' @title Adjust Visit Label/Code of Lab Data
#' @description
#'  This function is used to adjust the corresponding visit label/code of lab test.
#' @param lab_data A data.frame of lab test result data
#' @return A data.frame with the updated visit label.
#' @examples
#' \dontrun{
#' library(dplyr)
#' LAB_DATA <- adjust_lab_visitcode(lab_data = ADNIMERGE2::URMC_LABDATA)
#' }
#' @rdname adjust_lab_visitcode
#' @importFrom dplyr mutate case_when
#' @export
adjust_lab_visitcode <- function(lab_data) {
  require(dplyr)
  PTTYPE <- VISCODE <- COLPROT <- ORIGPROT <- NULL
  lab_data <- lab_data %>%
    mutate(PTTYPE = adni_study_track(COLPROT, ORIGPROT)) %>%
    mutate(VISCODE = case_when(
      VISCODE %in% "4_uns" ~ "4_sc",
      VISCODE %in% c("uns", "uns1") ~ "sc",
      VISCODE %in% c("year2coag", "year4coag") ~ paste0("y", str_extract(VISCODE, "[0-9]")),
      PTTYPE %in% "New" & VISCODE %in% c("basecoag") ~ "bl",
      PTTYPE %in% "Rollover" & VISCODE %in% c("basecoag") ~ "init",
      VISCODE %in% "unscoag" ~ "uns1",
      TRUE ~ VISCODE
    )) %>%
    select(-PTTYPE) %>%
    # VISCODE mapping in ADNI2 screening visit
    mutate(VISCODE = case_when(
      COLPROT %in% adni_phase()[3] & VISCODE %in% "sc" ~ "v01",
      TRUE ~ VISCODE
    ))
  return(lab_data)
}

#' @title Adjust Lab Data Comment Field - UPENNBIOMK
#' @description
#'  This function is used to adjust the comment field of a lab data for the
#'  corresponding lab test after transposing the data into a log format by lab test.
#' @param lab_data A data.frame of lab test data
#' @return A data.frame with the updated `COMMENT` variable.
#' @examples
#' \dontrun{
#' library(tidyverse)
#' library(assert)
#' upenn_cols <- c("ABETA40", "ABETA42", "TAU", "PTAU")
#' LAB_DATA <- ADNIMERGE2::UPENNBIOMK_ROCHE_ELECSYS %>%
#'   pivot_longer(
#'     cols = all_of(upenn_cols),
#'     names_to = "LBTESTCD",
#'     values_to = "LBORRES"
#'   )
#' LAB_DATA <- adjust_lab_comment(lab_data = LAB_DATA)
#' }
#' @rdname adjust_lab_comment
#' @export
#' @importFrom assert verify
#' @importFrom dplyr mutate case_when select
#' @importFrom tidyselect all_of
#' @importFrom stringr str_detect str_to_lower str_trim
#' @importFrom tidyr separate_wider_delim
adjust_lab_comment <- function(lab_data) {
  require(tidyverse)
  require(assertr)
  LBTESTCD <- NULL
  cols <- c("COMMENT", "LBTESTCD")
  labtestcd <- c("ABETA40", "ABETA42", "TAU", "PTAU")
  abeta_comm <- str_to_lower("Abeta42>1700|Abeta42<200")
  tau_comm <- str_to_lower("^Tau>1300, PTau>120$|^Tau<80, PTau<8$")
  ptau_comm <- str_to_lower("^Ptau<8$|^PTau>120$")
  # Long format data
  lab_data <- lab_data %>%
    verify(all(LBTESTCD %in% labtestcd)) %>%
    verify(all(labtestcd %in% LBTESTCD)) %>%
    mutate(across(
      all_of(cols[1]),
      ~ case_when(
        str_detect(tolower(.x), abeta_comm) & !get(cols[2]) %in% labtestcd[2] ~ NA_character_,
        str_detect(tolower(.x), tau_comm) & !get(cols[2]) %in% labtestcd[3:4] ~ NA_character_,
        str_detect(tolower(.x), ptau_comm) & !get(cols[2]) %in% labtestcd[4] ~ NA_character_,
        TRUE ~ .x
      )
    )) %>%
    separate(all_of(cols[1]), into = labtestcd[3:4], sep = ",", remove = FALSE) %>%
    mutate(across(
      all_of(labtestcd[3:4]),
      ~ case_when(str_detect(str_to_lower(.x), "tau") &
        str_detect(get(cols[1]), ",") ~ str_trim(.x, side = "left"))
    )) %>%
    mutate(across(all_of(cols[1]), ~ case_when(
      get(cols[2]) %in% labtestcd[3] & !is.na(get(labtestcd[3])) ~ get(labtestcd[3]),
      get(cols[2]) %in% labtestcd[4] & !is.na(get(labtestcd[4])) ~ get(labtestcd[4]),
      TRUE ~ .x
    ))) %>%
    select(-all_of(labtestcd[3:4]))
  return(lab_data)
}

# Add cross chcek function in test-that with list of this batch name
#' @title Create Lab Test Result Limit Values - UPENNBIOMK
#' @description
#' Create a data frame that contains the lower and upper limit of a lab test result
#' from UPENN with corresponding test batches.
#' @param assay Assay source,
#' \itemsize{
#'  \item "C2N": PrecivityAD2 Blood Test Results
#'  \item "Roche": UPENN CSF Biomarkers Elecsys ADNI1,GO,2,3
#'  \item "AlzBio3": UPENN CSF Biomarker Master Alzbio3
#'  \item "FQ": UPENN - Plasma Biomarkers measured by Fujirebio Lumipulse and SIMOA Quanterix HD-X
#'  }
#' @return A data.frame with the following columns based on assay source/type:
#' \itemize{
#'  \item BATCH: List of test batch
#'  \item LBTESTCD: Lab test code
#'  \item LBTEST: Lab test name
#'  \item LBORRESU: Original unit
#'  \item LBSTRESU: Standard unit
#'  \item LBNAM: Vendor name
#'  \item LBORNRLO: Lower limit of original lab result, only for "Roche"
#'  \item LBORNRHI: Upper limit of original lab result, only for "Roche"
#'  \item LBSTNRLO: Lower limit of standard lab result, only for "Roche"
#'  \item LBSTNRHI: Upper limit of standard lab result, only for "Roche"
#' }
#' @details
#' The data are generated for research use only (RUO).
#' @examples
#' \dontrun{
#' get_biomarker_details(assay = "C2N")
#' get_biomarker_details(assay = "Roche")
#' get_biomarker_details(assay = "AlzBio3")
#' get_biomarker_details(assay = "FQ")
#' }
#' @rdname get_biomarker_details
#' @export
#' @importFrom tidyr expand_grid
#' @importFrom tibble tibble
#' @importFrom dplyr mutate
#' @importFrom rlang arg_match0
#' @importFrom assertr assert not_na
#' @importFrom stringr str_remove_all
get_biomarker_details <- function(assay = "Roche") {
  rlang::arg_match0(arg = assay, values = c("C2N", "Roche", "AlzBio3", "FQ"))
  LBTESTCD <- LBTEST <- LBNAM <- LBMETHOD <- LBORRESU <- LBSTRESU <- NULL
  LBORNRLO <- LBORNRHI <- LBSTNRLO <- LBSTNRHI <- LBSPEC <- NULL
  if (assay %in% "C2N") {
    BIOMARKER_DATA_LIMITU <- tibble::tibble(
      LBTESTCD = c("AB40", "AB42", "AB42AB40", "APS2", "NPT217", "PT217", "PTNPT217"),
      LBTEST = c(
        "Amyloid Beta 1-40", "Amyloid Beta 1-42", "Amyloid Beta 1-42/Amyloid Beta 1-40",
        "Amyloid Probability Score 2", "Non-Phosphorylated Tau217 Protein",
        "Phosphorylated Tau217 Protein", "Percentage of Phosphorylated Tau217 Protein"
      )
    ) %>%
      dplyr::mutate(
        LBORRESU = "pg/mL",
        LBSTRESU = "pg/mL",
        LBNAM = "C2N",
        LBMETHOD = "C2N Assay", 
        SOURCE = "C2N_PRECIVITYAD2_PLASMA"
      ) %>%
      dplyr::mutate(dplyr::across(
        tidyselect::ends_with("SU"),
        ~ dplyr::case_when(
          LBTESTCD %in% c("AB42AB40", "APS2", "PTNPT217") ~ "",
          TRUE ~ .x
        )
      ))
  }
  if (assay %in% "Roche") {
    # test batch
    batch_list <- c(
      "ADNI1/GO/2 batch", "ADNI3 1st batch",
      "ADNI3 2nd batch", "ADNI3 3rd & 4th batches"
    )
    # Lower and upper limits corresponding to lab result
    BIOMARKER_DATA_LIMITU <- tidyr::expand_grid(
      BATCH = batch_list,
      tibble::tibble(
        LBTESTCD = c("ABETA40", "ABETA42", "TPROT", "PTAU181"),
        LBORNRLO = c(NA_character_, "<200", "<80", "<8"),
        LBORNRHI = c(NA_character_, ">1700", ">1300", ">120"),
        LBTEST = c(
          "Amyloid Beta 1-40", "Amyloid Beta 1-42",
          "Total Tau Protein", "Phosphorylated Tau181 Protein"
        )
      )
    ) %>%
      dplyr::mutate(
        LBSTNRLO = LBORNRLO,
        LBSTNRHI = LBORNRHI,
        LBORRESU = "pg/mL",
        LBSTRESU = "pg/mL",
        LBNAM = "UPENN",
        LBMETHOD = "Roche Elecsys",
        LBSPEC = "PLASMA", 
        SOURCE = "UPENNBIOMK_ROCHE_ELECSYS"
      )
  }
  if (assay %in% "AlzBio3") {
    BIOMARKER_DATA_LIMITU <- tidyr::expand_grid(
      tibble::tibble(
        LBTESTCD = c("ABETA42", "TPROT", "PTAU181"),
        LBTEST = c(
          "Amyloid Beta 1-42", "Total Tau Protein",
          "Phosphorylated Tau181 Protein"
        )
      ),
      tibble::tibble(
        code_suffix = c("S", "R"),
        test_suffix = c(" - Scaled", " - Raw")
      )
    ) %>%
      dplyr::mutate(
        LBTESTCD = paste0(LBTESTCD, code_suffix),
        LBTEST = paste0(LBTEST, test_suffix),
        LBORRESU = "pg/mL",
        LBSTRESU = "pg/mL",
        LBNAM = "UPENN",
        LBMETHOD = "INNO-BIA AlzBio3",
        LBSPEC = "PLASMA", 
        SOURCE = "UPENNBIOMK_MASTER"
      ) %>%
      dplyr::select(-ends_with("suffix"))
  }

  if (assay %in% "FQ") {
    BIOMARKER_DATA_LIMITU <- tibble::tibble(
      LBTESTCD = c(
        "PT217F", "AB42F", "AB40F", "AB42AB40F", "PT217AB42F", "NFLQ", "GFAPQ"
      ),
      LBTEST = c(
        "Phosphorylated Tau217 Protein", "Amyloid Beta 1-40", "Amyloid Beta 1-42",
        "Amyloid Beta 1-42/Amyloid Beta 1-40", "P-Tau217/Abeta42 Ratio",
        "Neurofilament Light Chain Protein", "Glial Fibrillary Acidic Protein"
      )
    ) %>%
      dplyr::mutate(
        LBORRESU = "pg/mL",
        LBSTRESU = "pg/mL",
        LBSPEC = "PLASMA",
        LBNAM = "UPENN",
        LBMETHOD = dplyr::case_when(
          stringr::str_detect(LBTESTCD, "F$") ~ toupper("Fujirebio Lumipulse"),
          stringr::str_detect(LBTESTCD, "Q$") ~ toupper("SIMOA Quanterix HD-X")
        ), 
        SOURCE = "UPENN_PLASMA_FUJIREBIO_QUANTERIX"
      ) %>%
      dplyr::mutate(LBTESTCD = stringr::str_remove_all(LBTESTCD, "F$|Q$")) %>%
      assertr::assert(assertr::not_na, LBMETHOD)
  }
  return(BIOMARKER_DATA_LIMITU)
}
