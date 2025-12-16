# ADNI Study Phase ----
## List of ADNI study phase ----
#' @title List ADNI Study Protocol/Phases
#' @description This function is used to generate all the ADNI study phases.
#' @return A character vector with list of ADNI study phases.
#' @examples
#' \dontrun{
#' adni_study_phase <- adni_phase()
#' adni_study_phase
#' }
#' @rdname adni_phase
#' @family ADNI study protocol/phase
#' @keywords adni_procotol_fun
#' @export
adni_phase <- function() {
  return(c("ADNI1", "ADNIGO", "ADNI2", "ADNI3", "ADNI4"))
}

## Get Original ADNI Study Phase/Protocol -----
#' @title ADNI Original Protocol Version
#' @description
#' This function is used to identify the original ADNI study phase of subject
#'  (i.e. when subject enrolled in ADNI study as new-enrollee) based on their RID.
#' @param RID Subject RID
#' @return
#'  A character vector with the same size as RID with original study
#'  protocol (i.e. ADNI study phases).
#' @examples
#' \dontrun{
#' RID <- c(0001, 1250, 7015, 10002)
#' origprot <- original_study_protocol(RID = RID)
#' origprot
#' }
#' @rdname original_study_protocol
#' @family ADNI study protocol/phase
#' @keywords adni_procotol_fun
#' @importFrom dplyr case_when
#' @export
original_study_protocol <- function(RID) {
  origprot <- case_when(
    RID >= 1 & RID < 2000 ~ "ADNI1",
    RID >= 2000 & RID < 3000 ~ "ADNIGO",
    RID >= 3000 & RID < 6000 ~ "ADNI2",
    RID >= 6000 & RID < 10000 ~ "ADNI3",
    RID >= 10000 ~ "ADNI4"
  )
  return(origprot)
}

## ADNI Study Phase Order Number ----
#' @title ADNI Study Protocol Order
#' @description This function is used to extract the ADNI study protocol order number.
#' @param phase
#'  ADNI study protocol phases: \code{ADNI1}, \code{ADNIGO}, \code{ADNI2},
#'  \code{ADNI3} or \code{ADNI4}
#' @return A numeric vector with the same size as \code{phase} with study protocol order number.
#' @examples
#' \dontrun{
#' adni_phase_order_num(phase = c("ADNI1", "ADNI3", "ADNI4", "ADNIGO"))
#' }
#' @rdname adni_phase_order_num
#' @family ADNI study protocol/phase
#' @keywords adni_procotol_fun
#' @importFrom dplyr case_when
#' @importFrom rlang arg_match
#' @export
adni_phase_order_num <- function(phase) {
  rlang::arg_match(
    arg = phase, values = adni_phase(),
    multiple = TRUE
  )
  phase_order_num <- case_when(
    phase %in% "ADNI1" ~ 1,
    phase %in% "ADNIGO" ~ 2,
    phase %in% "ADNI2" ~ 3,
    phase %in% "ADNI3" ~ 4,
    phase %in% "ADNI4" ~ 5
  )
  return(phase_order_num)
}

## Convert ADNI Study Phase Order Number ----
#' @title Convert ADNI Study Phase Order Number
#' @description
#' This function is used to convert/replace the order number of ADNI study phase
#' with the actual study phase name.
#' @param phase_num Variable that represents the ADNI study phase order number
#' @return A character vector with list of ADNI study phases.
#' @examples
#' \dontrun{
#' convert_adni_phase_order_num(phase_num = c(1, 3, 5))
#' }
#' @rdname convert_adni_phase_order_num
#' @family ADNI study protocol/phase
#' @keywords adni_procotol_fun
#' @importFrom dplyr case_when
#' @importFrom cli cli_abort
#' @export
convert_adni_phase_order_num <- function(phase_num) {
  if (any(!phase_num[!is.na(phase_num)] %in% seq_along(adni_phase()))) {
    cli_abort(
      message = paste0(
        "{.var {phase_num}} must be either {.val {seq_along(adni_phase())}}",
        " or {.val {'NULL'}}"
      )
    )
  }
  check_object_type(phase_num, "numeric")
  phase_num <- as.numeric(phase_num)
  phase_name <- adni_phase()[phase_num]
  return(phase_name)
}

## Subject Type (Study Track) ----
#' @title Get ADNI Study Track
#' @description
#'  This function is used to identify study track (i.e. subject type) based
#'  on the provided ADNI phase.
#' @param cur_study_phase Current ADNI study phase
#' @param orig_study_phase
#'  Original study protocol that subject enrolled newly for the
#'  first time in the ADNI study
#' @return
#'  A character vector with the same size as \code{cur_study_phase} with study track.
#' @rdname adni_study_track
#' @family ADNI study protocol/phase
#' @keywords adni_procotol_fun
#' @importFrom tibble tibble
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate across case_when
#' @importFrom assertr verify
#' @importFrom cli cli_abort
#' @export
adni_study_track <- function(cur_study_phase, orig_study_phase) {
  cur_protocol <- orig_protocol <- NULL

  if (any(is.na(cur_study_phase) | is.na(orig_study_phase))) {
    cli_abort(
      message = paste0(
        "Either {.var cur_study_phase} or ",
        "{.var orig_study_phase} must not be missing"
      )
    )
  }

  output_result <- tibble(
    cur_protocol = as.character(cur_study_phase),
    orig_protocol = as.character(orig_study_phase)
  ) %>%
    mutate(across(c(cur_protocol, orig_protocol),
      ~ adni_phase_order_num(phase = .x),
      .names = "{col}_num"
    )) %>%
    mutate(pttype = case_when(
      cur_protocol_num > orig_protocol_num ~ adni_pttype()[1],
      cur_protocol_num == orig_protocol_num ~ adni_pttype()[2]
    )) %>%
    verify(nrow(.) == length(cur_study_phase))

  if (any(is.na(output_result$pttype))) {
    cli_abort(
      message = paste0("Check PTTYPE")
    )
  }

  return(output_result$pttype)
}

#' @title ADNI PTYPE
#' @return A character vector of ADNI PTYPE.
#' @examples
#' \dontrun{
#' adni_pttype()
#' }
#' @rdname adni_pttype
#' @family ADNI study protocol/phase
#' @keywords adni_procotol_fun
#' @export
adni_pttype <- function() {
  return(c("Rollover", "New"))
}

## Create ORIGPROT variable -----
#' @title Create ORIGPROT Column
#' @description
#' This function is used to create ORIGPROT in the dataset based subject RID.
#' @param .data Data.frame that contains \code{RID} variable
#' @return
#'  A data frame the same as \code{.data} with appended column of \code{ORIGPROT}.
#' @examples
#' \dontrun{
#' example_data <- tibble(RID = c(1, 1000, 3500, 6645, 1000))
#' create_orig_protocol(data = example_data)
#' }
#' @rdname create_orig_protocol
#' @family ADNI study protocol/phase
#' @keywords adni_procotol_fun
#' @importFrom dplyr relocate mutate
#' @importFrom cli cli_alert_warning
#' @export
create_orig_protocol <- function(.data) {
  RID <- ORIGPROT <- NULL
  check_colnames(.data = .data, col_names = "RID", strict = TRUE)
  orig_col <- get_cols_name(.data = .data, col_name = "ORIGPROT")
  if (!is.na(orig_col)) {
    cli::cli_alert_warning(text = paste0("{.val ORIGPROT} variable is overwritten!"))
  }
  .data <- .data %>%
    # Replaced if there is an existed ORGIPROT column
    mutate(ORIGPROT = original_study_protocol(RID = as.numeric(RID))) %>%
    relocate(ORIGPROT) %>%
    # mutate(ORIGPROT = factor(ORIGPROT, levels = adni_phase())) %>%
    assert_non_missing(ORIGPROT)
  return(.data)
}

## Create COLPROT variable ----
#' @title Create COLPROT Column
#' @description
#' This function is used to create COLPROT column in the dataset by renaming any
#' specified columns. It also checks whether the renamed columns contains only ADNI study phase.
#' @param .data Data.frame
#' @param phaseVar Phase column
#' @param .strict_check A Boolean value to apply strict check-in for missing or known values in \code{COLPOROT} variables
#' @return A data.frame the same as \code{data} with appended column of \code{COLPROT}.
#' @examples
#' \dontrun{
#' create_col_protocol(
#'   data = ADNIMERGE2::REGISTRY,
#'   phaseVar = "Phase",
#'   .strict_check = TRUE
#' )
#' create_col_protocol(
#'   data = ADNIMERGE2::VISITS,
#'   phaseVar = "Phase",
#'   .strict_check = FALSE
#' )
#' }
#' @rdname create_col_protocol
#' @family ADNI study protocol/phase
#' @keywords adni_procotol_fun
#' @importFrom cli cli_abort
#' @importFrom tidyselect all_of
#' @importFrom dplyr rename_with relocate
#' @importFrom assertr verify
#' @export
create_col_protocol <- function(.data, phaseVar = NULL, .strict_check = TRUE) {
  COLPROT <- NULL
  check_object_type(.strict_check, "logical")
  if (is.null(phaseVar)) phaseVar <- c("Phase", "PHASE", "ProtocolID", "COLPROT")
  exst_cols <- get_cols_name(.data = .data, col_name = phaseVar)
  if (length(exst_cols) > 1) {
    cli_abort(
      message = c(
        "The provided Phase/PHASE columns must be length of one. \n",
        "{.val {exst_cols}} are presented in the data."
      )
    )
  }

  value_list <- adni_phase()
  if (!.strict_check) value_list <- c(value_list, NA_character_)

  .data <- .data %>%
    {
      if (!is.na(exst_cols)) {
        mutate(., across(all_of(exst_cols), as.character)) %>%
          rename_with(., ~ paste0("COLPROT"), all_of(exst_cols)) %>%
          # mutate(., COLPROT = factor(COLPROT, levels = adni_phase())) %>%
          relocate(., COLPROT) %>%
          {
            if (.strict_check) assert_non_missing(., COLPROT) else (.)
          } %>%
          verify(., all(COLPROT %in% value_list))
      } else {
        (.)
      }
    }
  return(.data)
}

# Get Coded Values from DATADIC dataset ----
## Split Strings -----
#' @title Split Strings With Two Split Parameters
#' @description
#' This function is used to split a string using two split parameters and return a data.frame.
#' @param input_string Input string/character
#' @param spliter1 First split parameter/pattern, Default: '; |;'
#' @param spliter2 Second split parameter/pattern, Default: '=| = '
#' @return A data.frame that includes prefix and suffix values.
#' @examples
#' \dontrun{
#' input_string <- "1=BUTTER; 2=ARM; 3=SHORE; 4=LETTER; 5=QUEEN; 6=CABIN"
#' split_strings(
#'   input_string = input_string,
#'   spliter1 = "; ",
#'   spliter2 = "="
#' )
#' }
#' @rdname split_strings
#' @family Split strings
#' @keywords utils_fun
#' @seealso \code{\link{get_factor_levels_datadict}()}
#' @importFrom cli cli_abort
#' @export
split_strings <- function(input_string, spliter1 = "; |;", spliter2 = "=| = ") {
  # Splitting the input_string with spliter1
  split_pairs <- unlist(strsplit(x = input_string, split = spliter1))
  if (length(split_pairs) > 0) {
    # Splitting the input_string with spliter2
    splitted_strings <- strsplit(x = split_pairs, split = spliter2)
  } else {
    splitted_strings <- NULL
  }
  if (length(splitted_strings) > 0) {
    prefix <- sapply(splitted_strings, function(x) {
      x[[1]]
    })
    suffix <- sapply(splitted_strings, function(x) {
      x[[2]]
    })
  } else {
    prefix <- NULL
    suffix <- NULL
  }
  if (length(prefix) != length(suffix)) {
    cli_abort(
      message = c(
        "The length of {.var prefix} and {.var suffix}  must be the same. \n",
        "The length of {.var prefix} is {.cls {length(prefix)}}. \n",
        "The length of {.var suffix} is {.cls {length(suffix)}}."
      )
    )
  }
  # Remove whitespace at the beginning of the string value
  prefix <- trimws(as.character(prefix), which = "left")
  suffix <- trimws(as.character(suffix), which = "left")
  return(tibble(prefix, suffix))
}

## Utils function for get_factor_levels_datadict
create_string_split <- function(CODES, spliter1 = ";| ;| ; ", spliter2 = "=| =| = ") {
  return(split_strings(
    input_string = CODES$CODE,
    spliter1 = spliter1,
    spliter2 = spliter2
  ))
}

## Gets factor levels from DATADIC dataset ----
#' @title Gets Factor Levels from DATADIC Dataset
#' @description
#'  This function is used to generate the coded levels of FLDNAME using
#'  a data dictionary dataset \code{\link{DATADIC}()} downloaded from
#'  <https://adni.loni.usc.edu/data-samples/adni-data/>.
#' @param .datadic Data dictionary dataset
#' @param tbl_name
#'  Table name, Default: NULL that generate for all available TBLNAMEs in the \code{data_dict}
#' @param spliter1 First split parameter/pattern, Default:";| ;| ; "
#' @param spliter2 Second split parameter/pattern, Default: "=| =| = "
#' @param nested_value
#'  Unnest the factor level variables in long format if \code{TRUE}
#'  otherwise nested with \code{CODES} variable.
#' @return A data.frame that appended with the following variables:
#' \itemize{
#'   \item \emph{prefix}: Actual value in the dataset
#'   \item \emph{suffix}: Coded values from \code{\link{DATADIC}()}
#'   \item \emph{class_type}: Class type, "factor"
#'   }
#' @examples
#' \dontrun{
#' get_factor_levels_datadict(
#'   .datadic = ADNIMERGE2::DATADIC,
#'   tbl_name = NULL,
#'   nested_value = TRUE
#' )
#' }
#' @rdname get_factor_levels_datadict
#' @family data dictionary related functions
#' @keywords adni_datadic_fun
#' @seealso \code{\link{split_strings}()}
#' @importFrom stringr str_detect
#' @importFrom tidyr nest unnest
#' @importFrom purrr map
#' @importFrom dplyr filter mutate case_when rowwise ungroup
#' @export
get_factor_levels_datadict <- function(.datadic,
                                       tbl_name = NULL,
                                       nested_value = FALSE,
                                       spliter1 = ";| ;| ; ",
                                       spliter2 = "=| =| = ") {
  TBLNAME <- CODE <- CODES <- NULL
  check_colnames(
    .data = .datadic,
    col_names = c("PHASE", "TYPE", "TBLNAME", "FLDNAME", "CODE"),
    strict = TRUE
  )
  if (is.null(tbl_name)) tbl_name <- unique(.datadic$TBLNAME)
  tbl_name <- tbl_name[!is.na(tbl_name)]
  output_data <- extract_codelist_datadict(.datadic) %>%
    as_tibble() %>%
    filter(TBLNAME %in% tbl_name) %>%
    nest(CODES = CODE) %>%
    mutate(CODES = map(CODES, ~ create_string_split(
      CODES = .x,
      spliter1 = spliter1,
      spliter2 = spliter2
    ))) %>%
    {
      if (nested_value == FALSE) {
        unnest(., cols = CODES)
      } else {
        (.)
      }
    } %>%
    mutate(class_type = "factor")

  output_data <- set_datadict_tbl(output_data)

  return(output_data)
}

## Function to Extract Codelist from DATADIC ----
#' @title Extract Codedlist from DATADIC
#' @description
#' This function is used to extract codelist from a data dictionary \code{\link{DATADIC}()}.
#' @param .datadic Data dictionary dataset
#' @return A data.frame the same as \code{.datadic} with only coded list.
#' @examples
#' \dontrun{
#' extract_codelist_datadict(.datadic = ADNIMERGE2::DATADIC)
#' }
#' @rdname extract_codelist_datadict
#' @family data dictionary related functions
#' @keywords adni_datadic_fun
#' @importFrom dplyr mutate case_when filter
#' @importFrom stringr str_detect
#' @export
extract_codelist_datadict <- function(.datadic) {
  CODE <- TYPE <- TBLNAME <- FLDNAME <- NULL
  check_colnames(
    .data = .datadic,
    col_names = c("PHASE", "TYPE", "TBLNAME", "FLDNAME", "CODE"),
    strict = TRUE
  )
  # Convert -4 as missing values
  .datadic <- .datadic %>%
    convert_to_missing_value(col_name = "CODE", value = "-4", missing_char = NA)
  # Convert coded range values: i.e., 0...10 as missing values
  exc_range <- paste0(
    c(
      "\\b\\d+\\.\\.\\d+\\b", "\\b\\d+\\.\\.\\..\\d+\\b",
      'Range: 0+", "Range:  0+|^See|^Calculated from: '
    ),
    collapse = "|"
  )
  .datadic <- .datadic %>%
    mutate(CODE = case_when(
      str_detect(CODE, exc_range) == TRUE ~ NA,
      TRUE ~ CODE
    ))
  # Convert text values as missing values
  exc_text <- c('crfname|\\"indexes\\"|\\<display\\>|\"Pass;|\"OTF Lumos;|\"a; b; c; d|select distinct ')
  .datadic <- .datadic %>%
    mutate(CODE = case_when(
      str_detect(CODE, exc_text) == TRUE ~ NA,
      TRUE ~ CODE
    ))
  # Convert hardcode texts as missing values
  exc_add_text <- c(
    "in(0,1)", "Pass/Fail", "complete/partial", "Pass/Fail/Partial", "Text field",
    "ADNI code", "MM/DD/YYYY", '"ADNI" or "DIAN"', "pg/mL", "complete; partial",
    "-4=Insufficient sample", "-4 = Not Available", "NA = Not Available",
    "-4=Insufficient sample; -5=Sample quantity not sufficient"
  )
  .datadic <- .datadic %>%
    mutate(CODE = case_when(
      CODE %in% exc_add_text ~ NA,
      TRUE ~ CODE
    ))

  # Convert code with specific types as missing values
  exc_type <- c(
    "char", "date", "datetime", "decimal",
    "text", "time", "varchar", "d", "s"
  )
  .datadic <- .datadic %>%
    mutate(TYPE = tolower(TYPE)) %>%
    mutate(CODE = case_when(TYPE %in% exc_type ~ NA, TRUE ~ CODE))

  # Convert codelist of some pre-specified TBLNAME/FLDNAME as missing values
  exc_cdr <- c(
    "CDMEMORY", "CDORIENT", "CDJUDGE", "CDCOMMUN", "CDHOME",
    "CDCARE", "CDGLOBAL", "CDRSB"
  )
  exc_mri_infacts <- c("INFARCT.NUMBER", "LOCATION.XYZ", "SIDE", "SIZE")
  exc_nuropath <- c(
    "NACCMOD", "NPFORMVER", "ADCID", "NACCWRI1",
    "NACCWRI2", "NACCWRI3", "NACCYOD"
  )
  exc_uscffsxs51 <- c("IMAGETYPE")
  exc_ptdemog <- c("PTCOGBEG", "PTORIENT", "PTADDX")
  exc_fldname <- c(
    "INCNEWPT", "EXCCRIT", "FAILEXCLU", "CatFlu_Practise",
    "CATFLU_PRACTISE", "GDS"
  )
  exc_tbl <- c(
    "ADNI_DIAN_COMPARISON", "AMPRION_ASYN_SAA", "BATEMANLAB",
    "BATEMANLAB_ADNI_Plasma_Abeta4240_20221118", "BATEMANLAB_ADNI_PLASMA_ABETA4240_20221118"
  )
  exc_tbl_add <- c(
    "MRIFIND", "MRIQC", "MRINFQ", "MRIFind", "MRIQSM",
    "MAYOADIRL_MRI_MCH", "MAYOADIRL_MRI_TBMSYN"
  )

  .datadic <- .datadic %>%
    mutate(CODE = case_when(
      ((FLDNAME %in% exc_cdr & TBLNAME %in% "CDR") |
        (FLDNAME %in% exc_mri_infacts & TBLNAME %in% "MRI_INFARCTS") |
        (FLDNAME %in% exc_nuropath & TBLNAME %in% "NEUROPATH") |
        (FLDNAME %in% exc_uscffsxs51 & TBLNAME %in% "UCSFFSX51") |
        (FLDNAME %in% exc_ptdemog & TBLNAME %in% "PTDEMOG") |
        (FLDNAME %in% exc_fldname) |
        (TBLNAME %in% c(exc_tbl, exc_tbl_add))) ~ NA_character_,
      CODE %in% "" ~ NA_character_,
      TRUE ~ CODE
    ))

  # Removing all missing values
  .datadic <- .datadic %>%
    filter(!is.na(CODE))

  return(.datadic)
}

## Gets Factor Field Name (FLDNAME) ----
#' @title Gets Factor Field Name (FLDNAME)
#' @description
#'   This function is used to identify factor field name (FLDNAME) based on a
#'   data dictionary dataset \code{\link{DATADIC}()}
#'   from <https://adni.loni.usc.edu/data-samples/adni-data/>.
#' @param .datadic
#'  Data dictionary dataset that generated using \code{\link{get_factor_levels_datadict}()}
#' @param tbl_name Table name
#' @param dd_fldnames Variable names in the actual dataset, Default = NULL.
#' @return A character vector of variable names.
#' @examples
#' \dontrun{
#' data_dict_dd <- get_factor_levels_datadict(
#'   .datadic = ADNIMERGE2::DATADIC,
#'   tbl_name = NULL,
#'   nested_value = TRUE
#' )
#' # List of available factor columns in data dictionary
#' # \code{\link{DATADIC}()} for the \code{\link{CDR}()} dataset
#' get_factor_fldname(
#'   .datadic = data_dict_dd,
#'   tbl_name = "CDR",
#'   dd_fldnames = NULL
#' )
#' # List of factor columns that available in \code{\link{CDR}()} and data dictionary DATADIC
#' get_factor_fldname(
#'   .datadic = data_dict_dd,
#'   tbl_name = "CDR",
#'   dd_fldnames = colnames(ADNIMERGE2::CDR)
#' )
#' }
#' @rdname get_factor_fldname
#' @family data dictionary related functions
#' @keywords adni_datadic_fun
#' @seealso \code{\link{get_factor_levels_datadict}()} \code{\link{DATADIC}()}
#' @importFrom dplyr select filter
#' @importFrom tibble as_tibble
#' @export
get_factor_fldname <- function(.datadic, tbl_name, dd_fldnames = NULL) {
  TBLNAME <- FLDNAME <- class_type <- NULL
  is_datadict_tbl(.datadic)
  check_colnames(
    .data = .datadic,
    col_names = c("TBLNAME", "FLDNAME", "class_type"),
    strict = TRUE
  )
  temp_output <- .datadic %>%
    as_tibble() %>%
    filter(TBLNAME %in% tbl_name & class_type %in% "factor") %>%
    {
      if (!is.null(dd_fldnames)) {
        filter(., FLDNAME %in% c(dd_fldnames))
      } else {
        (.)
      }
    } %>%
    select(FLDNAME)

  unique_fldname <- unique(temp_output$FLDNAME)

  return(unique_fldname)
}

# Collecting Coded Values ----
## One Variable - Collecting Coded Values ----
#' @title Collecting Variable Coded Values
#' @description
#'  This function is used to collect the coded and decoded values for a given
#'  variable based on a data dictionary \code{\link{DATADIC}()}.
#' @param .datadic
#'    Data dictionary dataset created using \code{\link{get_factor_levels_datadict}()} function
#' @param tbl_name Dataset name
#' @param fld_name Field/column name
#' @return A list value:
#' \itemize{
#'  \item \emph{code}: Coded values
#'  \item \emph{decode}: Values that will replace the coded values (\code{code})
#'  }
#' @examples
#' \dontrun{
#' data_dict_dd <- get_factor_levels_datadict(
#'   .datadic = ADNIMERGE2::DATADIC,
#'   tbl_name = "ADAS",
#'   nested_value = TRUE
#' )
#' collect_value_mapping_single_var(
#'   .datadic = data_dict_dd,
#'   tbl_name = "ADAS",
#'   fld_name = "COCOMND"
#' )
#' }
#' @rdname collect_value_mapping_single_var
#' @family data dictionary codelist related functions
#' @keywords adni_datadic_fun
#' @seealso \code{\link{get_factor_levels_datadict}()}
#' @importFrom rlang arg_match
#' @importFrom dplyr filter select
#' @importFrom tidyr unnest
#' @importFrom tibble as_tibble
collect_value_mapping_single_var <- function(.datadic, tbl_name, fld_name) {
  FLDNAME <- PHASE <- TBLNAME <- CODES <- NULL
  is_datadict_tbl(.datadic)
  check_colnames(
    .data = .datadic,
    col_names = c("PHASE", "TBLNAME", "FLDNAME", "CODES"),
    strict = TRUE,
    stop_message = TRUE
  )
  rlang::arg_match(
    arg = fld_name,
    values = unique(.datadic$FLDNAME),
    multiple = TRUE
  )
  rlang::arg_match(
    arg = tbl_name,
    values = unique(.datadic$TBLNAME),
    multiple = TRUE
  )

  df <- .datadic %>%
    datadict_as_tibble() %>%
    filter(TBLNAME %in% tbl_name & FLDNAME %in% fld_name) %>%
    select(PHASE, TBLNAME, FLDNAME, CODES) %>%
    unnest(cols = CODES)

  if (nrow(df) > 0) {
    phase_list <- unique(df$PHASE)
    phase_value_list <- lapply(phase_list, function(x) {
      code <- df[df$PHASE %in% x, ]$prefix
      decode <- df[df$PHASE %in% x, ]$suffix
      return(
        list(
          "code" = code,
          "decode" = decode
        )
      )
    })
    names(phase_value_list) <- phase_list
  } else {
    phase_value_list <- list(
      "code" = NA,
      "decode" = NA
    )
    names(phase_value_list) <- NA
  }

  return(phase_value_list)
}

## Multiple Variable - Collecting Coded Values ----
#' @title Collecting Coded Values for Multiple Variables
#' @description This function is used to collect the coded values for a given set of variables.
#' @inheritParams collect_value_mapping_single_var
#' @param all_fld_name List of column names that contains re-coded values
#' @return
#'  List value that contains the \code{coded} and \code{decoded} values with corresponding
#'  column names that obtained using \code{\link{collect_value_mapping_single_var}} function.
#' @examples
#' \dontrun{
#' data_dict_dd <- get_factor_levels_datadict(
#'   .datadic = ADNIMERGE2::DATADIC,
#'   tbl_name = "CDR",
#'   nested_value = TRUE
#' )
#' tbl_unique_fldname <- get_factor_fldname(
#'   .datadic = data_dict_dd,
#'   tbl_name = "CDR",
#'   dd_fldnames = colnames(ADNIMERGE2::CDR)
#' )
#' collect_value_mapping(
#'   .datadic = data_dict_dd,
#'   tbl_name = "CDR",
#'   all_fld_name = tbl_unique_fldname
#' )
#' }
#' @rdname collect_value_mapping
#' @family data dictionary codelist related functions
#' @keywords adni_datadic_fun
#' @seealso \code{\link{get_factor_levels_datadict}()} \code{\link{get_factor_fldname}()}
#' @export
collect_value_mapping <- function(.datadic, tbl_name, all_fld_name) {
  is_datadict_tbl(.datadic)
  output_list <- lapply(all_fld_name, function(fld_name) {
    collect_value_mapping_single_var(
      .datadic = .datadic,
      tbl_name = tbl_name,
      fld_name = fld_name
    )
  })
  names(output_list) <- all_fld_name
  return(output_list)
}

## Convert List Collected Coded Values ----
#' @title Convert List Collected Coded Values
#' @description
#'  This function is used to convert list coded values that is created using
#'  \code{\link{collect_value_mapping}()} function.
#' @param coded_values Variable name that contains coded values
#' @param tbl_name Dataset name, Default: NULL
#' @return A data frame that contains the following columns:
#'  \item{TBLNAME }{Dataset name}
#'  \item{FLDNAME }{Variable name}
#'  \item{PHASE }{ADNI study phase/protocol}
#'  \item{CODES }{Nested dataset that contains code and decode values}
#' @examples
#' \dontrun{
#' data_dict_dd <- get_factor_levels_datadict(
#'   .datadic = ADNIMERGE2::DATADIC,
#'   tbl_name = "CDR",
#'   nested_value = TRUE
#' )
#' tbl_unique_fldname <- get_factor_fldname(
#'   .datadic = data_dict_dd,
#'   tbl_name = "CDR",
#'   dd_fldnames = colnames(ADNIMERGE2::CDR)
#' )
#' coded_value <- collect_value_mapping(
#'   .datadic = data_dict_dd,
#'   tbl_name = "CDR",
#'   all_fld_name = tbl_unique_fldname
#' )
#' convert_value_mapping(coded_value = coded_value)
#' }
#' @rdname convert_value_mapping
#' @family data dictionary codelist related functions
#' @keywords adni_datadic_fun
#' @importFrom purrr pluck
#' @importFrom dplyr mutate relocate bind_rows
#' @importFrom cli cli_abort
convert_value_mapping <- function(coded_values, tbl_name = NULL) {
  TBLNAME <- FLDNAME <- PHASE <- CODES <- NULL
  check_object_type(coded_values, "list")
  output_data <- lapply(coded_values, convert_value_mapping_phase_specific) %>%
    bind_rows(., .id = "FLDNAME") %>%
    mutate(TBLNAME = tbl_name) %>%
    relocate(any_of(c("TBLNAME", "FLDNAME", "PHASE", "CODES")))

  return(output_data)
}

#' @title Convert List Collected Coded Values - PHASE Levels
#' @description This function is used to convert list coded values at phase levels.
#' @param coded_values List object of coded values with study phase name
#' @return A data.frame that contains the following columns:
#' \itemize{
#'   \item \emph{FLDNAME:} Variable name
#'   \item \emph{PHASE:} ADNI study phase/protocol
#'   \item \emph{CODED_VALUE:} Nested dataset that contains code and decode values
#'  }
#' @rdname convert_value_mapping_phase_specific
#' @family data dictionary codelist related functions
#' @keywords adni_datadic_fun
#' @importFrom dplyr bind_rows group_by ungroup
#' @importFrom tidyselect everything
#' @importFrom tidyr nest replace_na
#' @importFrom cli cli_abort
convert_value_mapping_phase_specific <- function(coded_values) {
  PHASE <- CODES <- NULL
  check_object_type(coded_values, "list")
  names(coded_values) <- replace_na(names(coded_values), "NA")
  output_data <- lapply(coded_values, bind_rows) %>%
    bind_rows(., .id = "PHASE") %>%
    mutate(PHASE = ifelse(PHASE == "NA", NA_character_, PHASE)) %>%
    group_by(PHASE) %>%
    nest(CODES = everything() & -PHASE) %>%
    ungroup()
  return(output_data)
}

# Value Replacement -----
## Replace Multiple Values ----
#' @title Replace/Substitute Multiple Values
#' @description This function is used to replace string values.
#' @param input_string Input string
#' @param code Coded values
#' @param decode Actual string values
#' @return
#'  A character vector similar to \code{input_string} with replaced values.
#' @examples
#' \dontrun{
#' input_string <- c("-2", "1", "1", "-1")
#' code <- c("null", "1", "-1", "-2")
#' decode <- c(
#'   "pending enrollment", "randomized - assigned a scan category",
#'   "screen failed", "screen failed after randomization"
#' )
#' replaced_values <- replace_multiple_values(
#'   input_string = input_string,
#'   code = code,
#'   decode = decode
#' )
#' replaced_values
#' }
#' @rdname replace_multiple_values
#' @family function to replace values
#' @keywords utils_fun
#' @export
replace_multiple_values <- function(input_string, code, decode) {
  # To ensure the same length of decode and code
  if (length(code) != length(decode)) {
    cli_abort(
      message = c(
        "The length of {.var code} and {.var decode}  must be the same. \n",
        "The length of {.var code} is {.clas {length(code)}}. \n",
        "The length of {.var decode} is {.clas {length(decode)}}."
      )
    )
  }

  # To make sure exact replacement
  unique_place_holder <- paste0("PLACE_HOLDERS_", seq_along(code))
  output_string <- input_string

  # Checking for negative values
  negative_value <- detect_numeric_value(
    value = code,
    num_type = "negative",
    stop_message = FALSE
  )

  if (negative_value == TRUE) {
    adjusted_code <- gsub(pattern = "-", x = code, replacement = "negative")
    output_string <- gsub("-", x = output_string, replacement = "negative")
  } else {
    adjusted_code <- code
  }

  # Checking for decimal places
  decimal_value <- detect_decimal_value(value = code)

  if (decimal_value == TRUE) {
    adjusted_code <- gsub(pattern = "\\.", x = adjusted_code, replacement = "decimal")
    output_string <- gsub("\\.", x = output_string, replacement = "decimal")
  }

  # To ensure exact replacement
  adjusted_code <- paste0("\\b", adjusted_code, "\\b")
  # Replace values with actual string values
  for (i in seq_along(code)) {
    output_string <- gsub(
      pattern = adjusted_code[i],
      x = output_string,
      replacement = unique_place_holder[i], fixed = FALSE
    )
  }

  # Replace values with actual string values
  unique_place_holder <- paste0("\\b", unique_place_holder, "\\b")
  for (i in seq_along(code)) {
    output_string <- gsub(
      pattern = unique_place_holder[i], x = output_string,
      replacement = decode[i], fixed = FALSE
    )
  }

  return(output_string)
}

### Detect Numeric Values ----
#' @title Detect Numeric Values
#' @description This function is used to detect any numeric values from a string.
#' @param value Input string
#' @param num_type Numeric value type: \code{"any"}, \code{"positive"} or \code{"negative"}, Default: 'any'
#' @param stop_message
#'  A Boolean value to return a stop message when the specified numeric
#'  value type is detected \code{num_type}, Default: \code{FALSE}
#' @return A Boolean value or a stop message if \code{stop_message} is \code{TRUE}:
#'   \item{TRUE }{If any of the specified numeric type value is presented.}
#'   \item{FALSE }{Otherwise}
#' @examples
#' \dontrun{
#' string1 <- c("-2", "1", "1", "-1")
#' string2 <- c("ADNI1", "ADNI2", "ADNIGO")
#' detect_numeric_value(value = string1, num_type = "any")
#' detect_numeric_value(value = string2, num_type = "any")
#' detect_numeric_value(value = string1, num_type = "negative")
#' detect_numeric_value(value = string1, num_type = "positive", stop_message = TRUE)
#' }
#' @rdname detect_numeric_value
#' @importFrom rlang arg_match0
#' @importFrom cli cli_abort
#' @family checks function
#' @keywords utils_fun
#' @export
detect_numeric_value <- function(value, num_type = "any", stop_message = FALSE) {
  rlang::arg_match0(arg = num_type, values = c("any", "positive", "negative"))
  check_object_type(stop_message, "logical")
  value <- suppressWarnings(as.numeric(value))
  if (all(is.na(value))) {
    result <- FALSE
  }
  if (any(!is.na(value))) {
    value <- value[!is.na(value)]
    if (num_type == "any") {
      result <- is.numeric(value)
      message_prefix <- "Numeric"
    }
    if (num_type == "negative") {
      if (any(value < 0)) result <- TRUE else result <- FALSE
      message_prefix <- "Negative"
    }
    if (num_type == "positive") {
      if (any(value > 0)) result <- TRUE else result <- FALSE
      message_prefix <- "Positive"
    }
  }

  if (stop_message == TRUE & result == TRUE) {
    cli_abort(
      message = paste0(message_prefix, " value is found in the provided string!")
    )
  }

  return(result)
}

### Detect Decimal Values ----
#' @title Detect Decimal Values
#' @description This function is used to detect decimal values.
#' @param value Input string
#' @return A Boolean value
#' @examples
#' \dontrun{
#' # No decimal value is presented: return FALSE
#' detect_decimal_value(1:5)
#' detect_decimal_value(NA)
#'
#' # Detect a decimal value: return TRUE
#' detect_decimal_value(seq(1, 5, by = 0.5))
#'
#' # Detect a decimal value in a mixed string: return TRUE
#' detect_decimal_value(c("1", "2.0", "3", "999"))
#' }
#' @rdname detect_decimal_value
#' @family checks function
#' @keywords utils_fun
detect_decimal_value <- function(value) {
  # convert to numeric values
  if (all(is.na(value)) | is.null(value)) {
    return(FALSE)
  }
  numeric_value <- as.numeric(value)
  numeric_value <- numeric_value[!is.na(numeric_value)]
  contain_decimal <- FALSE
  if (any(is.numeric(numeric_value))) {
    contain_decimal <- any(grepl(pattern = "\\.", x = value, perl = FALSE, fixed = FALSE))
  }
  return(contain_decimal)
}

## Single Variable - Single Phase Specific Value Replacement -----
#' @title Replace Phase Specific Values for Single Variable
#' @description
#'  This function is used to recode/replace phase specific values of a
#'  single variable in a dataset.
#' @param .data Data.frame
#' @param fld_name Variable name
#' @param phase ADNI study phase/protocol name
#' @param phaseVar Variable name for the ADNI study protocol, Default: "PHASE"
#' @param code Values that will be replaced
#' @param decode Values that will replace coded value (\code{code})
#' @return A data.frame with replaced values for the provided variable.
#' @examples
#' \dontrun{
#' data_dict_dd <- get_factor_levels_datadict(
#'   data_dict = ADNIMERGE2::DATADIC,
#'   tbl_name = "REGISTRY", # NULL
#'   nested_value = TRUE
#' )
#' input_values <- collect_value_mapping(
#'   data_dict = data_dict_dd,
#'   tbl_name = "REGISTRY",
#'   all_fld_name = "VISTYPE"
#' )
#' decode <- input_values$VISTYPE$ADNI1$decode
#' code <- input_values$VISTYPE$ADNI1$code
#' result_dataset <- replace_values_phase_specific(
#'   .data = ADNIMERGE2::REGISTRY,
#'   phaseVar = "COLPROT",
#'   fld_name = "VISTYPE",
#'   phase = "ADNI1",
#'   code = code,
#'   decode = decode
#' )
#' }
#' @rdname replace_values_phase_specific
#' @family functions to replace values
#' @keywords adni_replace_fun
#' @importFrom rlang arg_match
#' @importFrom cli cli_abort
#' @importFrom dplyr mutate across filter pull bind_rows arrange rename_with select
#' @importFrom tidyselect all_of
#' @importFrom stringr str_detect str_c str_extract str_remove_all
replace_values_phase_specific <- function(.data, fld_name, phase,
                                          phaseVar = "PHASE", code, decode) {
  phase_var <- NULL
  if (length(fld_name) != 1) {
    cli_abort(
      message = c(
        "{.var fld_name} must be a single character vector.",
        "The length of {.va fld_name} is {.code length(fld_name)}."
      )
    )
  }
  if (length(phase) != 1) {
    cli_abort(
      message = c(
        "{.var phase} must be a single character vector.",
        "The length of {.var phase} is {.code length(phase)}."
      )
    )
  }
  if (!is.na(phase)) {
    rlang::arg_match(
      arg = phase,
      values = adni_phase(),
      multiple = TRUE
    )
  }

  rlang::arg_match(arg = phaseVar, values = names(.data))

  .data <- .data %>%
    rename_with(~ paste0("phase_var"), all_of(phaseVar)) %>%
    mutate(across(all_of(fld_name), as.character))

  phase_data <- .data %>%
    filter(phase_var %in% phase)

  phase_data[, fld_name] <- replace_multiple_values(
    input_string = phase_data %>%
      select(all_of(fld_name)) %>%
      pull(),
    code = code,
    decode = decode
  )

  # Checking for phase-specific possible values are replaced correctly
  # Only checks for non-concatenated values
  data_values <- unique(phase_data[, fld_name])
  data_values <- data_values[!is.na(data_values)]
  split_pattern <- c(";", "\\|", ":")
  check_split_pattern <- str_detect(
    string = decode,
    pattern = str_c(split_pattern, collapse = "|")
  )
  if (any(check_split_pattern)) {
    remove_split_pattern <- str_extract(
      string = decode,
      pattern = str_c(split_pattern, collapse = "|")
    )
    remove_split_pattern <- unique(remove_split_pattern[!is.na(remove_split_pattern)])
    if (length(remove_split_pattern) == 1 & all(is.na(remove_split_pattern))) {
      split_pattern <- split_pattern
    } else {
      split_pattern <- str_remove_all(
        string = split_pattern,
        pattern = remove_split_pattern
      )
    }
    split_pattern <- split_pattern[!split_pattern %in% ""]
  }
  check_value_match(
    values = data_values,
    check_list = decode,
    stop_message = TRUE,
    excluded.na = TRUE,
    add_stop_message = str_c(" in ", fld_name, " for study phase ", phase),
    value_split = TRUE,
    split_pattern = str_c(split_pattern, collapse = "|")
  )

  # Bind with previous dataset
  output_data <- bind_rows(
    phase_data,
    .data %>%
      filter(!phase_var %in% phase)
  ) %>%
    mutate(phase_var = factor(phase_var, levels = adni_phase())) %>%
    arrange(phase_var) %>%
    rename_with(~ paste0(phaseVar), phase_var)

  return(output_data)
}

## Single Variable - Multiple Phase Specific Value Replacement -----
#' @title Replace Values for Single Variable Across Multiple Phase
#' @description
#'  This function is used to replace values for a single column in the dataset
#'  across different ADNI study phases.
#' @param .data Data frame
#' @param fld_name Variable name
#' @param phaseVar Variable name for the ADNI study protocol, Default: "PHASE"
#' @param input_values
#'  A list value associated with each ADNI study phase and the format will be
#'   \emph{[phase_name]$values}.
#' \itemize{
#'   \item \emph{code}: Value that will be replaced, see more \code{\link{collect_value_mapping_single_var}()}
#'   \item \emph{decode}: Values that will replace the coded values, \code{code}
#' }
#' @return A data frame with replaced values.
#' @examples
#' \dontrun{
#' data_dict_dd <- get_factor_levels_datadict(
#'   data_dict = ADNIMERGE2::DATADIC,
#'   tbl_name = NULL,
#'   nested_value = TRUE
#' )
#' input_values <- collect_value_mapping(
#'   data_dict = data_dict_dd,
#'   tbl_name = "REGISTRY",
#'   all_fld_name = "VISTYPE"
#' )
#' input_values <- input_values$VISTYPE
#' result_dataset <- replace_values_single_var(
#'   .data = ADNIMERGE2::REGISTRY,
#'   phaseVar = "COLPROT",
#'   fld_name = "VISTYPE",
#'   input_values = input_values
#' )
#' }
#' @rdname replace_values_single_var
#' @family functions to replace values
#' @keywords adni_replace_fun
#' @importFrom cli cli_abort cli_alert_warning
replace_values_single_var <- function(.data, fld_name, phaseVar = "PHASE", input_values) {
  check_object_type(input_values, "list")
  phase_name_list <- names(input_values)
  # Adjust for non-phase specific replacements
  phase_name_list_no_na <- phase_name_list[!is.na(phase_name_list)]
  if (all(is.na(phase_name_list))) {
    cli_alert_warning(
      text = c(
        "No ADNI phase specific list name used in {.var input_values} \n",
        "{.var input_values} is a list object with names of {.val {names(input_values)}}"
      )
    )
  } else {
    if (!any(phase_name_list_no_na %in% adni_phase())) {
      cli_abort(
        message = c(
          "The name of {.var input_values} must be one of {.val {adni_phase()}}."
        )
      )
    }
  }

  for (phase in phase_name_list) {
    if (is.na(phase)) {
      adjust_phase_list <- which(is.na(phase_name_list))
      if (length(adjust_phase_list) > 1) {
        cli_abort(
          message = c("{.val {fld_name}} variable must contains only single missing value for the phase name.")
        )
      }
    } else {
      adjust_phase_list <- phase
    }
    .data <- replace_values_phase_specific(
      .data = .data,
      fld_name = fld_name,
      phaseVar = phaseVar,
      phase = phase,
      decode = input_values[[adjust_phase_list]]$decode,
      code = input_values[[adjust_phase_list]]$code
    )
  }

  return(.data)
}

## Multiple Variable - Value Replacement -----
#' @title Replace Values for Multiple Variable - Wrapper
#' @description
#'  This function is used to replace the value of multiple columns in the dataset
#'  based on the provided corresponding input values.
#' @param .data Data.frame
#' @param phaseVar Variable name for the ADNI study protocol, Default: "PHASE"
#' @param input_values
#'  A nested list values of each columns associated with corresponding ADNI
#'  study phase and the format will be: [column_name][[phase_name]]$values
#' \itemize{
#'   \item \emph{code} Value that will be replaced
#'   \item \emph{decode} Values that will replace coded value (\emph{code})
#' }
#' @return A data frame with replaced values
#' @examples
#' \dontrun{
#' data_dict_dd <- get_factor_levels_datadict(
#'   .datadic = ADNIMERGE2::DATADIC,
#'   tbl_name = NULL,
#'   nested_value = TRUE
#' )
#' all_fld_name <- get_factor_fldname(
#'   .datadic = data_dict_dd,
#'   tbl_name = "REGISTRY",
#'   dd_fldnames = colnames(ADNIMERGE2::REGISTRY)
#' )
#' input_values <- collect_value_mapping(
#'   .datadic = data_dict_dd,
#'   tbl_name = "REGISTRY",
#'   all_fld_name = all_fld_name
#' )
#' result_dataset <- replace_values_dataset(
#'   .data = ADNIMERGE2::REGISTRY,
#'   phaseVar = "COLPROT",
#'   input_values = input_values
#' )
#' }
#' @rdname replace_values_dataset
#' @family functions to replace values
#' @keywords adni_replace_fun
#' @importFrom cli cli_abort
#' @export
replace_values_dataset <- function(.data, phaseVar = "PHASE", input_values) {
  check_object_type(input_values, "list")
  check_colnames(
    .data = .data,
    col_names = phaseVar,
    stop_message = TRUE,
    strict = TRUE
  )
  check_colnames(
    .data = .data,
    col_names = names(input_values),
    stop_message = TRUE,
    strict = FALSE
  )
  for (fld_name in names(input_values)) {
    .data <- replace_values_single_var(
      .data = .data,
      fld_name = fld_name,
      phaseVar = phaseVar,
      input_values = input_values[[fld_name]]
    )
  }

  return(.data)
}

## Re-coding specific value (-4) as missing value ----
#' @title Convert Value into Missing Values
#' @description
#'  This function is used to convert specific value into missing values.
#' @param .data Data.frame
#' @param col_name Variable name
#' @param value
#'  Specific value that will be considered as missing value, Default: '-4'
#' @param missing_char Character for missing value, Default: NA
#' @param phase
#'  Phase-specific value replacement, Default: NULL for all ADNI phases
#' @return A data frame with replaced value.
#' @examples
#' \dontrun{
#' convert_to_missing_value(.data = ADNIMERGE2::ADAS)
#' }
#' @rdname convert_to_missing_value
#' @family functions to replace values
#' @keywords adni_replace_fun utils_fun
#' @importFrom cli cli_abort cli_alert_info
#' @importFrom dplyr across filter if_all case_when
#' @importFrom tibble as_tibble
#' @importFrom rlang arg_match
#' @importFrom tidyselect all_of
#' @export
convert_to_missing_value <- function(.data, col_name = NULL, value = "-4",
                                     missing_char = NA, phase = NULL) {
  if (is.null(phase)) {
    overall_replacement <- TRUE
    phase <- adni_phase()
  } else {
    overall_replacement <- FALSE
  }
  rlang::arg_match(arg = phase, values = adni_phase(), multiple = TRUE)
  phase_var <- get_cols_name(.data = .data, col_name = c("COLPROT", "PHASE", "Phase", "ProtocolID"))
  if (length(phase_var) > 1) {
    cli_abort(
      message = c(
        "Phase column must be a length of single character vector.\n",
        "There are more than one {.val {'PHASE'}} variables: {.val {phase_var}}."
      )
    )
  }
  column_list <- get_cols_value(.data = .data, value = value, col_name = col_name)

  if (all(is.na(column_list))) {
    cli_alert_info(
      text = c("No variable contains {.val {value}} value")
    )
  }

  # To make sure '-1' as missing values only in ADNI1 phases
  if (all(value %in% "-1" & "ADNI1" %in% phase & length(phase) > 1)) {
    cli_abort(
      message = c(
        "{.var {value}} must be converted to missing value for ADNI1 phase only. \n",
        "The current study phase: {.val {phase}}"
      )
    )
  }

  output_data <- .data %>%
    {
      if (all(is.na(column_list))) {
        (.)
      } else if (overall_replacement == TRUE) {
        mutate(., across(all_of(column_list), ~ case_when(
          .x %in% value ~ missing_char,
          TRUE ~ .x
        )))
      } else if (overall_replacement == FALSE & !is.na(phase_var)) {
        mutate(., across(all_of(column_list), ~ case_when(
          get(phase_var) %in% c(phase) & .x %in% value ~ missing_char,
          TRUE ~ .x
        )))
      } else {
        (.)
      }
    }

  return(output_data)
}

# Additional Utils Function ----
## Gets columns that contains specific values ------
#' @title Get Columns With Specific Values
#' @description
#'  This function is used to list columns that contains a specific value.
#' @param .data Data.frame
#' @param value Specific value
#' @param col_name
#'  Character vector of columns, Default: \code{NULL}. By default, all available columns will be checked.
#' @return
#'  A character vector of column names that contains the provided specific value.
#'  Otherwise return \code{NA}.
#' @examples
#' \dontrun{
#' get_cols_value(data = ADNIMERGE2::CDR, value = "Telephone Call")
#' }
#' @rdname get_cols_value
#' @keywords utils_fun
#' @importFrom dplyr mutate filter across if_all
#' @importFrom tibble as_tibble
#' @export
get_cols_value <- function(.data, value, col_name = NULL) {
  .data <- .data %>%
    as_tibble()
  list_columns <- lapply(colnames(.data), function(col_names) {
    temp_data <- .data %>%
      mutate(across(all_of(col_names), as.character)) %>%
      filter(if_all(all_of(col_names), ~ .x == value))
    if (nrow(temp_data) > 0) result <- col_names else result <- NA
    return(result)
  })

  list_columns <- unlist(list_columns)
  list_columns <- list_columns[!is.na(list_columns)]
  if (!is.null(col_name)) list_columns <- list_columns[list_columns %in% col_name]
  if (length(list_columns) == 0) list_columns <- NA
  return(list_columns)
}

## Checking column names in the dataset ----
#' @title Checks Column Name Exist
#' @description
#'  This function is used to check if the provided column names are existed in the dataset.
#' @param .data Data.frame
#' @param col_names Column names
#' @param strict A Boolean value to apply strict checking.
#' @param stop_message
#'  A Boolean value to return a stop message if the criteria does not met.
#' @return
#' \itemize{
#'  \item \code{TRUE} if the provided column names are existed in the dataset based on the \code{strict} argument.
#'  \item Otherwise list of column names that are not existed in the dataset or a stop message if \code{stop_message} is \code{TRUE}.
#'  }
#' @examples
#' \dontrun{
#' check_colnames(
#'   .data = ADNIMERGE2::CDR,
#'   col_names = c("Phase", "VISCODE"),
#'   strict = FALSE
#' )
#' check_colnames(
#'   .data = ADNIMERGE2::CDR,
#'   col_names = c("RID", "VISCODE"),
#'   strict = TRUE
#' )
#' }
#' @rdname check_colnames
#' @family checks function
#' @keywords utils_fun
#' @importFrom cli cli_abort
#' @export
check_colnames <- function(.data, col_names, strict = FALSE, stop_message = TRUE) {
  check_object_type(strict, "logical")
  check_object_type(stop_message, "logical")
  if (strict == TRUE) status <- !all(col_names %in% colnames(.data))
  if (strict == FALSE) status <- !any(col_names %in% colnames(.data))

  if (status) {
    non_existed_col <- col_names[!col_names %in% colnames(.data)]
    add_notes <- ifelse(length(non_existed_col) == 1,
      " column is not found in the dataset",
      " columns are not found in the dataset"
    )
    if (stop_message) {
      cli_abort(
        message = paste0(toString(non_existed_col), add_notes)
      )
    } else {
      return(paste0(non_existed_col, collapse = "; "))
    }
  } else {
    return(TRUE)
  }
}

## Get column names from a dataset ----
#' @title Extract Column Name
#' @description
#'   This function is used to get column names if the provided column name
#'   is existed in the dataset.
#' @param .data Data.frame
#' @param col_name Column names
#' @return
#'  A character vector of column names that are existed in the dataset.
#'  Otherwise return \code{NA}.
#' @examples
#' \dontrun{
#' get_cols_name(.data = ADNIMERGE2::CDR, col_name = c("Phase", "VISCODE"))
#' get_cols_name(.data = ADNIMERGE2::CDR, col_name = c("RID", "VISCODE"))
#' }
#' @rdname get_cols_name
#' @keywords utils_fun
#' @export
get_cols_name <- function(.data, col_name) {
  columns_chars <- colnames(.data)[colnames(.data) %in% col_name]
  if (length(columns_chars) == 0) columns_chars <- NA
  return(columns_chars)
}

## Value Matching Functions ----
#' @title Checks Variable Value Matching
#' @description
#'  This function is used to check if the variable values are the same as
#'  the input values.
#' @param values Existed variable values
#' @param check_list Vector of input values
#' @param excluded.na
#'  A Boolean to skip \code{NA} from the existed variable values \code{values},
#'   Default: \code{TRUE}
#' @param stop_message
#'  A Boolean value to return a stop message if one of the existed values
#'  does not match with the check list, Default: \code{FALSE}
#' @param add_stop_message
#'  Additional text message that will be added in the stop message.
#' @param value_split
#'  A Boolean value whether to split the values with specified split pattern \code{split_pattern}
#' @param split_pattern
#' Split string pattern. Only applicable if \code{value_split = TRUE}
#' @return
#' \itemize{
#'   \item {\code{TRUE}}: If all the existed variable values are matched with the input values
#'   \item {\code{FALSE}}: Otherwise and with a stop message if \code{stop_message} is \code{TRUE}
#' }
#' @examples
#' \dontrun{
#' check_value_match(
#'   values = c("-2", "2"),
#'   check_list = c("-2"),
#'   stop_message = FALSE
#' )
#' check_value_match(
#'   values = c("-2", "2", NA),
#'   check_list = c("-2", "2"),
#'   excluded.na = TRUE,
#'   stop_message = FALSE
#' )
#' }
#' @rdname check_value_match
#' @family checks function
#' @keywords internal
#' @importFrom cli cli_abort
check_value_match <- function(values,
                              check_list,
                              excluded.na = TRUE,
                              stop_message = FALSE,
                              add_stop_message = NULL,
                              value_split = FALSE,
                              split_pattern = "\\||:|;") {
  check_object_type(excluded.na, "logical")
  check_object_type(stop_message, "logical")
  values <- as.character(values)
  check_list <- as.character(check_list)
  if (excluded.na == TRUE) {
    values <- values[!is.na(values)]
    check_list <- check_list[!is.na(check_list)]
  }
  if (value_split) values <- unlist(strsplit(x = values, split = split_pattern))
  if (any(!values %in% c(check_list))) result <- FALSE else result <- TRUE
  if (result == FALSE & stop_message == TRUE) {
    non_existed_values <- values[!values %in% check_list]
    if (length(non_existed_values) < 1) {
      cli_abort(
        message = c("The length of non-existing value must be zero.")
      )
    }
    cli_abort(
      message = paste0("{.val {non_existed_values}} value{?s} {?is/are} not found ", add_stop_message)
    )
  }
  return(result)
}

## Duplicate Records Checks ----
#' @title Checks Duplicated Records - Internal
#' @description
#'  This function is used to check for any duplicated records in a dataset
#'  based on the combination of provided columns.
#' @param .data Data.frame
#' @param col_names Character vector of column names
#' @param stop_message
#'  A Boolean value to return a stop message if there is any duplicated records, Default: \code{TRUE}
#' @param return_duplicate_record
#'  A Boolean value to return any duplicated records, Default: \code{FALSE}
#' @param extra_cols
#'  Additional columns that will be returned if \code{stop_message} is \code{FALSE}
#' @return
#'  The same data.frame  as input dataset if there is not any duplicated records.
#'  Otherwise either an assert stop message or duplicated records.
#' @examples
#' \dontrun{
#' check_duplicate_records(
#'   dd = ADNIMERGE2::QS,
#'   col_names = c("USUBJID", "QSDTC", "QSTESTCD"),
#'   stop_message = TRUE,
#'   add_cols = NULL
#' )
#' }
#' @rdname check_duplicate_records
#' @family checks function
#' @keywords utils_fun
#' @importFrom dplyr select mutate across n if_all
#' @importFrom tidyr unite
#' @importFrom tidyselect all_of any_of
#' @importFrom cli cli_alert_info cli_abort
#' @export
check_duplicate_records <- function(.data,
                                    col_names,
                                    stop_message = TRUE,
                                    return_duplicate_record = FALSE,
                                    extra_cols = NULL) {
  COMBINED_ID <- NUM_RECORDS <- NULL
  check_object_type(stop_message, "logical")
  check_object_type(return_duplicate_record, "logical")

  if (is.null(col_names)) {
    cli_abort(
      message = c("{.var {col_names}} must not be missing")
    )
  }
  check_object_type(col_names, "character")

  check_records <- .data %>%
    select(all_of(col_names), any_of(extra_cols)) %>%
    filter(if_all(all_of(col_names), ~ !is.na(.x))) %>%
    mutate(across(all_of(col_names), ~., .names = "prefix_id_{col}")) %>%
    unite("COMBINED_ID", all_of(c(paste0("prefix_id_", col_names))), sep = "-/")

  if (return_duplicate_record) stop_message <- FALSE
  if (stop_message) {
    check_records <- check_records %>%
      assert_uniq(COMBINED_ID)
  }
  if (return_duplicate_record) {
    output_data <- check_records %>%
      group_by(COMBINED_ID) %>%
      mutate(NUM_RECORDS = n()) %>%
      ungroup() %>%
      filter(NUM_RECORDS > 1) %>%
      relocate(COMBINED_ID, NUM_RECORDS)
  } else {
    cli_alert_info(
      text = c("No duplicated records across {.val {col_names}} column{?s}.")
    )
    output_data <- .data
  }

  return(output_data)
}

## ID Format Functions ----
#' @title Create USUBJID from RID and SITEID
#' @description This function is used to create USUBJID from RID and SITEID.
#' @param x RID variable
#' @param y SITEID variable
#' @return
#'  A character vector of the same size as the input args with \code{STUDYID-} prefix.
#' @rdname create_usubjid
#' @family ID format functions
#' @keywords adni_id_fun
#' @importFrom stringr str_pad
#' @export
create_usubjid <- function(x, y) {
  y <- stringr::str_pad(y, width = 3, pad = "0", side = "left")
  x <- stringr::str_pad(x, width = 5, pad = "0", side = "left")
  return(paste0("ADNI-", y, "-", x))
}

#' @title Convert USUBJID to RID Format
#' @param x Character vector of ID in \code{STUDYID-SITEID-RID} format
#' @param studyID Study ID, Default: 'ADNI'
#' @return A numeric vector of \code{RID}
#' @examples
#' \dontrun{
#' random_ids <- sample(1:10000, size = 10)
#' random_siteid <- sample(1:60, size = 1)
#' example_usubjid <- create_usubjid(x = random_ids, y = create_usubjid)
#' example_rid <- convert_usubjid_to_rid(x = example_usubjid)
#' # Should be similar to random_ids values
#' example_rid
#' }
#' @rdname convert_usubjid_to_rid
#' @family ID format functions
#' @keywords adni_id_fun
#' @importFrom stringr str_remove_all
#' @export

convert_usubjid_to_rid <- function(x, studyID = "ADNI") {
  x <- stringr::str_remove_all(x, pattern = paste0(studyID, "-", "[0-9]{3}-"))
  x <- as.numeric(x)
  return(x)
}

## datadict_tbl object type -----
#' @title Convert \code{datadict_tbl} class object into \code{tibble} object
#' @param .data Input data
#' @return A tibble object
#' @rdname datadict_as_tibble
#' @keywords adni_datadic_fun
#' @importFrom tibble as_tibble
#' @export
datadict_as_tibble <- function(.data) {
  return(as_tibble(.data))
}

#' @title Set \code{datadict_tbl} object class
#' @param .data Input data
#' @return The same as the input data with additional class type of \code{datadict_tbl}.
#' @rdname set_datadict_tbl
#' @keywords adni_datadic_fun
#' @importFrom cli cli_abort
#' @export
set_datadict_tbl <- function(.data) {
  check_object_type(.data, "data.frame")
  .data <- structure(.data, class = c(class(.data), "datadict_tbl"))
  return(.data)
}

#' @title Load Single/Multiple CSV File(s) to Specific Environment
#' @description
#' This function is used to load single/multiple CSV files after importing
#' the file(s) from a specific directory or providing the full file path.
#'
#' @param input_dir Directory path, Default: NULL
#' @param full_file_path Full file path, Default: NULL.
#' @param .envr Environment for loading the data, Default: NULL.
#'    By default, it loads the data into global environment,
#'   please see \code{\link[rlang]{caller_env}}
#' @return Load imported CSV data.
#' @examples
#' \dontrun{
#' input_dir <- system.file("extradata/pacc-raw-input", package = "ADNIMERGE2")
#' # Load all CSV files to global environment
#' load_csv_files(
#'   input_dir = input_dir,
#'   full_file_path = NULL,
#'   .envr = globalenv()
#' )
#' }
#' @rdname load_csv_files
#' @keywords utils_fun
#' @importFrom rlang set_names caller_env
#' @importFrom purrr map
#' @importFrom readr read_csv
#' @importFrom cli cli_abort cli_alert_success
#' @export

load_csv_files <- function(input_dir = NULL, full_file_path = NULL, .envr = NULL) {
  if (!is.null(input_dir) & !is.null(full_file_path)) {
    cli::cli_abort(
      message = "Only one of {.var input_dir} and {.var full_file_path} must be provide."
    )
  }
  if (is.null(input_dir) & is.null(full_file_path)) {
    cli::cli_abort(
      message = paste0(
        "At least one of {.var input_dir} and ",
        "{.var full_file_path} must not be missing."
      )
    )
  }
  if (!is.null(input_dir)) {
    full_file_path <- list.files(
      path = input_dir,
      pattern = "\\.csv$",
      full.names = TRUE,
      all.files = FALSE
    )
  }

  combined_data_file <- full_file_path %>%
    rlang::set_names(basename(full_file_path)) %>%
    purrr::map(
      ~ readr::read_csv(.x, guess_max = Inf, show_col_types = FALSE)
    )
  names(combined_data_file) <- gsub("\\.csv$", "", (names(combined_data_file)))
  if (is.null(.envr)) .envr <- rlang::caller_env()
  list2env(combined_data_file, envir = .envr)
  success_text <- sprintf(
    paste0(
      "Load {.val {names(combined_data_file)[%d]}} data to ",
      "{.cls {rlang::env_name(.envr)}} environment. \n"
    ),
    seq_along(names(combined_data_file))
  )
  cli::cli_alert_success(
    text = success_text
  )
}
