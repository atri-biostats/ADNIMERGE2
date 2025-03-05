# ADNI Study Phase ----
## List of ADNI study phase ----
#' @title Function that list ADNI study phase
#' @description This function is used to generate all the ADNI study phases.
#' @return A character vector with list of ADNI study phases.
#' @rdname adni_phase
#' @examples
#' \dontrun{
#' adni_study_phase <- adni_phase()
#' adni_study_phase
#' }
#' @family ADNI study protocol/phase
#' @export
adni_phase <- function() {
  return(c("ADNI1", "ADNIGO", "ADNI2", "ADNI3", "ADNI4"))
}

## Get Original ADNI Study Phase/Protocol -----
#' @title ADNI Original Protocol Version
#' @description This function is used to identify the original ADNI study phase of participant (i.e. when participant newly enrolled in ADNI study) based on their RID.
#' @param RID Participant RID
#' @return A character vector with the same size as RID with original study protocol (i.e. ADNI study phases).
#' @examples
#' \dontrun{
#' RID <- c(0001, 1250, 7015, 10002)
#' origprot <- original_study_protocol(RID = RID)
#' origprot
#' }
#' @keywords ADNI study protocol
#' @family ADNI study protocol/phase
#' @rdname original_study_protocol
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
#' @param phase ADNI study protocol phases: ADNI1, ADNIGO, ADNI2, ADNI3 or ADNI4
#' @return A numeric vector with the same size as `phase` with study protocol order number.
#' @examples
#' \dontrun{
#' phase_order_num <- adni_phase_order_num(phase = c("ADNI1", "ADNI3", "ADNI4", "ADNIGO"))
#' phase_order_num
#' }
#' @keywords ADNI study protocol
#' @family ADNI study protocol/phase
#' @rdname adni_phase_order_num
#' @importFrom dplyr case_when
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
#' @description This function is used to convert/replace the order number of ADNI study phase with the actual study phase name.
#' @param phase_num Variable that represents the ADNI study phase order number
#' @return A character vector with list of ADNI study phases.
#' @rdname convert_adni_phase_order_num
#' @examples
#' \dontrun{
#' convert_adni_phase_order_num(phase_num = c(1, 3, 5))
#' }
#' @keywords ADNI study protocol
#' @family ADNI study protocol/phase
#' @importFrom dplyr case_when
#' @export
convert_adni_phase_order_num <- function(phase_num) {
  if (any(!phase_num[!is.na(phase_num)] %in% seq_along(adni_phase()))) {
    stop("`phase_num` value must be either ", seq_along(adni_phase()), " or NULL")
  }
  if (!is.numeric(phase_num)) stop("`phase_num` must be numeric.")
  phase_num <- as.numeric(phase_num)
  phase_name <- adni_phase()[phase_num]
  return(phase_name)
}

## Participant Type (Study Track) ----
#' @title Get Participant Type (ADNI Study Track)
#' @description This function is used to identify participant type (i.e. study track) based on the provided ADNI phase.
#' @param cur_study_phase Current ADNI study phase
#' @param orig_study_phase Original study protocol that a participant enrolled newly for the first time in the ADNI study
#' @return A character vector with the same size as `cur_study_phase` with study track.
#' @keywords ADNI study track
#' @importFrom tibble tibble
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate across case_when
#' @importFrom assertr verify
#' @rdname adni_study_track
#' @export
adni_study_track <- function(cur_study_phase, orig_study_phase) {
  cur_protocol <- orig_protocol <- NULL

  if (any(is.na(cur_study_phase) | is.na(orig_study_phase))) stop("Either `cur_study_phase` or `orig_study_phase` must not be empty")

  output_result <- tibble(
    cur_protocol = cur_study_phase,
    orig_protocol = orig_study_phase
  ) %>%
    mutate(across(c(cur_protocol, orig_protocol),
      ~ adni_phase_order_num(phase = .x),
      .names = "{col}_num"
    )) %>%
    mutate(study_track = case_when(
      cur_protocol_num > orig_protocol_num ~ "Rollover",
      cur_protocol_num == orig_protocol_num ~ "New"
    )) %>%
    verify(nrow(.) == length(cur_study_phase))

  if (any(is.na(output_result$study_track))) stop("Check study protocol phase")

  return(output_result$study_track)
}

## Create ORIGPROT variable -----
#' @title Create ORIGPROT Column
#' @description This function is used to create ORIGPROT in the dataset based participant RID.
#' @param data Data frame that contains `RID` variable
#' @return A data frame the same as `data` with appended columns of "ORIGPROT".
#' @examples
#' \dontrun{
#' example_data <- tibble(RID = c(1, 1000, 3500, 6645, 1000))
#' create_orig_protocol(data = example_data)
#' }
#' @family ADNI study protocol/phase
#' @rdname create_orig_protocol
#' @importFrom dplyr relocate mutate
#' @export
create_orig_protocol <- function(data) {
  RID <- ORIGPROT <- NULL
  check_colnames(data = data, col_names = "RID", strict = TRUE)
  data <- data %>%
    # Replaced if there is an existed ORGIPROT column
    mutate(ORIGPROT = original_study_protocol(RID = as.numeric(RID))) %>%
    relocate(ORIGPROT) %>%
    # mutate(ORIGPROT = factor(ORIGPROT, levels = adni_phase())) %>%
    assert_non_missing(ORIGPROT)
  return(data)
}

## Create COLPROT variable ----
#' @title Create COLPROT Column
#' @description This function is used to create COLPROT column in the dataset by renaming any specified columns.
#'  It also checks whether the renamed columns contains only ADNI study phase.
#' @param data Data frame
#' @param phaseVar Phase column
#' @return A data frame the same as `data` with appended columns of "COLPROT"
#' @family ADNI study protocol/phase
#' @examples
#' \dontrun{
#' create_col_protocol(data = ADNIMERGE2::VISITS, phaseVar = "Phase")
#' }
#' @rdname create_col_protocol
#' @importFrom tidyselect all_of
#' @importFrom dplyr rename_with relocate
#' @importFrom assertr verify
#' @export
create_col_protocol <- function(data, phaseVar = NULL) {
  COLPROT <- NULL
  if (is.null(phaseVar)) phaseVar <- c("Phase", "PHASE", "ProtocolID", "COLPROT")
  existed_column <- extract_cols(data = data, col_name = phaseVar)
  if (length(existed_column) > 1) stop("Check number of column names of Phase/PHASE")

  data <- data %>%
    {
      if (!is.na(existed_column)) {
        mutate(., across(all_of(existed_column), as.character)) %>%
          rename_with(., ~ paste0("COLPROT"), .cols = all_of(existed_column)) %>%
          # mutate(., COLPROT = factor(COLPROT, levels = adni_phase())) %>%
          relocate(., COLPROT) %>%
          assert_non_missing(., COLPROT) %>%
          verify(all(COLPROT %in% adni_phase()))
      } else {
        (.)
      }
    }
  return(data)
}

# Get Coded Values from DATADIC dataset ----
## Split Strings -----
#' @title Split strings with two split parameters
#' @description This function is used to split a string using two split parameters and returned a data.frame.
#' @param input_string Input string/character
#' @param spliter1 First split parameter/pattern, Default: '; |;'
#' @param spliter2 Second split parameter/pattern, Default: '=| = '
#' @return A data.frame that includes prefix and suffix values.
#' @examples
#' \dontrun{
#' input_string <- "1=BUTTER; 2=ARM; 3=SHORE; 4=LETTER; 5=QUEEN; 6=CABIN"
#' split_strings(input_string = input_string)
#' }
#' @rdname split_strings
#' @seealso \code{\link{get_factor_levels_datadict}()}
#' @export
split_strings <- function(input_string, spliter1, spliter2) {
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
  if (length(prefix) != length(suffix)) stop("The length of prefix and suffix must be the same")
  # Removing whitespace at the beginning of the character value
  prefix <- trimws(as.character(prefix), which = "left")
  suffix <- trimws(as.character(suffix), which = "left")
  return(tibble(prefix, suffix))
}

## Hepler function in get_factor_levels_datadict function
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
#'  This function is used to generate the coded levels of FLDNAME using the DATADIC dataset \\href{https://adni.loni.usc.edu/data-samples/adni-data/}{https://adni.loni.usc.edu/data-samples/adni-data/}.
#' @param data_dict Data dictionary dataset (DATADIC from the package or loni website)
#' @param tbl_name Table name, Default: NULL that generate for all available TBLNAMEs in the `data_dict`
#' @param spliter1 First split parameter/pattern, Default:";| ;| ; "
#' @param spliter2 Second split parameter/pattern, Default: "=| =| = "
#' @param nested_value Unnest the factor level variables in long format if 'TRUE' otherwise nested with `CODES` column
#' @return A data.frame that appended with `prefix` (actual value in the dataset), `suffix` (coded values) and `class_type` ("factor") columns.
#' @examples
#' \dontrun{
#' get_factor_levels_datadict(
#'   data_dict = ADNIMERGE2::DATADIC,
#'   tbl_name = NULL,
#'   nested_value = TRUE
#' )
#' }
#' @rdname get_factor_levels_datadict
#' @seealso \code{\link{split_strings}()}
#' @importFrom stringr str_detect
#' @importFrom tidyr nest unnest
#' @importFrom purrr map
#' @importFrom dplyr filter mutate case_when rowwise ungroup
#' @export
get_factor_levels_datadict <- function(data_dict, tbl_name = NULL, nested_value = FALSE,
                                       spliter1 = ";| ;| ; ", spliter2 = "=| =| = ") {
  TBLNAME <- CODE <- CODES <- NULL
  check_colnames(
    data = data_dict,
    col_names = c("PHASE", "TYPE", "TBLNAME", "FLDNAME", "CODE"),
    strict = TRUE
  )
  if (is.null(tbl_name)) tbl_name <- unique(data_dict$TBLNAME)
  tbl_name <- tbl_name[!is.na(tbl_name)]
  output_data <- extract_codelist_datadict(data_dict) %>%
    as_tibble() %>%
    filter(TBLNAME %in% tbl_name) %>%
    rowwise() %>%
    nest(CODES = CODE) %>%
    ungroup() %>%
    mutate(CODES = map(CODES, ~ create_string_split(CODES = .x, spliter1 = ";| ;| ; ", spliter2 = "=| =| = "))) %>%
    {
      if (nested_value == FALSE) {
        unnest(., cols = CODES)
      } else {
        (.)
      }
    } %>%
    mutate(class_type = "factor")

  return(output_data)
}

## Function to Extract Codelist from DATADIC ----
#' @title Extract Codedlist from DATADIC
#' @description This function is used to extract codelist FLDNAME from DATADIC.
#' @param data_dict Data dictionary dataset, DATADIC
#' @return A data.frame the same as `data_dict` with only coded list.
#' @examples
#' \dontrun{
#' extract_codelist_datadict(data_dict = ADNIMERGE2::DATADIC)
#' }
#' @rdname extract_codelist_datadict
#' @importFrom dplyr mutate case_when filter
#' @importFrom stringr str_detect
#' @export
extract_codelist_datadict <- function(data_dict) {
  CODE <- TYPE <- TBLNAME <- FLDNAME <- NULL
  check_colnames(
    data = data_dict,
    col_names = c("PHASE", "TYPE", "TBLNAME", "FLDNAME", "CODE"),
    strict = TRUE
  )
  # Convert -4 as missing values
  data_dict <- data_dict %>%
    convert_to_missing_value(col_name = "CODE", value = "-4", missing_char = NA)
  # Convert coded range values: i.e., 0...10 as missing values
  exc_range <- paste0(c("\\b\\d+\\.\\.\\d+\\b", "\\b\\d+\\.\\.\\..\\d+\\b", "Range: 0+", "Range:  0+"),
    collapse = "|"
  )
  data_dict <- data_dict %>%
    mutate(CODE = case_when(
      str_detect(CODE, exc_range) == TRUE ~ NA,
      TRUE ~ CODE
    ))
  # Convert text values as missing values
  exc_text <- c('crfname|\\"indexes\\"|\\<display\\>|\"Pass;|\"OTF Lumos;|\"a; b; c; d|select distinct ')
  data_dict <- data_dict %>%
    mutate(CODE = case_when(
      str_detect(CODE, exc_text) == TRUE ~ NA,
      TRUE ~ CODE
    ))
  # Convert hardcode texts as missing values
  exc_add_text <- c(
    "in(0,1)", "Pass/Fail", "complete/partial", "Pass/Fail/Partial", "Text field",
    "ADNI code", "MM/DD/YYYY", '"ADNI" or "DIAN"', "pg/mL", "complete; partial",
    "-4=Insufficient sample", "-4=Insufficient sample; -5=Sample quantity not sufficient", 
    "-4 = Not Available", "NA = Not Available"
  )
  data_dict <- data_dict %>%
    mutate(CODE = case_when(
      CODE %in% exc_add_text ~ NA,
      TRUE ~ CODE
    ))

  # Convert code with specific types as missing values
  exc_type <- c("char", "date", "datetime", "decimal", "text", "time", "varchar", "d", "s")
  data_dict <- data_dict %>%
    mutate(TYPE = tolower(TYPE)) %>%
    mutate(CODE = case_when(TYPE %in% exc_type ~ NA, TRUE ~ CODE))

  # Convert codelist of some pre-specified TBLNAME/FLDNAME as missing values
  exc_mri_infacts <- c("INFARCT.NUMBER", "LOCATION.XYZ", "SIDE", "SIZE")
  exc_nuropath <- c("NACCMOD", "NPFORMVER", "ADCID", "NACCWRI1", "NACCWRI2", "NACCWRI3", "NACCYOD")
  exc_uscffsxs51 <- c("IMAGETYPE")
  exc_ptdemog <- c("PTCOGBEG", "PTORIENT")
  exc_fldname <- c(
    "INCNEWPT", "EXCCRIT", "FAILEXCLU", "CatFlu_Practise", "CATFLU_PRACTISE",
    "CDMEMORY", "CDORIENT", "CDJUDGE", "CDCOMMUN",
    "CDHOME", "CDCARE", "CDGLOBAL", "CDRSB", "GDS"
  )
  exc_tbl <- c(
    "ADNI_DIAN_COMPARISON", "AMPRION_ASYN_SAA", "BATEMANLAB",
    "BATEMANLAB_ADNI_Plasma_Abeta4240_20221118", "BATEMANLAB_ADNI_PLASMA_ABETA4240_20221118"
  )
  exc_tbl_add <- c("MRIFIND", "MRIQC", "MRINFQ", "MRIFind", "MRIQSM", "MAYOADIRL_MRI_MCH", "MAYOADIRL_MRI_TBMSYN")

  data_dict <- data_dict %>%
    mutate(CODE = case_when(
      ((FLDNAME %in% exc_mri_infacts & TBLNAME %in% "MRI_INFARCTS") |
        (FLDNAME %in% exc_nuropath & TBLNAME %in% "NEUROPATH") |
        (FLDNAME %in% exc_uscffsxs51 & TBLNAME %in% "UCSFFSX51") |
        (FLDNAME %in% exc_ptdemog & TBLNAME %in% "PTDEMOG") |
        (FLDNAME %in% exc_fldname) |
        (TBLNAME %in% c(exc_tbl, exc_tbl_add))) ~ NA,
      TRUE ~ CODE
    ))

  # Removing all missing values
  data_dict <- data_dict %>%
    filter(!is.na(CODE))

  return(data_dict)
}

## Gets Factor Field Name (FLDNAME) ----
#' @title Gets Factor Field Name (FLDNAME)
#' @description
#'   This function is used to identify factor field name (FLDNAME) based on the DATADIC dataset from \href{https://adni.loni.usc.edu/data-samples/adni-data/}{https://adni.loni.usc.edu/data-samples/adni-data/}.
#' @param data_dict Data dictionary dataset that generated using get_factor_levels_datadict
#' @param tbl_name Table name
#' @param dd_fldnames List of column names in the actual dataset, Default = NULL.
#' @return A character vector
#' @examples
#' \dontrun{
#' data_dict_dd <- get_factor_levels_datadict(
#'   data_dict = ADNIMERGE2::DATADIC,
#'   tbl_name = NULL,
#'   nested_value = TRUE
#' )
#' # List of available factor columns in data dictionary DATADIC for ADAS_ADNIGO23 dataset
#' get_factor_fldname(
#'   data_dict = data_dict_dd,
#'   tbl_name = "ADAS",
#'   dd_fldnames = NULL
#' )
#' # List of factor columns that available in `ADAS_ADNIGO23` and data dictionary DATADIC
#' get_factor_fldname(
#'   data_dict = data_dict_dd,
#'   tbl_name = "ADAS",
#'   dd_fldnames = colnames(ADNIMERGE2::ADAS_ADNIGO23)
#' )
#' }
#' @rdname get_factor_fldname
#' @seealso \code{\link{get_factor_levels_datadict}()} \code{\link{DATADIC}()}
#' @importFrom dplyr select filter
#' @importFrom tibble as_tibble
#' @export
get_factor_fldname <- function(data_dict, tbl_name, dd_fldnames = NULL) {
  TBLNAME <- FLDNAME <- class_type <- NULL
  check_colnames(
    data = data_dict,
    col_names = c("TBLNAME", "FLDNAME", "class_type"),
    strict = TRUE
  )
  temp_output <- data_dict %>%
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
#' @description This function is used to collect the coded values for a given variable.
#' @param data_dict Data dictionary dataset that generated using get_factor_levels_datadict functions
#' @param tbl_name Dataset name
#' @param fld_name Field/column name
#' @return List value
#'  \itemize{
#'    \item `code` Coded values
#'    \item `decode` Values that will replace the old values, `code`
#'  }
#' @examples
#' \dontrun{
#' data_dict_dd <- get_factor_levels_datadict(
#'   data_dict = ADNIMERGE2::DATADIC,
#'   tbl_name = "ADAS",
#'   nested_value = TRUE
#' )
#' single_collect_values(
#'   data_dict = data_dict_dd,
#'   tbl_name = "ADAS",
#'   fld_name = "COCOMND"
#' )
#' }
#' @rdname single_collect_values
#' @family extract coded values
#' @seealso \code{\link{get_factor_levels_datadict}()}
#' @importFrom rlang arg_match
#' @importFrom dplyr filter select
#' @importFrom tidyr unnest
single_collect_values <- function(data_dict, tbl_name, fld_name) {
  FLDNAME <- PHASE <- TBLNAME <- CODES <- NULL
  check_colnames(
    data = data_dict,
    col_names = c("PHASE", "TBLNAME", "FLDNAME", "CODES"),
    strict = TRUE,
    stop_message = TRUE
  )
  rlang::arg_match(
    arg = fld_name,
    values = unique(data_dict$FLDNAME),
    multiple = TRUE
  )
  rlang::arg_match(
    arg = tbl_name,
    values = unique(data_dict$TBLNAME),
    multiple = TRUE
  )

  df <- data_dict %>%
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
#' @param data_dict Data dictionary dataset that generated using get_factor_levels_datadict functions
#' @param tbl_name Dataset name
#' @param all_fld_name List of column names that contains re-coded values
#' @return
#'    List value that contains `code` and `decode` from \code{\link{single_collect_values}} with corresponding column names.
#' @rdname collect_values
#' @family Extract Coded Values
#' @seealso \code{\link{get_factor_levels_datadict}()}
#' @examples
#' \dontrun{
#' data_dict_dd <- get_factor_levels_datadict(
#'   data_dict = ADNIMERGE2::DATADIC,
#'   tbl_name = "ADAS",
#'   nested_value = TRUE
#' )
#' tbl_unique_fldname <- get_factor_fldname(
#'   data_dict = data_dict_dd,
#'   tbl_name = "ADAS",
#'   dd_fldnames = colnames(ADNIMERGE2::ADAS_ADNIGO23)
#' )
#' collect_values(
#'   data_dict = data_dict_dd,
#'   tbl_name = "ADAS",
#'   all_fld_name = tbl_unique_fldname
#' )
#' }
#' @export
collect_values <- function(data_dict, tbl_name, all_fld_name) {
  output_list <- lapply(all_fld_name, function(fld_name) {
    single_collect_values(
      data_dict = data_dict,
      tbl_name = tbl_name,
      fld_name = fld_name
    )
  })
  names(output_list) <- all_fld_name
  return(output_list)
}


## Convert List Collected Coded Values ----
#' @title Convert List Collected Coded Values
#' @description This function is used to convert list coded values that obtained using collect_values function.
#' @param coded_values Coded values that generated using [{collect_values}()]
#' @param tbl_name Dataset name, Default: NULL
#' @return A data frame that contains the following columns:
#'  \itemize{
#'    \item{TBLNAME }{Dataset name}
#'     \item{FLDNAME }{Variable name}
#'     \item{PHASE }{ADNI study phase/protocol}
#'     \item{CODES}{Nested dataset that contains code and decode values}
#'  }
#' @examples
#' \dontrun{
#' data_dict_dd <- get_factor_levels_datadict(
#'   data_dict = ADNIMERGE2::DATADIC,
#'   tbl_name = "ADAS",
#'   nested_value = TRUE
#' )
#' tbl_unique_fldname <- get_factor_fldname(
#'   data_dict = data_dict_dd,
#'   tbl_name = "ADAS",
#'   dd_fldnames = colnames(ADNIMERGE2::ADAS_ADNIGO23)
#' )
#' coded_value <- collect_values(
#'   data_dict = data_dict_dd,
#'   tbl_name = "ADAS",
#'   all_fld_name = tbl_unique_fldname
#' )
#' convert_collect_values(coded_value = coded_value)
#' }
#' @rdname convert_collect_values
#' @importFrom purrr pluck
#' @importFrom dplyr mutate relocate bind_rows
convert_collect_values <- function(coded_values, tbl_name = NULL) {
  TBLNAME <- FLDNAME <- PHASE <- CODES <- NULL
  if (!is.list(coded_values)) stop("`coded_values` must be a list object.")

  output_data <- lapply(names(coded_values), function(fld) {
    curt_coded_values <- coded_values %>%
      pluck(., fld)
    # Apply phase-specific function
    phase_result <- convert_phase_specific(coded_values = curt_coded_values) %>%
      mutate(FLDNAME = fld)
    return(phase_result)
  }) %>%
    bind_rows() %>%
    mutate(TBLNAME = tbl_name) %>%
    relocate(any_of(c("TBLNAME", "FLDNAME", "PHASE", "CODES")))

  return(output_data)
}

#' @title Convert List Collected Coded Values - PHASE Levels
#' @description This function is used to convert list coded values at phase levels.
#' @param coded_values Collect coded values
#' @return A data frame that contains the following columns:
#'  \itemize{
#'     \item{FLDNAME }{Variable name}
#'     \item{PHASE }{ADNI study phase/protocol}
#'     \item{CODED_VALUE}{Nested dataset that contains code and decode values}
#'  }
#' @rdname convert_phase_specific
#' @importFrom purrr pluck
#' @importFrom dplyr mutate relocate bind_rows group_by ungroup
#' @importFrom tidyselect everything
#' @importFrom tidyr nest
convert_phase_specific <- function(coded_values) {
  PHASE <- CODES <- NULL
  if (!is.list(coded_values)) stop("`coded_values` must be a list object.")
  output_data <- lapply(names(coded_values), function(phase_name) {
    if (is.na(phase_name)) {
      coded_values$`NA` %>%
        bind_rows() %>%
        mutate(PHASE = phase_name)
    } else {
      coded_values %>%
        pluck(., phase_name) %>%
        bind_rows() %>%
        mutate(PHASE = phase_name)
    }
  }) %>%
    bind_rows() %>%
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
#' @param code Old value
#' @param decode New value
#' @return A character vector similar to `input_string` with replaced values
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
#' @family replace values
#' @export
replace_multiple_values <- function(input_string, code, decode) {
  # To ensure the same length of decode and code
  if (length(code) != length(decode)) stop("The length of decode and code must be the same.")

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
  # To ensure exact replacement
  adjusted_code <- paste0("\\b", adjusted_code, "\\b")
  # Replace values with actual new values
  for (i in seq_along(code)) {
    output_string <- gsub(
      pattern = adjusted_code[i],
      x = output_string,
      replacement = unique_place_holder[i], fixed = FALSE
    )
  }

  # Replace values with actual new values
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
#' @param num_type Numeric value type: `any`, `positive` or `negative`, Default: 'any'
#' @param stop_message A boolean value to return a stop message when there is the specified numeric value type `num_type`, Default: `FALSE`
#' @return A boolean value: `TRUE` if any of the specified numeric type value is presented. Otherwise `FALSE`.
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
#' @export
detect_numeric_value <- function(value, num_type = "any", stop_message = FALSE) {
  rlang::arg_match0(arg = num_type, values = c("any", "positive", "negative"))
  if (!is.logical(stop_message)) stop("`stop_message` must be a boolean value")

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

  if (stop_message == TRUE & result == TRUE) stop(message_prefix, " value is found in the provided string!")

  return(result)
}

## Single Variable - Single Phase Specific Value Replacement -----
#' @title Replace Phase Specific Values for Single Variable
#' @description This function is used to recode/replace phase specific values of a single variable in a dataset.
#' @param data Data frame
#' @param fld_name Variable name
#' @param phase ADNI study phase/protocol name
#' @param phaseVar Variable name for the ADNI study protocol, Default: "PHASE"
#' @param code Values that will be replaced
#' @param decode Values that will replace the old values, 'code'
#' @return A data frame with replaced values for the provided variable.
#' @examples
#' \dontrun{
#' data_dict_dd <- get_factor_levels_datadict(
#'   data_dict = ADNIMERGE2::DATADIC,
#'   tbl_name = "REGISTRY", # NULL
#'   nested_value = TRUE
#' )
#' input_values <- collect_values(
#'   data_dict = data_dict_dd,
#'   tbl_name = "REGISTRY",
#'   all_fld_name = "VISTYPE"
#' )
#' decode <- input_values$VISTYPE$ADNI1$decode
#' code <- input_values$VISTYPE$ADNI1$code
#' result_dataset <- phase_specific_value_replacement(
#'   data = ADNIMERGE2::REGISTRY,
#'   phaseVar = "COLPROT",
#'   fld_name = "VISTYPE",
#'   phase = "ADNI1",
#'   code = code,
#'   decode = decode
#' )
#' }
#' @rdname phase_specific_value_replacement
#' @family replace values
#' @importFrom rlang arg_match
#' @importFrom dplyr mutate across filter pull bind_rows arrange rename_with
#' @importFrom tidyselect all_of
#' @importFrom stringr str_detect str_c str_extract str_remove_all
phase_specific_value_replacement <- function(data, fld_name, phase = NULL, phaseVar = "PHASE", code, decode) {
  phase_var <- NULL
  if (is.null(phase)) {
    rlang::arg_match(
      arg = phase,
      values = adni_phase(),
      multiple = TRUE
    )
  }

  rlang::arg_match(arg = phaseVar, values = names(data))

  data <- data %>%
    rename_with(~ paste0("phase_var"), all_of(phaseVar)) %>%
    mutate(across(all_of(fld_name), ~ as.character(.x)))

  phase_data <- data %>%
    filter(phase_var %in% phase)

  phase_data[, fld_name] <- replace_multiple_values(
    input_string = phase_data %>% pull(fld_name),
    code = code,
    decode = decode
  )

  # Checking for all phase-specific possible values are replaced correctly
  # Only checks if there no concatenate values
  data_values <- unique(phase_data %>% pull(fld_name))
  data_values <- data_values[!is.na(data_values)]
  split_pattern <- c(";", "\\|", ":")
  check_split_pattern <- str_detect(string = decode, pattern = str_c(split_pattern, collapse = "|"))
  if (any(check_split_pattern)) {
    remove_split_pattern <- str_extract(string = decode, pattern = str_c(split_pattern, collapse = "|"))
    remove_split_pattern <- unique(remove_split_pattern[!is.na(remove_split_pattern)])
    if (length(remove_split_pattern) == 1 & all(is.na(remove_split_pattern))) {
      split_pattern <- split_pattern
    } else {
      split_pattern <- str_remove_all(string = split_pattern, pattern = remove_split_pattern)
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
    data %>%
      filter(!phase_var %in% phase)
  ) %>%
    mutate(phase_var = factor(phase_var, levels = adni_phase())) %>%
    arrange(phase_var) %>%
    rename_with(~ paste0(phaseVar), phase_var)

  return(output_data)
}

## Single Variable - Multiple Phase Specific Value Replacement -----
#' @title Value Replace for Single Column
#' @description This function is used to replace values for a single column in the dataset across different ADNI study phases.
#' @param data Data frame
#' @param fld_name Variable name
#' @param phaseVar Variable name for the ADNI study protocol, Default: "PHASE"
#' @param input_values A list value associated with each ADNI study phase and should be in this format: [phase_list]$values
#'    \itemize{
#'     \item `code` Value that will be replaced
#'     \item `decode` Values that will replace the old values, `code`
#'  }
#' @return A data frame with replaced values
#' @examples
#' \dontrun{
#' data_dict_dd <- get_factor_levels_datadict(
#'   data_dict = ADNIMERGE2::DATADIC,
#'   tbl_name = NULL,
#'   nested_value = TRUE
#' )
#' input_values <- collect_values(
#'   data_dict = data_dict_dd,
#'   tbl_name = "REGISTRY",
#'   all_fld_name = "VISTYPE"
#' )
#' input_values <- input_values$VISTYPE
#' result_dataset <- multiple_phase_value_replacement(
#'   data = ADNIMERGE2::REGISTRY,
#'   phaseVar = "COLPROT",
#'   fld_name = "VISTYPE",
#'   input_values = input_values
#' )
#' }
#' @rdname multiple_phase_value_replacement
#' @family replace values
multiple_phase_value_replacement <- function(data, fld_name, phaseVar = "PHASE", input_values) {
  # Input values should be in list format
  if (!is.list(input_values)) stop("`input_values` must be a list object")

  phase_name_list <- names(input_values)
  # Adjust for non-phase specific replacements
  phase_name_list_no_na <- phase_name_list[!is.na(phase_name_list)]
  if (all(is.na(phase_name_list))) {
    warning("No ADNI phase specific list name used")
  } else {
    if (length(phase_name_list_no_na) != length(phase_name_list)) stop("the list name should not contains `NA` and actual ADNI phase")
    if (!any(phase_name_list_no_na %in% adni_phase())) stop("the list name should be ADNI phase")
  }

  for (phase_list in phase_name_list) {
    if (is.na(phase_list)) adjust_phase_list <- 1 else adjust_phase_list <- phase_list
    data <- phase_specific_value_replacement(
      data = data,
      fld_name = fld_name,
      phaseVar = phaseVar,
      phase = phase_list,
      decode = input_values[[adjust_phase_list]]$decode,
      code = input_values[[adjust_phase_list]]$code
    )
  }

  return(data)
}

## Multiple Variable - Value Replacement -----
#' @title Replace Multiple Column Values
#' @description This function is used to replace the value of multiple columns in the dataset based on the provided corresponding input values.
#' @param data Data frame
#' @param phaseVar Variable name for the ADNI study protocol, Default: "PHASE"
#' @param input_values A nested list values of each columns associated with corresponding ADNI study phase and should be in this format: [column_name][[phase_list]]$values
#'    \itemize{
#'     \item `code` Value that will be replaced
#'     \item `decode` Values that will replace the old values, `code`
#'  }
#' @return A data frame with replaced values
#' @examples
#' \dontrun{
#' data_dict_dd <- get_factor_levels_datadict(
#'   data_dict = ADNIMERGE2::DATADIC,
#'   tbl_name = NULL,
#'   nested_value = TRUE
#' )
#' all_fld_name <- get_factor_fldname(
#'   data_dict = data_dict_dd,
#'   tbl_name = "REGISTRY",
#'   dd_fldnames = colnames(ADNIMERGE2::REGISTRY)
#' )
#' input_values <- collect_values(
#'   data_dict = data_dict_dd,
#'   tbl_name = "REGISTRY",
#'   all_fld_name = all_fld_name
#' )
#' result_dataset <- data_value_replacement(
#'   data = ADNIMERGE2::REGISTRY,
#'   phaseVar = "COLPROT",
#'   input_values = input_values
#' )
#' }
#' @rdname data_value_replacement
#' @family replace values
#' @export
data_value_replacement <- function(data, phaseVar = "PHASE", input_values) {
  # Input values should be in list format
  if (!is.list(input_values)) stop("`input_values` must be a list object.")

  if (!any(names(input_values) %in% names(data))) stop("Check the list name")

  for (fld_name in names(input_values)) {
    cur_input_values <- input_values[[fld_name]]
    data <- multiple_phase_value_replacement(
      data = data,
      fld_name = fld_name,
      phaseVar = phaseVar,
      input_values = cur_input_values
    )
  }

  return(data)
}

## Re-coding specific value (-4) as missing value ----
#' @title Convert specific value as missing value
#' @description
#'  This function is used to replace specific value as missing value.
#'  The value '-1' will be considered as a missing values only in ADNI1 phase.
#' @param data Data frame
#' @param col_name Variable name
#' @param value Specific value that will be considered as missing value, Default: '-4'
#' @param missing_char Character for missing value, Default: NA
#' @param phase Phase-specific value replacement, Default: NULL for all ADNI phases
#' @return A data frame with replaced value.
#' @examples
#' \dontrun{
#' convert_to_missing_value(data = ADNIMERGE2::ADAS_ADNIGO23)
#' }
#' @rdname convert_to_missing_value
#' @importFrom dplyr across filter if_all case_when
#' @importFrom tibble as_tibble
#' @importFrom rlang arg_match sym
#' @importFrom tidyselect all_of
#' @export
convert_to_missing_value <- function(data, col_name = NULL, value = "-4", missing_char = NA, phase = NULL) {
  if (is.null(phase)) {
    overall_replacement <- TRUE
    phase <- adni_phase()
  } else {
    overall_replacement <- FALSE
  }
  rlang::arg_match(arg = phase, values = adni_phase(), multiple = TRUE)
  phase_var <- extract_cols(data = data, col_name = c("COLPROT", "PHASE", "Phase", "ProtocolID"))
  if (length(phase_var) > 1) stop("`phase_var` must not be more than one length")
  column_list <- extract_cols_value(data = data, value = value, col_name = col_name)

  # To make sure '-1' as missing values only in ADNI1 phases
  if (all(value %in% "-1" & "ADNI1" %in% phase & length(phase) > 1)) stop("`-1` must be replaced for ADNI1 phase dataset only")

  output_data <- data %>%
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
          !!sym(phase_var) %in% c(phase) & .x %in% value ~ missing_char,
          TRUE ~ .x
        )))
      } else {
        (.)
      }
    }

  return(output_data)
}

# Additional Utils Function ----
## Extracting columns that contains the specific value ------
#' @title Extract Columns With Specific Value
#' @description This function is used to list columns that contains the provided specific value.
#' @param data Data.frame
#' @param value Specific value
#' @param col_name Required columns, Default: `NULL` to check for all available columns.
#' @return A character vector that contains the list of column names that contains the provided specific value. Otherwise returned `NA`.
#' @examples
#' \dontrun{
#' extract_cols_value(data = ADNIMERGE2::ADAS_ADNIGO23, value = "Letter")
#' }
#' @rdname extract_cols_value
#' @importFrom dplyr mutate filter across if_all
#' @importFrom tibble tibble
#' @export
extract_cols_value <- function(data, value, col_name = NULL) {
  data <- tibble(data)
  list_columns <- lapply(colnames(data), function(col_names) {
    temp_data <- data %>%
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
#' @title Checking Column Exist in the Dataset
#' @description This function is used to check if the provided column names are existed in the dataset.
#' @param data Data frame
#' @param col_names Column names
#' @param strict A boolean value to apply strict checking.
#' @param stop_message A boolean value to return a stop message if the criteria does not met.
#' @return
#' \itemize{
#'    \item `TRUE` if the provided column names are existed in the dataset based on the `strict` argument.
#'    \item Otherwise list of column names that are not existed in the dataset `data` or a stop message if `stop_message = TRUE`
#'  }
#' @examples
#' \dontrun{
#' check_colnames(dd = ADNIMERGE2::ADAS_ADNIGO23, col_names = c("Phase", "VISCODE"), strict = FALSE)
#' check_colnames(dd = ADNIMERGE2::ADAS_ADNIGO23, col_names = c("RID", "VISCODE"), strict = TRUE)
#' }
#' @rdname check_colnames
check_colnames <- function(data, col_names, strict = FALSE, stop_message = TRUE) {
  if (strict == TRUE) status <- !all(col_names %in% colnames(data))
  if (strict == FALSE) status <- !any(col_names %in% colnames(data))

  if (status) {
    col_not_existed <- col_names[!col_names %in% colnames(data)]
    add_notes <- ifelse(length(col_not_existed) == 1,
      " column is not found in the dataset",
      " columns are not found in the dataset"
    )
    if (stop_message) {
      stop(paste0(toString(col_not_existed), add_notes))
    } else {
      return(paste0(col_not_existed, collapse = "; "))
    }
  } else {
    return(TRUE)
  }
}

## Extracting column name from a dataset ----
#' @title Extract Column Name
#' @description This function is used to extract column names if the provided column name is existed in the dataset.
#' @param data Data.frame
#' @param col_name Column names
#' @return A character vector with list of provided column names that are existed in the dataset. Otherwise `NA`.
#' @examples
#' \dontrun{
#' extract_cols(data = ADNIMERGE2::ADAS_ADNIGO23, col_name = c("Phase", "VISCODE"))
#' extract_cols(data = ADNIMERGE2::ADAS_ADNIGO23, col_name = c("RID", "VISCODE"))
#' }
#' @rdname extract_cols
#' @export
extract_cols <- function(data, col_name) {
  list_columns <- colnames(data)[colnames(data) %in% col_name]
  if (length(list_columns) == 0) list_columns <- NA
  return(list_columns)
}

## Value Matching Functions ----
#' @title Checking Variable Values Matching
#' @description This function is used to check if the variable values are the same as the input values.
#' @param values Existed variable values
#' @param check_list Vector of input values
#' @param excluded.na A boolean to skip `NA` from the existed variable values `values`, Default: `TRUE`
#' @param stop_message A boolean value to return a stop message if one of the existed values does not match with the check list, Default: `FALSE`
#' @param add_stop_message Additional text message that will be added in the stop message.
#' @param value_split A boolen value whether to split the values with specified split pattern `split_pattern`
#' @param split_pattern Split string pattern. Only applicable if `value_split = TRUE`
#' @return
#' \itemize{
#'    \item `TRUE` if all the existed variable values are matched with the input values
#'    \item `FALSE` otherwise and with a stop message if `stop_message = TRUE`
#'  }
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
check_value_match <- function(values, check_list, excluded.na = TRUE, stop_message = FALSE,
                              add_stop_message = NULL, value_split = FALSE, split_pattern = "\\||:|;") {
  if (!is.logical(excluded.na)) stop("`excluded.na` must be a boolean value")
  if (!is.logical(stop_message)) stop("`stop_message` must be a boolean value")
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
    if (length(non_existed_values) < 1) stop("The length of non existed values should be more than 1.")
    stop("`", toString(non_existed_values), "` value(s) are not found ", add_stop_message)
  }
  return(result)
}

## Extract Death Flag ----
#' @title Extract Death Flag
#' @description This function is used to extract death records in the study based on the adverse events record (i.e. in `ADVERSE` for ADNI3-4 and `RECADV` in ADNI1-GO-2) and study sum record (i.e. in `STUDSUM` for ADNI3-4).
#' @param adverse_dd Adverse events record data frame for ADNI3-4, similar to `ADVERSE`
#' @param recadv_dd Adverse events record data frame for ADNI1-GO-2, similar to `RECADV`
#' @param studysum_dd Final dispositions(study sum) data frame for ADNI3-4, similar ro `STUDYSUM`
#' @return A data frame with the following columns:
#' \itemize{
#'    \item `RID` Participant ID
#'    \item `ORIGPROT` Original study protocols
#'    \item `COLPROT` Current study protocols which the event was recorded
#'    \item `DTHDTC` Death date
#'    \item `DTHFL` Death flag, `Yes`
#'  }
#' @examples
#' \dontrun{
#' extract_death_flag(
#'   studysum_dd = ADNIMERGE2::STUDYSUM,
#'   adverse_dd = ADNIMERGE2::ADVERSE,
#'   recadv_dd = ADNIMERGE2::RECADV
#' )
#' }
#' @rdname extract_death_flag
#' @importFrom dplyr full_join distinct group_by ungroup filter select mutate
#' @importFrom assertr assert
#' @export
extract_death_flag <- function(studysum_dd, adverse_dd, recadv_dd) {
  SDPRIMARY <- RID <- ORIGPROT <- COLPROT <- SAEDEATH <- AEHDTHDT <- AEHDTHDT <- NULL
  VISCODE <- AEHDEATH <- DTHFL <- DTHDTC <- NULL

  # Based on reported study disposition; for ADNI3 & ADNI4 phases
  check_colnames(
    data = studysum_dd,
    col_names = c("RID", "ORIGPROT", "COLPROT", "SDPRIMARY", "SDPRIMARY"),
    strict = TRUE,
    stop_message = TRUE
  )
  death_studysum <- studysum_dd %>%
    assert(is.character, SDPRIMARY) %>%
    filter(SDPRIMARY == "Death") %>%
    select(RID, ORIGPROT, COLPROT, SDPRIMARY) %>%
    assert_uniq(RID)

  # Based on reported adverse events: ADNI3 & ADNI4 phases
  check_colnames(
    data = adverse_dd,
    col_names = c("RID", "ORIGPROT", "COLPROT", "VISCODE", "SAEDEATH", "AEHDTHDT", "SAEDEATH"),
    strict = TRUE,
    stop_message = TRUE
  )

  death_adverse_even_adni34 <- adverse_dd %>%
    assert(is.character, SAEDEATH) %>%
    filter(SAEDEATH == "Yes" | !is.na(AEHDTHDT)) %>%
    select(RID, ORIGPROT, COLPROT, VISCODE, AEHDTHDT, DEATH = SAEDEATH) %>%
    assert_uniq(RID)

  # Based on reported adverse events: ADNI1, ADNIGO, and ADNI2 phases
  check_colnames(
    data = recadv_dd,
    col_names = c("RID", "ORIGPROT", "COLPROT", "VISCODE", "AEHDEATH", "AEHDEATH"),
    strict = TRUE,
    stop_message = TRUE
  )

  death_adverse_even_adni12go <- recadv_dd %>%
    assert(is.character, AEHDEATH) %>%
    filter(AEHDEATH == "Yes" | !is.na(AEHDTHDT)) %>%
    select(RID, ORIGPROT, COLPROT, VISCODE, AEHDTHDT, DEATH = AEHDEATH) %>%
    distinct() %>%
    group_by(RID, ORIGPROT, COLPROT) %>%
    assert_non_missing(VISCODE) %>%
    filter(VISCODE == min(VISCODE)) %>%
    ungroup() %>%
    assert_uniq(RID)

  death_event_dataset <- full_join(
    x = death_studysum,
    y = death_adverse_even_adni34 %>%
      bind_rows(death_adverse_even_adni12go) %>%
       assert_uniq(RID),
    by = c("RID", "ORIGPROT", "COLPROT")
  ) %>%
    assert_uniq(RID) %>%
    mutate(DTHFL = "Yes", DTHDTC = AEHDTHDT) %>%
    select(RID, ORIGPROT, COLPROT, DTHDTC, DTHFL)

  return(death_event_dataset)
}

## Duplicate Records Check Function ----
#' @title Function to Check Duplicated Records
#' @description This function is used to check if there are duplicated records in the data based on the combination of the provided column names.
#' @param data Data frame
#' @param col_names Character vector of column names
#' @param stop_message A boolean value to return a stop message if there is any duplicated records, Default: `TRUE`
#' @param return_duplicate_record A boolean value to return any duplicated records, Defualt: `FALSE`
#' @param add_cols Additional columns that will be returned for `stop_message == FALSE`
#' @return The same data frame `data` if there is not duplicated records. Otherwise an error message.
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
#' @importFrom dplyr select mutate across n
#' @importFrom stats na.omit
#' @importFrom tidyr unite
#' @importFrom tidyselect all_of any_of
#' @export
check_duplicate_records <- function(data, col_names, stop_message = TRUE, return_duplicate_record = FALSE, add_cols = NULL) {
  combined_id <- num_records <- NULL
  if (!is.logical(stop_message)) stop("`stop_message`. must be a boolean value.")
  if (is.null(col_names)) stop("`col_names` must not be missing.")
  if (!is.character(col_names)) stop("`col_names` must be charcater vector of column names.")

  check_records <- data %>%
    select(any_of(c(col_names, add_cols))) %>%
    na.omit() %>%
    mutate(across(all_of(col_names), ~., .names = "id_{col}")) %>%
    unite("combined_id", all_of(c(paste0("id_", col_names))), sep = "-/")

  if (stop_message) {
    check_records <- check_records %>%
      assert_uniq(combined_id)

    output_data <- data
  } else {
    if (return_duplicate_record) {
      output_data <- check_records %>%
        group_by(combined_id) %>%
        mutate(num_records = n()) %>%
        ungroup() %>%
        filter(num_records == 2)
    } else {
      warning("No duplicated records with the provided columns!")
    }
  }

  return(output_data)
}
