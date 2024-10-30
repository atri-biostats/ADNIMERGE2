# ADNI Study Phase ----
## List of ADNI study phase ----
#' @title List ADNI Study Phase
#' @description This function is generate all the ADNI study phases.
#' @return A character vector with list of ADNI study phases
#' @rdname adni_phase
adni_phase <- function() {
  return(c("ADNI1", "ADNIGO", "ADNI2", "ADNI3", "ADNI4"))
}

## Get Original ADNI Study Phase/Protocol -----
#' @title ADNI Original Protocol Version
#' @description This function is used to identify the original ADNI study phase for participant (i.e. when the participant was newly enrolled in ADNI study) based on their RID.
#' @param RID Participant RID
#' @return A character vector with the same size as RID with original study protocol (i.e. ADNI study phases).
#' @examples
#' \dontrun{
#' RID <- c(0001, 1250, 5015, 1002)
#' origprot <- original_study_protocol(RID = RID)
#' origprot
#' }
#' @keywords ADNI study protocol
#' @rdname original_study_protocol
#' @export
original_study_protocol <- function(RID) {
  origprot <- case_when(
    RID >= 1 & RID < 2000 ~ "ADNI1",
    RID > 2000 & RID < 3000 ~ "ADNIGO",
    RID > 3000 & RID < 6000 ~ "ADNI2",
    RID > 6000 & RID < 10000 ~ "ADNI3",
    RID >= 10000 ~ "ADNI4"
  )
  return(origprot)
}

## ADNI Study Phase Order Number ----
#' @title ADNI Study Protocol Order
#' @description This function is used to extract the ADNI original study protocol order number.
#' @param phase ADNI study protocol phases: ADNI1, ADNIGO, ADNI2, ADNI3 or ADNI4
#' @return A character vector with the same size as phase with original study protocol order number.
#' @examples
#' \dontrun{
#' phase_order_num <- adni_phase_order_num(phase = c("ADNI1", "ADNI3", "ADNI4", "ADNIGO"))
#' phase_order_num
#' }
#' @keywords ADNI study protocol
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

## Participant Type (Study Track) ----
#' @title Get Participant Type (ADNI Study Track)
#' @description This function is used to identify participant type (i.e. study track) based on the provided ADNI phases.
#' @param cur_study_phase Current ADNI study phase
#' @param orig_study_phase ADNI study phase which a participant enrolled as new participant for the first time in ADNI study
#' @return A character vector with the same size as `cur_study_phase` with study track.
#' @keywords ADNI study track
#' @importFrom tibble tibble
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr across
#' @importFrom assertr verify
#' @rdname adni_study_track
adni_study_track <- function(cur_study_phase, orig_study_phase) {
  cur_protocol <- orig_protocol <- NULL

  if (any(is.na(cur_study_phase) | is.na(orig_study_phase))) stop("Either cur_study_phase or orig_study_phase must not be empty")

  temp_dd <- tibble(
    cur_protocol = cur_study_phase,
    orig_protocol = orig_study_phase
  ) %>%
    mutate(across(c(cur_protocol, orig_protocol),
      ~ adni_phase_order_num(phase = .x),
      .names = "{col}_num"
    )) %>%
    mutate(study_track = case_when(
      cur_protocol_num < orig_protocol_num ~ "Rollover",
      cur_protocol_num == orig_protocol_num ~ "New"
    )) %>%
    verify(nrow(.) == length(cur_study_phase))

  if (any(is.na(temp_dd$study_track))) stop("Check study protocol phase")

  return(temp_dd$study_track)
}

## Create ORIGPROT variable -----
#' @title Creating ORIGPROT Column
#' @description This function is used to create ORIGPROT in the dataset based RID value.
#' @param dd Data frame
#' @return A data frame the same as `dd` with appended columns of "ORIGPROT"
#' @rdname create_orig_protocol
#' @importFrom dplyr relocate
#' @importFrom assertr num_row_NAs
#' @importFrom assertr within_bounds
#' @importFrom assertr assert_rows
create_orig_protocol <- function(dd) {
  RID <- ORIGPROT <- NULL
  check_colnames(dd = dd, col_names = "RID")
  dd <- dd %>%
    mutate(ORIGPROT = factor(original_study_protocol(RID = RID),
      levels = adni_phase()
    )) %>%
    relocate(ORIGPROT) %>% 
    assert_rows(num_row_NAs, within_bounds(0,0.05), ORIGPROT)
  return(dd)
}

## Create CURPROT variable ----
#' @title Creating CURPROT Column
#' @description This function is used to create ORIGPROT in the dataset based RID value.
#' @param dd Data frame
#' @param phaseVar Phase column
#' @return A data frame the same as `dd` with appended columns of "CURPROT"
#' @rdname create_col_protocol
#' @importFrom tidyselect all_of
#' @importFrom dplyr rename_with
#' @importFrom dplyr relocate
#' @importFrom assertr num_row_NAs
#' @importFrom assertr within_bounds
#' @importFrom assertr assert_rows
create_col_protocol <- function(dd, phaseVar = NULL) {
  CURPROT <- NULL
  if (is.null(phaseVar)) phaseVar <- c("Phase", "PHASE", "ProtocolID")
  existed_column <- extract_cols(dd, col_name = phaseVar)
  if (length(existed_column) > 1) stop("Check number of column names of Phase/PHASE")

  dd <- dd %>%
    {
      if (!is.na(existed_column)) {
        mutate(., across(all_of(existed_column), as.character)) %>%
          rename_with(., ~ paste0("CURPROT"), .cols = all_of(existed_column)) %>%
          mutate(., CURPROT = factor(CURPROT, levels = adni_phase())) %>%
          relocate(., CURPROT) %>% 
          assert_rows(., num_row_NAs, within_bounds(0,0.05), CURPROT)
      } else {
        (.)
      }
    }
  return(dd)
}

# Get Coded Values from DATADIC dataset ----
## Split Strings -----
#' @title Split strings with two split parameters
#' @description This function is used to split a string using two split parameters and create a dataframe
#' @param input_string Input string/character
#' @param first_split_paramter First split parameter/pattern, Default: '; |;'
#' @param second_split_paramter Second split parameter/pattern, Default: '=| = '
#' @return A data.frame that includes prefix and suffix values.
#' @details DETAILS
#' @examples
#' \dontrun{
#' input_string <- "1=BUTTER; 2=ARM; 3=SHORE; 4=LETTER; 5=QUEEN; 6=CABIN"
#' split_strings(input_string = input_string)
#' }
#' @rdname split_strings
#' @export
split_strings <- function(input_string,
                          first_split_paramter = "; |;",
                          second_split_paramter = "=| = ") {
  # Splitting the input_string with first_split_paramter
  split_pairs <- unlist(strsplit(x = input_string, split = first_split_paramter))
  if (length(split_pairs) > 0) {
    # Splitting the input_string with second_split_paramter
    splitted_strings <- strsplit(x = split_pairs, split = second_split_paramter)
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
  prefix <- as.character(prefix)
  suffix <- as.character(suffix)
  return(tibble(prefix, suffix))
}

## Hepler function in get_factor_levels_datadict function
create_string_split <- function(data) {
  return(split_strings(
    input_string = data$CODE,
    first_split_paramter = "; |;",
    second_split_paramter = "=| = "
  ))
}

## Get factor levels from DATADIC dataset ----
#' @title Get Factor Levels from DATADIC dataset
#' @description This function is used to generate the coded levels of FLDNAME using the DATADIC dataset (from loni)
#' @param data_dict Data dictionary dataset (DATADIC from the package or loni website)
#' @param tbl_name Table Name, Default: NULL that generate for the avaiable TBLNAME in data_dict
#' @param nested_value Unnest the factor level variables in long format if 'TRUE' otherwise nested with `code_list` columns
#' @return A data.frame that appended with prefix (actual value in the dataset), suffix (coded values) and class_type ("factor").
#' @examples
#' \dontrun{
#' get_factor_levels_datadict(
#'   data_dict = ADNIMERGE2::DATADIC,
#'   tbl_name = NULL,
#'   nested_value = TRUE
#' )
#' }
#' @rdname get_factor_levels_datadict
#' @importFrom stringr str_detect
#' @importFrom tidyr nest
#' @importFrom purrr map
#' @importFrom tidyr unnest
#' @export
get_factor_levels_datadict <- function(data_dict,
                                       tbl_name = NULL,
                                       nested_value = FALSE) {
  TBLNAME <- TYPE <- CODE <- FLDNAME <- code_list <- data <- NULL
  if (is.null(tbl_name)) tbl_name <- unique(data_dict$TBLNAME)
  check_colnames(dd = data_dict, col_names = c("PHASE", "TYPE", "TBLNAME", "FLDNAME", "CODE"))

  ## Column types that might needed to be removed
  exc_type <- tolower(c(
    "bigint", "bit", "char", "C", "character", "d", "date",
    "datetime", "datetime2", "decimal", "f", "float",
    "floating value", "i", "integer", "mediumint", "notes",
    "s", "string", "Text", "Time", "varchar"
  ))
  ## Coded range values
  exc_range_number <- paste0(
    c(
      "\\b\\d+\\.\\.\\d+\\b",
      "\\b\\d+\\.\\.\\..\\d+\\b",
      "Range: 0+", "Range:  0+"
    ),
    collapse = "|"
  )
  ## Dataset name list
  exc_tblname <- c(
    "CDR", "ADNI_DIAN_COMPARISON", "AMPRION_ASYN_SAA", "BATEMANLAB",
    "BATEMANLAB_ADNI_Plasma_Abeta4240_20221118", "MRIQC", "MRINFQ",
    "MRIFind", "MRIQSM", "TCV", "ADSP_PHC_ADNI_T1_1.0_MetaData",
    "MAYOADIRL_MRI_MCH", "MAYOADIRL_MRI_TBMSYN", "MRI_INFARCTS",
    "TRANSFER", "UCD_ADNI1_WMH", "UCD_ADNI2_WMH", "UCD_WMH",
    "UPENNPLASMA", "UWOVENT", "MAYOADIRL_MRI_FMRI_NFQ", "NEUROPATH"
  )
  ## Field name list
  exc_fldname <- c(
    "INCNEWPT", "EXCCRIT", "FAILEXCLU", "CatFlu_Practise",
    "ADCID", "NACCMOD", "NACCWRI1", "NACCWRI2", "NACCWRI3",
    "NACCYOD", "NPFORMVER", "QCRating", "IMAGETYPE"
  )

  temp_dd <- data_dict %>%
    as_tibble() %>%
    filter(TBLNAME %in% tbl_name) %>%
    # Excluding missing value
    mutate(
      CODE = case_when(
        CODE == -4 | str_detect(
          string = CODE,
          pattern = 'crfname|\\"indexes\\"|\\<display\\>'
        ) == TRUE |
          CODE %in% c(
            "in(0,1)", "Pass/Fail", "complete/partial",
            "Pass/Fail/Partial"
          ) ~ NA_character_,
        TRUE ~ CODE
      ),
      TYPE = tolower(TYPE)
    ) %>%
    filter(!is.na(CODE)) %>%
    # Excluding pre-specified type, dataset name and field name
    filter(!c(TYPE %in% exc_type |
      TBLNAME %in% exc_tblname |
      FLDNAME %in% exc_fldname)) %>%
    # Excluding coded range values
    filter(!str_detect(CODE, exc_range_number)) %>%
    nest(data = CODE) %>%
    mutate(code_list = map(data, create_string_split)) %>%
    unnest(cols = data) %>%
    {
      if (nested_value == FALSE) {
        unnest(., cols = code_list)
      } else {
        (.)
      }
    } %>%
    mutate(
      class_type = "factor"
    )

  return(temp_dd)
}

## Get Factor Field Name (FLDNAME) ----
#' @title Get Factor Field Name (FLDNAME)
#' @description This function is used to identify factor field name (FLDNAME) based on the DATADIC dataset (from loni website)
#' @param data_dict Data dictionary dataset that generated using get_factor_levels_datadict
#' @param tbl_name Table Name
#' @param dd_fldnames List of column names in the actual dataset, Default = NULL.
#' @return A character vector
#' @examples
#' \dontrun{
#' data_dict_dd <- get_factor_levels_datadict(
#'   data_dict = ADNIMERGE2::DATADIC,
#'   tbl_name = NULL,
#'   nested_value = TRUE
#' )
#' # List of available factor columns in data dictionary DATADIC for adas_pooled dataset
#' get_factor_fldname(
#'   data_dict = data_dict_dd,
#'   tbl_name = "ADAS",
#'   dd_fldnames = NULL
#' )
#' # List of factor columns that available in adas_pooled and data dictionary DATADIC
#' get_factor_fldname(
#'   data_dict = data_dict_dd,
#'   tbl_name = "ADAS",
#'   dd_fldnames = colnames(ADNIMERGE2::adas_pooled)
#' )
#' }
#' @rdname get_factor_fldname
#' @importFrom dplyr select
#' @export
get_factor_fldname <- function(data_dict, tbl_name, dd_fldnames = NULL) {
  TBLNAME <- FLDNAME <- class_type <- NULL
  colNames <- c("TBLNAME", "FLDNAME", "class_type")
  check_colnames(dd = data_dict, col_names = colNames)
  temp_dd <- data_dict %>%
    as_tibble() %>%
    filter(TBLNAME %in% tbl_name &
      class_type == "factor") %>%
    {
      if (!is.null(dd_fldnames)) {
        filter(., FLDNAME %in% c(dd_fldnames))
      } else {
        (.)
      }
    } %>%
    select(FLDNAME)

  unique_fldname <- unique(temp_dd$FLDNAME)

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
#'    \item `old_values` Coded values
#'    \item `new_values` New values that will replace the old values, `old_values`
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
#' @seealso [collect_values()]
single_collect_values <- function(data_dict, tbl_name, fld_name) {
  FLDNAME <- PHASE <- TBLNAME <- code_list <- NULL
  colNames <- c("PHASE", "TBLNAME", "FLDNAME", "code_list")
  check_colnames(dd = data_dict, col_names = colNames)

  rlang::arg_match(
    arg = fld_name, values = unique(data_dict$FLDNAME),
    multiple = TRUE
  )

  rlang::arg_match(
    arg = tbl_name, values = unique(data_dict$TBLNAME),
    multiple = TRUE
  )

  df <- data_dict %>%
    filter(TBLNAME %in% tbl_name & FLDNAME %in% fld_name) %>%
    select(PHASE, TBLNAME, FLDNAME, code_list) %>%
    unnest(cols = code_list)

  if (nrow(df) > 0) {
    phase_list <- unique(df$PHASE)
    phase_value_list <- lapply(phase_list, function(x) {
        old_values <- df[df$PHASE %in% x, ]$prefix
        new_values <- df[df$PHASE %in% x, ]$suffix 
      return(
        list(
          "old_values" = old_values,
          "new_values" = new_values
        )
      )
    })

    names(phase_value_list) <- phase_list
  } else {
    phase_value_list <- list(
      "old_values" = NA,
      "new_values" = NA
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
#' @param all_fld_name List of column names that have re-coded values
#' @return List value that contains \code{old_values\link{single_collect_values}} and \code{new_values\link{single_collect_values}} with corresponding column names
#' @rdname collect_values
#' @seealso
#'     \code{single_collect_values}
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
#'   dd_fldnames = colnames(ADNIMERGE2::adas_pooled)
#' )
#' collect_values(
#'   data_dict = data_dict_dd,
#'   tbl_name = "ADAS",
#'   all_fld_name = tbl_unique_fldname
#' )
#' }
#' @export
collect_values <- function(data_dict, tbl_name, all_fld_name) {
  temp_values <- lapply(all_fld_name, function(fld_name) {
    single_collect_values(
      data_dict = data_dict,
      tbl_name = tbl_name,
      fld_name = fld_name
    )
  })

  names(temp_values) <- all_fld_name
  return(temp_values)
}

# Value Replacement -----
## Replace Multiple Values ----
#' @title Replace/Substitute Multiple Values
#' @description This function is used to replace string values.
#' @param input_string Input string
#' @param old_values Old values
#' @param new_values New values
#' @return A character vector similar to `input_string` with replaced values
#' @examples
#' \dontrun{
#' input_string <- c("-2", "1", "1", "-1")
#' old_values <- c("null", "1", "-1", "-2")
#' new_values <- c(
#'   "pending enrollment", "randomized - assigned a scan category",
#'   "screen failed", "screen failed after randomization"
#' )
#' replaced_values <- replace_multiple_values(
#'   input_string = input_string,
#'   old_values = old_values,
#'   new_values = new_values
#' )
#' replaced_values
#' }
#' @rdname replace_multiple_values
#' @export
replace_multiple_values <- function(input_string,
                                    old_values,
                                    new_values) {
  # To ensure the same length of new_values and old_values
  if (length(old_values) != length(new_values)) stop("The length of new_values and old_values must be the same.")

  # To make sure exact replacement
  unique_place_holder <- paste0("PLACE_HOLDERS_", seq_along(old_values))
  output_string <- input_string

  # Checking for negative values
  negative_value <- detect_negative_value(input_value = old_values)

  if (negative_value) {
    adjusted_old_values <- gsub(pattern = "-", x = old_values, replacement = "negative")
    output_string <- gsub("-", x = output_string, replacement = "negative")
  } else {
    adjusted_old_values <- old_values
  }
  # To ensure exact replacement
  adjusted_old_values <- paste0("\\b", adjusted_old_values, "\\b")

  # Replace values with actual new values
  for (i in seq_along(old_values)) {
    output_string <- gsub(
      pattern = adjusted_old_values[i],
      x = output_string,
      replacement = unique_place_holder[i], fixed = FALSE
    )
  }

  # Replace values with actual new values
  for (i in seq_along(old_values)) {
    output_string <- gsub(
      pattern = unique_place_holder[i], x = output_string,
      replacement = new_values[i], fixed = FALSE
    )
  }

  return(output_string)
}

### Detect negative values ----
#' @title Detect Negative Values in a String
#' @description This function is used to detect if there is any negative value in the string.
#' @param input_value Input string
#' @return `TRUE` if there is a negative value otherwise FALSE
#' @examples
#' \dontrun{
#' input_string <- c("-2", "1", "1", "-1")
#' detect_negative_value(input_value = input_string)
#' }
#' @rdname detect_negative_value

detect_negative_value <- function(input_value) {
  input_value <- as.numeric(input_value)
  if (all(is.na(input_value))) {
    result <- FALSE
  } else {
    input_value <- input_value[!is.na(input_value)]
    if (any(input_value < 0)) result <- TRUE else result <- FALSE
  }
  return(result)
}

## Single Variable - Single Phase Specific Value Replacement -----
#' @title Replace Phase Specific Values for Single Variable -------
#' @description This function is used to recode/replace phase specific values of a single variable in a dataset.
#' @param dd Data frame
#' @param fld_name Variable name
#' @param phase ADNI study phase/protocol name
#' @param phaseVar Variable name for the ADNI study protocol, Default: "PHASE"
#' @param old_values Values that will be replaced
#' @param new_values New values that will replace the old values, 'old_values'
#' @return A data frame with replaced values for the provided variable.
#' @examples
#' \dontrun{
#' data_dict_dd <- get_factor_levels_datadict(
#'   data_dict = ADNIMERGE2::DATADIC,
#'   tbl_name = "ARM", # NULL
#'   nested_value = TRUE
#' )
#' input_values <- collect_values(
#'   data_dict = data_dict_dd,
#'   tbl_name = "ARM",
#'   all_fld_name = "ENROLLED"
#' )
#' new_values <- input_values$ENROLLED$ADNI1$new_values
#' old_values <- input_values$ENROLLED$ADNI1$old_values
#' result_dataset <- phase_specific_value_replacement(
#'   dd = ADNIMERGE2::adni_arm_pooled,
#'   phaseVar = "CURPROT",
#'   fld_name = "ENROLLED",
#'   phase = "ADNI1",
#'   old_values = old_values,
#'   new_values = new_values
#' )
#' }
#' @rdname phase_specific_value_replacement
#' @seealso [multiple_phase_value_replacement()]
#' @importFrom rlang sym
#' @importFrom dplyr pull bind_rows arrange rename
phase_specific_value_replacement <- function(dd,
                                             fld_name,
                                             phase = NULL,
                                             phaseVar = "PHASE",
                                             old_values,
                                             new_values) {
  phase_var <- NULL
  if (is.null(phase)) {
    rlang::arg_match(
      arg = phase,
      values = adni_phase(),
      multiple = TRUE
    )
  }

  rlang::arg_match(arg = phaseVar, values = names(dd))

  dd <- dd %>%
    rename("phase_var" = !!sym(phaseVar)) %>%
    mutate(across(!!sym(fld_name), ~ as.character(.x)))

  phase_dd <- dd %>%
    filter(phase_var %in% phase)

  phase_dd[, fld_name] <- replace_multiple_values(
    input_string = phase_dd %>% pull(fld_name),
    old_values = old_values,
    new_values = new_values
  )
  # Bind with previous dataset
  output_data <- phase_dd %>%
    bind_rows(dd %>% filter(!phase_var %in% phase)) %>%
    mutate(phase_var = factor(phase_var, levels = adni_phase())) %>%
    arrange(phase_var) %>%
    rename_with(~ paste0(phaseVar), phase_var)

  return(output_data)
}

## Single Variable - Multiple Phase Specific Value Replacement -----
#' @title Value Replace for Single Column
#' @description This function is used to replace values for a single column in the dataset
#' @param dd Data frame
#' @param fld_name Variable name
#' @param phaseVar Variable name for the ADNI study protocol, Default: "PHASE"
#' @param input_values A list value associated with each ADNI study phase and should be in this format: [phase_list]$values
#'    \itemize{
#'     \item `old_values` Value that will be replaced
#'     \item `new_values` New values that will replace the old values, `old_values`
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
#'   tbl_name = "ARM",
#'   all_fld_name = "ENROLLED"
#' )
#' input_values <- input_values$ENROLLED
#' result_dataset <- multiple_phase_value_replacement(
#'   dd = ADNIMERGE2::adni_arm_pooled,
#'   phaseVar = "CURPROT",
#'   fld_name = "ENROLLED",
#'   input_values = input_values
#' )
#' }
#' @rdname multiple_phase_value_replacement
#' @seealso
#'     \code{phase_specific_value_replacement}
multiple_phase_value_replacement <- function(dd,
                                             fld_name,
                                             phaseVar = "PHASE",
                                             input_values) {
  # Input values should be in list format
  if (!is.list(input_values)) stop("input_values should be a list object")
  
  phase_name_list <- names(input_values)
  # Adjust for non-phase specific replacements
  phase_name_list_no_na <- phase_name_list[!is.na(phase_name_list)]
  if (all(is.na(phase_name_list))) {
    warning("No ADNI phase specific list name used")
  } else{
    if (length(phase_name_list_no_na) != length(phase_name_list))  stop("the list name should not contains `NA` and actual ADNI phase")
    if (!any(phase_name_list_no_na %in% adni_phase())) stop("the list name should be ADNI phase") 
  } 
  
  for (phase_list in phase_name_list) {
    if (is.na(phase_list)) adjust_phase_list <- 1 else adjust_phase_list <- phase_list
    dd <- phase_specific_value_replacement(
      dd = dd,
      fld_name = fld_name,
      phaseVar = phaseVar,
      phase = phase_list,
      new_values = input_values[[adjust_phase_list]]$new_values,
      old_values = input_values[[adjust_phase_list]]$old_values
    )
  }

  return(dd)
}

## Multiple Variable - Value Replacement -----
#' @title Replace Multiple Column Values
#' @description This function is used to replace the value of multiple columns in the dataset based on the provided corresponding input values.
#' @param dd Data frame
#' @param phaseVar Variable name for the ADNI study protocol, Default: "PHASE"
#' @param input_values A nested list values of each columns associated with corresponding ADNI study phase and should be in this format: [column_name][[phase_list]]$values
#'    \itemize{
#'     \item `old_values` Value that will be replaced
#'     \item `new_values` New values that will replace the old values, `old_values`
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
#'   tbl_name = "ARM",
#'   dd_fldnames = colnames(ADNIMERGE2::adni_arm_pooled)
#' )
#' input_values <- collect_values(
#'   data_dict = data_dict_dd,
#'   tbl_name = "ARM",
#'   all_fld_name = all_fld_name
#' )
#' result_dataset <- data_value_replacement(
#'   dd = ADNIMERGE2::adni_arm_pooled,
#'   phaseVar = "CURPROT",
#'   input_values = input_values
#' )
#' }
#' @rdname data_value_replacement
#' @export
data_value_replacement <- function(dd,
                                   phaseVar = "PHASE",
                                   input_values) {
  # Input values should be in list format
  if (!is.list(input_values)) stop("input_values should be a list object")

  if (!any(names(input_values) %in% names(dd))) stop("Check the list name")

  for (fld_name in names(input_values)) {
    curt_input_values <- input_values[[fld_name]]

    dd <- multiple_phase_value_replacement(
      dd = dd,
      fld_name = fld_name,
      phaseVar = phaseVar,
      input_values = curt_input_values
    )
  }

  return(dd)
}

## Re-coding specific value (-4) as missing value ----
#' @title Make -4 as missing value
#' @description This function is used to replace a value as missing value
#' @param dd Data frame
#' @param col_name Variable name
#' @param value Value that will be considered as missing, Default: '-4'
#' @param missing_char Missing value value, Default: NA
#' @return A data frame with replaced value.
#' @rdname make_missing_value
#' @importFrom dplyr filter
#' @importFrom dplyr if_all
#' @importFrom tibble as_tibble
#' @export
make_missing_value <- function(dd, col_name = NULL, value = "-4", missing_char = NA) {
  column_list <- extract_cols_value(dd = dd, value = value, col_name = col_name)

  dd <- dd %>%
    {
      if (all(is.na(column_list))) {
        (.)
      } else {
        mutate(., across(all_of(column_list), ~ case_when(
          .x == value ~ missing_char,
          TRUE ~ .x
        )))
      }
    }

  return(dd)
}

## Extracting columns that contains the specific value ------
#' @title Extract Columns With Specific Value
#' @description This function is used to list the columns of the data that contains the provided specific value.
#' @param dd Data.frame
#' @param value Specific value
#' @param col_name Columns that needs to be checked, Default: NULL
#' @return A character vector that contains the list of column names
#' @examples
#' \dontrun{
#' extract_cols_value(dd = ADNIMERGE2::adas_pooled, value = "Letter")
#' }
#' @rdname extract_cols_value
extract_cols_value <- function(dd, value, col_name = NULL) {
  dd <- tibble(dd)
  list_columns <- lapply(colnames(dd), function(col_names) {
    temp_data <- dd %>%
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

# Additional Utils Function ----
## Checking column names in the dataset ----
#' @title Checking Column Exist in the Dataset
#' @description This function is used to check if the provided column names are existed in the dataset.
#' @param dd Data frame
#' @param col_names Column names
#' @return "TRUE" if all provided column names are existed in the dataset. Otherwise return a stop message
#' @examples
#' \dontrun{
#' check_colnames(dd = ADNIMERGE2::adas_pooled, col_names = c("Phase", "VISCODE"))
#' check_colnames(dd = ADNIMERGE2::adas_pooled, col_names = c("RID", "VISCODE"))
#' }
#' @rdname check_colnames
check_colnames <- function(dd, col_names) {
  if (!any(col_names %in% colnames(dd))) {
    col_not_existed <- col_names[!col_names %in% colnames(dd)]
    add_notes <- ifelse(length(col_not_existed) == 1,
      " column is not found in the dataset",
      " columns are not found in the dataset"
    )

    stop(paste0(toString(col_not_existed), add_notes))
  }

  return(TRUE)
}

## Extracting column name from a dataset ----
#' @title Extract Column Name
#' @description This function is used to extract column names if the provided column name is existed in the dataset.
#' @param dd Data.frame
#' @param col_name Column names
#' @return A character vector with list of provided column names that existed in the dataset
#' @examples
#' \dontrun{
#' extract_cols(dd = ADNIMERGE2::adas_pooled, col_name = c("Phase", "VISCODE"))
#' extract_cols(dd = ADNIMERGE2::adas_pooled, col_name = c("RID", "VISCODE"))
#' }
#' @rdname extract_cols
extract_cols <- function(dd, col_name) {
  list_columns <- colnames(dd)[colnames(dd) %in% col_name]
  if (length(list_columns) == 0) list_columns <- NA
  return(list_columns)
}
