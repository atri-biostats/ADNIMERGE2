# Wrapper Functions Exported From `sdtm.oak` Package -----
## Generate OAK ID Variables ----
#' @title ADNI study specific wrapper function to generate oak_id_vars
#' @description
#'  ADNI study specific wrapper function to generate oak_id_vars based on
#'  \code{\link[sdtm.oak]{generate_oak_id_vars}} function
#'  with \code{RID} as a default parameter value of \code{pat_var}.
#' @param ... \code{\link[sdtm.oak]{generate_oak_id_vars}} arguments
#' @inheritParams sdtm.oak::generate_oak_id_vars
#' @return Similar output result to \code{\link[sdtm.oak]{generate_oak_id_vars}}
#' @rdname generate_oak_id_vars_adni
#' @export
#' @importFrom sdtm.oak generate_oak_id_vars
generate_oak_id_vars_adni <- function(...) {
  sdtm.oak::generate_oak_id_vars(..., pat_var = "RID")
}

## Flag Baseline Records ----
#' @title ADNI study specific wrapper function to derive baseline flag
#' @description
#'   This function is used to derive baseline flag variable in the ADNI study
#'   derived dataset using \code{\link[sdtm.oak]{derive_blfl}} function.
#' @inheritParams sdtm.oak::derive_blfl
#' @param ref_var Reference study date, Default: 'RFSTDTC', see \code{\link[sdtm.oak]{derive_blfl}}
#' @param num_ref_days Number of days since enrollment/reference date, Default: 90
#' @param .strict
#'  A Boolean value to apply more ADNI study specific strict baseline record identification, see the \code{Deatils} section.
#'
#' @details
#' The following additional algorithm will be applied in this function
#'
#'  If \code{.strict} is set to \code{TRUE}, then a record that collected at
#'  baseline visit will be prioritized and selected regardless the actual
#'  visit/collection date. For missing values at baseline visit, the latest
#'  records collected close to the enrollment window period end date
#'  (i.e. enrollment window period is defined as 90 days within actual enrollment date)
#'  will be selected.
#'
#'  If \code{.strict} is set to \code{FALSE}, then the algorithm defined in
#'  \code{\link[sdtm.oak]{derive_blfl}} will be applied.
#'
#' @return
#'  Similar output result to \code{\link[sdtm.oak]{derive_blfl}} with
#'  adjustment of \code{VISIT} and \code{EPOCH} variables.
#' @rdname derive_blfl_adni
#' @export
#' @importFrom sdtm.oak derive_blfl
#' @importFrom dplyr rename_with mutate across select
#' @importFrom assertr verify
#' @importFrom tidyselect all_of
#' @importFrom stringr str_remove_all str_c str_extract_all
derive_blfl_adni <- function(sdtm_in, dm_domain, tgt_var, ref_var = "RFSTDTC", num_ref_days = 90,
                             baseline_visits = "Baseline", baseline_timepoints = character(),
                             .strict = TRUE) {
  require(sdtm.oak)
  require(dplyr)
  require(assertr)

  check_object_type(.strict, "logical")
  sdtm_in <- sdtm_in %>%
    # Since visit name might different across study study
    rename_with(~ str_c("ACTUAL_", .x), all_of("VISIT")) %>%
    rename_with(~ str_c("VISIT"), all_of("EPOCH")) %>%
    sdtm.oak::derive_blfl(
      sdtm_in = .,
      dm_domain = dm_domain %>%
        # Study-specific parameter - observational study
        mutate(across(all_of(ref_var), ~ as.Date(.x) + num_ref_days)) %>%
        mutate(across(all_of(ref_var), as.character)),
      ref_var = ref_var,
      baseline_visits = baseline_visits,
      tgt_var = tgt_var,
      baseline_timepoints = baseline_timepoints
    ) %>%
    verify(nrow(.) == nrow(sdtm_in))

  if (.strict) {
    # Get baseline records based on visit name/code
    domain <- extract_domain(sdtm_in)
    sdtm_in <- drive_bfl_visit(
      .data = sdtm_in %>%
        left_join(
          dm_domain %>%
            rename_with(~ paste0("ENRFLG"), all_of(ref_var)) %>%
            select(all_of(c("USUBJID", "ENRFLG"))),
          by = "USUBJID"
        ),
      visit_col = "VISIT",
      baseline_visits = baseline_visits,
      value_col = paste0(domain, "ORRES"),
      status_col = paste0(domain, "STAT")
    ) %>%
      mutate(across(all_of(tgt_var), ~ case_when(!is.na(get("BLFG")) ~ get("BLFG"), TRUE ~ .x))) %>%
      select(-all_of(c("ENRFLG", "BLFG"))) %>%
      verify(nrow(.) == nrow(sdtm_in))

    sdtm_in <- adjust_multiple_blfs(
      .data = sdtm_in,
      tgt_var = tgt_var,
      baseline_visits = baseline_visits
    )
    # Checking for multiple flagged baseline records
    checks_multiple_blfs(
      .data = sdtm_in,
      tgt_var = tgt_var,
      action_type = "error",
      show_alert = FALSE
    )
  }

  sdtm_in <- sdtm_in %>%
    # Remapping to the original variable name
    rename_with(~ str_c("EPOCH"), all_of("VISIT")) %>%
    rename_with(~ str_remove_all(.x, "ACTUAL\\_"), all_of("ACTUAL_VISIT"))

  return(sdtm_in)
}

#' @title Check for Multiple Flagged Baseline Records
#' @param .data Data.frame
#' @inheritParams sdtm.oak::derive_blfl
#' @param action_type Output result type, Default: 'error'
#'  Either an error message (\code{'error'}) or
#'  record list (\code{'return_records'}).
#' @param show_alert
#'  A Boolean value to hidden an alert information when there are no multiple flagged records.
#' @param id_col Id columns, Default: 'USUBJID'
#' @return
#'  An error message if there are any multiple baseline flagged records.
#'  Otherwise, a data.frame of any multiple baseline flagged records
#'  if \code{action_type} is set to \code{return_records}.
#' @seealso
#'  \code{\link[adjust_multiple_blfs]{}}
#' @rdname checks_multiple_blfs
#' @keywords internal
#' @importFrom rlang arg_match0
#' @importFrom cli cli_abort
#' @importFrom dplyr filter if_all group_by n ungroup relocate
#' @importFrom tidyselect all_of

checks_multiple_blfs <- function(.data, tgt_var, action_type = "error", show_alert = TRUE, id_col = "USUBJID") {
  rlang::arg_match0(arg = action_type, values = c("return_records", "error"))
  domain <- substr(tgt_var, start = 1, stop = 2)
  domain <- check_domain_abbrv(domain)
  col_vars <- paste0(domain, c("TESTCD", "CAT", "SCAT"))
  check_colnames(
    .data = .data,
    col_names = c("VISIT", id_col, col_vars[1]),
    strict = TRUE,
    stop_message = TRUE
  )
  col_vars <- get_cols_name(.data = .data, col_name = col_vars)
  grp_vars <- c(id_col, col_vars)

  multiple_blf_records <- .data %>%
    filter(if_all(all_of(tgt_var), ~ .x %in% "Y")) %>%
    group_by(across(all_of(grp_vars))) %>%
    filter(n() > 1) %>%
    ungroup() %>%
    as_tibble() %>%
    relocate(all_of(c(grp_vars, tgt_var)))

  if (action_type %in% "error" & nrow(multiple_blf_records) > 0) {
    cli::cli_abort(
      message = paste0(
        "Multiple {.val {tgt_var}} records per {.val {grp_vars}} variable{?s} \n ",
        "The following records have multiple flagged baseline records: \n",
        "Only the first top six records are listed. Set {.val action_type = 'return_records'}",
        " to see the full records \n {print(head(multiple_blf_records))}"
      )
    )
  }

  if (action_type %in% "error" & nrow(multiple_blf_records) == 0) {
    check_object_type(show_alert, "logical")
    if (show_alert) {
      cli_alert_info(
        text = "No multiple flagged {.val {tgt_var}} records per {.val {grp_vars}} variable{?s}"
      )
    }
    invisible(multiple_blf_records)
  }

  if (action_type %in% "return_records") {
    return(multiple_blf_records)
  }
}

#' @title Adjust for Multiple Flagged Baseline Records
#' @description
#'  This function is used to select any baseline records that was collected at
#'  specific baseline visit if there are multiple flagged records as baseline values.
#' @inheritParams sdtm.oak::derive_blfl
#' @inheritParams checks_multiple_blfs
#' @return A data.frame with adjusted baseline records.
#' @details
#'  This function checks for any multiple flagged records as baseline values across
#'  any of \code{id_col}, \code{--TESTCD}, \code{-CAT}, and \code{--SCAT} columns.
#'  At least the first two columns should be existed in the data.
#' @seealso
#'  \code{\link[sdtm.oak]{oak_id_vars}}
#' @rdname adjust_multiple_blfs
#' @keywords internal
#' @importFrom sdtm.oak oak_id_vars
#' @importFrom dplyr group_by across mutate case_when ungroup select left_join
#' @importFrom tidyselect all_of any_of
#' @importFrom assertr verify

adjust_multiple_blfs <- function(.data, tgt_var, baseline_visits = "Baseline", id_col = "USUBJID") {
  NUM_BLFS <- TEMP_BLF <- ADJ_BLF <- NULL
  domain <- substr(tgt_var, start = 1, stop = 2)
  domain <- check_domain_abbrv(domain)
  col_vars <- paste0(domain, c("TESTCD", "CAT", "SCAT"))
  check_colnames(
    .data = .data,
    col_names = c("VISIT", id_col, col_vars[1]),
    strict = TRUE,
    stop_message = TRUE
  )
  col_vars <- get_cols_name(.data = .data, col_name = col_vars)
  grp_vars <- c(id_col, col_vars)
  # Get multiple flagged baseline records per group_vars
  multiple_blf_records <- checks_multiple_blfs(
    .data = .data,
    tgt_var = tgt_var,
    action_type = "return_records"
  )
  if (nrow(multiple_blf_records) > 0) {
    join_vars <- c(sdtm.oak::oak_id_vars(), grp_vars)

    temp_adjust_data <- multiple_blf_records %>%
      group_by(across(all_of(grp_vars))) %>%
      mutate(across(
        all_of(tgt_var),
        ~ case_when(
          !get("VISIT") %in% baseline_visits ~ NA_character_,
          TRUE ~ .x
        )
      )) %>%
      mutate(NUM_BLFS = sum(!is.na(get(tgt_var)))) %>%
      ungroup() %>%
      verify(all(NUM_BLFS == 1)) %>%
      select(-NUM_BLFS) %>%
      mutate(TEMP_BLF = get(tgt_var))

    output_data <- .data %>%
      left_join(
        temp_adjust_data %>%
          select(all_of(c(join_vars, "TEMP_BLF"))) %>%
          mutate(ADJ_BLF = "Yes"),
        by = join_vars
      ) %>%
      verify(nrow(.) == nrow(.data)) %>%
      mutate(across(all_of(tgt_var), ~ case_when(
        ADJ_BLF %in% "Yes" ~ get("TEMP_BLF"),
        TRUE ~ .x
      ))) %>%
      select(-any_of(c("ADJ_BLF", "TEMP_BLF")))
  } else {
    output_data <- .data
  }
  return(output_data)
}

# Derive baseline records based on study visit
#' @title Flag Baseline Records Based on Study Visit Name/Visit Code
#' @inheritParams checks_multiple_blfs
#' @param visit_col Visit name or visit code column name, Default: 'VISIT'
#' @param baseline_visits Baseline visit name/code, Default: 'Baseline'
#' @return A data.frame with appended \code{BLFG} column.
#' @details
#'  This algorithm require to include an enrollment flag column (\code{ENRFLG})
#'  in the input data.
#' @rdname drive_bfl_visit
#' @importFrom dplyr mutate across case_when
#' @importFrom tidyselect all_of

drive_bfl_visit <- function(.data, visit_col = "VISIT", baseline_visits = "Baseline",
                            value_col = NULL, status_col = NULL) {
  check_colnames(
    .data = .data,
    col_names = "ENRFLG",
    strict = TRUE,
    stop_message = TRUE
  )

  .data <- .data %>%
    mutate(across(all_of(visit_col), ~ case_when(.x %in% baseline_visits ~ "Y"), .names = "BLFG")) %>%
    mutate(across(all_of("BLFG"), ~ case_when(!is.na(get("ENRFLG")) ~ .x)))

  if (!is.null(value_col)) {
    check_non_missing_value(status_col)
    .data <- .data %>%
      mutate(across(all_of("BLFG"), ~ case_when(tolower(get(value_col)) %in% c("nd", "not done") | is.na(get(value_col)) ~ NA_character_, TRUE ~ .x)))
  }

  if (!is.null(status_col)) {
    check_non_missing_value(value_col)
    .data <- .data %>%
      mutate(across(all_of("BLFG"), ~ case_when(tolower(get(status_col)) %in% c("nd", "not done") ~ NA_character_, TRUE ~ .x)))
  }

  return(.data)
}
## Drive Study Day -----
#' @title ADNI study specific wrapper function to compute study day
#' @description
#'   This function is used to compute study day variable in the ADNI study
#'   derived dataset using \code{\link[sdtm.oak]{derive_study_day}} function.
#' @inheritParams derive_blfl_adni
#' @inheritParams sdtm.oak::derive_study_day
#' @param domain Domain abbreviation, Default, NULL
#'  \code{DOMAIN} variable in the data \code{sdtm_in} will be used a source of
#'   domain abbreviation if \code{domain} argument is \code{'NULL'}
#' @return Similar result as \code{\link[sdtm.oak]{derive_study_day}}
#' @rdname derive_study_day_adni
#' @importFrom sdtm.oak derive_study_day
#' @importFrom dplyr rename_with mutate across
#' @importFrom assertr verify
derive_study_day_adni <- function(sdtm_in, domain = NULL, dm_domain, refdt = "RFSTDTC") {
  require(sdtm.oak)
  require(assertr)
  if (is.null(domain)) {
    domain <- extract_domain(.data = sdtm_in)
  }
  domain <- check_domain_abbrv(domain, char_result = TRUE)
  tgdt_cur <- paste0(domain, "DTC")
  study_day_var_cur <- paste0(domain, "DY")
  sdtm_in <- sdtm_in %>%
    sdtm.oak::derive_study_day(
      sdtm_in = .,
      dm_domain = dm_domain %>%
        mutate(across(all_of(refdt), as.character)),
      refdt = refdt,
      tgdt = tgdt_cur,
      study_day_var = study_day_var_cur,
      merge_key = "USUBJID"
    ) %>%
    verify(nrow(.) == nrow(sdtm_in))

  return(sdtm_in)
}

# Utils Functions -----
#' @title Assign STUDYID and DOMAIN Abbreviation
#' @description
#'  This function used to assign STUDYID and DOAMIN abbreviation on input data.frame.
#' @param .data Data.frame
#' @param studyid Study id character, Default: 'ADNI'
#' @param domain Domain abbreviation
#' @return A data.frame appended with \code{STUDYID} and \code{DOMAIN} columns.
#' @rdname assign_studyid_domain
#' @importFrom dplyr mutate
#' @export
assign_studyid_domain <- function(.data, studyid = "ADNI", domain) {
  require(dplyr)
  domain <- check_domain_abbrv(domain, char_result = TRUE)
  .data <- .data %>%
    mutate(
      STUDYID = toupper(studyid),
      DOMAIN = domain
    )
  return(.data)
}

#' @title Assign Variable Label
#' @description
#'  This function is used to set variable labels and select variables
#'  that are listed in a data specs.
#' @param .data Data.frame
#' @param data_dict Data specs that could contain \code{FLDNAME} and \code{LABEL} variables.
#' @param .strict A Boolean value to apply strict variable selection, Default: TRUE
#' @return A data.frame with variables that are in the provided data specs \code{data_dict}.
#' @rdname assign_vars_label
#' @export
#' @importFrom assertr assert not_na is_uniq
#' @importFrom dplyr select
#' @importFrom tidyselect all_of any_of
#' @importFrom labelled set_variable_labels
assign_vars_label <- function(.data, data_dict, .strict = TRUE) {
  require(dplyr)
  require(tidyselect)
  require(labelled)
  require(assertr)
  check_object_type(.strict, "logical")
  if (.strict) {
    select_of <- function(x) {
      tidyselect::all_of(x)
    }
  }
  if (!.strict) {
    select_of <- function(x) {
      tidyselect::any_of(x)
    }
  }

  data_dict <- data_dict %>%
    assert(not_na, FLDNAME, LABEL) %>%
    assert(is_uniq, FLDNAME) %>%
    assert(is_uniq, LABEL)

  names(data_dict$LABEL) <- data_dict$FLDNAME

  .data <- .data %>%
    select(select_of(data_dict$FLDNAME)) %>%
    labelled::set_variable_labels(
      .labels = data_dict$LABEL,
      .strict = .strict
    )

  return(.data)
}

#' @title Check Domain Abbreviation Length
#' @description This function is used to check the length of domain abbreviation.
#' @param domain Domain abbreviation character
#' @param char_result
#'  A Boolean value to return domain abbreviation if it is a length of two characters, Default: TRUE
#' @return A character value
#' @examples
#' \dontrun{
#' check_domain_abbrv(domain = "dm", char_result = TRUE)
#' check_domain_abbrv(domain = "dm", char_result = FALSE)
#' check_domain_abbrv(domain = "test", char_result = TRUE)
#' }
#' @rdname check_domain_abbrv
#' @importFrom cli cli_abort
#' @export
check_domain_abbrv <- function(domain, char_result = TRUE) {
  check_object_type(char_result, "logical")
  domain <- toupper(domain)
  if (nchar(domain) != 2) {
    cli_abort(
      message = c(
        "{.var domain} must be a single character object with length of two. \n",
        "{.var domain} is a {.cls {class(domain)}} object with length of {.clas {nchar(domain)}}."
      )
    )
  }
  if (char_result) result <- domain else result <- TRUE
  return(result)
}

#' @title Extract Domain Abbreviation
#' @description
#'  A function to extract the domain abbreviation from a data.frame.
#' @param .data Data.frame
#' @return A single character domain abbreviation
#' @examples
#' \dontrun{
#' extract_domain(.data = ADNIMERGE2::DM)
#' }
#' @rdname extract_domain
#' @importFrom cli cli_abort
extract_domain <- function(.data) {
  check_colnames(
    .data = .data,
    col_names = "DOMAIN",
    strict = TRUE,
    stop_message = TRUE
  )
  domain_value <- unique(.data$DOMAIN)
  if (length(domain_value) != 1) {
    cli_abort(
      message = c(
        "{.var DOMAIN} variable in the data must contain a single unique value. \n",
        "{.var DOMAIN} variable in the data contains {.val {domain_value}} value{?s}."
      )
    )
  }
  domain_value <- check_domain_abbrv(domain = domain_value, char_result = TRUE)
  return(domain_value)
}
