# Wrapper Functions of `sdtm.oak` Package -----
#' @title ADNI study specific wrapper function to generate oak_id_vars
#' @description
#'  ADNI study specific wrapper function to generate oak_id_vars based on
#'  \code{\link[sdtm.oak]{generate_oak_id_vars}} function
#'  with `RID` as a default parameter value of `pat_var`.
#' @param ... \code{\link[sdtm.oak]{generate_oak_id_vars}} arguments
#' @inheritParams sdtm.oak::generate_oak_id_vars
#' @return Similar result of \code{\link[sdtm.oak]{generate_oak_id_vars}}
#' @rdname generate_oak_id_vars_adni
#' @export
#' @importFrom sdtm.oak generate_oak_id_vars
generate_oak_id_vars_adni <- function(...) {
  sdtm.oak::generate_oak_id_vars(..., pat_var = "RID")
}

#' @title ADNI study specific wrapper function to derive baseline flag
#' @description
#'   This function is used to derive baseline flag variable in the ADNI study
#'   derived dataset using \code{\link[sdtm.oak]{derive_blfl}} function.
#' @inheritParams sdtm.oak::derive_blfl
#' @param ref_var Reference study date, Default: 'RFSTDTC', see \code{\link[sdtm.oak]{derive_blfl}}
#' @return Similar result of \code{\link[sdtm.oak]{derive_blfl}} with adjustment of `VISIT` and `EPOCH` variable.
#' @rdname derive_blfl_adni
#' @export
#' @importFrom sdtm.oak derive_blfl
#' @importFrom dplyr rename_with mutate across
#' @importFrom assertr verify
#' @importFrom tidyselect all_of
#' @importFrom stringr str_remove_all str_c str_extract_all
derive_blfl_adni <- function(sdtm_in, dm_domain, tgt_var, ref_var = "RFSTDTC",
                             baseline_visits = "Baseline", baseline_timepoints = character()) {
  require(sdtm.oak)
  require(dplyr)
  require(assertr)
  TEMP_VISIT <- NULL
  sdtm_in <- sdtm_in %>%
    # Since visit name might different across study study
    rename_with(~ str_c("ACTUAL_", .x), all_of("VISIT")) %>%
    rename_with(~ str_c("VISIT"), all_of("EPOCH")) %>%
    sdtm.oak::derive_blfl(
      sdtm_in = .,
      dm_domain = dm_domain %>%
        # Study-specific parameter - observational study
        mutate(across(all_of(ref_var), ~ as.Date(.x) + 30)) %>%
        mutate(across(all_of(ref_var), as.character)),
      ref_var = ref_var,
      baseline_visits = baseline_visits,
      tgt_var = tgt_var,
      baseline_timepoints = baseline_timepoints
    ) %>%
    # Mapping to original variable name
    rename_with(~ str_c("EPOCH"), all_of("VISIT")) %>%
    rename_with(~ str_remove_all(.x, "ACTUAL\\_"), all_of("ACTUAL_VISIT")) %>%
    verify(nrow(.) == nrow(sdtm_in))

  return(sdtm_in)
}

#' @title ADNI study specific wrapper function to compute study day
#' @description
#'   This function is used to compute study day variable in the ADNI study
#'   derived dataset using \code{\link[sdtm.oak]{derive_study_day}} function.
#' @inheritParams derive_blfl_adni
#' @inheritParams sdtm.oak::derive_study_day
#' @param domain Domain abbreviation, Default, NULL,
#'        `DOMAIN` variable in the data `sdtm_in` will be used a source of
#'         domain abbreviation if `domain` arg is NULL
#' @return Similar result of \code{\link[sdtm.oak]{derive_study_day}}
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

# Utility Functions -----
#' @title Assign STUDYID and DOMAIN Abbreviation
#' @description
#'  This function used to assign STUDYID and DOAMIN abbreviation on input data.frame.
#' @param .data Data.frame
#' @param studyid Study id character, Default: 'ADNI'
#' @param domain Domain abbreviation
#' @return A data.frame appended with `STUDYID` and `DOMAIN` columns.
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
#' @param data_dict Data specs that could contain `FLDNAME` and `LABEL` variables.
#' @param .strict A Boolean value to apply strict variable selection, Default: TRUE
#' @return A data.frame with variables that are in the provided data specs `data_dict`
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
  check_is_logical(.strict)
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
  check_is_logical(char_result)
  domain <- toupper(domain)
  if (nchar(domain) != 2) {
    cli_abort(
      message = c(
        "{.var domain} must be a single character object with length of two. \n",
        "{.var domain} is a {.cls {class(domain)}} object with length {.clas {nchar(domain)}}."
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
        "{.var DOMAIN} variable in the must contain a single unique value. \n",
        "{.var DOMAIN} variable in the data contains {.val {domain_value}}."
      )
    )
  }
  domain_value <- check_domain_abbrv(domain = domain_value, char_result = TRUE)
  return(domain_value)
}
