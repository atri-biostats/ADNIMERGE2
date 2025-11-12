# Function to extract baseline visit date/ enrollment date -----
#' @title ADNI Enrollment Date/First Baseline Visit Date
#' @description
#'  This function is used to extract enrollment date (baseline visit date)
#'  when subjects are enrolled in ADNI study for the first time.
#' @param .registry Data.frame of \code{\link{REGISTRY}()} eCRF
#' @return
#'  A data.frame of overall enrollment in ADNI study with the following variables:
#' \itemize{
#'  \item {\code{RID}}: Subject ID
#'  \item {\code{ORIGPROT}}: Original study protocols
#'  \item {\code{COLPROT}}: Study data collection protocols
#'  \item {\code{EXAMDATE}}: Enrollment date (i.e. first baseline visit date)
#' }
#' @examples
#' \dontrun{
#' overall_enroll_registry <- get_adni_enrollment(
#'   .registry = ADNIMERGE2::REGISTRY
#' )
#' }
#' @seealso \code{\link{get_adni_screen_date}()}
#' @rdname get_adni_enrollment
#' @family ADNI specific functions
#' @keywords adni_enroll_fun
#' @importFrom rlang arg_match
#' @importFrom dplyr mutate across case_when filter select starts_with if_any
#' @importFrom assertr verify is_uniq
#' @importFrom magrittr %>%
#' @export
get_adni_enrollment <- function(.registry) {
  COLPROT <- ORIGPROT <- RID <- EXAMDATE <- PTTYPE <- OVERALL_ENRLFG <- ENRLFG <- NULL
  check_colnames(
    .data = .registry,
    col_names = c("RID", "ORIGPROT", "COLPROT", "VISCODE", "VISTYPE", "EXAMDATE"),
    strict = TRUE
  )
  detect_numeric_value(
    value = .registry$VISTYPE,
    num_type = "any",
    stop_message = TRUE
  )
  detect_numeric_value(
    value = .registry$RGCONDCT,
    num_type = "any",
    stop_message = TRUE
  )

  .registry <- .registry %>%
    # Create study track
    mutate(PTTYPE = adni_study_track(COLPROT, ORIGPROT)) %>%
    # Enrollment flag
    mutate(
      OVERALL_ENRLFG = case_when(
        COLPROT %in% adni_phase()[5] & VISCODE %in% "4_bl" & PTTYPE %in% adni_pttype()[2] & VISTYPE != "Not done" ~ "Yes",
        COLPROT %in% adni_phase()[4] & VISCODE %in% "bl" & PTTYPE %in% adni_pttype()[2] & VISTYPE != "Not done" ~ "Yes",
        COLPROT %in% adni_phase()[3] & VISCODE %in% "v03" & PTTYPE %in% adni_pttype()[2] & VISTYPE != "Not done" ~ "Yes",
        COLPROT %in% adni_phase()[2] & VISCODE %in% "bl" & PTTYPE %in% adni_pttype()[2] & VISTYPE != "Not done" ~ "Yes",
        COLPROT %in% adni_phase()[1] & VISCODE %in% "bl" & PTTYPE %in% adni_pttype() & RGCONDCT == "Yes" ~ "Yes"
      )
    )

  output_dd <- .registry %>%
    filter(ORIGPROT == COLPROT) %>%
    filter(OVERALL_ENRLFG %in% "Yes") %>%
    filter(!is.na(EXAMDATE)) %>%
    verify(all(PTTYPE == adni_pttype()[2])) %>%
    assert_uniq(RID) %>%
    select(RID, ORIGPROT, COLPROT, EXAMDATE, ENRLFG = OVERALL_ENRLFG)

  return(output_dd)
}

# Function to extract screening date -----
#' @title Get ADNI Screening Date
#'
#' @details
#' This function is used to extract subject's screening date in ADNI study when
#' they participate in the study for the first time.
#'
#' \strong{Overall Screening}: Across the whole ADNI study phases
#' \strong{Phase Specific Screening}: Based on a particular set of ADNI study phases
#' \strong{Multiple Screening Visit}: Accounts for multiple screening stage,
#'  particularly in \code{ADNIGO} and \code{ADNI2} study phases.
#'
#' @param .registry Data.frame of \code{\link{REGISTRY}()} eCRF
#' @param phase Either \code{Overall} or phase-specific screening date, Default: 'Overall'

#' @param multiple_screen_visit
#'  A Boolean value to include multiple screen visits in ADNIGO and ADNI2 phases, Default: FALSE
#' @return
#'  A data.frame contains the following variables:
#'  \itemize{
#'   \item {\code{RID}}: Subject ID
#'   \item {\code{ORIGPROT}}: Original study protocols
#'   \item {\code{COLPROT}}: Study data collection protocols
#'   \item {\code{SCREENDATE}}: Screening visit date
#'   \item {\code{VISCODE}}: Screening visit code only when \code{multiple_screen_visit} is \code{TRUE}
#' }
#' @examples
#' \dontrun{
#' library(tidyverse)
#' library(ADNIMERGE2)
#'
#' # Overall screening: ----
#' # When subjects were screened for the first time in ADNI study.
#' overall_screen_registry <- get_adni_screen_date(
#'   .registry = ADNIMERGE2::REGISTRY,
#'   phase = "Overall",
#'   multiple_screen_visit = FALSE
#' )
#'
#' # Phase-specific screening:
#' # When subject screened for the first time in ADNI3 study phase.
#' adni3_screen_registry <- get_adni_screen_date(
#'   .registry = ADNIMERGE2::REGISTRY,
#'   phase = "ADNI3",
#'   multiple_screen_visit = FALSE
#' )
#'
#' # Multiple screen visits in ADNIGO and ADNI2 study phases.
#' adnigo2_screen_registry <- get_adni_screen_date(
#'   .registry = ADNIMERGE2::REGISTRY,
#'   phase = c("ADNIGO", "ADNI2"),
#'   multiple_screen_visit = TRUE
#' )
#'
#' # Screening across each ADNI study phases
#' phase_screen_registry <- get_adni_screen_date(
#'   .registry = ADNIMERGE2::REGISTRY,
#'   phase = ADNIMERGE2::adni_phase(),
#'   multiple_screen_visit = FALSE
#' )
#' }
#' @seealso \code{\link{get_adni_enrollment}()}
#' @rdname get_adni_screen_date
#' @family ADNI specific functions
#' @keywords adni_enroll_fun
#' @importFrom rlang arg_match
#' @importFrom dplyr mutate across case_when filter select starts_with if_any rename
#' @importFrom assertr verify assert
#' @importFrom magrittr %>%
#' @export
get_adni_screen_date <- function(.registry, phase = "Overall", multiple_screen_visit = FALSE) {
  RID <- COLPROT <- ORIGPROT <- EXAMDATE <- VISCODE <- PTTYPE <- NULL
  overall_screen_flag <- adni4_screen_flag <- adni3_screen_flag <- NULL
  adni2_screen_flag <- adnigo_screen_flag <- adni1_screen_flag <- second_screen_visit <- NULL
  rlang::arg_match(arg = phase, values = c("Overall", adni_phase()), multiple = TRUE)
  col_name_list <- c("RID", "ORIGPROT", "COLPROT", "VISCODE", "VISTYPE", "EXAMDATE")
  check_colnames(
    .data = .registry,
    col_names = col_name_list,
    strict = TRUE
  )
  check_object_type(multiple_screen_visit, "logical")
  check_overall_phase(phase = phase)

  detect_numeric_value(
    value = .registry$VISTYPE,
    num_type = "any",
    stop_message = TRUE
  )

  .registry <- .registry %>%
    # Create study track
    mutate(PTTYPE = adni_study_track(cur_study_phase = COLPROT, orig_study_phase = ORIGPROT)) %>%
    # First screening visit
    mutate(
      adni4_screen_flag = case_when(COLPROT %in% adni_phase()[5] & VISCODE %in% "4_sc" & PTTYPE %in% adni_pttype()[2] & VISTYPE != "Not done" ~ "Yes"),
      adni3_screen_flag = case_when(COLPROT %in% adni_phase()[4] & VISCODE %in% "sc" & PTTYPE %in% adni_pttype()[2] & VISTYPE != "Not done" ~ "Yes"),
      adni2_screen_flag = case_when(COLPROT %in% adni_phase()[3] & VISCODE %in% c("v01", "v02") & PTTYPE %in% adni_pttype()[2] & VISTYPE != "Not done" ~ "Yes"),
      adnigo_screen_flag = case_when(COLPROT %in% adni_phase()[2] & VISCODE %in% c("sc", "scmri") & PTTYPE %in% adni_pttype()[2] & VISTYPE != "Not done" ~ "Yes"),
      adni1_screen_flag = case_when(COLPROT %in% adni_phase()[1] & VISCODE %in% c("sc", "f") ~ "Yes")
    ) %>%
    mutate(second_screen_visit = case_when(
      (adni2_screen_flag %in% "Yes" & VISCODE %in% "v02") |
        (adnigo_screen_flag %in% "Yes" & VISCODE %in% "scmri") ~ "Yes"
    )) %>%
    {
      if (multiple_screen_visit == FALSE) {
        mutate(., across(
          c(adni2_screen_flag, adnigo_screen_flag),
          ~ case_when(
            !is.na(second_screen_visit) ~ NA_character_,
            TRUE ~ .x
          )
        ))
      } else {
        (.)
      }
    } %>%
    mutate(overall_screen_flag = case_when(
      ORIGPROT == COLPROT &
        (!is.na(adni4_screen_flag) |
          !is.na(adni3_screen_flag) |
          !is.na(adni2_screen_flag) |
          !is.na(adnigo_screen_flag) |
          !is.na(adni1_screen_flag)) ~ "Yes"
    ))

  if (all(phase %in% "Overall")) {
    # Overall screening dataset
    output_data <- .registry %>%
      filter(overall_screen_flag %in% "Yes") %>%
      verify(ORIGPROT == COLPROT) %>%
      verify(all(PTTYPE == adni_pttype()[2])) %>%
      # Only first screen date, does not account for re-screen
      filter(is.na(second_screen_visit)) %>%
      assert_uniq(RID) %>%
      select(RID, ORIGPROT, COLPROT, EXAMDATE)
  }

  # Phase-specific screening dataset
  if (any(!phase %in% "Overall")) {
    rlang::arg_match(arg = phase, values = adni_phase())
    screen_flag_patterns <- paste0(tolower(phase), "_screen_flag")
    output_data <- .registry %>%
      filter(COLPROT %in% phase) %>%
      {
        if (any(!phase %in% "Overall")) {
          filter(., if_any(.cols = starts_with(c(screen_flag_patterns)), .fns = ~ .x %in% "Yes"))
        } else {
          (.)
        }
      } %>%
      {
        if (nrow(.) > 0) {
          assert_uniq(., RID, COLPROT, VISCODE)
        } else {
          (.)
        }
      } %>%
      {
        if (multiple_screen_visit == FALSE) {
          select(., RID, ORIGPROT, COLPROT, VISCODE, EXAMDATE)
        } else {
          select(., RID, ORIGPROT, COLPROT, EXAMDATE)
        }
      }
  }

  output_data <- output_data %>%
    rename("SCREENDATE" = EXAMDATE)

  return(output_data)
}

# Function to extract baseline/screening diagnostics status ----
#' @title Get ADNI Baseline/Screening Diagnostics Summary
#'
#' @details
#'  This function is used to extract either the baseline or screening diagnostics status
#'  subjects when they participate for the first time in ADNI study.
#'
#' \strong{Screening Diagnostics Status}: The diagnostics status collected at screening visit.
#'
#' \strong{Baseline Diagnostics Status}:
#'
#' \itemize{
#'  \item The diagnostics status collected at baseline visit.
#'
#'  \item The diagnostics status collected at screening status for subjects that
#'  did not have a baseline collected diagnostics status.
#'  The corresponding screening visit/exam date will be used as a baseline record.
#'
#'  \strong{NOTE}: The result data will contains the screening diagnostics status
#'   of those who did not enrolled in the study (i.e. with screening failure) as their
#'   baseline diagnostics status. Required to adjust baseline diagnostics with
#'   corresponding subject enrollment date/enrollment flag.
#'   See examples for more information.
#' }
#'
#' @param .dxsum Data.frame of \code{\link{DXSUM}()} eCRF
#' @param visit_type
#'  Either \code{baseline} or \code{screen} diagnostic status, Default: 'baseline'
#' @param phase
#'  Either \code{Overall} or ADNI study phase specific, Default: 'Overall'
#' @return
#'  A data.frame with the following variables:
#' \itemize{
#'  \item {\code{RID}}: Subject ID
#'  \item {\code{ORIGPROT}}: Original study protocols
#'  \item {\code{COLPROT}}: Study data collection protocols
#'  \item {\code{EXAMDATE}}: Baseline/screening diagnostics assessment collection date
#'  \item {\code{DIAGNOSIS}}: Diagnostics status.
#' }
#' @examples
#' \dontrun{
#' library(tidyverse)
#' library(ADNIMERGE2)
#'
#' # Enrollment flag
#' enrollment_flag <- ADNIMERGE2::REGISTRY %>%
#'   get_adni_enrollment(.registry = .) %>%
#'   mutate(ENRLDT = as.Date(EXAMDATE)) %>%
#'   select(RID, ENRLDT)
#'
#' # Baseline diagnostics status for newly enrolled subject in ADNI study
#' overall_baseline_dx <- get_adni_blscreen_dxsum(
#'   .dxsum = ADNIMERGE2::DXSUM,
#'   phase = "Overall",
#'   visit_type = "baseline"
#' ) %>%
#'   left_join(
#'     enrollment_flag,
#'     by = "RID"
#'   ) %>%
#'   # Only enrolled subjects
#'   filter(ENRLFG %in% "Yes")
#'
#' # Phase-specific baseline diagnostic status: -----
#' # When subject enrolled in ADNI3 study phase
#' adni3_baseline_dx <- get_adni_blscreen_dxsum(
#'   .dxsum = ADNIMERGE2::DXSUM,
#'   phase = "ADNI3",
#'   visit_type = "baseline"
#' ) %>%
#'   left_join(
#'     enrollment_flag,
#'     by = "RID"
#'   ) %>%
#'   # Only enrolled subjects
#'   filter(ENRLFG %in% "Yes")
#'
#' # Screening Diagnostics Status: -----
#' # When subject were screened for first time in ADNI study.
#' first_screen_dx <- get_adni_blscreen_dxsum(
#'   .dxsum = ADNIMERGE2::DXSUM,
#'   phase = "Overall",
#'   visit_type = "screen"
#' )
#' # Phase-specific screening diagnostic status: ------
#' # When subject were screened for ADNI4 study phase
#' adni3_screen_dx <- get_adni_blscreen_dxsum(
#'   .dxsum = ADNIMERGE2::DXSUM,
#'   phase = "ADNI4",
#'   visit_type = "screen"
#' )
#' }
#' @rdname get_adni_blscreen_dxsum
#' @family ADNI specific functions
#' @keywords adni_enroll_fun
#' @importFrom rlang arg_match
#' @importFrom dplyr mutate across case_when filter select starts_with if_any
#' @importFrom assertr verify
#' @importFrom magrittr %>%
#' @importFrom tidyselect all_of
#' @importFrom cli cli_abort
#' @export
get_adni_blscreen_dxsum <- function(.dxsum, visit_type = "baseline", phase = "Overall") {
  COLPROT <- ORIGPROT <- PTTYPE <- NULL
  rlang::arg_match(arg = phase, values = c("Overall", adni_phase()), multiple = TRUE)
  rlang::arg_match0(arg = visit_type, values = c("baseline", "screen"))
  check_overall_phase(phase = phase)
  col_names <- c("RID", "ORIGPROT", "COLPROT", "VISCODE", "EXAMDATE", "DIAGNOSIS")

  output_data <- .dxsum %>%
    select(all_of(col_names)) %>%
    adjust_scbl_record(
      .data = ,
      adjust_date_col = "EXAMDATE",
      check_col = "DIAGNOSIS",
      extra_id_cols = "ORIGPROT"
    ) %>%
    # Create study track
    mutate(PTTYPE = adni_study_track(COLPROT, ORIGPROT)) %>%
    {
      if (visit_type %in% "baseline") {
        filter(., if_all(all_of("VISCODE"), ~ .x %in% get_baseline_vistcode()))
      } else {
        filter(., if_all(all_of("VISCODE"), ~ .x %in% get_screen_vistcode(type = "all")))
      }
    } %>%
    assert_uniq(all_of(c("RID", "COLPROT"))) %>%
    filter(if_any(.cols = all_of(c("EXAMDATE", "DIAGNOSIS")), ~ !is.na(.x)))

  if (all(!phase %in% "Overall")) {
    rlang::arg_match(arg = phase, values = adni_phase(), multiple = TRUE)
    output_data <- output_data %>%
      filter(if_any(all_of("COLPROT"), ~ .x %in% phase))
  }

  output_data <- output_data %>%
    verify(ORIGPROT == COLPROT) %>%
    verify(all(PTTYPE == adni_pttype()[2])) %>%
    select(all_of(col_names))

  if (visit_type %in% "baseline") {
    warning(
      paste0(
        "The output data may contains the screening diagnostics status of ",
        "subjects with screening failure as a baseline diagnostics status. ",
        "Required to use enrollment date/flag."
      )
    )
  }
  return(output_data)
}

## Utility ------
#' @title Checks for Overall and PHASE List Combination
#' @param phase Phase argument
#' @return
#'  An error if a combination of 'Overall' and phase list character vector is provided.
#' @rdname check_overall_phase
#' @keywords internal
#' @importFrom cli cli_abort
check_overall_phase <- function(phase) {
  if (grepl("Overall|overall", phase) & length(phase) > 1) {
    cli::cli_abort(
      message = c(
        "{.var phase} must be either {.val {'Overall'}} or {.val {adni_phase()}}. \n",
        "{.var phase} contains {.val {phase}}"
      )
    )
  }
  invisible(phase)
}

# Get Death Flag -----
#' @title Death Flag
#' @description
#' This function is used to extract death records in the study based on the
#' adverse events and disposition records.
#' @param .adverse
#'  Adverse events record for ADNI3-4 study phase, see \code{\link{ADVERSE}()}
#' @param .recadv
#'  Adverse events record for ADNI1-GO-2, see \code{\link{RECADV}()}
#' @param .studysum
#'   Dispositions record for ADNI3-4, see \code{\link{STUDYSUM}()}
#' @return A data.frame with the following columns:
#' \itemize{
#'  \item {\code{RID}}: Subject ID
#'  \item {\code{ORIGPROT}}: Original study protocols
#'  \item {\code{COLPROT}}: Study data collection protocols which the event was recorded
#'  \item {\code{DTHDTC}}: Death date
#'  \item {\code{DTHFL}}: Death flag, \code{Yes}
#' }
#' @examples
#' \dontrun{
#' library(tidyverse)
#' library(ADNIMERGE2)
#' get_death_flag(
#'   .studysum = ADNIMERGE2::STUDYSUM,
#'   .adverse = ADNIMERGE2::ADVERSE,
#'   .recadv = ADNIMERGE2::RECADV
#' )
#' }
#' @rdname get_death_flag
#' @family ADNI flag functions
#' @keywords adni_enroll_fun
#' @importFrom dplyr full_join distinct group_by ungroup filter select mutate
#' @importFrom assertr assert
#' @export
get_death_flag <- function(.studysum, .adverse, .recadv) {
  SDPRIMARY <- RID <- ORIGPROT <- COLPROT <- SAEDEATH <- AEHDTHDT <- AEHDTHDT <- NULL
  VISCODE <- AEHDEATH <- DTHFL <- DTHDTC <- NULL

  # Based on reported study disposition; for ADNI3 & ADNI4 phases
  check_colnames(
    .data = .studysum,
    col_names = c("RID", "ORIGPROT", "COLPROT", "SDPRIMARY", "SDPRIMARY"),
    strict = TRUE,
    stop_message = TRUE
  )
  death_studysum <- .studysum %>%
    assert(is.character, SDPRIMARY) %>%
    filter(SDPRIMARY == "Death") %>%
    select(RID, ORIGPROT, COLPROT, SDPRIMARY) %>%
    assert_uniq(RID)

  # Based on reported adverse events: ADNI3 & ADNI4 phases
  check_colnames(
    .data = .adverse,
    col_names = c("RID", "ORIGPROT", "COLPROT", "VISCODE", "SAEDEATH", "AEHDTHDT", "SAEDEATH"),
    strict = TRUE,
    stop_message = TRUE
  )

  detect_numeric_value(
    value = .adverse$SAEDEATH,
    num_type = "any",
    stop_message = TRUE
  )

  death_adverse_even_adni34 <- .adverse %>%
    assert(is.character, SAEDEATH) %>%
    filter(SAEDEATH == "Yes" | !is.na(AEHDTHDT)) %>%
    select(RID, ORIGPROT, COLPROT, VISCODE, AEHDTHDT, DEATH = SAEDEATH) %>%
    assert_uniq(RID)

  # Based on reported adverse events: ADNI1, ADNIGO, and ADNI2 phases
  check_colnames(
    .data = .recadv,
    col_names = c("RID", "ORIGPROT", "COLPROT", "VISCODE", "AEHDEATH", "AEHDEATH"),
    strict = TRUE,
    stop_message = TRUE
  )

  detect_numeric_value(
    value = .recadv$AEHDEATH,
    num_type = "any",
    stop_message = TRUE
  )

  death_adverse_even_adni12go <- .recadv %>%
    assert(is.character, AEHDEATH) %>%
    filter(AEHDEATH == "Yes" | !is.na(AEHDTHDT)) %>%
    select(RID, ORIGPROT, COLPROT, VISCODE, AEHDTHDT, DEATH = AEHDEATH) %>%
    distinct() %>%
    assert_non_missing(VISCODE) %>%
    group_by(RID, ORIGPROT, COLPROT) %>%
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


# Get Discontinuation Flag -----
#' @title Study Discontinuation Flag
#' @description This function is used to get early discontinuation list in ADNI study.
#'   Based on the \code{\link{REGISTRY}()} eCRF for ADNI1-GO-2 and
#'   \code{\link{STUDYSUM}} eCRF for ADNI3-4.
#' @param .registry Registry record for ADNI1-GO-2, see \code{\link{REGISTRY}()}
#' @param .studysum Disposition record for ADNI3-4, see \code{\link{STUDYSUM}()}
#' @return A data.frame with the following columns:
#' \itemize{
#'  \item {\code{RID}}: Subject ID
#'  \item {\code{ORIGPROT}}: Original study protocols
#'  \item {\code{COLPROT}}: Study data collection protocols which the event was recorded
#'  \item {\code{SDSTATUS}}: Disposition status
#'  \item {\code{SDDATE}}: Disposition date
#' }
#' @examples
#' \dontrun{
#' get_disposition_flag(
#'   .registry = ADNIMERGE2::REGISTRY,
#'   .studysum = ADNIMERGE2::STUDYSUM
#' )
#' }
#' @rdname get_disposition_flag
#' @family ADNI flag functions
#' @keywords adni_enroll_fun
#' @importFrom dplyr filter distinct pull group_by select bind_rows row_number
#' @importFrom tidyr nest
#' @export
get_disposition_flag <- function(.registry, .studysum) {
  RID <- ORIGPROT <- COLPROT <- PTSTATUS <- EXAMDATE <- VISCODE <- NULL
  SDSTATUS <- SDDATE <- INCLUSION <- EXCLUSION <- INCROLL <- VERSION <- NULL
  INCNEWPT <- EXCCRIT <- MRIFIND <- NVRDISC <- NVROT <- AENUM <- REASONS <- NULL

  # Adjusting for any conducted follow-up visits
  get_rid_followup <- function(check_rid, check_phase, registry = .registry) {
    RID <- COLPORT <- NULL
    rid_list <- registry %>%
      filter(RID %in% check_rid) %>%
      filter(COLPROT %in% check_phase) %>%
      distinct(RID) %>%
      pull(RID)
    return(rid_list)
  }

  detect_numeric_value(
    value = .registry$PTSTATUS,
    num_type = "any",
    stop_message = TRUE
  )

  # Warning?
  # Early discontinuations in ADNIGO and ADNI2
  adnigo2 <- .registry %>%
    filter(COLPROT %in% adni_phase()[2:3]) %>%
    filter(str_detect(string = PTSTATUS, pattern = "Discontinued")) %>%
    group_by(RID, COLPROT) %>%
    filter((all(is.na(EXAMDATE)) & row_number() == 1) |
      (EXAMDATE == min(EXAMDATE, na.rm = TRUE))) %>%
    ungroup() %>%
    assert_uniq(RID, COLPROT) %>%
    select(RID, ORIGPROT, COLPROT, PTSTATUS, EXAMDATE, VISCODE)

  # Early discontinuations in ADNI3 and ADNI4
  detect_numeric_value(
    value = .studysum$SDSTATUS,
    num_type = "any",
    stop_message = TRUE
  )

  disc_lvls <- c("Enrolled - Early discontinuation of study visits", "Never enrolled")
  detail_cols <- c(
    "INCLUSION", "EXCLUSION", "INCROLL", "VERSION", "INCNEWPT",
    "EXCCRIT", "MRIFIND", "NVRDISC", "NVROT", "AENUM"
  )
  adni34 <- .studysum %>%
    filter(SDSTATUS %in% disc_lvls) %>%
    select(
      RID, ORIGPROT, COLPROT, SDSTATUS, SDDATE, INCLUSION, EXCLUSION, INCROLL,
      VERSION, INCNEWPT, EXCCRIT, MRIFIND, NVRDISC, NVROT, AENUM
    ) %>%
    distinct() %>%
    nest(REASONS = all_of(detail_cols)) %>%
    assert_uniq(RID, COLPROT)

  input_phase <- adni_phase()[2:4]
  phase_rid <- lapply(input_phase, function(x) {
    if (x %in% adni_phase()[2:3]) .data <- adnigo2 else .data <- adni34
    get_rid_followup(
      check_rid = .data %>%
        filter(COLPROT %in% x) %>%
        pull(RID),
      check_phase = adni_phase()[adni_phase_order_num(phase = x) + 1:5]
    )
  })
  names(phase_rid) <- input_phase
  rid_list <- lapply(names(phase_rid), function(x) {
    setdiff(
      unlist(phase_rid[names(phase_rid) %in% x]),
      unlist(phase_rid[!names(phase_rid) %in% x])
    )
  }) %>%
    unlist()

  # Overall Never Enrolled or Early Discontinued
  early_discon_adni <- adni34 %>%
    assert_non_missing(SDDATE) %>%
    bind_rows(adnigo2 %>%
      select(RID, ORIGPROT, COLPROT, SDSTATUS = PTSTATUS, SDDATE = EXAMDATE)) %>%
    filter(!RID %in% rid_list) %>%
    select(RID, ORIGPROT, COLPROT, SDSTATUS, SDDATE) %>%
    assert_non_missing(SDSTATUS)

  return(early_discon_adni)
}

# Screening/Baseline Visit Code -----
#' @title Get Screening Visit Code
#' @param type
#'  Type either all screening visit codes or the first screening visit per
#'  study phase, Default: 'all'
#' @return A character vector
#' @examples
#' \dontrun{
#' get_screen_vistcode(type = "all")
#' get_screen_vistcode(type = "first")
#' }
#' @seealso
#'  \code{\link{get_baseline_vistcode}()}
#'  \code{\link{VISITS}()}
#' @rdname get_screen_vistcode
#' @family ADNI visit codes
#' @keywords adni_utils
#' @export
#' @importFrom rlang arg_match0

get_screen_vistcode <- function(type = "all") {
  rlang::arg_match0(arg = type, values = c("all", "first"))
  sc_vsitcode <- c("sc", "f", "v01", "v02", "4_sc")
  if (type %in% "first") {
    sc_vsitcode <- sc_vsitcode[-4]
  }
  return(sc_vsitcode)
}

#' @title Get Baseline Visit Code
#' @return A character vector
#' @examples
#' \dontrun{
#' get_baseline_vistcode()
#' }
#' @seealso
#'  \code{\link{get_screen_vistcode}()}
#'  \code{\link{VISITS}()}
#' @rdname get_baseline_vistcode
#' @keywords adni_utils
#' @family ADNI visit codes
#' @export
get_baseline_vistcode <- function() {
  return(c("bl", "v03", "4_bl"))
}
