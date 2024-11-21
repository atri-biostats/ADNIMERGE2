# Function to extract baseline/initial visits examdate -----
#' @title Function to extract baseline/initial visits examdate
#' @description
#'  This function is used to extract baseline visit of participants when they enrolled first time in ADNI study or enrolled as either newly or rollover in specific ADNI study phases.
#' @param dd Data frame similar as REGISTRY eCRF
#' @param phase Either `Overall` or phase-specific enrollment, Default: 'Overall'
#' @param both A boolean to include both overall and phase-specific enrollment list, Default: FALSE
#' @return
#'  * If `both == TRUE`: a list of data frames that includes both overall (`overall_enroll`) and phase-specific enrollment (`phase_enroll`)
#'  * Otherwise a data frame that contains `RID`, ""corresponding to the provided input arguments:
#'      + Overall enrollment if `phase == "Overall"`
#'      + Phase specific enrollment if `phase != "Overall"`
#'  * The data frame will contains `RID`, `ORIGPROT`, `COLPROT`, and `EXAMDATE` variables.
#' @examples
#' \dontrun{
#' # Overall enrollment: when participants enrolled as newly enrollee in ADNI study for the first time.
#' overall_enroll_registry <- adni_enrollment(
#'   dd = ADNIMERGE2::REGISTRY,
#'   phase = "Overall",
#'   both = FALSE
#' )
#' # Phase-specific enrollment: when participants enrolled in ADNI3 study phase.
#' adni3_enroll_registry <- adni_enrollment(
#'   dd = ADNIMERGE2::REGISTRY,
#'   phase = "ADNI3",
#'   both = FALSE
#' )
#' # Enrollment across each ADNI phases
#' phase_enroll_registry <- adni_enrollment(
#'   dd = ADNIMERGE2::REGISTRY,
#'   phase = adni_phase(),
#'   both = FALSE
#' )
#' # Overall and phase-specific enrollment
#' both_enroll_registry <- adni_enrollment(
#'   dd = ADNIMERGE2::REGISTRY,
#'   phase = "Overall",
#'   both = TRUE
#' )
#' }
#' @seealso [extract_adni_screen_date()]
#' @rdname adni_enrollment
#' @export
#' @importFrom rlang arg_match
#' @importFrom dplyr mutate
#' @importFrom dplyr across
#' @importFrom dplyr case_when
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr starts_with
#' @importFrom dplyr if_any
#' @importFrom assertr verify
#' @importFrom assertr is_uniq
#' @importFrom magrittr %>%
adni_enrollment <- function(dd, phase = "Overall", both = FALSE) {
  COLPROT <- ORIGPROT <- RID <- EXAMDATE <- participant_type <- NULL
  overall_baseline_flag <- NULL
  rlang::arg_match(arg = phase, values = c("Overall", adni_phase()), multiple = TRUE)
  col_name_list <- c("RID", "ORIGPROT", "COLPROT", "VISCODE", "VISTYPE", "EXAMDATE")
  check_colnames(dd = dd, col_names = col_name_list, strict = TRUE)
  if (!is.logical(both)) stop("both must be a boolean value")
  # `VISTYPE` and `RGCONDUCT` must not contains any numeric value
  detect_numeric_value(
    input_value = dd$VISTYPE,
    num_type = "any",
    stop_message = TRUE
  )
  detect_numeric_value(
    input_value = dd$RGCONDCT,
    num_type = "any",
    stop_message = TRUE
  )

  dd <- dd %>%
    mutate(across(c(COLPROT, ORIGPROT), as.character)) %>%
    # Adding participant types (i.e. study track)
    mutate(participant_type = adni_study_track(
      cur_study_phase = COLPROT,
      orig_study_phase = ORIGPROT
    )) %>%
    # Identify baseline visits: that includes initial visits of rollovers
    mutate(
      adni4_baseline_flag = case_when(
        COLPROT %in% "ADNI4" & VISCODE %in% "4_init" &
          participant_type %in% "Rollover" & VISTYPE != "Not done" ~ "Yes",
        COLPROT %in% "ADNI4" & VISCODE %in% "4_bl" &
          participant_type %in% "New" & VISTYPE != "Not done" ~ "Yes"
      ),
      adni3_baseline_flag = case_when(
        COLPROT %in% "ADNI3" & VISCODE %in% "init" &
          participant_type %in% "Rollover" & VISTYPE != "Not done" ~ "Yes",
        COLPROT %in% "ADNI3" & VISCODE %in% "bl" &
          participant_type %in% "New" & VISTYPE != "Not done" ~ "Yes"
      ),
      adni2_baseline_flag = case_when(
        COLPROT %in% "ADNI2" & VISCODE %in% "v06" &
          participant_type %in% "Rollover" & VISTYPE != "Not done" ~ "Yes",
        COLPROT %in% "ADNI2" & VISCODE %in% "v03" &
          participant_type %in% "New" & VISTYPE != "Not done" ~ "Yes"
      ),
      # The screening visits in ADNIGO may be considered as their baseline visit???
      adnigo_baseline_flag = case_when(
        COLPROT %in% "ADNIGO" & VISCODE %in% "bl" &
          participant_type %in% "New" & VISTYPE != "Not done" ~ "Yes",
        COLPROT %in% "ADNIGO" & VISCODE %in% "sc" &
          participant_type %in% "Rollover" & VISTYPE != "Not done" ~ "Yes"
      ),
      adni1_baseline_flag = case_when(COLPROT %in% "ADNI1" & VISCODE %in% "bl" &
        participant_type %in% c("New", "Rollover") & RGCONDCT == "Yes" ~ "Yes")
    ) %>%
    mutate(overall_baseline_flag = case_when(
      ORIGPROT == COLPROT &
        c(!is.na(adni4_baseline_flag) |
          !is.na(adni3_baseline_flag) |
          !is.na(adni2_baseline_flag) |
          !is.na(adnigo_baseline_flag) |
          !is.na(adni1_baseline_flag)) ~ "Yes"
    ))

  if (both == TRUE | all(phase %in% "Overall")) {
    # Overall enrollment dataset
    overall_enroll_registry <- dd %>%
      filter(overall_baseline_flag %in% "Yes") %>%
      verify(ORIGPROT == COLPROT) %>%
      verify(participant_type == "New") %>%
      assert(is_uniq, RID) %>%
      select(RID, ORIGPROT, COLPROT, EXAMDATE)
  }

  # Phase-specific enrollment dataset
  if (both == TRUE & all(phase %in% "Overall") == TRUE) phase <- adni_phase()
  baseline_flag_patterns <- paste0(tolower(phase), "_baseline_flag")
  phase_enroll_registry <- dd %>%
    filter(COLPROT %in% phase) %>%
    {
      if (any(!phase %in% "Overall")) {
        filter(., if_any(.cols = starts_with(c(baseline_flag_patterns)), .fns = ~ .x %in% "Yes"))
      } else {
        (.)
      }
    } %>%
    select(RID, ORIGPROT, COLPROT, EXAMDATE) %>%
    {
      if (nrow(.) > 0) {
        assert_rows(., col_concat, is_uniq, RID, COLPROT)
      } else {
        (.)
      }
    }

  if (both) {
    output_dd <- list(
      "overall_enroll" = overall_enroll_registry,
      "phase_enroll" = phase_enroll_registry
    )
  } else {
    if (all(phase %in% "Overall")) output_dd <- overall_enroll_registry else output_dd <- phase_enroll_registry
  }

  return(output_dd)
}

# Function to extract screen date -----
#' @title Function to extract screen date
#' @description
#'  This function is used to extract screening date of participants when they screened for first time in ADNI study.
#' @param dd Data frame similar as REGISTRY eCRF
#' @param phase Either `Overall` or phase-specific screening date, Default: 'Overall'
#' @param both A boolean to include both overall and phase-specific enrollment list, Default: FALSE
#' @param multiple_screen_visit A boolean to include multiple screen visits in ADNIGO and ADNI2 phases, Default: FALSE
#' @return
#'  * If `both == TRUE`: a list of data frames that includes both overall (`overall_screen`) and phase-specific enrollment (`phase_screen`)
#'  * Otherwise a data frame corresponding to the provided input arguments:
#'     + Overall screen if `phase == "Overall"` and will contains one records per participant regardless `multiple_screen_visit` value.
#'      + Phase specific screen if `phase != "Overall"` and will contains one records per participant if `multiple_screen_visit == TRUE`.
#'  * The data frame will contains `RID`, `ORIGPROT`, `COLPROT`, and `SCREENDATE` variables.
#'      + The data frame will contains `VISCODE` for `multiple_screen_visit == TRUE`.
#' @examples
#' \dontrun{
#' # Overall screening: when participants screened for the first time in ADNI study.
#' overall_screen_registry <- extract_adni_screen_date(
#'   dd = ADNIMERGE2::REGISTRY,
#'   phase = "Overall",
#'   both = FALSE,
#'   multiple_screen_visit = FALSE
#' )
#' # Phase-specific screening: when participants screened for the first time in ADNI3 study phase.
#' adni3_screen_registry <- extract_adni_screen_date(
#'   dd = ADNIMERGE2::REGISTRY,
#'   phase = "ADNI3",
#'   both = FALSE,
#'   multiple_screen_visit = FALSE
#' )
#' # Multiple screens visit in each ADNIGO and ADNI2 study phases.
#' adnigo2_screen_registry <- extract_adni_screen_date(
#'   dd = ADNIMERGE2::REGISTRY,
#'   phase = c("ADNIGO", "ADNI2"),
#'   both = FALSE,
#'   multiple_screen_visit = TRUE
#' )
#' # Screening across each ADNI phases
#' phase_screen_registry <- extract_adni_screen_date(
#'   dd = ADNIMERGE2::REGISTRY,
#'   phase = adni_phase(),
#'   both = FALSE,
#'   multiple_screen_visit = FALSE
#' )
#' # Overall and phase-specific screening
#' both_screen_registry <- extract_adni_screen_date(
#'   dd = ADNIMERGE2::REGISTRY,
#'   phase = "Overall",
#'   both = TRUE,
#'   multiple_screen_visit = FALSE
#' )
#' }
#' @seealso [adni_enrollment()]
#' @rdname extract_adni_screen_date
#' @export
#' @importFrom rlang arg_match
#' @importFrom dplyr mutate
#' @importFrom dplyr across
#' @importFrom dplyr case_when
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr starts_with
#' @importFrom dplyr if_any
#' @importFrom assertr verify
#' @importFrom assertr assert
#' @importFrom assertr is_uniq
#' @importFrom assertr col_concat
#' @importFrom assertr assert_rows
#' @importFrom magrittr %>%

## Questions: For participants that failed screening in ADNI1, does the exam date implies their screening date/ disposition?
extract_adni_screen_date <- function(dd, phase = "Overall", both = FALSE, multiple_screen_visit = FALSE) {
  RID <- COLPROT <- ORIGPROT <- EXAMDATE <- VISCODE <- participant_type <- NULL
  overall_screen_flag <- adnigo_screen_flag <- adni2_screen_flag <- second_screen_visit <- NULL
  rlang::arg_match(arg = phase, values = c("Overall", adni_phase()), multiple = TRUE)
  col_name_list <- c("RID", "ORIGPROT", "COLPROT", "VISCODE", "VISTYPE", "EXAMDATE")
  check_colnames(dd = dd, col_names = col_name_list, strict = TRUE)
  if (!is.logical(both)) stop("`both` must be a boolean value")
  if (!is.logical(multiple_screen_visit)) stop("`multiple_screen_visit` must be a boolean value")

  # `VISTYPE` must not contains any numeric value
  detect_numeric_value(
    input_value = dd$VISTYPE,
    num_type = "any",
    stop_message = TRUE
  )

  dd <- dd %>%
    mutate(across(c(COLPROT, ORIGPROT), as.character)) %>%
    # Adding participant types (i.e. study track)
    mutate(participant_type = adni_study_track(
      cur_study_phase = COLPROT,
      orig_study_phase = ORIGPROT
    )) %>%
    # Identify the first screen visits
    mutate(
      adni4_screen_flag = case_when(COLPROT %in% "ADNI4" & VISCODE %in% "4_sc" & participant_type %in% "New" & VISTYPE != "Not done" ~ "Yes"),
      adni3_screen_flag = case_when(COLPROT %in% "ADNI3" & VISCODE %in% "sc" & participant_type %in% "New" & VISTYPE != "Not done" ~ "Yes"),
      adni2_screen_flag = case_when(COLPROT %in% "ADNI2" & VISCODE %in% c("v01", "v02") & participant_type %in% "New" & VISTYPE != "Not done" ~ "Yes"),
      adnigo_screen_flag = case_when(COLPROT %in% "ADNIGO" & VISCODE %in% c("sc", "scmri") & participant_type %in% "New" & VISTYPE != "Not done" ~ "Yes"),
      adni1_screen_flag = case_when(COLPROT %in% "ADNI1" & VISCODE %in% "sc" & participant_type %in% "New" ~ "Yes")
    ) %>%
    mutate(second_screen_visit = case_when(c(adni2_screen_flag %in% "Yes" & VISCODE %in% "v02") |
      c(adnigo_screen_flag %in% "Yes" & VISCODE %in% "scmri") ~ "Yes")) %>%
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
    # Checks for any screening forms for rollovers??
    mutate(overall_screen_flag = case_when(ORIGPROT == COLPROT & c(!is.na(adni4_screen_flag) |
      !is.na(adni3_screen_flag) |
      !is.na(adni2_screen_flag) |
      !is.na(adnigo_screen_flag) |
      !is.na(adni1_screen_flag)) ~ "Yes"))

  if (both == TRUE | all(phase %in% "Overall")) {
    # Overall enrollment dataset
    overall_screen_registry <- dd %>%
      filter(overall_screen_flag %in% "Yes") %>%
      verify(ORIGPROT == COLPROT) %>%
      verify(participant_type == "New") %>%
      # Only first screen date
      filter(is.na(second_screen_visit)) %>%
      assert(is_uniq, RID) %>%
      select(RID, ORIGPROT, COLPROT, "SCREENDATE" = EXAMDATE)
  }

  # Phase-specific enrollment dataset
  if (both == TRUE & all(phase %in% "Overall") == TRUE) phase <- adni_phase()
  screen_flag_patterns <- paste0(tolower(phase), "_screen_flag")
  phase_screen_registry <- dd %>%
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
        assert_rows(., col_concat, is_uniq, RID, COLPROT, VISCODE)
      } else {
        (.)
      }
    } %>%
    {
      if (multiple_screen_visit == FALSE) {
        select(., RID, ORIGPROT, COLPROT, VISCODE, "SCREENDATE" = EXAMDATE)
      } else {
        select(., RID, ORIGPROT, COLPROT, "SCREENDATE" = EXAMDATE)
      }
    }

  if (both) {
    output_dd <- list(
      "overall_screen" = overall_screen_registry,
      "phase_screen" = phase_screen_registry
    )
  } else {
    if (all(phase %in% "Overall")) output_dd <- overall_screen_registry else output_dd <- phase_screen_registry
  }

  return(output_dd)
}

# Function to extract baseline/screen diagnostics status ----
#' @title Function to extract baseline/screens diagnostics status
#' @description
#'  This function is used to extract the baseline/screen diagnostics status of when they enrolled/participated in ADNI study.
#' @param dd Data frame similar as DXSUM eCRF
#' @param phase Either `Overall` or phase-specific enrollment, Default: 'Overall'
#' @param visit_type Either `baseline` or `screen` diagnostic status, Default: 'baseline'
#' @return A data frame that contains `RID`, `RID`, `ORIGPROT`, `COLPROT`, `EXAMDATE` and either `BL.DIAGNOSIS` for baseline visit or `BL.DIAGNOSIS` for screen visit.
#' @examples
#' \dontrun{
#' # Baseline diagnostics status:
#' # When participants enrolled as newly enrollee in ADNI study for the first time.
#' overall_baseline_dx <- extract_blscreen_dxsum(
#'   dd = ADNIMERGE2::DXSUM,
#'   phase = "Overall",
#'   visit_type = "baseline"
#' )
#' # Phase-specific baseline diagnostic status: when participants enrolled in ADNI3 study phase.
#' adni3_baseline_dx <- extract_blscreen_dxsum(
#'   dd = ADNIMERGE2::DXSUM,
#'   phase = "ADNI3",
#'   visit_type = "baseline"
#' )
#' # Screen diagnostics status: when participants participated in ADNI study for the first time.
#' first_screen_dx <- extract_blscreen_dxsum(
#'   dd = ADNIMERGE2::DXSUM,
#'   phase = "Overall",
#'   visit_type = "screen"
#' )
#' # Phase-specific baseline diagnostic status: when participants participated in ADNI3 study phase.
#' adni3_screen_dx <- extract_blscreen_dxsum(
#'   dd = ADNIMERGE2::DXSUM,
#'   phase = "ADNI3",
#'   visit_type = "screen"
#' )
#' }
#' @rdname extract_blscreen_dxsum
#' @export
#' @importFrom rlang arg_match
#' @importFrom dplyr mutate
#' @importFrom dplyr across
#' @importFrom dplyr case_when
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr starts_with
#' @importFrom dplyr if_any
#' @importFrom assertr verify
#' @importFrom assertr is_uniq
#' @importFrom magrittr %>%

extract_blscreen_dxsum <- function(dd, phase = "Overall", visit_type = "baseline") {
  RID <- COLPROT <- ORIGPROT <- EXAMDATE <- VISCODE <- DIAGNOSIS <- NULL
  overall_baseline_dx_flag <- overall_screen_dx_flag <- participant_type <- NULL
  rlang::arg_match(arg = phase, values = c("Overall", adni_phase()), multiple = TRUE)
  rlang::arg_match0(arg = visit_type, value = c("baseline", "screen"))
  col_name_list <- c("RID", "ORIGPROT", "COLPROT", "VISCODE", "EXAMDATE", "DIAGNOSIS")
  check_colnames(dd = dd, col_names = col_name_list, strict = TRUE)

  if (visit_type %in% "baseline") prefix <- "BL" else prefix <- "SC"

  dd <- dd %>%
    mutate(across(c(COLPROT, ORIGPROT), as.character)) %>%
    # Add participant types (i.e. study track)
    mutate(participant_type = adni_study_track(
      cur_study_phase = COLPROT,
      orig_study_phase = ORIGPROT
    )) %>%
    # Identify baseline diagnostics status
    {
      if (visit_type %in% "baseline") {
        mutate(.,
          adni4_baseline_dx_flag = case_when(
            COLPROT %in% "ADNI4" & VISCODE %in% "4_init" &
              participant_type %in% "Rollover" ~ "Yes",
            COLPROT %in% "ADNI4" & VISCODE %in% "4_bl" &
              participant_type %in% "New" ~ "Yes"
          ),
          adni3_baseline_dx_flag = case_when(
            COLPROT %in% "ADNI3" & VISCODE %in% "init" &
              participant_type %in% "Rollover" ~ "Yes",
            COLPROT %in% "ADNI3" & VISCODE %in% "bl" &
              participant_type %in% "New" ~ "Yes"
          ),
          adni2_baseline_dx_flag = case_when(
            COLPROT %in% "ADNI2" & VISCODE %in% "v06" &
              participant_type %in% "Rollover" ~ "Yes",
            COLPROT %in% "ADNI2" & VISCODE %in% "v03" &
              participant_type %in% "New" ~ "Yes"
          ),
          adnigo_baseline_dx_flag = case_when(COLPROT %in% "ADNIGO" & VISCODE %in% "bl" &
            participant_type %in% c("New", "Rollover") ~ "Yes"),
          adni1_baseline_dx_flag = case_when(COLPROT %in% "ADNI1" & VISCODE %in% "bl" &
            participant_type %in% c("New", "Rollover") ~ "Yes")
        ) %>%
          mutate(., overall_baseline_dx_flag = case_when(
            ORIGPROT == COLPROT &
              c(!is.na(adni4_baseline_dx_flag) |
                !is.na(adni3_baseline_dx_flag) |
                !is.na(adni2_baseline_dx_flag) |
                !is.na(adnigo_baseline_dx_flag) |
                !is.na(adni1_baseline_dx_flag)) ~ "Yes"
          ))
      } else {
        mutate(.,
          adni4_screen_dx_flag = case_when(COLPROT %in% "ADNI4" & VISCODE %in% "4_sc" & participant_type %in% "New" ~ "Yes"),
          adni3_screen_dx_flag = case_when(COLPROT %in% "ADNI3" & VISCODE %in% "sc" & participant_type %in% "New" ~ "Yes"),
          adni2_screen_dx_flag = case_when(COLPROT %in% "ADNI2" & VISCODE %in% c("v01", "v02") & participant_type %in% "New" ~ "Yes"),
          adnigo_screen_dx_flag = case_when(COLPROT %in% "ADNIGO" & VISCODE %in% c("sc", "scmri") & participant_type %in% "New" ~ "Yes"),
          adni1_screen_dx_flag = case_when(COLPROT %in% "ADNI1" & VISCODE %in% c("sc", "f") & participant_type %in% "New" ~ "Yes")
        ) %>%
          mutate(., overall_screen_dx_flag = case_when(ORIGPROT == COLPROT &
            c(!is.na(adni4_screen_dx_flag) |
              !is.na(adni3_screen_dx_flag) |
              !is.na(adni2_screen_dx_flag) |
              !is.na(adnigo_screen_dx_flag) |
              !is.na(adni1_screen_dx_flag)) ~ "Yes"))
      }
    }

  overall_blscreen_dxsum <- dd %>%
    {
      if (visit_type %in% "baseline") {
        filter(., overall_baseline_dx_flag %in% "Yes")
      } else {
        filter(., overall_screen_dx_flag %in% "Yes")
      }
    } %>%
    verify(ORIGPROT == COLPROT) %>%
    verify(participant_type == "New") %>%
    assert(is_uniq, RID) %>%
    select(RID, ORIGPROT, COLPROT, EXAMDATE, DIAGNOSIS) %>%
    rename_with(~ paste0(prefix, ".", .x), DIAGNOSIS)

  # Phase-specific baseline/initial visits diagnostic status
  if (all(phase %in% "Overall") == TRUE) phase_name_list <- adni_phase() else phase_name_list <- phase
  blscreen_dx_flag_patterns <- paste0(tolower(phase_name_list), "_", visit_type, "_dx_flag")

  if (any(phase_name_list %in% adni_phase())) {
    phase_blscreen_dxsum <- dd %>%
      filter(COLPROT %in% phase_name_list) %>%
      filter(if_any(
        .cols = starts_with(c(blscreen_dx_flag_patterns)),
        .fns = ~ .x %in% "Yes"
      )) %>%
      select(RID, ORIGPROT, COLPROT, EXAMDATE, DIAGNOSIS) %>%
      {
        if (nrow(.) > 0) {
          assert_rows(., col_concat, is_uniq, RID, COLPROT)
        } else {
          (.)
        }
      } %>%
      rename_with(~ paste0(prefix, ".", .x), DIAGNOSIS)
  }

  if (all(phase %in% "Overall")) {
    output_dd <- overall_blscreen_dxsum
  } else {
    output_dd <- phase_blscreen_dxsum
  }

  return(output_dd)
}