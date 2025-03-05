# Function to extract baseline visit examdate -----
#' @title Function to extract enrollment date (baseline visit examdate)
#' @description
#'  This function is used to extract enrollment date (baseline visit examdate) when participant are enrolled in ADNI study for the first time.
#' @param data_registry Data.frame of REGISTRY eCRF
#' @return
#'  \itemize{
#'    \item A data frame of overall enrollment in ADNI study that will contains `RID`, `ORIGPROT`, `COLPROT`, and `EXAMDATE` variables.
#'  }
#' @examples
#' \dontrun{
#' # Overall enrollment: when participants enrolled as newly enrollee in ADNI study for the first time.
#' overall_enroll_registry <- adni_enrollment(
#'   data_registry = ADNIMERGE2::REGISTRY
#' )
#' }
#' @seealso \code{\link{extract_adni_screen_date}()}
#' @rdname adni_enrollment
#' @family adni_fun_enroll
#' @export
#' @importFrom rlang arg_match
#' @importFrom dplyr mutate across case_when filter select starts_with if_any
#' @importFrom assertr verify is_uniq
#' @importFrom magrittr %>%
adni_enrollment <- function(data_registry) {
  COLPROT <- ORIGPROT <- RID <- EXAMDATE <- PTTYPE <- NULL
  overall_baseline_flag <- NULL
  check_colnames(
    data = data_registry,
    col_names = c("RID", "ORIGPROT", "COLPROT", "VISCODE", "VISTYPE", "EXAMDATE"),
    strict = TRUE
  )
  # `VISTYPE` and `RGCONDUCT` must not contains any numeric value
  detect_numeric_value(
    value = data_registry$VISTYPE,
    num_type = "any",
    stop_message = TRUE
  )
  detect_numeric_value(
    value = data_registry$RGCONDCT,
    num_type = "any",
    stop_message = TRUE
  )

  data_registry <- data_registry %>%
    mutate(across(c(COLPROT, ORIGPROT), as.character)) %>%
    # Adding participant types (i.e. study track)
    mutate(PTTYPE = adni_study_track(cur_study_phase = COLPROT, orig_study_phase = ORIGPROT)) %>%
    # Enrollment flag
    mutate(overall_baseline_flag = case_when(
      COLPROT %in% "ADNI4" & VISCODE %in% "4_bl" &
        PTTYPE %in% "New" & VISTYPE != "Not done" ~ "Yes",
      COLPROT %in% "ADNI3" & VISCODE %in% "bl" &
        PTTYPE %in% "New" & VISTYPE != "Not done" ~ "Yes",
      COLPROT %in% "ADNI2" & VISCODE %in% "v03" &
        PTTYPE %in% "New" & VISTYPE != "Not done" ~ "Yes",
      COLPROT %in% "ADNIGO" & VISCODE %in% "bl" &
        PTTYPE %in% "New" & VISTYPE != "Not done" ~ "Yes",
      COLPROT %in% "ADNI1" & VISCODE %in% "bl" &
        PTTYPE %in% c("New", "Rollover") & RGCONDCT == "Yes" ~ "Yes"
    ))

  output_dd <- data_registry %>%
    filter(ORIGPROT == COLPROT) %>%
    filter(overall_baseline_flag %in% "Yes") %>%
    verify(PTTYPE == "New") %>%
    assert_uniq(RID) %>%
    select(RID, ORIGPROT, COLPROT, EXAMDATE)

  return(output_dd)
}

# Function to extract screening date -----
#' @title Gets Screening Date
#' @description
#'  This function is used to extract screening date of participants in ADNI study.
#' @param data_registry Data frame of REGISTRY eCRF
#' @param phase Either `Overall` or phase-specific screening date, Default: 'Overall'
#' @param both A boolean value to include both overall and phase-specific enrollment list, Default: FALSE
#' @param multiple_screen_visit A boolean value to include multiple screen visits in ADNIGO and ADNI2 phases, Default: FALSE
#' @return
#'  \itemize{
#'    \item If `both = TRUE`: a list of data frames that includes both overall (`overall_screen`) and phase-specific enrollment (`phase_screen`)
#'    \item  Otherwise a data frame corresponding to the provided input arguments:
#'         \itemize{
#'            \item Overall screen if `phase = "Overall"` and will contains one records per participant regardless the value of `multiple_screen_visit`
#'            \item Phase specific screen if `phase != "Overall"` and will contains one records per participant if `multiple_screen_visit = TRUE`.
#'  }
#'  \item The data frame will contains `RID`, `ORIGPROT`, `COLPROT`, and `SCREENDATE` variables.
#'        \itemize{
#'             \item The data frame will contains `VISCODE` for `multiple_screen_visit = TRUE`
#'  }
#'  }
#' @examples
#' \dontrun{
#' # Overall screening: when participants screened for the first time in ADNI study.
#' overall_screen_registry <- extract_adni_screen_date(
#'   data_registry = ADNIMERGE2::REGISTRY,
#'   phase = "Overall",
#'   both = FALSE,
#'   multiple_screen_visit = FALSE
#' )
#' # Phase-specific screening: when participants screened for the first time in ADNI3 study phase.
#' adni3_screen_registry <- extract_adni_screen_date(
#'   data_registry = ADNIMERGE2::REGISTRY,
#'   phase = "ADNI3",
#'   both = FALSE,
#'   multiple_screen_visit = FALSE
#' )
#' # Multiple screens visit in each ADNIGO and ADNI2 study phases.
#' adnigo2_screen_registry <- extract_adni_screen_date(
#'   data_registry = ADNIMERGE2::REGISTRY,
#'   phase = c("ADNIGO", "ADNI2"),
#'   both = FALSE,
#'   multiple_screen_visit = TRUE
#' )
#' # Screening across each ADNI phases
#' phase_screen_registry <- extract_adni_screen_date(
#'   data_registry = ADNIMERGE2::REGISTRY,
#'   phase = adni_phase(),
#'   both = FALSE,
#'   multiple_screen_visit = FALSE
#' )
#' # Overall and phase-specific screening
#' both_screen_registry <- extract_adni_screen_date(
#'   data_registry = ADNIMERGE2::REGISTRY,
#'   phase = "Overall",
#'   both = TRUE,
#'   multiple_screen_visit = FALSE
#' )
#' }
#' @seealso \code{\link{adni_enrollment}()}
#' @rdname extract_adni_screen_date
#' @family adni_fun_enroll
#' @export
#' @importFrom rlang arg_match
#' @importFrom dplyr mutate across case_when filter select starts_with if_any
#' @importFrom assertr verify assert
#' @importFrom magrittr %>%
extract_adni_screen_date <- function(data_registry, phase = "Overall", both = FALSE, multiple_screen_visit = FALSE) {
  RID <- COLPROT <- ORIGPROT <- EXAMDATE <- VISCODE <- PTTYPE <- NULL
  overall_screen_flag <- adnigo_screen_flag <- adni2_screen_flag <- second_screen_visit <- NULL
  arg_match(arg = phase, values = c("Overall", adni_phase()), multiple = TRUE)
  check_colnames(
    data = data_registry,
    col_names = c("RID", "ORIGPROT", "COLPROT", "VISCODE", "VISTYPE", "EXAMDATE"),
    strict = TRUE
  )
  if (!is.logical(both)) stop("`both` must be a boolean value")
  if (!is.logical(multiple_screen_visit)) stop("`multiple_screen_visit` must be a boolean value")

  # `VISTYPE` must not contains any numeric value
  detect_numeric_value(
    value = data_registry$VISTYPE,
    num_type = "any",
    stop_message = TRUE
  )

  data_registry <- data_registry %>%
    mutate(across(c(COLPROT, ORIGPROT), as.character)) %>%
    # Add participant types (i.e. study track)
    mutate(PTTYPE = adni_study_track(cur_study_phase = COLPROT, orig_study_phase = ORIGPROT)) %>%
    # First screening visits
    mutate(
      adni4_screen_flag = case_when(COLPROT %in% "ADNI4" & VISCODE %in% "4_sc" & PTTYPE %in% "New" & VISTYPE != "Not done" ~ "Yes"),
      adni3_screen_flag = case_when(COLPROT %in% "ADNI3" & VISCODE %in% "sc" & PTTYPE %in% "New" & VISTYPE != "Not done" ~ "Yes"),
      adni2_screen_flag = case_when(COLPROT %in% "ADNI2" & VISCODE %in% c("v01", "v02") & PTTYPE %in% "New" & VISTYPE != "Not done" ~ "Yes"),
      adnigo_screen_flag = case_when(COLPROT %in% "ADNIGO" & VISCODE %in% c("sc", "scmri") & PTTYPE %in% "New" & VISTYPE != "Not done" ~ "Yes"),
      adni1_screen_flag = case_when(COLPROT %in% "ADNI1" & VISCODE %in% c("sc", "f") ~ "Yes")
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
    mutate(overall_screen_flag = case_when(
      ORIGPROT == COLPROT & c(!is.na(adni4_screen_flag) |
        !is.na(adni3_screen_flag) |
        !is.na(adni2_screen_flag) |
        !is.na(adnigo_screen_flag) |
        !is.na(adni1_screen_flag)) ~ "Yes"
    ))

  if (both == TRUE | all(phase %in% "Overall")) {
    # Overall screening dataset
    overall_screen_registry <- data_registry %>%
      filter(overall_screen_flag %in% "Yes") %>%
      verify(ORIGPROT == COLPROT) %>%
      verify(PTTYPE == "New") %>%
      # Only first screen date, does not account for re-screen
      filter(is.na(second_screen_visit)) %>%
      assert_uniq(RID) %>%
      select(RID, ORIGPROT, COLPROT, "SCREENDATE" = EXAMDATE)
  }

  # Phase-specific screening dataset
  if (both == TRUE & all(phase %in% "Overall") == TRUE) phase <- adni_phase()
  screen_flag_patterns <- paste0(tolower(phase), "_screen_flag")
  phase_screen_registry <- data_registry %>%
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

# Function to extract baseline/screening diagnostics status ----
#' @title Gets Baseline/Screening Diagnostics Status
#' @description
#'  This function is used to extract the baseline diagnostics status when participant are enrolled in ADNI study or screening diagnostics status of those were screened for the study.
#' @param data_dxsum Data frame of DXSUM eCRF
#' @param phase Either `Overall` or phase-specific diagnostics status, Default: 'Overall'
#' @param visit_type Either `baseline` or `screen` diagnostic status, Default: 'baseline'
#' @return
#'  A data frame that contains `RID`, `RID`, `ORIGPROT`, `COLPROT`, `EXAMDATE` and either `BL.DIAGNOSIS` for baseline visit or `SC.DIAGNOSIS` for screen visit.
#' @examples
#' \dontrun{
#' # Baseline diagnostics status of newly enrolled participant in ADNI study
#' overall_baseline_dx <- extract_blscreen_dxsum(
#'   data_dxsum = ADNIMERGE2::DXSUM,
#'   phase = "Overall",
#'   visit_type = "baseline"
#' )
#' # Phase-specific baseline diagnostic status: when participants enrolled in ADNI3 study phase.
#' adni3_baseline_dx <- extract_blscreen_dxsum(
#'   data_dxsum = ADNIMERGE2::DXSUM,
#'   phase = "ADNI3",
#'   visit_type = "baseline"
#' )
#' # Screening diagnostics status: when participants screened for first time in ADNI study.
#' first_screen_dx <- extract_blscreen_dxsum(
#'   data_dxsum = ADNIMERGE2::DXSUM,
#'   phase = "Overall",
#'   visit_type = "screen"
#' )
#' # Phase-specific screening diagnostic status: when participants screened for ADNI3 study phase.
#' adni3_screen_dx <- extract_blscreen_dxsum(
#'   data_dxsum = ADNIMERGE2::DXSUM,
#'   phase = "ADNI3",
#'   visit_type = "screen"
#' )
#' }
#' @rdname extract_blscreen_dxsum
#' @importFrom rlang arg_match
#' @importFrom dplyr mutate across case_when filter select starts_with if_any
#' @importFrom assertr verify
#' @importFrom magrittr %>%
#' @family adni_fun_enroll
#' @export
extract_blscreen_dxsum <- function(data_dxsum, phase = "Overall", visit_type = "baseline") {
  RID <- COLPROT <- ORIGPROT <- EXAMDATE <- VISCODE <- DIAGNOSIS <- NULL
  overall_baseline_dx_flag <- overall_screen_dx_flag <- PTTYPE <- NULL
  arg_match(arg = phase, values = c("Overall", adni_phase()), multiple = TRUE)
  arg_match0(arg = visit_type, values = c("baseline", "screen"))
  col_name_list <- c("RID", "ORIGPROT", "COLPROT", "VISCODE", "EXAMDATE", "DIAGNOSIS")
  check_colnames(
    data = data_dxsum,
    col_names = col_name_list,
    strict = TRUE
  )

  if (visit_type %in% "baseline") prefix <- "BL" else prefix <- "SC"

  data_dxsum <- data_dxsum %>%
    mutate(across(c(COLPROT, ORIGPROT), as.character)) %>%
    # Add participant types (i.e. study track)
    mutate(PTTYPE = adni_study_track(
      cur_study_phase = COLPROT,
      orig_study_phase = ORIGPROT
    )) %>%
    # Identify baseline diagnostics status
    {
      if (visit_type %in% "baseline") {
        mutate(.,
          adni4_baseline_dx_flag = case_when(
            COLPROT %in% "ADNI4" & VISCODE %in% "4_init" &
              PTTYPE %in% "Rollover" ~ "Yes",
            COLPROT %in% "ADNI4" & VISCODE %in% "4_bl" &
              PTTYPE %in% "New" ~ "Yes"
          ),
          adni3_baseline_dx_flag = case_when(
            COLPROT %in% "ADNI3" & VISCODE %in% "init" &
              PTTYPE %in% "Rollover" ~ "Yes",
            COLPROT %in% "ADNI3" & VISCODE %in% "bl" &
              PTTYPE %in% "New" ~ "Yes"
          ),
          adni2_baseline_dx_flag = case_when(
            COLPROT %in% "ADNI2" & VISCODE %in% "v06" &
              PTTYPE %in% "Rollover" ~ "Yes",
            COLPROT %in% "ADNI2" & VISCODE %in% "v03" &
              PTTYPE %in% "New" ~ "Yes"
          ),
          adnigo_baseline_dx_flag = case_when(COLPROT %in% "ADNIGO" & VISCODE %in% "bl" &
            PTTYPE %in% c("New", "Rollover") ~ "Yes"),
          adni1_baseline_dx_flag = case_when(COLPROT %in% "ADNI1" & VISCODE %in% "bl" &
            PTTYPE %in% c("New", "Rollover") ~ "Yes")
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
          adni4_screen_dx_flag = case_when(COLPROT %in% "ADNI4" & VISCODE %in% "4_sc" & PTTYPE %in% "New" ~ "Yes"),
          adni3_screen_dx_flag = case_when(COLPROT %in% "ADNI3" & VISCODE %in% "sc" & PTTYPE %in% "New" ~ "Yes"),
          adni2_screen_dx_flag = case_when(COLPROT %in% "ADNI2" & VISCODE %in% c("v01", "v02") & PTTYPE %in% "New" ~ "Yes"),
          adnigo_screen_dx_flag = case_when(COLPROT %in% "ADNIGO" & VISCODE %in% c("sc", "scmri") & PTTYPE %in% "New" ~ "Yes"),
          adni1_screen_dx_flag = case_when(COLPROT %in% "ADNI1" & VISCODE %in% c("sc", "f") & PTTYPE %in% "New" ~ "Yes")
        ) %>%
          mutate(., overall_screen_dx_flag = case_when(ORIGPROT == COLPROT &
            c(!is.na(adni4_screen_dx_flag) |
              !is.na(adni3_screen_dx_flag) |
              !is.na(adni2_screen_dx_flag) |
              !is.na(adnigo_screen_dx_flag) |
              !is.na(adni1_screen_dx_flag)) ~ "Yes"))
      }
    }

  overall_blscreen_dxsum <- data_dxsum %>%
    {
      if (visit_type %in% "baseline") {
        filter(., overall_baseline_dx_flag %in% "Yes")
      } else {
        filter(., overall_screen_dx_flag %in% "Yes")
      }
    } %>%
    verify(ORIGPROT == COLPROT) %>%
    verify(PTTYPE == "New") %>%
    assert_uniq(RID) %>%
    select(RID, ORIGPROT, COLPROT, EXAMDATE, DIAGNOSIS) %>%
    rename_with(~ paste0(prefix, ".", .x), DIAGNOSIS)

  # Phase-specific baseline diagnostics status
  if (all(phase %in% "Overall") == TRUE) phase_name_list <- adni_phase() else phase_name_list <- phase
  blscreen_dx_flag_patterns <- paste0(tolower(phase_name_list), "_", visit_type, "_dx_flag")

  if (any(phase_name_list %in% adni_phase())) {
    phase_blscreen_dxsum <- data_dxsum %>%
      filter(COLPROT %in% phase_name_list) %>%
      filter(if_any(
        .cols = starts_with(c(blscreen_dx_flag_patterns)),
        .fns = ~ .x %in% "Yes"
      )) %>%
      select(RID, ORIGPROT, COLPROT, EXAMDATE, DIAGNOSIS) %>%
      {
        if (nrow(.) > 0) {
          assert_uniq(., RID, COLPROT)
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
#' @family adni_fun_enroll
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

#' @title Detect Closest Baseline Score
#' @description
#'  This function is used to flag the closest assessment record score to the
#'  baseline visit date (i.e enrollment date) within a certain window period.
#' @param cur_record_date Date of current assessment record collected
#' @param enroll_date Enrollment date (i.e. Baseline visit date)
#' @param time_interval Minimum window period (in days) from baseline visit date, Default: 30
#' @return
#'  A character vector with the same length as the input values `cur_record_date`.
#'  The returned vector will contains `Yes` flag for the closest record
#'  within the specified window period. Otherwise, missing value.
#' @rdname detect_baseline_score
#' @family utils_fun
detect_baseline_score <- function(cur_record_date, enroll_date, time_interval = 30) {
  time_diff <- as.numeric(as.Date(cur_record_date) - as.Date(enroll_date))
  abs_time_diff <- abs(time_diff)
  flags <- abs_time_diff < time_interval
  # Adjustment for the nearest timeline
  if (length(flags[flags == TRUE]) > 1) {
    list_closet_timeline <- min(abs_time_diff[flags == TRUE])
    flags <- abs_time_diff == list_closet_timeline
  }
  flags <- ifelse(flags == TRUE, "Yes", NA_character_)
  return(flags)
}
