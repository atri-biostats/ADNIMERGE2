# Function to extract baseline visit date/ enrollment date -----
#' @title ADNI Enrollment Date (Baseline Visit Date)
#' @description
#'  This function is used to extract enrollment date (baseline visit date)
#'  when subjects are enrolled in ADNI study for the first time.
#' @param .registry Data.frame of REGISTRY eCRF
#' @return
#'  A data frame of overall enrollment in ADNI study that will
#'  contains `RID`, `ORIGPROT`, `COLPROT`, and `EXAMDATE` variables.
#' @examples
#' \dontrun{
#' overall_enroll_registry <- get_adni_enrollment(
#'   .registry = ADNIMERGE2::REGISTRY
#' )
#' }
#' @seealso \code{\link{get_adni_screen_date}()}
#' @rdname get_adni_enrollment
#' @family ADNI enrollment
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
#' @title Gets ADNI Screening Date
#' @description
#'  This function is used to extract subject screening date in the ADNI study.
#' @param .registry Data.frame of REGISTRY eCRF
#' @param phase Either `Overall` or phase-specific screening date, Default: 'Overall'
#' @param both
#'  A Boolean value to include both overall and phase-specific enrollment list, Default: FALSE
#' @param multiple_screen_visit
#'  A Boolean value to include multiple screen visits in ADNIGO and ADNI2 phases, Default: FALSE
#' @return
#' \itemize{
#' \item If `both = TRUE`: a list of data frames that includes both overall (`overall_screen`) and phase-specific enrollment (`phase_screen`)
#' \item Otherwise a data frame corresponding to the provided input arguments:
#'  \itemize{
#'    \item Overall screen if `phase = "Overall"` and will contains one records per subject regardless the value of `multiple_screen_visit`
#'    \item Phase specific screen if `phase != "Overall"` and will contains one records per subject if `multiple_screen_visit = FALSE`.
#' }
#' \item The data frame will contains `RID`, `ORIGPROT`, `COLPROT`, and `SCREENDATE` variables. The data frame will contains `VISCODE` for `multiple_screen_visit = TRUE`
#' }
#' @examples
#' \dontrun{
#' # Overall screening: when subject screened for the first time in ADNI study.
#' overall_screen_registry <- get_adni_screen_date(
#'   .registry = ADNIMERGE2::REGISTRY,
#'   phase = "Overall",
#'   both = FALSE,
#'   multiple_screen_visit = FALSE
#' )
#' # Phase-specific screening: when subject screened for the first time in ADNI3 study phase.
#' adni3_screen_registry <- get_adni_screen_date(
#'   .registry = ADNIMERGE2::REGISTRY,
#'   phase = "ADNI3",
#'   both = FALSE,
#'   multiple_screen_visit = FALSE
#' )
#' # Multiple screen visits in each ADNIGO and ADNI2 study phases.
#' adnigo2_screen_registry <- get_adni_screen_date(
#'   .registry = ADNIMERGE2::REGISTRY,
#'   phase = c("ADNIGO", "ADNI2"),
#'   both = FALSE,
#'   multiple_screen_visit = TRUE
#' )
#' # Screening across each ADNI phases
#' phase_screen_registry <- get_adni_screen_date(
#'   .registry = ADNIMERGE2::REGISTRY,
#'   phase = ADNIMERGE2::adni_phase(),
#'   both = FALSE,
#'   multiple_screen_visit = FALSE
#' )
#' # Overall and phase-specific screening
#' both_screen_registry <- get_adni_screen_date(
#'   .registry = ADNIMERGE2::REGISTRY,
#'   phase = "Overall",
#'   both = TRUE,
#'   multiple_screen_visit = FALSE
#' )
#' }
#' @seealso \code{\link{get_adni_enrollment}()}
#' @rdname get_adni_screen_date
#' @family ADNI screening
#' @keywords adni_enroll_fun
#' @importFrom rlang arg_match
#' @importFrom dplyr mutate across case_when filter select starts_with if_any
#' @importFrom assertr verify assert
#' @importFrom magrittr %>%
#' @export
get_adni_screen_date <- function(.registry, phase = "Overall", both = FALSE, multiple_screen_visit = FALSE) {
  RID <- COLPROT <- ORIGPROT <- EXAMDATE <- VISCODE <- PTTYPE <- NULL
  overall_screen_flag <- adni4_screen_flag <- adni3_screen_flag <- NULL
  adni2_screen_flag <- adnigo_screen_flag <- adni1_screen_flag <- second_screen_visit <- NULL
  rlang::arg_match(arg = phase, values = c("Overall", adni_phase()), multiple = TRUE)
  check_colnames(
    .data = .registry,
    col_names = c("RID", "ORIGPROT", "COLPROT", "VISCODE", "VISTYPE", "EXAMDATE"),
    strict = TRUE
  )
  check_is_logical(both)
  check_is_logical(multiple_screen_visit)

  detect_numeric_value(
    value = .registry$VISTYPE,
    num_type = "any",
    stop_message = TRUE
  )

  .registry <- .registry %>%
    # Create study track
    mutate(PTTYPE = adni_study_track(cur_study_phase = COLPROT, orig_study_phase = ORIGPROT)) %>%
    # First screening visits
    mutate(
      adni4_screen_flag = case_when(COLPROT %in% adni_phase()[5] & VISCODE %in% "4_sc" & PTTYPE %in% adni_pttype()[2] & VISTYPE != "Not done" ~ "Yes"),
      adni3_screen_flag = case_when(COLPROT %in% adni_phase()[4] & VISCODE %in% "sc" & PTTYPE %in% adni_pttype()[2] & VISTYPE != "Not done" ~ "Yes"),
      adni2_screen_flag = case_when(COLPROT %in% adni_phase()[3] & VISCODE %in% c("v01", "v02") & PTTYPE %in% adni_pttype()[2] & VISTYPE != "Not done" ~ "Yes"),
      adnigo_screen_flag = case_when(COLPROT %in% adni_phase()[2] & VISCODE %in% c("sc", "scmri") & PTTYPE %in% adni_pttype()[2] & VISTYPE != "Not done" ~ "Yes"),
      adni1_screen_flag = case_when(COLPROT %in% adni_phase()[1] & VISCODE %in% c("sc", "f") ~ "Yes")
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
      ORIGPROT == COLPROT &
        (!is.na(adni4_screen_flag) |
          !is.na(adni3_screen_flag) |
          !is.na(adni2_screen_flag) |
          !is.na(adnigo_screen_flag) |
          !is.na(adni1_screen_flag)) ~ "Yes"
    ))

  if (both == TRUE | all(phase %in% "Overall")) {
    # Overall screening dataset
    overall_screen_registry <- .registry %>%
      filter(overall_screen_flag %in% "Yes") %>%
      verify(ORIGPROT == COLPROT) %>%
      verify(all(PTTYPE == adni_pttype()[2])) %>%
      # Only first screen date, does not account for re-screen
      filter(is.na(second_screen_visit)) %>%
      assert_uniq(RID) %>%
      select(RID, ORIGPROT, COLPROT, "SCREENDATE" = EXAMDATE)
  }

  # Phase-specific screening dataset
  if (both == TRUE & all(phase %in% "Overall") == TRUE) phase <- adni_phase()
  screen_flag_patterns <- paste0(tolower(phase), "_screen_flag")
  phase_screen_registry <- .registry %>%
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
    if (all(phase %in% "Overall")) {
      output_dd <- overall_screen_registry
    } else {
      output_dd <- phase_screen_registry
    }
  }

  return(output_dd)
}

# Function to extract baseline/screening diagnostics status ----
#' @title Gets ADNI Baseline/Screening Diagnostics Summary
#' @description
#'  This function is used to extract the baseline diagnostics status
#'  when subjects are enrolled in the ADNI study. Also screening diagnostics
#'  status of those who were screened for the study.
#' @param .dxsum Data.frame of DXSUM eCRF
#' @param phase
#'  Either `Overall` or phase-specific diagnostics status, Default: 'Overall'
#' @param visit_type
#'  Either `baseline` or `screen` diagnostic status, Default: 'baseline'
#' @return
#'  A data frame that contains `RID`, `ORIGPROT`, `COLPROT`, `EXAMDATE`
#'  and either `BL.DIAGNOSIS` for baseline visit or `SC.DIAGNOSIS` for screen visit.
#' @examples
#' \dontrun{
#' # Baseline diagnostics status of newly enrolled subject in ADNI study
#' overall_baseline_dx <- get_adni_blscreen_dxsum(
#'   .dxsum = ADNIMERGE2::DXSUM,
#'   phase = "Overall",
#'   visit_type = "baseline"
#' )
#' # Phase-specific baseline diagnostic status:
#' # when subject enrolled in ADNI3 study phase.
#' adni3_baseline_dx <- get_adni_blscreen_dxsum(
#'   .dxsum = ADNIMERGE2::DXSUM,
#'   phase = "ADNI3",
#'   visit_type = "baseline"
#' )
#' # Screening diagnostics status:
#' # when subject screened for first time in ADNI study.
#' first_screen_dx <- get_adni_blscreen_dxsum(
#'   .dxsum = ADNIMERGE2::DXSUM,
#'   phase = "Overall",
#'   visit_type = "screen"
#' )
#' # Phase-specific screening diagnostic status:
#' # when subject screened for ADNI3 study phase.
#' adni3_screen_dx <- get_adni_blscreen_dxsum(
#'   .dxsum = ADNIMERGE2::DXSUM,
#'   phase = "ADNI3",
#'   visit_type = "screen"
#' )
#' }
#' @rdname get_adni_blscreen_dxsum
#' @family ADNI enrollment
#' @keywords adni_enroll_fun
#' @importFrom rlang arg_match
#' @importFrom dplyr mutate across case_when filter select starts_with if_any
#' @importFrom assertr verify
#' @importFrom magrittr %>%
#' @export
get_adni_blscreen_dxsum <- function(.dxsum, phase = "Overall", visit_type = "baseline") {
  RID <- COLPROT <- ORIGPROT <- EXAMDATE <- VISCODE <- DIAGNOSIS <- NULL
  overall_baseline_dx_flag <- overall_screen_dx_flag <- PTTYPE <- NULL
  adni4_screen_dx_flag <- adni3_screen_dx_flag <- adni2_screen_dx_flag <- adnigo_screen_dx_flag <- adni1_screen_dx_flag <- NULL
  rlang::arg_match(arg = phase, values = c("Overall", adni_phase()), multiple = TRUE)
  rlang::arg_match0(arg = visit_type, values = c("baseline", "screen"))
  col_name_list <- c("RID", "ORIGPROT", "COLPROT", "VISCODE", "EXAMDATE", "DIAGNOSIS")
  check_colnames(
    .data = .dxsum,
    col_names = col_name_list,
    strict = TRUE
  )

  if (visit_type %in% "baseline") prefix <- "BL" else prefix <- "SC"

  .dxsum <- .dxsum %>%
    # Create study track
    mutate(PTTYPE = adni_study_track(COLPROT, ORIGPROT)) %>%
    # To flag baseline diagnostics status
    {
      if (visit_type %in% "baseline") {
        mutate(.,
          adni4_baseline_dx_flag = case_when(
            COLPROT %in% adni_phase()[5] & VISCODE %in% "4_init" & PTTYPE %in% adni_pttype()[1] ~ "Yes",
            COLPROT %in% adni_phase()[5] & VISCODE %in% "4_bl" & PTTYPE %in% adni_pttype()[2] ~ "Yes"
          ),
          adni3_baseline_dx_flag = case_when(
            COLPROT %in% adni_phase()[4] & VISCODE %in% "init" & PTTYPE %in% adni_pttype()[1] ~ "Yes",
            COLPROT %in% adni_phase()[4] & VISCODE %in% "bl" & PTTYPE %in% adni_pttype()[2] ~ "Yes"
          ),
          adni2_baseline_dx_flag = case_when(
            COLPROT %in% adni_phase()[3] & VISCODE %in% "v06" & PTTYPE %in% adni_pttype()[1] ~ "Yes",
            COLPROT %in% adni_phase()[3] & VISCODE %in% "v03" & PTTYPE %in% adni_pttype()[2] ~ "Yes"
          ),
          adnigo_baseline_dx_flag = case_when(COLPROT %in% adni_phase()[2] & VISCODE %in% "bl" & PTTYPE %in% adni_pttype() ~ "Yes"),
          adni1_baseline_dx_flag = case_when(COLPROT %in% adni_phase()[1] & VISCODE %in% "bl" & PTTYPE %in% adni_pttype() ~ "Yes")
        ) %>%
          mutate(., overall_baseline_dx_flag = case_when(
            ORIGPROT == COLPROT &
              (!is.na(adni4_baseline_dx_flag) |
                !is.na(adni3_baseline_dx_flag) |
                !is.na(adni2_baseline_dx_flag) |
                !is.na(adnigo_baseline_dx_flag) |
                !is.na(adni1_baseline_dx_flag)) ~ "Yes"
          ))
      } else {
        mutate(.,
          adni4_screen_dx_flag = case_when(COLPROT %in% adni_phase()[5] & VISCODE %in% "4_sc" & PTTYPE %in% adni_pttype()[2] ~ "Yes"),
          adni3_screen_dx_flag = case_when(COLPROT %in% adni_phase()[4] & VISCODE %in% "sc" & PTTYPE %in% adni_pttype()[2] ~ "Yes"),
          adni2_screen_dx_flag = case_when(COLPROT %in% adni_phase()[3] & VISCODE %in% c("v01", "v02") & PTTYPE %in% adni_pttype()[2] ~ "Yes"),
          adnigo_screen_dx_flag = case_when(COLPROT %in% adni_phase()[2] & VISCODE %in% c("sc", "scmri") & PTTYPE %in% adni_pttype()[2] ~ "Yes"),
          adni1_screen_dx_flag = case_when(COLPROT %in% adni_phase()[1] & VISCODE %in% c("sc", "f") & PTTYPE %in% adni_pttype()[2] ~ "Yes")
        ) %>%
          mutate(., overall_screen_dx_flag = case_when(ORIGPROT == COLPROT &
            (!is.na(adni4_screen_dx_flag) |
              !is.na(adni3_screen_dx_flag) |
              !is.na(adni2_screen_dx_flag) |
              !is.na(adnigo_screen_dx_flag) |
              !is.na(adni1_screen_dx_flag)) ~ "Yes"))
      }
    }

  overall_blscreen_dxsum <- .dxsum %>%
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
    phase_blscreen_dxsum <- .dxsum %>%
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

# Get Death Flag -----
#' @title Death Flag
#' @description This function is used to extract death records in the study.
#'   Based on the adverse events record (i.e. in `ADVERSE` for ADNI3-4 and
#'   `RECADV` in ADNI1-GO-2) and study sum record (i.e. in `STUDSUM` for ADNI3-4).
#' @param .adverse
#'  Adverse events record data.frame for ADNI3-4 study phase, similar to `ADVERSE`
#' @param .recadv
#'  Adverse events record data frame for ADNI1-GO-2, similar to `RECADV`
#' @param .studysum
#'  Final dispositions (based on `STUDYSUM` form) data frame for ADNI3-4, similar to `STUDYSUM`
#' @return A data frame with the following columns:
#' \itemize{
#'  \item RID: Subject ID
#'  \item ORIGPROT: Original study protocols
#'  \item COLPROT: Current study protocols which the event was recorded
#'  \item DTHDTC: Death date
#'  \item DTHFL: Death flag, `Yes`
#' }
#' @examples
#' \dontrun{
#' get_death_flag(
#'   .studysum = ADNIMERGE2::STUDYSUM,
#'   .adverse = ADNIMERGE2::ADVERSE,
#'   .recadv = ADNIMERGE2::RECADV
#' )
#' }
#' @rdname get_death_flag
#' @family ADNI flag
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
#' @description This function is used to get study early discontinuation list.
#'   Based on the `REGISTRY` eCRF for ADNI1-GO-2 and `STUDYSUM` eCRF for ADNI3-4.
#' @param .registry Registry record data.frame for ADNI1-GO-2, similar to `REGISTRY`
#' @param .studysum Adverse events record data frame for ADNI1-GO-2, similar to `STUDYSUM`
#' @return A data frame with the following columns:
#' \itemize{
#'  \item RID: Subject ID
#'  \item ORIGPROT: Original study protocols
#'  \item COLPROT: Current study protocols which the event was recorded
#'  \item SDSTATUS: Disposition status
#'  \item SDDATE: Dispostion date
#' }
#' @examples
#' \dontrun{
#' get_disposition_flag(
#'   .registry = ADNIMERGE2::REGISTRY,
#'   .studysum = ADNIMERGE2::STUDYSUM
#' )
#' }
#' @rdname get_disposition_flag
#' @family ADNI flag
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

# Detect Closest Baseline Score -----
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
#' @family ADNI enrollment
#' @keywords utils_fun
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
