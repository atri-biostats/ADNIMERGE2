# Create Mapping IDs ----
#' @title Create USUBJID from RID and SITEID
#' @description
#'  This function is used to create USUBJID ID.
#' @param .data Data.frame
#' @param .registry REGISTRY form data.frame, Default: get("REGISTRY")
#' @param .roster ROSTER form data.frame,  Default: get("REGISTRY")
#' @param .ptdemog PTDEMOG form data.frame, Default: get("PTDEMOG")
#' @param varList Additional columns, Default: get("USUBJID")
#' @return
#' A data.frame appended with all columns that specified in the \code{varList} parameter.
#' @rdname derive_usubjid
#' @importFrom dplyr left_join select
#' @importFrom assertr verify
derive_usubjid <- function(.data,
                           varList = "USUBJID",
                           .registry = get("REGISTRY"),
                           .roster = get("ROSTER"),
                           .ptdemog = get("PTDEMOG")) {
  require(dplyr)
  require(assertr)
  RID <- SITEID <- USUBJID <- NULL
  join_col <- "RID"
  check_colnames(
    .data = .data,
    col_names = join_col,
    strict = TRUE,
    stop_message = TRUE
  )
  nrow_input <- nrow(.data)
  mapping_id_list <- get_id_mapping_list(
    .registry = .registry,
    .roster = .roster,
    .ptdemog = .ptdemog
  )
  .data <- .data %>%
    assert_non_missing(all_of(join_col)) %>%
    # filter(is.na(SCREENDATE)) %>%
    left_join(
      mapping_id_list %>%
        mutate(USUBJID = create_usubjid(RID, SITEID)) %>%
        select(all_of(c(join_col, varList))),
      by = join_col
    ) %>%
    verify(nrow(.) == nrow_input) %>%
    # Will be removed after data-entry checking-in
    filter(if_all(all_of(varList), ~ !is.na(.x))) %>%
    assert_non_missing(all_of(varList))

  return(.data)
}

#' @title Get ID Mapping List
#' @param .registry REGISTRY form data.frame, Default: get("REGISTRY")
#' @param .roster ROSTER form data.frame, Default: get("ROSTER")
#' @param .ptdemog PTDEMOG form data.frame, Default: get("PTDEMOG")
#' @return
#'  A data.frame that contains \code{RID} and \code{SITEID} columns.
#' @rdname get_id_mapping_list
#' @importFrom dplyr filter group_by ungroup select bind_rows distinct
get_id_mapping_list <- function(.registry = get("REGISTRY"),
                                .roster = get("ROSTER"),
                                .ptdemog = get("PTDEMOG")) {
  require(dplyr)
  RID <- USERDATE <- SITEID <- NULL
  get_unique_list <- function(df) {
    return(
      df %>%
        group_by(RID) %>%
        filter((any(!is.na(USERDATE)) & USERDATE == min(USERDATE, na.rm = TRUE)) |
          all(is.na(USERDATE) & row_number() == 1)) %>%
        ungroup() %>%
        as_tibble() %>%
        select(RID, SITEID) %>%
        distinct() %>%
        # Will be removed after checking data entry error
        filter(!is.na(SITEID)) %>%
        assert_non_missing(all_of(c("RID", "SITEID")))
    )
  }
  .registry_data <- get_unique_list(.registry)
  .roster_data <- get_unique_list(.roster)
  .ptdemog_data <- get_unique_list(.ptdemog)

  output_data <- bind_rows(.registry_data, .roster_data, .ptdemog_data) %>%
    distinct(RID, SITEID) %>%
    assert_uniq(RID)

  return(output_data)
}

# Add Enrollment/RFSTDTC Date ----
#' @title Add Enrollment/RFSTDTC Date
#' @description
#'  This function is used to add enrollment/RFSTDTC date based on \code{RID}.
#' @param .data Data.frame
#' @param .registry REGISTRY data.frame, Default: get("REGISTRY")
#' @return A data.frame appended with \code{RFSTDTC} variable.
#' @examples
#' \dontrun{
#' add_rfstdtc_date(
#'   .data = ADNIMERGE2::DM,
#'   .registry = ADNIMERGE2::REGISTRY
#' )
#' }
#' @rdname create_rfstdtc
#' @importFrom dplyr left_join mutate select
#' @importFrom assertr verify
#' @importFrom tidyselect all_of
create_rfstdtc <- function(.data, .registry = get("REGISTRY")) {
  require(dplyr)
  require(assertr)
  require(tidyselect)
  RFSTDTC <- NULL
  join_cols <- get_cols_name(.data, c("RID", "ORIGPORT"))
  check_colnames(
    .data = .data,
    col_names = "RID",
    strict = TRUE,
    stop_message = TRUE
  )
  nrow_input <- nrow(.data)
  .data <- .data %>%
    # Add enrollment date
    left_join(
      get_adni_enrollment(.registry = .registry) %>%
        mutate(RFSTDTC = as.character(EXAMDATE)) %>%
        select(all_of(c(join_cols, "RFSTDTC"))),
      by = join_cols
    ) %>%
    verify(nrow(.) == nrow_input)

  return(.data)
}

# Add VISITNUM and VISITNAME ----
#' @title Add VISITNUM and VISIT Name ----
#' @description
#'  This function is used to add visit number (\code{VISITNUM}) and visit name (\code{VISIT})
#'  based on a pre-generated visit record dataset per subject ID.
#' @param .data Data.frame
#' @param visit_record_data
#'  A data.frame of visit record per subject ID generated based on \code{REGISTRY}
#'  and `ROSTER` forms, Default: get("ADNI_VISIT_RECORD")
#' @param domain Domain abbreviation, Default: NULL
#' @param check_missing_visnum A Boolean value to check missing \code{VISITNUM}
#' @return A data frame appended with \code{VISIT} and \code{VISITNUM}.
#' @rdname assign_visit_attr
#' @keywords adni_visit_adjust
#' @importFrom dplyr left_join select mutate across case_when
#' @importFrom assertr verify
#' @importFrom tidyselect all_of
assign_visit_attr <- function(.data,
                              visit_record_data = get("ADNI_VISIT_RECORD"),
                              domain = NULL, check_missing_visnum = TRUE) {
  require(tidyverse)
  require(assertr)
  VISDTC <- VISTAT <- NULL
  check_object_type(check_missing_visnum, "logical")
  col_names <- c("RID", "ORIGPROT", "COLPROT", "VISCODE")
  join_cols <- get_cols_name(.data, col_names)
  check_colnames(
    .data = .data,
    col_names = col_names[-2],
    strict = TRUE,
    stop_message = TRUE
  )
  add_cols <- c("VISIT", "VISITNUM", "VISDTC", "VISTAT")
  if (is.null(domain)) {
    domain <- extract_domain(.data = .data)
  }
  domain <- check_domain_abbrv(domain = domain, char_result = TRUE)
  domain_stat_col <- paste0(domain, "STAT")
  domain_dtc_col <- paste0(domain, "DTC")
  nrow_input <- nrow(.data)
  skip_domain <- "AE"
  if (domain %in% skip_domain) check_missing <- FALSE

  .data <- .data %>%
    use_dtplyr() %>%
    left_join(
      visit_record_data %>%
        select(all_of(c(join_cols, add_cols))) %>%
        mutate(
          VISDTC = create_iso8601(VISDTC, .format = "y-m-d"),
          VISTAT = as.character(VISTAT)
        ),
      by = join_cols
    ) %>%
    as_tibble() %>%
    verify(nrow(.) == nrow_input) %>%
    # Assign visit name/label for unscheduled visit code
    assign_unsch_visit_label(.data = .) %>%
    {
      if (!domain %in% skip_domain) {
        # Adjust the visit number for unscheduled visits
        adjust_vistnum_unsch(
          .data = .,
          domain = domain
        ) %>%
          # Add visit completion date
          adjust_visit_date(
            .data = .,
            dtc_col = domain_dtc_col,
            visdtc_col = "VISDTC"
          ) %>%
          # Add visit completion status
          adjust_visit_status(
            .data = .,
            domain = domain,
            vistat_col = "VISTAT"
          )
      } else {
        (.)
      }
    }

  if (check_missing_visnum) {
    .data <- .data %>%
      assert_non_missing(VISITNUM, VISIT)
  }

  return(.data)
}

#' @title Adjust Visit Date
#' @description
#'  This function is used to replace any missing eCRF/derived data specific
#'  collection/completion date with corresponding conducted visit date
#'  from REGISTRY eCRF.
#' @param .data Data.frame
#' @param dtc_col Derived data specific date column
#' @param visdtc_col Visit completion date based on \code{REGISTRY} eCRF, Default: 'VISDTC'
#' @return A data.frame with adjusted value of \code{dtc_col} column.
#' @rdname adjust_visit_date
#' @keywords adni_visit_adjust
#' @importFrom dplyr mutate across case_when pull
#' @importFrom tidyselect all_of
#' @importFrom sdtm.oak create_iso8601
adjust_visit_date <- function(.data, dtc_col, visdtc_col = "VISDTC") {
  require(tidyverse)
  require(sdtm.oak)
  check_colnames(
    .data = .data,
    col_names = c(dtc_col, visdtc_col),
    strict = TRUE,
    stop_message = TRUE
  )

  .data <- .data %>%
    # To add visit completion date from REGISTRY eCRF (VISDTC) for corresponding
    # visits in specific derived/eCRF with missing completion/collection date (DTC)
    mutate(across(all_of(dtc_col), ~ case_when(
      is.na(.x) & !is.na(get(visdtc_col)) ~ get(visdtc_col),
      TRUE ~ .x
    )))

  return(.data)
}

#' @title Adjust Visit Completion Status
#' @description
#'  This function is used to adjust visit completion status for
#'  specific derived/eCRF data based on the corresponding visit records
#'  in the REGISTRY eCRF.
#' @inheritParams assign_visit_attr
#' @param vistat_col Visit completion status based on REGISTRY records, Default: 'VISTAT'
#' @return A data.frame with adjusted visit completion status.
#' @rdname adjust_visit_status
#' @keywords adni_visit_adjust
#' @importFrom dplyr mutate if_any across case_when select
#' @importFrom tidyselect any_of all_of
#' @importFrom cli cli_abort
adjust_visit_status <- function(.data, domain = NULL, vistat_col = "VISTAT") {
  require(tidyverse)
  CHECK_COL <- NULL
  check_colnames(
    .data = .data,
    col_names = vistat_col,
    strict = TRUE,
    stop_message = TRUE
  )
  if (is.null(domain)) {
    domain <- extract_domain(.data = .data)
  }
  dstat_col <- paste0(domain, "STAT")
  dcols <- paste0(domain, c("ORRES", "STRESC", "STRESN"))
  dcols <- get_cols_name(.data, dcols)
  # If any of this columns has non missing value then,
  #    visit status will be mapped from derived dataset.
  # Otherwise, it will be from REGISTRY eCRF
  non_comp_values <- c("NOT DONE", "ND")

  if (any(!is.na(dcols))) {
    .data <- .data %>%
      mutate(CHECK_COL = if_any(any_of(dcols), ~ !is.na(.x)))
  } else {
    check_var_list <- paste0(domain, c("ORRES", "STRESC", "STRESN"))
    cli::cli_abort(message = paset0(
      "At least one of {.val {check_var_list}} variable{?s}",
      " must be found in the data."
    ))
  }

  .data <- .data %>%
    {
      if (dstat_col %in% colnames(.)) {
        mutate(., across(all_of(dstat_col), ~ case_when(
          is.na(.x) & CHECK_COL == FALSE & get(vistat_col) %in% non_comp_values ~ as.character(get(vistat_col)),
          TRUE ~ as.character(.x)
        )))
      } else {
        mutate(., across(all_of(vistat_col),
          ~ case_when(.x %in% non_comp_values & CHECK_COL == FALSE ~ as.character(.x)),
          .names = dstat_col
        ))
      }
    } %>%
    select(-CHECK_COL)
  return(.data)
}

# Assign EPOCH ----
#' @title Assign EPOCH
#' @description
#'  This function is used to set EPOCH based on the different ADNI study phase visits.
#' @param .data Data.frame
#' @param epoch_data
#'  A data.frame that generated based on ADNI study phase visits, Default: get("EPOCH_LIST_LONG")
#' @return A data.frame appended with \code{EPOCH} variable.
#' @rdname set_epoch
#' @importFrom dplyr rename mutate left_join select
#' @importFrom assertr verify
#' @importFrom tidyselect all_of
assign_epoch <- function(.data, .epoch = get("EPOCH_LIST_LONG")) {
  require(dplyr)
  require(assertr)
  require(tidyselect)
  PTTYPE <- COLPROT <- ORIGPROT <- NULL
  join_cols <- c("COLPROT", "VISCODE", "PTTYPE")
  check_colnames(
    .data = .data,
    col_names = join_cols[-3],
    strict = TRUE,
    stop_message = TRUE
  )
  nrow_input <- nrow(.data)

  check_colnames(
    .data = .epoch,
    col_names = "PHASE",
    strict = TRUE,
    stop_message = TRUE
  )
  .epoch <- .epoch %>%
    {
      if ("PHASE" %in% colnames(.)) {
        rename(., "COLPROT" = PHASE)
      } else {
        (.)
      }
    }

  .data <- .data %>%
    mutate(PTTYPE = adni_study_track(as.character(COLPROT), as.character(ORIGPROT))) %>%
    verify(all(PTTYPE %in% adni_pttype() & !is.na(PTTYPE))) %>%
    left_join(
      .epoch %>%
        select(all_of(c(join_cols, "EPOCH"))),
      by = join_cols
    ) %>%
    verify(nrow(.) == nrow_input) %>%
    select(-PTTYPE)

  return(.data)
}

# Unscheduled Visit Label -----
#' @title Assign Visit Label for Unscheduled Visit (VISCODE)
#' @description
#'  This function used to assign visit label for unscheduled visit based on ADNI study VISCODE.
#' @param .data A data.frame that contains at least \code{VISCODE} and \code{VISIT} variables.
#' @return A data.frame the same as \code{.data} with adjusted vist name/code label for unscheduled visit.
#' @rdname assign_unsch_visit_label
#' @importFrom tibble tibble
#' @importFrom dplyr left_join mutate case_when cur_column select
#' @importFrom tidyselect any_of
#' @importFrom assertr verify
assign_unsch_visit_label <- function(.data) {
  require(tidyverse)
  require(assertr)
  VISCODE <- VISIT_LABEL <- VISITNUM_LABEL <- NULL
  vistcols <- c("VISIT", "VISITNUM")
  check_colnames(
    .data = .data,
    col_names = vistcols[1],
    strict = TRUE,
    stop_message = TRUE
  )
  # Possible unscheduled visitcode
  UNSCHED_VISIT <- tibble(
    VISCODE = c("nv", "uns", "uns1", "tau"),
    VISIT_LABEL = c(rep("Unscheduled Visit", 3), "ADNI2 Tau Only Visit"),
    VISITNUM_LABEL = c(rep(999, 4))
  )
  nrow_input <- nrow(.data)
  .data <- .data %>%
    left_join(UNSCHED_VISIT,
      by = "VISCODE"
    ) %>%
    mutate(across(any_of(vistcols), ~ {
      col_label <- get(paste0(cur_column(), "_LABEL"))
      case_when(
        is.na(.x) & !is.na(col_label) ~ col_label,
        TRUE ~ .x
      )
    })) %>%
    verify(nrow(.) == nrow_input) %>%
    select(-any_of(paste0(vistcols, "_LABEL")))

  return(.data)
}

# Utils function for `adjust_vistnum_unsch`
get_closest_visitnum <- function(data_dd) {
  require(dplyr)
  require(assertr)
  result_data <- data_dd %>%
    verify(all(EXIST_UNSCH_VIST == 1)) %>%
    mutate(across(all_of(c("CUR_VISDATE", "UNSCH_VISDATE")), as.character)) %>%
    mutate(
      TIME_DIFF = as.numeric(as.Date(CUR_VISDATE) - as.Date(UNSCH_VISDATE)),
      TIME_DIFF = ifelse(VISITNUM == 999, NA_real_, TIME_DIFF)
    ) %>%
    mutate(
      NEAREST_TIME_DIFF = max(TIME_DIFF, na.rm = TRUE),
      NEAREST_VISITNUM = case_when(NEAREST_TIME_DIFF == TIME_DIFF ~ VISITNUM),
      NEAREST_VISITNUM = min(NEAREST_VISITNUM, na.rm = TRUE)
    ) %>%
    mutate(NEAREST_VISITNUM = case_when(
      VISITNUM == 999 ~ NEAREST_VISITNUM + 0.5,
      TRUE ~ VISITNUM
    )) %>%
    select(-TIME_DIFF, -NEAREST_TIME_DIFF)
  return(result_data)
}

#' @title Adjust VISITNUM of Unscheduled Visit
#' @param .data Data.frame
#' @rdname adjust_vistnum_unsch
#' @export
#' @importFrom dplyr mutate select filter group_by across ungroup select case_when
#' @importFrom tidyr nest fill unnest
#' @importFrom tidyselect all_of
#' @importFrom mirai daemons
adjust_vistnum_unsch <- function(.data, domain = NULL) {
  require(tidyverse)
  EXIST_UNSCH_VIST <- CUR_VISDATE <- NEAREST_VISITNUM <- NULL
  vistcols <- c("VISIT", "VISITNUM")
  if (!is.null(domain)) {
    domain <- extract_domain(.data)
  }
  domain_cols <- paste0(domain, c("GRPID", "CAT", "SCAT", "TESTCD", "TEST", "METHOD"))
  domain_cols <- get_cols_name(.data = .data, col_name = domain_cols)
  stdmaok_ids <- c("oak_id", "raw_source", "patient_number")
  check_colnames(
    .data = .data,
    col_names = c(vistcols, domain_cols, stdmaok_ids),
    strict = TRUE,
    stop_message = TRUE
  )
  id_cols <- c(stdmaok_ids, domain_cols)
  nrow_data <- nrow(.data)
  .data <- .data %>%
    mutate(
      EXIST_UNSCH_VIST = case_when(VISITNUM == 999 ~ TRUE),
      CUR_VISDATE = get(paste0(domain, "DTC"))
    )

  exist_unsch_vist <- .data %>%
    filter(EXIST_UNSCH_VIST == TRUE) %>%
    nrow()

  if (exist_unsch_vist > 1) exist_unsch_vist <- TRUE else exist_unsch_vist <- FALSE

  if (exist_unsch_vist) {
    get_date_value <- function(x, y) {
      x[!y %in% 999] <- NA_character_
      return(x)
    }
    grp_cols <- id_cols[!id_cols %in% c("oak_id")]

    unsch_data <- .data %>%
      use_dtplyr() %>%
      group_by(across(all_of(grp_cols))) %>%
      mutate(
        UNSCH_VISDATE = case_when(EXIST_UNSCH_VIST == TRUE ~ CUR_VISDATE),
        EXIST_UNSCH_VIST = sum(!is.na(EXIST_UNSCH_VIST))
      ) %>%
      as_tibble() %>%
      fill(UNSCH_VISDATE, .direction = "updown") %>%
      use_dtplyr() %>%
      ungroup() %>%
      filter(EXIST_UNSCH_VIST == 1) %>%
      group_by(across(all_of(grp_cols))) %>%
      nest() %>%
      ungroup() %>%
      mutate(NEAREST_VISITNUM = map(data, get_closest_visitnum)) %>%
      as_tibble() %>%
      select(-data) %>%
      unnest(NEAREST_VISITNUM)

    .data <- .data %>%
      use_dtplyr() %>%
      left_join(
        unsch_data %>%
          select(all_of(c(id_cols, "NEAREST_VISITNUM"))),
        by = id_cols
      ) %>%
      mutate(across(
        all_of(vistcols[2]),
        ~ case_when(
          !is.na(NEAREST_VISITNUM) ~ NEAREST_VISITNUM,
          TRUE ~ .x
        )
      )) %>%
      as_tibble() %>%
      verify(nrow(.) == nrow_data)
  }

  .data <- .data %>%
    select(-NEAREST_VISITNUM, -EXIST_UNSCH_VIST, -CUR_VISDATE)

  return(.data)
}

# Set --TEST ----
#' @title Set Derived Dataset Specific --TEST
#' @description
#'  This function is used to set derived dataset specific test label.
#' @param .data A data.frame
#' @param .data_list A data.frame that contains derived dataset specific test label
#' @param merge_by Merging variables
#' @return
#'  A data.frame appended with the columns that are in \code{.data_list}.
#' @rdname set_dom_test
#' @importFrom dplyr left_join
#' @importFrom assertr assert not_na
set_dom_test <- function(.data, .data_list, merge_by) {
  require(dplyr)
  require(assertr)
  domain <- substr(merge_by, start = 1, stop = 2)
  domain <- check_domain_abbrv(domain, char_result = TRUE)
  dom_test <- paste0(domain, "TEST")
  nrow_input <- nrow(.data)
  .data <- .data %>%
    left_join(.data_list, by = merge_by) %>%
    assert(not_na, all_of(dom_test)) %>%
    verify(nrow(.) == nrow_input)
  return(.data)
}

# Fuzzy Merging ------
#' @title Left Fuzzy Join
#' @description
#'  This function is used to apply left fuzzy join using date column.
#' @param data1 Master data.frame
#' @param data2 Data.frame 2 that will be merged with the master dataset
#' @param join_by Join columns
#' @param check_cols Columns that will be added/merged, useful for inner checks
#' @param main_cols Main join columns, a subset of \code{join_by}
#' @param date_col Date column names, Default = NULL
#' @param relation Relationship, either "one-to-one" or "one-to-many", Default: "one-to-one"
#' @param method Merging method, Default = "jw",
#'    see \code{\link[fuzzyjoin]{stringdist_left_join}} and \code{stringdist::stringdist-metrics}
#' @param max_dist Maximum distance for merging two datasets, Default: 1,
#'  see \code{\link[fuzzyjoin]{stringdist_left_join}}
#' @param distance_col Distance column, Default: 'DIST',
#'  see \code{\link[fuzzyjoin]{stringdist_left_join}}
#' @return A data.frame
#' @seealso
#'  \code{\link[purrr]{map_dfr}}
#'  \code{\link[fuzzyjoin]{stringdist_join}}
#' @rdname left_fuzzy_join
#' @importFrom cli cli_abort cli_alert_warning
#' @importFrom rlang arg_match0
#' @importFrom purrr map_dfr
#' @importFrom fuzzyjoin stringdist_left_join
#' @importFrom dplyr left_join filter select mutate row_number bind_rows
#' @importFrom tidyselect all_of any_of ends_with
#' @importFrom data.table as.data.table
left_fuzzy_join <- function(data1, data2, join_by, check_cols, main_cols,
                            date_col = NULL, relation = "one-to-one",
                            method = "jw", max_dist = 1, distance_col = "DIST") {
  require(tidyverse)
  require(fuzzyjoin)
  require(cli)
  TEMP_ID <- TIME_DIFF <- EXISTED <- NULL
  if (is.null(distance_col)) cli::cli_abort(message = "{.var distance_col} must not be missing")
  relation_lvls <- c("one-to-one", "one-to-many")
  rlang::arg_match0(arg = relation, values = relation_lvls)
  nrow_data1 <- nrow(data1)

  data1 <- data1 %>%
    use_dtplyr() %>%
    left_join(data2, by = join_by) %>%
    as_tibble()

  data1_mapped <- data1 %>%
    filter(if_all(all_of(check_cols), ~ !is.na(.x)))

  data1_not_mapped <- data1 %>%
    filter(if_any(all_of(check_cols), ~ is.na(.x)))

  # Fuzzy merge for records that were not mapped
  if (nrow(data1_not_mapped) > 0) {
    non_main_cols <- names(join_by)
    non_main_cols <- non_main_cols[!non_main_cols %in% main_cols]
    non_main_cols.dist <- paste0(non_main_cols, ".", distance_col)

    data1_not_mapped <- data1_not_mapped %>%
      select(-all_of(colnames(data2)[!colnames(data2) %in% join_by])) %>%
      mutate(TEMP_ID = row_number())

    data1_not_mapped_split <- split(data1_not_mapped, data1_not_mapped$TEMP_ID)
    data1_not_mapped <- purrr::map_dfr(
      data1_not_mapped_split,
      function(.x) {
        names(non_main_cols) <- join_by[names(join_by) %in% non_main_cols]
        data2_temp <- data2 %>%
          use_dtplyr() %>%
          left_join(
            .x %>%
              select(all_of(as.character(non_main_cols))) %>%
              mutate(EXISTED = TRUE),
            by = non_main_cols
          ) %>%
          filter(EXISTED == TRUE) %>%
          select(-EXISTED) %>%
          as_tibble()

        mapping_dfr <- .x %>%
          fuzzyjoin::stringdist_left_join(
            x = .,
            y = data2_temp,
            max_dist = max_dist,
            by = join_by,
            method = method,
            distance_col = distance_col
          ) %>%
          filter(if_all(any_of(non_main_cols.dist), ~ .x == 0)) %>%
          {
            if (!is.null(date_col)) {
              mutate(., TIME_DIFF = get(names(join_by[date_col])) - get(join_by[date_col])) %>%
                filter(., abs(TIME_DIFF) == min(abs(TIME_DIFF), na.rm = TRUE)) %>%
                {
                  if (nrow(.) > 1 & relation %in% relation_lvls[1]) {
                    filter(., TIME_DIFF == min(TIME_DIFF)) %>%
                      filter(., row_number() == 1)
                  } else {
                    (.)
                  }
                }
            } else {
              filter(., if_all(
                all_of(paste0(main_cols, ".", distance_col)),
                ~ .x == min(.x, na.rm = TRUE)
              ))
            }
          } %>%
          {
            if (relation %in% relation_lvls[1]) {
              verify(., nrow(.) == 1)
            } else {
              (.)
            }
          }
        return(mapping_dfr)
      }
    ) %>%
      select(-any_of(c("TEMP_ID", "TIME_DIFF")), -ends_with(distance_col))

    data1_mapped <- bind_rows(data1_mapped, data1_not_mapped)
    data1 <- data1_mapped
  }
  if (relation %in% relation_lvls[1]) {
    if (nrow(data1) != nrow_data1) {
      cli::cli_abort(message = "Set {.cls {'one-to-many'}} relationship")
    }
  } else {
    cli::cli_alert_warning(text = "Duplicated records in merged data")
  }
  return(data1)
}

# Rename Variables ----
#' @title Rename Variable Using List/Named Characters
#' @description
#' A wrapper function of \code{\link[dplyr]{renam_with}} that uses
#'  list/named character values as input argument.
#' @param .data Data.frame
#' @param name_char List/named character column names
#' @param by_name A Boolean value to rename the by list name, Default: TRUE
#' @param .strict A Boolean to apply for all columns that are listed in \code{name_char}, Default: TRUE
#' @param prefix Prefix character, Default: NULL
#' @param suffix Suffix character, Default: NULL
#' @return A data.frame with the same result of \code{\link[dplyr]{renam_with}}.
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(ADNIMERGE2)
#' name_char <- c(
#'   "Phase" = "COLPROT", "VISCODE" = "VISIT CODE",
#'   "VISNAME" = "VSIT NAME", "VISORDER" = "VISIT ORDER"
#' )
#' # When a list/character name is found in the data
#' data1 <- ADNIMERGE2::VISITS %>%
#'   rename_with_list(., name_char = name_char, by_name = FALSE, .strict = TRUE)
#' # When a list/character name is not found in the data
#' data2 <- data1 %>%
#'   rename_with_list(., name_char = name_char, by_name = TRUE, .strict = TRUE, prefix = "RENAMED_")
#' # Without strict method
#' name_char <- c(name_char, "ORIGPROT" = "FIRST PHASE")
#' data3 <- data1 %>%
#'   rename_with_list(., name_char = name_char, by_name = TRUE, .strict = FALSE, suffix = "_RENAMED")
#' }
#' @seealso
#'  \code{\link[tidyselect]{all_of}}
#'  \code{\link[dplyr]{rename}}
#' @rdname rename_with_list
#' @export
#' @importFrom tidyselect all_of any_of
#' @importFrom dplyr rename_with
#' @importFrom cli cli_abort
rename_with_list <- function(.data, name_char, by_name = TRUE, .strict = TRUE,
                             prefix = NULL, suffix = NULL) {
  require(dplyr)
  require(tidyselect)
  require(cli)
  check_object_type(by_name, "logical")
  check_object_type(.strict, "logical")
  if (is.null(suffix)) suffix <- ""
  if (is.null(prefix)) prefix <- ""
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

  if (is.null(names(name_char))) {
    cli::cli_abort(message = "{.var name_char} must be either named list or character value.")
  }

  .data <- .data %>%
    {
      if (by_name) {
        dplyr::rename_with(
          ., ~ paste0(prefix, names(name_char)[name_char == .x], suffix),
          select_of(as.character(name_char))
        )
      } else {
        dplyr::rename_with(
          ., ~ paste0(prefix, name_char[names(name_char) == .x], suffix),
          select_of(names(name_char))
        )
      }
    }

  return(.data)
}

# Use `dtplyr`/`data.table` Workflow ----
#' @title Use dtplyr workflow
#' @description
#'  This function is used to apply \code{dtplyr} workflow to make fast computation within \code{dplyr} functions.
#' @param .data Data.frame
#' @return A \code{dtplyr_step_first} class object
#' @seealso
#'  \code{\link[dtplyr]{lazy_dt}}
#' @rdname use_dtplyr
#' @importFrom data.table as.data.table
#' @importFrom dtplyr lazy_dt
use_dtplyr <- function(.data) {
  require(data.table)
  require(dtplyr)
  result_data <- .data %>%
    data.table::as.data.table() %>%
    dtplyr::lazy_dt(immutable = FALSE)
  return(result_data)
}
