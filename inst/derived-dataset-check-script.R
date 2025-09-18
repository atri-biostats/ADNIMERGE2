#' @title Check Derived Dataset
#' @description
#'  This function is used to apply pre-specified check list with the
#'  derived dataset using `sdtmchecks` and `testthat` R package.
#' @param pkg Data package name, Default: "ADNIMERGE2"
#' @param dataset_name Character vector of derived dataset name
#' @param export_report A Boolean value to export report, Default: TRUE
#' @param output_dir Output directory if `export_report = TRUE`, Default: NULL
#' @param output_file Output file name if `export_report = TRUE`, Default: "check_report.xlsx"
#' @param apply_testthat A Boolean value to execute package test result, Default: FALSE
#' @param ncores Number of cores for parallel processing, Default: 6
#' @return `TRUE` if everything execute accordingly
#' @examples
#' \dontrun{
#' run_check_derived_dataset(
#'   pkg = "ADNIMERGE2",
#'   dataset_name = c("DM", "AE", "QS", "RS", "LB", "VS"),
#'   export_report = FALSE,
#'   output_dir = NULL,
#'   apply_testthat = TRUE,
#'   ncores = 6
#' )
#' run_check_derived_dataset(
#'   export_report = TRUE,
#'   output_dir = "./inst",
#'   output_file = "check_report.xlsx",
#'   apply_testthat = FALSE
#' )
#' }
#' @rdname run_check_derived_dataset
#' @importFrom sdtmchecks sdtmchecksmeta report_to_xlsx
#' @importFrom cli cli_abort
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr filter
#' @importFrom testthat expect_identical
run_check_derived_dataset <- function(pkg = "ADNIMERGE2",
                                      dataset_name = NULL,
                                      export_report = TRUE,
                                      output_dir = NULL,
                                      output_file = "check_report.xlsx",
                                      apply_testthat = FALSE,
                                      ncores = 6) {
  # Load required packages
  required_pkg <- c("tidyverse", "sdtmchecks", "testthat","cli", pkg)
  sapply(required_pkg[!required_pkg %in% "testthat"], require, character = TRUE)
  check_object_type(export_report, "logical")
  check_object_type(apply_testthat, "logical")
  # List of selected check list from sdtmchecksmeta as default values
  # Check list in DM
  dm_check_list <- c(
    "check_dm_age_missing", "check_dm_dthfl_dthdtc",
    "check_dm_usubjid_dup", "check_vs_height"
  )
  # Check list in AE
  ae_check_list <- c(
    "check_ae_aedecod", "check_ae_aedthdtc_aesdth", "check_ae_aeout",
    "check_ae_aesdth_aedthdtc", "check_ae_aestdtc_after_aeendtc",
    "check_ae_dup", "check_ae_fatal"
  )
  # Check list in QS
  qs_check_list <- c(
    "check_qs_dup", "check_qs_qsdtc_visit_ordinal_error",
    "check_qs_qsstat_qsreasnd", "check_qs_qsstat_qsstresc"
  )
  # Check list in RS
  rs_check_list <- c(
    "check_rs_rsdtc_across_visit",
    "check_rs_rsdtc_visit_ordinal_error"
  )
  # Check list in LB
  lb_check_list <- c("check_lb_lbdtc_visit_ordinal_error")
  # Check list in VS
  vs_check_list <- c("check_vs_sbp_lt_dbp", "check_vs_height")
  # Combined pre-specified check list
  combined_check_list <- c(
    dm_check_list, ae_check_list, qs_check_list, rs_check_list,
    lb_check_list, vs_check_list
  )

  if (is.null(dataset_name)) dataset_name <- c("DM", "AE", "QS", "RS", "LB", "VS")
  # Required to load dataset in the environment with lower case name format
  convert_lower_case <- lapply(dataset_name, function(i) {
    assign(tolower(i), get(i))
  })
  names(convert_lower_case) <- tolower(dataset_name)

  temp_report <- sdtmchecks::run_all_checks(
    metads = sdtmchecks::sdtmchecksmeta %>%
      filter(check %in% combined_check_list),
    priority = c("High", "Medium", "Low"),
    verbose = TRUE,
    ncores = ncores
  )

  if (export_report) {
    if (is.null(output_dir)) {
      cli_abort(message = "{.path output_dir} must not be missing.")
    }
    if (!grepl(pattern = "\\.xlsx$", x = output_file)) {
      cli_abort(message = "{.file output_file} must be an excel file with {val. .xlsx} name extension.")
    }
    # To store any identified issues/flag into local directory "./inst"
    sdtmchecks::report_to_xlsx(
      res = temp_report,
      outfile = file.path(output_dir, output_file),
      extrastring = ""
    )
  }

  num_records <- lapply(temp_report, pluck, "nrec") %>%
    bind_cols() %>%
    pivot_longer(
      cols = everything(),
      names_to = "check",
      values_to = "nrec"
    ) %>%
    as_tibble()

  expected_result <- tibble(
    check = names(temp_report),
    nrec = 0
  )

  if (apply_testthat) {
    sapply(required_pkg[required_pkg %in% "testthat"], require, character = TRUE)
    testthat::expect_identical(
      object = num_records,
      expected = expected_result,
      info = "Check derived dataset"
    )
  }
  return(TRUE)
}
