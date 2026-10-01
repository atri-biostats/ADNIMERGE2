#' @title Prepare PACC score input raw datasets
#'
#' @param data_source Input data source
#'
#'  \code{ADNIMERGE2}: if the input raw datasets are obtained from `ADNIMERGE2` R data package. This is the default setting.
#'
#'  \code{EXTERNAL}: if input raw datasets are obtained from a different data source other than the `ADNIMERGE2` R data package.
#'    However, this options required to provide three dataset(s) as input. Please see \code{external_data_list} argument.
#'
#' @param external_data_list A named list object of data.frame. Only applicable if \code{data_source = 'EXTERNAL'}
#'     If \code{data_source} is \code{'EXTERNAL'}, then \code{external_data_list} must include `ADAS`, `MMSE` and `NEUROBAT` data.
#'
#' @return A named list of three data.frames:
#' \itemize{
#'   \item PACC_ADAS_LONG: Question 4 (Q4) task subscore from ADAS-Cognitive Behavior assessment (`ADAS`)
#'   \item PACC_MMSE_LONG: Mini-Mental State Exam score (`MMSE`)
#'   \item PACC_NEUROBAT_LONG: Digit Symbol Substitution Test score, Logical Memory IIa Delayed Recall score and Trails B score from Neuropsychological Battery assessment (`NEUROBAT`)
#' }
#'
#' Each datasets will contains the following two columns in addition to `ORIGPROT`, `COLPROT`, `PTID`, `RID`, `VISCODE` and `VISDATE.
#' \itemize{
#'   \item SCORE_SOURCE: PACC score component name
#'   \item SCORE: Raw component score
#' }
#'
#' @details
#' This function is used to prepare `PACC` score component input dataset either
#' from `ADNIMERGE2` R package or external data source.
#'
#' **NOTE:** It is still required to install `ADNIMERGE2` R data package to
#' perform internal validation regarding data format.
#'
#' @examples
#' \dontrun{
#' # To use datasets from ADNIMERGE2 R data package
#' pacc_input_data <- prepare_pacc_input_data(
#'   data_source = "ADNIMERGE2",
#'   external_data_list = list()
#' )
#' names(pacc_input_data)
#' lapply(pacc_input_data, colnames)
#'
#' # To use external data source
#' # Suppose all the three required datasets are obtained from an external source
#' pacc_input_data1 <- prepare_pacc_input_data(
#'   data_source = "EXTERNAL",
#'   external_data_list = list(
#'     "ADAS" = ADNIMERGE2::ADAS,
#'     "MMSE" = ADNIMERGE2::MMSE,
#'     "NEUROBAT" = ADNIMERGE2::NEUROBAT
#'   )
#' )
#' names(pacc_input_data1)
#' lapply(pacc_input_data1, colnames)
#' }
#' @rdname prepare_pacc_input_data
#' @importFrom cli cli_abort
#' @importFrom dplyr mutate select
#' @importFrom tidyr pivot_longer
#' @importFrom purrr map pluck
#' @keywords internal
#' @family PACC score related

prepare_pacc_input_data <- function(
  data_source = "ADNIMERGE2",
  external_data_list = list()
) {
  .data <- NULL
  rlang::arg_match0(arg = data_source, values = c("ADNIMERGE2", "EXTERNAL"))
  pkg_name <- "ADNIMERGE2"
  if (!requireNamespace(pkg_name, quietly = TRUE)) {
    cli::cli_abort(
      message = "The package {.val {pkg_name}} is required but not installed."
    )
  } else {
    ns_envir <- loadNamespace(pkg_name)
  }
  if (data_source == "ADNIMERGE2") {
    DATA_LIST <- list(
      "ADAS" = get(x = "ADAS", envir = ns_envir),
      "MMSE" = get(x = "MMSE", envir = ns_envir),
      "NEUROBAT" = get(x = "NEUROBAT", envir = ns_envir)
    )
  }
  if (data_source == "EXTERNAL") {
    check_list_names(
      x = external_data_list,
      list_names = c("ADAS", "MMSE", "NEUROBAT")
    )
    DATA_LIST <- external_data_list
  }
  DATA_LIST <- purrr::map(DATA_LIST, prepare_common_data_format)
  last_cols <- c(
    "SCORE_SOURCE", "ORIGPROT", "COLPROT", "PTID",
    "RID", "VISCODE", "VISCODE2", "VISDATE", "SCORE"
  )
  # ADAS scores ----
  PACC_ADAS_LONG <- DATA_LIST %>%
    purrr::pluck("ADAS") %>%
    mutate(
      SCORE = .data$Q4SCORE,
      SCORE_SOURCE = "ADASQ4SCORE"
    ) %>%
    select(all_of(last_cols))

  # MMSE scores ----
  PACC_MMSE_LONG <- DATA_LIST %>%
    purrr::pluck("MMSE") %>%
    mutate(
      SCORE = .data$MMSCORE,
      SCORE_SOURCE = "MMSE"
    ) %>%
    select(all_of(last_cols))

  # NEUROBAT -----
  # Includes: Trails B Score: `TRABSCOR`
  #           Logical Memory IIa Delayed Recall Score: `LDELTOTL`
  #           Digit Symbol Substitution Test Score: `DIGITSCR`
  PACC_NEUROBAT_LONG <- DATA_LIST %>%
    purrr::pluck("NEUROBAT") %>%
    mutate(
      LDELTOTL = .data$LDELTOTAL,
      DIGITSCR = .data$DIGITSCOR,
      TRABSCOR = .data$TRABSCOR
    ) %>%
    pivot_longer(
      cols = all_of(c("LDELTOTL", "DIGITSCR", "TRABSCOR")),
      values_to = "SCORE",
      names_to = "SCORE_SOURCE"
    ) %>%
    select(all_of(last_cols))

  output <- list(
    "PACC_ADAS_LONG" = PACC_ADAS_LONG,
    "PACC_MMSE_LONG" = PACC_MMSE_LONG,
    "PACC_NEUROBAT_LONG" = PACC_NEUROBAT_LONG
  )
  return(output)
}

#' @title prepare_common_data_format
#' @description
#' Function to create a common data format for PACC component input raw dataset, and
#' perform some internal validation checks. Please see details section for more.
#'
#' @param .data A data.frame
#'
#' @return A data.frame similar to `.data` input where every columns are renamed
#'      with uppercase, and all variables are converted into character object.
#'
#' @details
#' The function can be used to:
#'  \itemize{
#'   \item Convert all columns into character object
#'   \item Rename all columns in uppercase format
#'   \item Verify if records across `COLPROT`, `RID` and `VISCODE` variables are unique
#'   \item Harmonize the screening visit code in `ADNI1` study phase, please see `convert_f_viscode_to_sc()`.
#'   \item Verify `COLPROT`,`PTID`, `RID`, and `VISCODE` variables contain only none missing values
#'   \item Exclude any records that are collected in TEAM-ADNI study phase
#' }
#'
#' @examples
#' \dontrun{
#' prepare_common_data_format(.data = ADNIMERGE2::ADAS)
#' }
#' @rdname prepare_common_data_format
#' @keywords internal
#' @family PACC score related

prepare_common_data_format <- function(.data) {
  id_cols <- c("COLPROT", "RID", "VISCODE")
  not_na_cols <- c("PTID", "VISCODE")
  .data <- .data %>%
    convert_f_viscode_to_sc(
      .data = .,
      code_var = c("VISCODE", "VISCODE2")
    ) %>%
    set_as_tibble() %>%
    assert_uniq(all_of(id_cols)) %>%
    assert_non_missing(all_of(id_cols)) %>%
    assert_non_missing(all_of(not_na_cols))

  .data <- filter_out_teamadni(.data = .data, "COLPROT")

  return(.data)
}
