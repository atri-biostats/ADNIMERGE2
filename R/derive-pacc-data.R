#' @title Derive PACC scores data
#'
#' @description
#'  This is a wrapper function to generate PACC scores data based on
#'  [ADNIMERGE2-PACC](https://atri-biostats.github.io/ADNIMERGE2/articles/ADNIMERGE2-PACC.html)
#'  articles.
#'
#' @param DATA_SOURCE A character vector of PACC input data source.
#'  Either \code{ADNIMERGE2} or \code{EXTERNAL} data source.
#'
#'  \code{ADNIMERGE2}: To use raw datasets from currently installed [ADNIMERGE2] R data package
#'
#'  \code{EXTERNAL}: To use an external data source.
#'
#' @param DATA_LIST A named list object. By default, it is a null list object.
#'  The default value is only applicable if the data source is \code{"ADNIMERGE2"}.
#'  If an external data source (i.e., \code{DATA_SOURCE = "EXTERNAL"}) is used,
#'  then \code{DATA_LIST} must be a named list object that contains the following dataset.
#'  \itemize{
#'    \item [ADAS]: ADAS-Cognitive Behavior Data
#'    \item [MMSE]: Mini-Mental State Exam Data
#'    \item [NEUROBAT]: Neuropsychological Battery Data
#'    \item [REGISTRY]: Registry Data
#'    \item [DXSUM]: Diagnostic Summary Data
#'  }
#'
#' **NOTE:** We suggest to make sure all the above datasets were prepared with
#'  similar data preparation workflow as in [ADNIMERGE2] that includes
#'  decode coded variables, convert missing values as \code{'NA'} and
#'  add common study protocol identifier variables (i.e., \code{ORIGPROT} and \code{COLPROT}).
#'  Please see [./data-raw/data-prep.R](https://github.com/atri-biostats/ADNIMERGE2/blob/main/data-raw/data-prep.R)
#'  and [./data-raw/ddata-prep-recode.R](https://github.com/atri-biostats/ADNIMERGE2/blob/main/data-raw/data-prep-recode.R)
#'  for more information regarding ADNIMERGE2 data preparation workflow.
#'
#' @param DATA_SOURCE_DATE Data source date. It must be a character/date object.
#'   By default, it is null if the data source is \code{'ADNIMERGE2'}
#'   (i.e., \code{DATA_SOURCE = "ADNIMERGE2"}) as the data download date from
#'   [ADNIMERGE2] R data package is used.
#'
#' @param VIGNETTE_PATH File path of [PACC] score vignettes.
#'   By default, \code{ADNIMERGE2-PACC} vignettes from [ADNIMERGE2] R package is used.
#'
#'   Please see \code{vignette(topic = "ADNIMERGE2-PACC", package = "ADNIMERGE2")} for more information.
#'
#'   **NOTE:** The remote GitHub version
#'   [ADNIMERGE2-PACC.Rmd](https://github.com/atri-biostats/ADNIMERGE2/blob/main/vignettes/ADNIMERGE2-PACC.Rmd)
#'   vignette script can be also used. Please see examples below.
#'
#' @param SHOW_PACC_DATADICT A Boolean value to show a data dictionary for PACC scores data.
#'  By default, a data dictionary dataset is returned along with the actual PACC scores data.
#'
#' @param ENVIR Render environment in which the code chunks are to be evaluated during knitting.
#'  However, the code chunks are evaluated in new environment by default.
#'
#' @param QUIET Inherited from \code{\link{rmarkdown}{render}}. By defualt, it is \code{TRUE}.
#'
#' @param ... Additional arguments that pass through \code{\link{rmarkdown}{render}}
#'
#' @return Either a listed data.frame or single data.frame.
#' \itemize{
#'   \item A listed data.frame of the actual PACC scores data ([PACC]) and corresponding data dictionary data (i.e., \code{'PACC_DATADICT'}) if \code{SHOW_PACC_DATADICT} is \code{TRUE}.
#'   \item A single data.frame that only includes PACC scores data if \code{SHOW_PACC_DATADICT} is \code{FALSE}..
#' }
#'
#' @examples
#' \dontrun{
#' # Generate PACC scores data based on currently installed ADNIMERGE2 data package
#'
#' pacc_data_list <- derive_pacc_data(
#'   DATA_SOURCE = "ADNIMERGE2",
#'   DATA_LIST = list(),
#'   DATA_SOURCE_DATE = NULL,
#'   VIGNETTE_PATH = system.file("doc/ADNIMERGE2-PACC.Rmd", package = "ADNIMERGE2"),
#'   SHOW_PACC_DATADICT = TRUE,
#'   ENVIR = new.env()
#' )
#' names(pacc_data_list)
#' pacc_data <- pacc_data_list$PACC
#' pacc_data_dict <- pacc_data_list$PACC_DATADICT
#'
#' # To return only PACC data and evaluated all code chunks in global environment
#' pacc_data1 <- derive_pacc_data(
#'   DATA_SOURCE = "ADNIMERGE2",
#'   DATA_SOURCE_DATE = NULL,
#'   VIGNETTE_PATH = system.file("doc/ADNIMERGE2-PACC.Rmd", package = "ADNIMERGE2"),
#'   SHOW_PACC_DATADICT = FALSE,
#'   ENVIR = globalenv()
#' )
#'
#' # Supposes the following datatsets were generated outside ADNIMERGE2 R package
#' # and the data preparation workflow follows similar procedures as
#' # ADNIMERGE2 data package build workflow.
#' EXTERNAL_DATA_LIST <- list(
#'   ADAS = ADNIMERGE2::ADAS,
#'   MMSE = ADNIMERGE2::MMSE,
#'   NEUROBAT = ADNIMERGE2::NEUROBAT,
#'   REGISTRY = ADNIMRGE2::REGISTRY,
#'   DXSUM = ADNIMERGE2::DXSUM
#' )
#' pacc_data2 <- derive_pacc_data(
#'   DATA_SOURCE = "EXTERNAL",
#'   DATA_LIST = EXTERNAL_DATA_LIST,
#'   DATA_SOURCE_DATE = ADNIMERGE2::DATA_DOWNLOADED_DATE,
#'   VIGNETTE_PATH = system.file("doc/ADNIMERGE2-PACC.Rmd", package = "ADNIMERGE2"),
#'   SHOW_PACC_DATADICT = FALSE
#' )
#'
#' # Using a local or remote GitHub version vignettes script to generate PACC scores data
#' # NOTE: please restart R session as needed.
#' remotes::install_github(repo = "atri-biostats/ADNIMERGE2")
#' PACC_VIGNETTE_PATH <- system.file("doc/ADNIMERGE2-PACC.Rmd", package = "ADNIMERGE2")
#' pacc_data3 <- derive_pacc_data(
#'   DATA_SOURCE = "EXTERNAL",
#'   DATA_LIST = EXTERNAL_DATA_LIST,
#'   DATA_SOURCE_DATE = "2026-09-28",
#'   VIGNETTE_PATH = PACC_VIGNETTE_PATH,
#'   SHOW_PACC_DATADICT = FALSE,
#'   ENVIR = globalenv()
#' )
#' }
#' @seealso
#'  \code{vignette(topic = "ADNIMERGE2-PACC", package = "ADNIMERGE2")}
#'  [compute_pacc_score]
#' @rdname derive_pacc_data
#' @importFrom cli cli_abort
#' @keywords adni_scoring_fun
#' @export
#'
derive_pacc_data <- function(DATA_SOURCE,
                             DATA_LIST = list(),
                             DATA_SOURCE_DATE = NULL,
                             VIGNETTE_PATH = system.file("doc/ADNIMERGE2-PACC.Rmd", package = "ADNIMERGE2"),
                             SHOW_PACC_DATADICT = TRUE,
                             ENVIR = new.env(),
                             QUIET = TRUE,
                             ...) {
  check_pacc_data_source(DATA_SOURCE)
  check_object_type(DATA_LIST, "list")
  check_object_type(SHOW_PACC_DATADICT, "logical")
  check_object_type(ENVIR, "environment")

  DATA_NAMES <- c("ADAS", "MMSE", "NEUROBAT", "REGISTRY", "DXSUM")
  if (DATA_SOURCE == "EXTERNAL") {
    check_list_names(x = DATA_LIST, list_names = DATA_NAMES)
    data_status <- lapply(DATA_NAMES, function(x) {
      check_object_type(DATA_LIST[[x]], "data.frame")
    })
    check_non_missing_value(x = DATA_SOURCE_DATE)
  } else {
    DATA_SOURCE_DATE <- ADNIMERGE2::DATA_DOWNLOADED_DATE
  }
  HOLD_DATA_SOURCE_DATE <- DATA_SOURCE_DATE
  DATA_SOURCE_DATE <- as.Date(DATA_SOURCE_DATE, format = "%Y-%m-%d")
  if (is.na(DATA_SOURCE_DATE) || is.null(DATA_SOURCE_DATE)) {
    cli::cli_abort(
      message = c(
        "{.var DATA_SOURCE_DATE} must be a date/character object in {.val YYYY-MM-DD} format. \n ",
        "{.var DATA_SOURCE_DATE} is {.val {HOLD_DATA_SOURCE_DATE}}"
      )
    )
  }
  PACC_V_PATH <- VIGNETTE_PATH
  if (!file.exists(PACC_V_PATH)) {
    cli::cli_abort(
      message = "Cann't find {.val ADNIMERGE2-PACC} vignettes {.file {PACC_V_PATH}}"
    )
  }
  rlang::check_installed(
    pkg = "rmarkdown",
    reason = paste0("To render ", PACC_V_PATH)
  )
  validate_pacc_yaml(PACC_V_PATH)
  # Render file in a new environment
  rmarkdown::render(
    input = PACC_V_PATH,
    params = list(
      DATA_SOURCE = DATA_SOURCE,
      DATA_LIST = DATA_LIST
    ),
    envir = ENVIR,
    quiet = QUIET,
    ...
  )

  PACC_DATA <- ENVIR$PACC
  PACC_DATA$DATA_SOURCE_DATE <- rep(DATA_SOURCE_DATE, nrow(PACC_DATA))

  PACC_DATADICT <- ENVIR$pacc_data_dic
  PACC_DATADICT <- bind_rows(
    PACC_DATADICT,
    dplyr::tibble(
      FLDNAME = "DATA_SOUCE_DATE",
      LABEL = "Data Source Date",
      TYPE = "date",
      TEXT = " ",
      DERIVED = TRUE,
      TBLNAME = unique(PACC_DATADICT$TBLNAME),
      CRFNAME = unique(PACC_DATADICT$CRFNAME)
    )
  )
  ENVIR$PACC <- PACC_DATA
  ENVIR$PACC_DATADICT <- PACC_DATADICT
  OUTPUT <- list(
    "PACC" = PACC_DATA,
    "PACC_DATADIC" = PACC_DATADICT
  )
  if (SHOW_PACC_DATADICT == FALSE) {
    OUTPUT <- PACC_DATA
  }
  return(OUTPUT)
}

## Utils -----
#' @title Check PACC data source
#' @param x A single character vector
#' @return Invisible object of x
#' @examples
#' \dontrun{
#' # Without error message
#' check_pacc_data_source("ADNIMERGE2")
#' # With an error message
#' check_pacc_data_source("ADNIMERGE")
#' }
#' @rdname check_pacc_data_source
#' @importFrom rlang arg_match0
#' @keywords internal
check_pacc_data_source <- function(x) {
  rlang::arg_match0(arg = x, values = c("ADNIMERGE2", "EXTERNAL"))
  invisible(x)
}

#' @title Verify params yaml format in PACC score vignettes
#' @param rmd_path A rmarkdown file path
#' @return Invisible Boolean value
#' @examples
#' \dontrun{
#' pacc_vignette_path <- system.file("doc/ADNIMERGE2-PACC.Rmd", package = "ADNIMERGE2")
#' validate_pacc_yaml(pacc_vignette_path)
#' }
#' @rdname validate_pacc_yaml
#' @importFrom cli cli_abort
#' @keywords internal
validate_pacc_yaml <- function(rmd_path) {
  file_ext <- tools::file_ext(rmd_path)
  if (!file_ext %in% "Rmd") {
    cli::cli_abort(
      message = "{.file {rmd_path}} must be a rmarkdown file."
    )
  }
  params_list <- rmarkdown::yaml_front_matter(rmd_path)[["params"]]
  if (is.null(params_list)) {
    cli::cli_abort("Cann't find {.arg params} in {.file {rmd_path}}.")
  }
  params_names <- names(params_list)
  match_names <- c("DATA_SOURCE", "DATA_LIST")
  not_exit_params <- match_names[!match_names %in% params_names]
  if (length(not_exit_params) > 0 && !is.na(not_exit_params)) {
    cli::cli_abort(
      message = "Cann't find {.val {not_exit_params}} params in {.file rmd_path}"
    )
  }
  data_list_names <- names(params_list$DATA_LIST$value)
  data_list_params <- c("ADAS", "MMSE", "NEUROBAT", "REGISTRY", "DXSUM")
  data_list_status <- data_list_params %in% data_list_names
  not_exist_data <- data_list_params[data_list_status == FALSE]
  if (any(data_list_status == FALSE)) {
    cli::cli_abort(
      message = "Cann't find {.val {not_exist_data}} data list params in {.file {rmd_path}}"
    )
  }
  invisible(TRUE)
}
