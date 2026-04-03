# List derived dataset -----
#' @title `ADNIMERGE2` Derived Dataset(s)
#' @description List of derived/analysis dataset(s) in `ADNIMERGE2` R package
#'
#' @param type Data type
#'  + `derived`: To list derived data
#'  + `analysis`: To list analysis ready data
#'  + `metadata`: To list metadata
#'  + `NULL`: To list all available derived/analysis data in `ADNIMERGE2`
#'
#' @return A data.frame with **Dataset Code**, **Dataset Label** and *Data Type* columns.
#'
#' @details
#' Please refer to the following vignettes for more detailed information
#' about how these derived datasets are generated in `ADNIMERGE2` R package.
#'
#'  + `vignette(topic = 'ADNIMERGE2-Derived-Data', package = 'ADNIMERGE2')`
#'
#'  + `vignette(topic = 'ADNIMERGE2-Analysis-Meta-Specs', package = 'ADNIMERGE2')`
#'
#'  + `vignette(topic = 'ADNIMERGE2-Analysis-Data', package = 'ADNIMERGE2')`
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(dplyr)
#'
#  # To List all available derived/analysis dataset
#' list_derived_data(type = NULL)
#'
#' # Derived data
#' list_derived_data(type = "derived")
#' # Analysis data
#' list_derived_data(type = "analysis")
#' # Metadata
#' list_derived_data(type = "metadata")
#' }
#'
#' @name DERIVED
#' @rdname DERIVED
#' @keywords derived derived_dataset
#' @family derived datasets
#' @importFrom dplyr distinct mutate arrange rename filter select all_of
#' @importFrom stringr str_remove_all str_to_upper
#' @export

list_derived_data <- function(type = NULL) {
  TBLNAME <- CRFNAME <- `Dataset Code` <- `Dataset Label` <- NULL
  type_list <- c("derived", "analysis", "metadata")
  if (any(!is.null(type))) {
    rlang::arg_match(
      arg = type,
      values = type_list,
      multiple = TRUE
    )
  }
  if (all(is.null(type))) {
    type <- c(type_list, NA_character_)
  }
  derived_data_list <- ADNIMERGE2::DERIVED_DATADIC %>%
    dplyr::select(dplyr::all_of(c("TBLNAME", "CRFNAME"))) %>%
    dplyr::distinct() %>%
    # Create data type/category
    dplyr::mutate(
      data_type = dplyr::case_when(
        nchar(TBLNAME) == 2 ~ "derived",
        nchar(TBLNAME) > 2 & TBLNAME %in% "PACC" ~ "derived",
        nchar(TBLNAME) > 2 & !TBLNAME %in% "PACC" ~ "analysis"
      )
    ) %>%
    dplyr::mutate(
      CRFNAME = stringr::str_remove_all(
        string = CRFNAME,
        pattern = "^\\[ Derived \\] "
      )
    )

  # Rename columns
  derived_data_list <- derived_data_list %>%
    dplyr::rename(
      `Dataset Code` = TBLNAME,
      `Dataset Label` = CRFNAME,
      `Data Type` = data_type
    )
  # Add METCORES/METADATA description
  derived_data_list <- dplyr::bind_rows(
    derived_data_list,
    list_metadata()
  ) %>%
    dplyr::arrange(`Dataset Code`)

  # Filter out by data type
  derived_data_list <- derived_data_list %>%
    dplyr::filter(`Data Type` %in% type) %>%
    dplyr::mutate(`Data Type` = stringr::str_to_upper(string = `Data Type`))

  # Check for duplicated records
  derived_data_list <- derived_data_list %>%
    {
      if (nrow(.) > 0) {
        ADNIMERGE2::assert_uniq(., dplyr::all_of("Dataset Code"))
      } else {
        (.)
      }
    }

  derived_data_list
}

# Get metadata labels -----
#' @title Get [ADNIMERGE2] metadata specs description
#' @inherit DERIVED return
#' @details
#' Please refer to
#' `vignette(topic = 'ADNIMERGE2-Analysis-Meta-Specs', package = 'ADNIMERGE2')`
#' for more detailed about how this metadata was generated.
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' list_metadata()
#' }
#' @rdname list_metadata
#' @keywords internal
#' @importFrom rlang env
#' @importFrom utils data
#' @importFrom tibble as_tibble tibble
#' @importFrom stats na.omit
#' @importFrom dplyr filter mutate select

list_metadata <- function() {
  Item <- Title <- `Dataset Code` <- `Dataset Label` <- `Data Type` <- NULL
  new_env <- rlang::env()
  pkg_data <- utils::data(package = "ADNIMERGE2", envir = new_env)$results
  pkg_data <- tibble::as_tibble(pkg_data)
  metadata <- pkg_data %>%
    dplyr::filter(Item == "METACORES") %>%
    dplyr::mutate(`Data Type` = "metadata") %>%
    dplyr::select(
      `Dataset Code` = Item,
      `Dataset Label` = Title,
      `Data Type`
    ) %>%
    stats::na.omit()
  if (nrow(metadata) == 0) {
    metadata <- tibble::tibble(
      `Dataset Code` = NA_character_,
      `Dataset Label` = NA_character_,
      `Data Type` = NA_character_
    ) %>%
      stats::na.omit()
  }
  metadata
}
