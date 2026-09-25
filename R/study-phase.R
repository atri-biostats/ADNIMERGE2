## TEAM-ADNI ----
#' @title Function to keep or filter out records that are collected in/associated with TEAM-ADNI study phase
#'
#' @description
#'  `keep_teamadni()` is to keep/filter records that are associated with TEAM-ADNI study phase
#'
#'  `filter_out_teamadni()` is to exclude/remove records that are associated with TEAM-ADNI study phase
#'
#' @param .data A data.frame
#' @param cols_name A character vector of column(s) that contains ADNI study phase identifier.
#'        For examples, similar to `ORIGPROT` or `COLPROT` columns.
#'
#' @return A data.frame
#' @name filter_teamadni
#' @family ADNI study protocol/phase
#' @keywords adni_procotol_fun
#' @importFrom dplyr filter filter_out if_any all_of
NULL

#' @examples
#' \dontrun{
#' # To keep TEAM-ADNI study phase records from `REGISTRY` or `DXSUM` data
#' keep_teamadni(
#'   .data = ADNIMERGE2::REGISTRY,
#'   cols_name = "COLPROT"
#' )
#' keep_teamadni(
#'   .data = ADNIMERGE2::DXSUM,
#'   cols_name = "COLPROT"
#' )
#'
#' # To keep any records for those who started joined ADNI study 
#' # since the start of TEAM-ADNI study phase
#' filter_out_teamadni(
#'   .data = ADNIMERGE2::REGISTRY,
#'   cols_name = "COLPROT"
#' )
#' filter_out_teamadni(
#'   .data = ADNIMERGE2::DXSUM,
#'   cols_name = "COLPROT"
#' )
#' }
#' @rdname filter_teamadni
keep_teamadni <- function(.data, cols_name) {
  .data <- .data %>%
    filter(if_any(all_of(cols_name), ~ .x %in% adni_phase()[6]))
  return(.data)
}

filter_out_teamadni <- function(.data, cols_name) {
  .data <- .data %>%
    filter_out(if_any(all_of(cols_name), ~ .x %in% adni_phase()[6]))
  return(.data)
}
