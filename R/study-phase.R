## TEAM-ADNI ----
#' @title filter_teamadni
#' @description
#'  Functions to keep or filter out records that are collected in/associated with TEAM-ADNI study phase.
#'
#'  `keep_teamadni()` is used to keep/filter records that are associated with TEAM-ADNI study phase.
#'
#'  `filter_out_teamadni()` is used to exclude/remove records that are associated with TEAM-ADNI study phase.
#'
#' @param .data A data.frame
#' @param phase_cols A character vector of column(s) that contains ADNI study phase identifier.
#'        For examples, similar to `ORIGPROT` or `COLPROT` columns.
#'
#' @return A data.frame
#' @name filter_teamadni
#' @family ADNI study protocol/phase
#' @keywords internal
#' @importFrom dplyr filter filter_out if_any all_of
NULL

#' @examples
#' \dontrun{
#' # To keep TEAM-ADNI study phase records from `REGISTRY` or `DXSUM` data
#' keep_teamadni(
#'   .data = ADNIMERGE2::REGISTRY,
#'   phase_cols = "COLPROT"
#' )
#' keep_teamadni(
#'   .data = ADNIMERGE2::DXSUM,
#'   phase_cols = "COLPROT"
#' )
#' }
#' @rdname filter_teamadni
keep_teamadni <- function(.data, phase_cols) {
  .data <- .data %>%
    filter(if_any(all_of(phase_cols), ~ .x %in% adni_phase()[6]))
  return(.data)
}

#' @rdname filter_teamadni
#' @examples
#' \dontrun{
#' # To keep any records for those who started joined ADNI study
#' # since the start of TEAM-ADNI study phase
#' filter_out_teamadni(
#'   .data = ADNIMERGE2::REGISTRY,
#'   phase_cols = "COLPROT"
#' )
#' filter_out_teamadni(
#'   .data = ADNIMERGE2::DXSUM,
#'   phase_cols = "COLPROT"
#' )
#' }
filter_out_teamadni <- function(.data, phase_cols) {
  .data <- .data %>%
    filter_out(if_any(all_of(phase_cols), ~ .x %in% adni_phase()[6]))
  return(.data)
}
