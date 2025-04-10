#' @title Checks Duplicated Values/Records
#' @description
#'  This function is used to check any duplicated records within the provided column.
#'  Either a single column or multiple columns.
#' @param ... \code{\link[assertr]{assert_rows}} arguments
#' @return A stop message if there is at least one duplicated records.
#' @rdname assert_uniq
#' @family assert checks
#' @keywords utils_fun
#' @seealso
#'   \code{\link[assertr]{assert_rows}}
#'   \code{\link[assertr]{col_concat}}
#'   \code{\link[assertr]{is_uniq}}
#' @importFrom assertr assert_rows col_concat is_uniq
#' @export
assert_uniq <- function(...) {
  assert_rows(
    row_reduction_fn = col_concat,
    predicate = is_uniq,
    ...
  )
}
#' @title Checks Missing Values
#' @description
#'  This function is used to check any missing values within the provided column.
#' @param ... \code{\link[assertr]{assert}} arguments
#' @return A stop message if there is at least one duplicated records.
#' @rdname assert_non_missing
#' @family assert checks
#' @keywords utils_fun
#' @seealso
#'  \code{\link[assertr]{assert}}
#'  \code{\link[assertr]{not_na}}
#' @importFrom assertr assert not_na
#' @export
assert_non_missing <- function(...) {
  assert(
    predicate = not_na,
    ...
  )
}
