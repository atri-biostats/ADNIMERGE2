#' @title Checks Duplicated Values/Records
#' @description
#'  This function is used to check any duplicated records within the provided column.
#'  Either a single column or multiple columns.
#' @param ... \code{\link[assertr]{assert_rows}} arguments
#' @examples
#' \dontrun{
#' library(assertr)
#' ADNIMERGE2::REGISTRY %>%
#'   assert_uniq(RID, VISCODE)
#' }
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
#' @examples
#' \dontrun{
#' library(assertr)
#' ADNIMERGE2::REGISTRY %>%
#'   assert_non_missing(RID)
#' }
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

# Check A Boolean Value -----
#' @title Check A Boolean Value
#' @param x Input value
#' @return A stop error if the value is not a Boolean value
#' @examples
#' \dontrun{
#' check_is_logical(x = "text")
#' check_is_logical(x = TRUE)
#' }
#' @rdname check_is_logical
#' @family checks function
#' @keywords utils_fun
#' @importFrom cli cli_abort
#' @export
check_is_logical <- function(x) {
  if (!is.logical(x)) {
    cli_abort(
      message = c(
        "{.var x} must be a Boolean value. \n",
        "{.var x} is a {.cls {class(x)}} object."
      )
    )
  }
  invisible(x)
}

# Check is_datadict_tbl Class Type -----
#' @title Check \code{is_datadict_tbl} Object Class Type
#' @param x Object
#' @return A stop error if the class object is not \code{is_datadict_tbl}.
#' @rdname is_datadict_tbl
#' @family checks function
#' @keywords adni_datadic_fun
#' @importFrom cli cli_abort
#' @export
is_datadict_tbl <- function(x) {
  if (!"datadict_tbl" %in% class(x)) {
    cli_abort(
      message = paste0(
        "{.var x} must be a class of {.cls datadict_tb} ",
        "created by get_factor_levels_datadict function"
      )
    )
  }
  invisible(x)
}
