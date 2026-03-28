#' @title Checks Duplicated Values/Records
#' @description
#'  This function is used to check any duplicated records within provided column(s).
#'  Either a single column or across multiple columns.
#' @param ... \code{\link[assertr]{assert_rows}} arguments
#' @examples
#' \dontrun{
#' library(assertr)
#' ADNIMERGE2::REGISTRY %>%
#'   assert_uniq(RID, VISCODE)
#' }
#' @return An assertr error message if there is any duplicated records.
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
#'  This function is used to check any missing values within a provided column.
#' @param ... \code{\link[assertr]{assert}} arguments
#' @examples
#' \dontrun{
#' library(assertr)
#' ADNIMERGE2::REGISTRY %>%
#'   assert_non_missing(RID)
#' }
#' @return An assertr error message if there is record with missing values.
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

# Check object type -----
#' @title Check for Object Type Mismatch
#' @param x Input Object
#' @param type Object type
#' @return An error message if the input object and type does not match.
#' @details
#'
#' This function is used to check the object type of an input value based on
#' \code{base} R package. In \code{base} R package, object type functions are
#' specified in \code{is.ObjectType} format. For instance, \code{base::is.logical}
#' is used for checking a logical/Boolean value. Please see the examples how it
#' is translated in this function.
#'
#' @examples
#' \dontrun{
#' check_object_type(x = letters[1:12], type = "character")
#' check_object_type(x = ADNIMERGE2::CDR, type = "data.frame")
#' check_object_type(x = TRUE, type = "logical")
#' # Error message
#' check_object_type(x = letters[1:12], type = "numeric")
#' check_object_type(x = letters[1:12], type = "factor")
#' }
#' @rdname check_object_type
#' @family checks function
#' @keywords utils_fun
#' @export
#' @importFrom cli cli_abort

check_object_type <- function(x, type) {
  new_envir <- new.env()
  temp_funs <- paste0("is.", type)
  if (!get(temp_funs, envir = new_envir)(x)) {
    cli::cli_abort(
      message = c(
        "{.var x} must be a {.cls {type}} object. \n",
        "{.var x} is a {.cls {class(x)}} object."
      )
    )
  }
  invisible(x)
}


# Check is_datadict_tbl Class Type -----
#' @title Check \code{is_datadict_tbl} Object Class Type
#' @param x Object
#' @return An error message if the class object is not \code{is_datadict_tbl}.
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


# Check named list object -----
#' @title Checks a named list object
#' @description
#'  This function is used to check whether a provided object is a named list and
#'  the list names are a valid non-missing names.
#'
#' @param obj A list object
#' @param list_names
#'    A character vector of list names.
#'    By default, it is missing (\code{'NULL'}).
#' @param arg see \code{\link{rlang}{caller_arg}}
#' @param call see \code{\link{rlang}{caller_env}}
#' @return
#'  An invisible object. Otherwise, an error message if there is any missing or
#'  mismatch names in the list object when comparing with pre-specified name list.
#' @rdname check_list_names
#' @keywords utils_fun
#' @importFrom cli cli_abort
#' @importFrom rlang call_args caller_env

check_list_names <- function(obj, list_names = NULL, arg = rlang::caller_arg(obj), call = rlang::caller_env()) {
  check_object_type(obj, "list")
  # Checking for any unnamed list
  unnamed_status <- c(is.null(names(obj)), is.na(names(obj)))
  if (any(unnamed_status == TRUE)) {
    cli_abort(
      message = c(
        "{.arg {arg}} must be fully named list object. \n",
        "{.arg {arg}} contains unnamed list object."
      ),
      call = call
    )
  }

  # Checking for any misspelled/omitted list names
  if (all(is.null(list_names))) missing_names <- NULL

  if (any(!is.null(list_names))) {
    missing_names <- list_names[!list_names %in% names(obj)]
  }
  if (length(missing_names) > 0) {
    cli_abort(
      message = c(
        "{.arg {arg}} contain unnamed list value. \n",
        "Can't find {.val {missing_names}} names{?s} {?is/are} in {.arg {arg}}."
      ),
      call = call
    )
  }

  invisible(obj)
}
