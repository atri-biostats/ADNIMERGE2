# Assertive checks ----
#' @title Checks Duplicated Values/Records
#' @description
#'  This function is used to check any duplicated records either within a single
#'  column or across multiple columns.
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
#'  This function is used to check any missing values within a single column
#'  or multiple columns.
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
#' @title Check for mismatch of object type
#' @description
#' Function to check mismatch between actual object class and anticipated type
#'
#' @param x Input value
#' @param type Object type
#' @param arg See \code{\link[rlang]{caller_arg}}
#' @param call See \code{\link[rlang]{caller_env}}
#' @return
#'  An error message if the input object and type does not match.
#'  Otherwise, an invisible \code{TRUE}.
#' @details
#'
#' This function is used to check the object type of an input value based on
#' \code{base} R package. In \code{base} R package, object type functions are
#' specified in \code{is.ObjectType} format.
#'
#' For instance, \code{base::is.logical} is used for checking a logical/Boolean
#' value. Please see examples below how base functions are translated in this function.
#'
#' @examples
#' \dontrun{
#' # Without error message
#' check_object_type(x = letters[1:12], type = "character")
#' check_object_type(x = ADNIMERGE2::CDR, type = "data.frame")
#' check_object_type(x = TRUE, type = "logical")
#'
#' # With error message
#' check_object_type(x = letters[1:12], type = "numeric")
#' check_object_type(x = letters[1:12], type = "factor")
#' }
#' @rdname check_object_type
#' @family checks function
#' @keywords utils_fun
#' @importFrom cli cli_abort
#' @export

check_object_type <- function(x,
                              type,
                              arg = rlang::caller_arg(x),
                              call = rlang::caller_env()) {
  new_envir <- asNamespace("base", base.OK = TRUE)
  if (length(type) != 1) {
    cli::cli_abort(
      message = c(
        "x" = "{.arg type} must be a single character object. \n",
        "i" = "{.val type} is a length of {.val {length(type)}} vector."
      )
    )
  }
  temp_funs <- paste0("is.", type)
  if (!get(temp_funs, envir = new_envir)(x)) {
    cli::cli_abort(
      message = c(
        "x" = "Mismatch between {.arg {arg}} object class and {.arg type} object type arg. \n",
        "i" = "{.arg {arg}} is a {.cls {class(x)}} class object."
      )
    )
  }
  invisible(TRUE)
}


# Check is_datadict_tbl Class Type -----
#' @title Check \code{is_datadict_tbl} Object Class Type
#' @param x Object
#' @return
#'  An error message if the class object is not \code{is_datadict_tbl}.
#'  Otherwise, an invisible \code{TRUE} value.
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
  invisible(TRUE)
}


# Check named list object -----
#' @title Checks a named list object
#' @description
#'  This function is used to check whether a provided object is a named list and
#'  the list names are a valid non-missing names.
#'
#' @param x A list object
#' @param list_names
#'    A character vector of list names.
#'    By default, it is missing (\code{'NULL'}).
#' @inheritParams check_object_type
#' @return
#'  An invisible object. Otherwise, an error message if there is any missing or
#'  mismatch names in the list object when comparing with pre-specified name list.
#' @rdname check_list_names
#' @family checks function
#' @keywords utils_fun
#' @importFrom cli cli_abort
#' @importFrom rlang call_args caller_env

check_list_names <- function(x,
                             list_names = NULL,
                             arg = rlang::caller_arg(x),
                             call = rlang::caller_env()) {
  check_object_type(x, "list")
  # Checking for any unnamed list
  unnamed_status <- c(is.null(names(x)), is.na(names(x)))
  if (any(unnamed_status == TRUE)) {
    cli_abort(
      message = c(
        "{.arg {arg}} must be fully named list object. \n",
        "{.arg {arg}} contains unnamed list object."
      ),
      call = call
    )
  }

  # Checking for any misspelled/omitted/mismatch list names
  if (all(is.null(list_names))) missing_names <- NULL

  if (any(!is.null(list_names))) {
    missing_names <- list_names[!list_names %in% names(x)]
  }
  if (length(missing_names) > 0) {
    cli_abort(
      message = c(
        "{.arg {arg}} contains unnamed list value. \n",
        "Can't find {.val {missing_names}} names{?s} {?is/are} in {.arg {arg}}."
      ),
      call = call
    )
  }

  invisible(TRUE)
}

# Check vector length ----
#' @title Verify vector length
#' @description
#'  This function is used to check if a vector length match with pre-specified
#'  vector length (size).
#' @param x Input vector
#' @param size A numeric object of anticipated vector length.
#' @inheritParams check_object_type
#' @return
#'   An error message if there is a mismatch between vector length and size.
#'   Otherwise, an invisible object.
#' @examples
#' \dontrun{
#'
#' # Without an error message
#' check_vector_length(
#'   x = letters[1:12],
#'   size = 12
#' )
#'
#' # With error message
#' check_vector_length(
#'   x = letters[1:12],
#'   size = 10
#' )
#' }
#' @rdname check_vector_length
#' @family checks function
#' @keywords utils_fun
#' @importFrom rlang caller_arg caller_env
#' @importFrom cli cli_abort
#' @export
check_vector_length <- function(x,
                                size,
                                arg = rlang::caller_arg(x),
                                call = rlang::caller_env()) {
  check_object_type(size, "numeric")
  if (length(x) != size) {
    cli::cli_abort(
      message = c(
        paste0(
          "{cli::col_red(cli::symbol$cross)} ",
          "Mismatch between the length of input vector {.arg {arg}} and provided {.var size} args. \n",
          "{cli::col_green(cli::symbol$info)} ",
          "{.arg {arg}} is a length of {.val {length(x)}} vector, but the provided size was {.val {size}}"
        )
      ),
      call = call
    )
  }
  invisible(TRUE)
}

# Check non missing vector value -----
#' @title Check for non-missing value
#' @param x Input value
#' @inheritParams check_vector_length
#' @return
#'   An error message if input value contains missing value.
#'   Otherwise, an invisible \code{TRUE} value.
#' @examples
#' \dontrun{
#' # Without error message
#' check_non_missing_value(x = LETTERS[1:10])
#'
#' # With error message
#' check_non_missing_value(x = NULL)
#' check_non_missing_value(x = c(LETTERS[1:10], NA_character_))
#' }
#' @rdname check_non_missing_value
#' @family checks function
#' @keywords utils_fun
#' @importFrom cli cli_abort
#' @export

check_non_missing_value <- function(x,
                                    arg = rlang::caller_arg(x),
                                    call = rlang::caller_env()) {
  status <- any(c(any(is.null(x)), any(is.na(x))))
  if (status == TRUE) {
    cli::cli_abort(
      message = "{.arg {arg}} can not be a missing value.",
      call = call
    )
  }
  invisible(TRUE)
}
