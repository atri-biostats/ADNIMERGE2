# Convert Number of Days ------
#' @title Convert Number of Days
#' @description
#'  A function to convert number of days into one of specified time unit measurements,
#'  either into number of `weeks`, `months` or `years` with specific bin values.
#' @param x Numeric vector
#' @param unit Unit of measurements: either `week`, `month` or `year`, Default: 'month'
#' @param bin Bin value, Default: 1
#' @param digits Rounding decimal digits, Default: 52
#' @param adjust_negative_value Indicator to adjust for negative value
#' @return A numeric vector
#' @details
#'  A small positive values will be added to zero numeric value, specially to
#'  adjust time interval for baseline visits.
#' @examples
#' \dontrun{
#' set.seed(123456)
#' x <- rnorm(100, mean = 100, sd = 10)
#' # Convert number of days into number of weeks
#' convert_number_days(x = x, unit = "week", bin = 1, digits = 1L)
#' # Convert number of days into number of months binned into 3 months
#' convert_number_days(x = x, unit = "month", bin = 3, digits = 2L)
#' # Convert number of days into number of years
#' convert_number_days(x = x, unit = "year", bin = 1, digits = 5L)
#' }
#' @rdname convert_number_days
#' @keywords utils_fun
#' @family Utility functions used for preparing analysis data
#' @export
#' @importFrom rlang arg_match0
#' @importFrom cli cli_abort
convert_number_days <- function(x, unit = "month", bin = 1, digits = 2L, adjust_negative_value = FALSE) {
  rlang::arg_match0(arg = unit, values = c("week", "month", "year"))
  check_is_logical(adjust_negative_value)
  if (unit %in% "week") intv <- 7
  if (unit %in% "month") intv <- 30
  if (unit %in% "year") intv <- 365.25
  intv <- as.numeric(intv)
  arg_names <- c("x", "bin", "digits")
  checks <- lapply(arg_names, function(i) {
    temp_value <- get(i)
    if (any(!is.numeric(temp_value))) {
      cli_abort(
        message = c(
          "{.var {i}} must be a numberic object.",
          "{.var {i}} is a class of {.cla {class(i)}} object."
        )
      )
    }
  })
  y <- as.numeric(x)
  y <- round(y / (intv * bin), digits)
  # adjust for baseline visit (zero values)
  y[x == 0] <- round(1 / (intv * bin), max(c(5, digits)))
  if (adjust_negative_value) {
    y[x < 0] <- round(1 / (intv * bin), max(c(5, digits)))
  }
  return(y)
}


# Convert DX status ------
#' @title Get/ Convert DX Status
#' @description
#'  `get_numeric_dx_status` and `convert_numeric_dx_status` functions are used to
#'  convert either character diagnostics status into numeric values or
#'  numeric values into the corresponding diagnostics status, respectively.
#' @param x String
#' @return
#'  Returns a numeric vector for `get_numeric_dx_status` and a character vector for
#'  `convert_numeric_dx_status`.
#' @details
#'  The input value, `x`, must be a string value for `get_numeric_dx_status` and
#'  numeric value for `convert_numeric_dx_status`.
#' @examples
#' \dontrun{
#' get_numeric_dx_status(x = c("CN", "MCI", "DEM"))
#' convert_numeric_dx_status(x = c(1, 2, 3, 5))
#' }
#' @rdname dx_status_levels
#' @keywords utils_fun
#' @family Utility functions used for preparing analysis data
#' @export
#' @importFrom dplyr case_when
#' @importFrom cli cli_abort
get_numeric_dx_status <- function(x) {
  if (!any(is.character(x) | is.factor(x))) {
    cli::cli_abort(
      message = c(
        "{.var x} must be a character/factor string object. \n",
        "{.val x} is a {.cls {class(x)}} object."
      )
    )
  }

  value <- unique(x)
  value <- value[!is.na(value)]
  value <- value[!value %in% c("CN", "MCI", "DEM", "DEATH")]
  if (all(!is.na(value)) & length(value) > 0) {
    cli::cli_abort(
      message = c(
        "{.var x} must be either {.val {c('CN', 'MCI', 'DEM', 'DEATH')}} values. \n",
        "{.var x} contain {.val {value}} values."
      )
    )
  }

  x <- dplyr::case_when(
    x == "CN" ~ 1,
    x == "MCI" ~ 2,
    x == "DEM" ~ 3,
    x == "DEATH" ~ 4,
    TRUE ~ NA_real_
  )
  return(x)
}

#' @rdname dx_status_levels
#' @export
convert_numeric_dx_status <- function(x) {
  if (!is.numeric(x)) {
    cli::cli_abort(
      message = c(
        "{.var x} must be a numeric object. \n",
        "{.val x} is a {.cls {class(x)}} object."
      )
    )
  }
  value <- unique(x)
  value <- value[!is.na(value)]
  value <- value[!value %in% 1:4]
  if  (all(!is.na(value)) & length(value) > 0) {
    cli::cli_abort(
      message = c(
        "{.var x} must be either {.val {1:4}} values. \n",
        "{.var x} contain {.val {value}} values."
      )
    )
  }
  x <- dplyr::case_when(
    x == 1 ~ "CN",
    x == 2 ~ "MCI",
    x == 3 ~ "DEM",
    x == 4 ~ "DEATH",
    TRUE ~ NA_character_
  )
  return(x)
}
