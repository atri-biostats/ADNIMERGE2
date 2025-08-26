# Convert Number of Days ------
#' @title Convert Number of Days
#' @description
#'  A function to convert number of days into one of specified time unit measurements,
#'  either into number of \code{weeks}, \code{months} or \code{years} with specific bin values.
#' @param x Numeric vector
#' @param unit Unit of measurements: either \code{week}, \code{month} or \code{year}, Default: 'month'
#' @param bin Bin value, Default: 1
#' @param digits Rounding decimal digits, Default: 52
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
#' @export
#' @importFrom rlang arg_match0
#' @importFrom cli cli_abort
convert_number_days <- function(x, unit = "month", bin = 1, digits = 2L) {
  rlang::arg_match0(arg = unit, values = c("week", "month", "year"))
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
  return(y)
}


# Detect Closest Baseline Score -----
#' @title Detect Closest Baseline Score
#' @description
#'  This function is used to flag the closest assessment record score to the
#'  baseline visit date (i.e enrollment date) within a certain window period.
#' @param cur_record_date Date of current assessment record collected
#' @param enroll_date Enrollment date (i.e. baseline visit date)
#' @param time_interval Minimum window period (in days) from baseline visit date, Default: 30
#' @return
#'  A character vector with the same length as \code{cur_record_date} and 
#'  contain a flag value, \code{Yes}, for the closest record
#'  within the specified window period. Otherwise, missing value.
#' @rdname detect_baseline_score
#' @family ADNI enrollment
#' @keywords utils_fun
detect_baseline_score <- function(cur_record_date, enroll_date, time_interval = 30) {
  time_diff <- as.numeric(as.Date(cur_record_date) - as.Date(enroll_date))
  abs_time_diff <- abs(time_diff)
  flags <- abs_time_diff < time_interval
  # Adjustment for the nearest timeline
  if (length(flags[flags == TRUE]) > 1) {
    list_closet_timeline <- min(abs_time_diff[flags == TRUE])
    flags <- abs_time_diff == list_closet_timeline
  }
  flags <- ifelse(flags == TRUE, "Yes", NA_character_)
  return(flags)
}
