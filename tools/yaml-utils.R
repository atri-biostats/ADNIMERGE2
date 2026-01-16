# Modify rmarkdown params ------
#' @title Modify Rmarkdown Param YAML Values
#' @description
#' This function is used to modify the existing params YAML in rmarkdown file(s).
#' The modification can also be preformed across files in a specific directory.
#'
#' @param dir_path Directory path, Default: NULL
#' @param file_path Full file path, Default: NULL
#' @param current_param Current param value
#' @param new_param New param value
#' @return An error message if the param value is not modified for existed param name in a rmarkdown file.
#'
#' @rdname modify_rmd_param_yaml
#' @keywords utils_fun
#' @family modify rmarkdown params
#' @importFrom cli cli_alert_info cli_inform
#' @importFrom rlang caller_env
#' @importFrom xfun gsub_file

modify_rmd_param_yaml <- function(dir_path = NULL, file_path = NULL, current_param, new_param, value_as_logical = FALSE) {
  require(tidyverse)
  require(rmarkdown)
  require(cli)
  check_load_input(dir_path, file_path)
  check_param_format(current_param)
  check_param_format(new_param)
  param_name <- get_param_name_value(current_param, "param")
  param_value <- get_param_name_value(current_param, "value")
  param_name_new <- get_param_name_value(new_param, "param")
  new_param_value <- get_param_name_value(new_param, "value")
  if (param_name != param_name_new) {
    cli::cli_abort(message = "Different param name is provided: {.val {current_param}} and {.val {new_param}}")
  }
  rmd_pattern <- "\\.Rmd$"
  if (!is.null(dir_path)) {
    check_dir_path(dir_path)
    rmd_file_list <- get_full_file_path(dir_path, rmd_pattern)
  } else {
    rmd_file_list <- file_path
    rmd_file_status <- stringr::str_detect(rmd_file_list, rmd_pattern)
    if (any(rmd_file_status == FALSE)) {
      cli::cli_abort(message = c(
        "{.val {length(rmd_file_list[rmd_file_status == FALSE])}} file{?s} {?is/are} not rmarkdown file. \n",
        "None rmarkdown file{?s} {?is/are}: {.val {rmd_file_list[rmd_file_status == FALSE]}}"
      ))
    }
  }
  if (length(rmd_file_list) == 0) {
    if (!is.null(dir_path)) cli::cli_inform(message = c("{cli::symbol$info} Rmarkdown file is not found in {.path {dir_path}}."))
    if (!is.null(file_path)) cli::cli_inform(message = c("{cli::symbol$info} Rmarkdown file is not provided in {.var file_path}."))
  } else {
    for (i in seq_along(rmd_file_list)) {
      status <- set_rmd_param_yaml(
        file_path = rmd_file_list[i],
        param_name = param_name,
        param_value = param_value,
        new_param_value = new_param_value,
        value_as_logical = value_as_logical
      )
      if (status == "Yes") {
        cli::cli_inform(
          message = c(
            paste0(
              "{cli::col_green(cli::symbol$tick)} Modified {.var {param_name}} ",
              "param YAML in {.path {rmd_file_list[i]}}.\n"
            ),
            paste0("{.field {current_param}} {cli::col_green(cli::symbol$arrow_right)} {.field {new_param}}")
          )
        )
      }
      if (status == "No") {
        cli::cli_inform(
          message = c(
            paste0(
              "{cli::col_yellow(cli::symbol$warning)} {.var {param_name}} param YAML ",
              "has not been modified in {.path {rmd_file_list[i]}}.\n"
            ),
            paste0(
              "{cli::col_yellow(cli::symbol$info)} {.val {param_name}} with ",
              "{.val {param_value}} param YAML value in {.path {rmd_file_list[i]}}"
            )
          )
        )
      }
      if (status == "None") {
        cli::cli_inform(
          message = paste0(
            "{cli::col_red(cli::symbol$cross)} {.val {param_name}} ",
            "param YAML is not found in {.path {rmd_file_list[i]}}."
          )
        )
      }
    }
  }
  invisible(dir_path)
}

#' @title Set Param Values in Rmd File
#' @description
#' This function is used to modify param values in a single Rmd file.
#' @param file_path File path
#' @param param_name Param name
#' @param param_value Current param value
#' @param new_param_value New param value
#' @param value_as_logical A Boolean value to convert both the current and new param values in UPPERCASE logical value.
#' @return A character vector either
#'    \code{'Yes'} When the param yaml value has been modified correctly and see an updated file for the modification.
#'    \code{'No'} When the param yaml value has not been modified correctly.
#'    \code{'None'} The provided rmarkdown file does not contains this \code{param_name} param name.
#' @examples
#' \dontrun{
#' # Not to include PACC score related summary in enrollment report
#' set_rmd_param_yaml(
#'   file_path = file.path(".", "vignettes", "ADNI-Enrollment.Rmd"),
#'   param_name = "INCLUDE_PACC",
#'   param_value = "TRUE",
#'   new_param_value = "FALSE"
#' )
#' }
#' @rdname set_rmd_param_yaml
#' @keywords utils_fun
#' @family modify rmarkdown params
#' @importFrom rmarkdown yaml_front_matter
#' @importFrom stringr str_locate
#' @importFrom tibble rownames_to_column
#' @importFrom cli cli_abort

set_rmd_param_yaml <- function(file_path, param_name, param_value, new_param_value, value_as_logical = FALSE) {
  require(tidyverse)
  require(rmarkdown)
  if (!is.logical(value_as_logical)) {
    cli::cli_abort(message = c(
      "{.var value_as_logical} must be a Boolean value. \n",
      "{.var value_as_logical} is a {.val {class(value_as_logical)}} object."
    ))
  }
  if (value_as_logical == TRUE) {
    check_logical_param_value(x = c(param_value, new_param_value))
    param_value <- c(tolower(param_value), toupper(param_value))
    exist_param_pattern <- paste0(param_name, ": ", param_value)
    exist_param_pattern <- paste0(exist_param_pattern, collapse = "|")
    new_param_value <- toupper(new_param_value)
    new_param_pattern <- paste0(param_name, ": ", new_param_value)
  } else {
    exist_param_pattern <- paste0(param_name, ": ", param_value)
    new_param_pattern <- paste0(param_name, ": ", new_param_value)
  }
  param_status <- detect_file_param(file_path, param_name)
  if (param_status == TRUE) {
    file_yaml <- rmarkdown::yaml_front_matter(file_path)
    # TRUE if the current and new param values are different
    # FALSE if the current and new param values are the same
    last_status <- file_yaml$params[[param_name]] != new_param_value
  } else {
    last_status <- FALSE
  }
  # Modify param values if the current and new param values are different
  if (last_status == TRUE) {
    full_file_content <- readLines(file_path)
    # Detect yaml position in a file
    yaml_char <- "---"
    yaml_last_pos <- stringr::str_locate(string = full_file_content, pattern = yaml_char)
    yaml_last_pos <- tibble::as_tibble(yaml_last_pos) %>%
      tibble::rownames_to_column(., var = "pos") %>%
      dplyr::filter(start == 1 & end == 3) %>%
      dplyr::pull(pos)
    yaml_last_pos <- max(yaml_last_pos)
    # Modify values in yaml section only
    read_yaml_text <- readLines(file_path, n = yaml_last_pos)
    replaced_yaml_text <- gsub(
      x = read_yaml_text,
      pattern = exist_param_pattern,
      replacement = new_param_pattern
    )
    full_file_content[seq_len(yaml_last_pos)] <- replaced_yaml_text
    test_file <- file.path(tempdir(), "test_file.Rmd")
    writeLines(full_file_content, test_file)
    test_yaml <- rmarkdown::yaml_front_matter(test_file)
    test_status <- test_yaml$params[[param_name]] == new_param_value
    # To overwrite local file
    if (test_status == TRUE) {
      writeLines(full_file_content, file_path)
    }
  }
  last_status <- if (last_status == TRUE && param_status == TRUE) {
    "Yes"
  } else if (last_status == FALSE && param_status == TRUE) {
    "No"
  } else {
    "None"
  }
  return(last_status)
}

#' @title Detect Param Name in Rmarkdown File
#' @description
#'  This function is used to detect whether specific params name is exist in a rmarkdown file.
#' @param file_path File path
#' @param param_name Character vector of params name
#' @return \code{TRUE} if the params name is found in a rmarkdown file. Otherwise, \code{FALSE}
#' @examples
#' \dontrun{
#' # Suppose to check whetehr `INCLUDE_PACC` params YAML in "./ADNI-Enrollment.Rmd" vignette
#' detect_file_param(
#'   file_path = "./vignettes/ADNI-Enrollment.Rmd",
#'   param_name = "INCLUDE_PACC"
#' )
#' }
#' @rdname detect_file_param
#' @keywords utils_fun
#' @family modify rmarkdown params
#' @importFrom cli cli_abort
#' @importFrom rmarkdown yaml_front_matter

detect_file_param <- function(file_path, param_name) {
  require(tidyverse)
  require(rmarkdown)
  if (length(param_name) != 1) {
    cli::cli_abort(message = c(
      "{.var {param_name}} must be a length of one character vector. \n",
      "{.var param_name} is a length of {.val {length(param_name)}} character vector."
    ))
  }
  file_yaml <- rmarkdown::yaml_front_matter(file_path)
  if (!"params" %in% names(file_yaml)) last_status <- FALSE
  if ("params" %in% names(file_yaml)) {
    param_list <- file_yaml$params
    last_status <- param_name %in% names(param_list)
  }
  return(last_status)
}

#' @title Get Param Names or Values
#' @param x Character vector
#' @param show_result Either param names or param values
#' @return A character vector
#' @examples
#' \dontrun{
#' # To return a param name: "INCLUDE_PACC"
#' get_param_name_value(
#'   x = "INCLUDE_PACC: TRUE",
#'   show_result = "param"
#' )
#' # To return a param value: 'TRUE'
#' get_param_name_value(
#'   x = "INCLUDE_PACC: true",
#'   show_result = "value"
#' )
#' }
#' @rdname get_param_name
#' @keywords utils_fun
#' @family modify rmarkdown params
#' @importFrom stringr str_split
#' @importFrom rlang arg_match0

get_param_name_value <- function(x, show_result = "param") {
  require(tidyverse)
  require(cli)
  check_param_format(x)
  if (length(x) != 1) {
    cli::cli_abort(message = c(
      "{.var x} param must be single character vector. \n",
      "{.var x} is a length of {.val {length(x)}} character vector."
    ))
  }
  rlang::arg_match0(arg = show_result, values = c("param", "value"))
  split_char <- ": "
  param_value <- stringr::str_split(string = x, pattern = split_char)
  param <- sapply(param_value, function(y) {
    y[[1]]
  })
  value <- sapply(param_value, function(y) {
    y[[2]]
  })
  if (length(param) == 0 || any(is.na(param))) {
    cli::cli_abort(message = "{.var param} must not be missing!")
  }
  if (length(value) == 0 || any(is.na(value))) {
    cli::cli_abort(message = "{.var value} must not be missing!")
  }
  last_result <- if (show_result == "param") {
    param
  } else {
    value
  }
  return(last_result)
}

#' @title Check Params Format
#' @inheritParams arg_utils
#' @return
#'  An error message if the input argument does not contain \code{":"} character
#'   or does not start with double blank space \code{"  "}.
#' @examples
#' \dontrun{
#' # Suppose for changing the PACC score params in vignettes rmd file with
#' # a default param values of 'INCLUDE_PACC: TRUE'
#' # Without error
#' check_param_format(x = "INCLUDE_PACC: TRUE")
#' # With errors
#' check_param_format(x = "INCLUDE_PACC: TRUE ")
#' check_param_format(x = " INCLUDE_PACC TRUE")
#' }
#' @rdname check_param_format
#' @keywords utils_fun
#' @family modify rmarkdown params
#' @importFrom rlang caller_arg caller_env
#' @importFrom stringr str_detect str_trim
#' @importFrom cli cli_abort

check_param_format <- function(x,
                               arg = rlang::caller_arg(x),
                               call = rlang::caller_env()) {
  require(stringr)
  require(cli)
  params_format_text <- "{.arg {arg}} arg must be in {.val {'params_name: params_value'}} format."
  split_char <- ": "
  if (stringr::str_detect(string = x, pattern = split_char, negate = TRUE)) {
    cli::cli_abort(
      message = c(
        "{.arg {arg}} arg must contain {.val {split_char}} character. \n",
        params_format_text
      ),
      call = call
    )
  }
  text_left_space <- stringr::str_trim(string = x, side = "left")
  if (text_left_space != x) {
    cli::cli_abort(
      message = c(
        "{.arg {arg}} arg must not start with {.val {'blank space'}}.\n",
        "{.arg {arg}} start with {.val {nchar(x) - nchar(text_left_space)}} blank space. \n",
        params_format_text
      ),
      call = call
    )
  }
  text_right_space <- stringr::str_trim(string = x, side = "right")
  if (text_right_space != x) {
    cli::cli_abort(
      message = c(
        "{.arg {arg}} arg must not end with {.val {'blank space'}}.\n",
        "{.arg {arg}} end with {.val {nchar(x) - nchar(text_right_space)}} blank space. \n",
        params_format_text
      ),
      call = call
    )
  }
  invisible(x)
}

#' @title Check Boolean Value for Param Value
#' @param x Input value
#' @return An error message if input value(s) are not a Boolean value
#' @rdname check_logical_param_value
#' @keywords utils_fun
#' @family modify rmarkdown params
check_logical_param_value <- function(x) {
  last_status <- lapply(length(x), function(y) {
    status <- as.logical(x[y])
    if (is.na(status)) {
      cli::cli_abort(
        message = c(
          "{.var {x[y]}} is not a Boolean param value. \n",
          "Did you set {.val {'value_as_logical = TRUE'}} accidentaly?"
        )
      )
    }
  })
  invisible(last_status)
}
