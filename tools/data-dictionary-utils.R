# Summarize Single Variable ----
#' @title Summarize Single Variable
#' @param .data A data.frame
#' @param var_name Variable name
#' @param wide_format
#'   A Boolean value to generate the variable possible values in wide or long format, Default: FALSE
#' @return A data.frame contains at least the following top four variables:
#'   \item{field_name}{Variable name}
#'   \item{field_class}{Variable class type: 'factor', 'character', 'date', 'POSIXct", "POSIXt" or 'logical'}
#'   \item{field_label}{Variable labels}
#'   \item{field_notes}{Text notes that could be used to generate the roxygen document for the dataset}
#'   \item{field_values_order}{Level order, only returned for factor variable}
#'   \item{summary_type}{Summary type either 'min' or 'max' for numeric variable}
#' @details
#' This function is used to summarize single variable with variable attributes.
#' The variable attribute includes variable name, labels, class, notes and possible values
#' such as levels for factor variable and range values for numeric variable.
#' The output result can be used to create a data dictionary file.
#'
#' @examples
#' \dontrun{
#' library(tidyverse)
#' library(ADNIEMRGE2)
#'
#' summarize_variable(
#'   .data = ADNIMERGE2::DM,
#'   var_name = "ORIGPROT",
#'   wide_format = FALSE
#' )
#' }
#' @rdname summarize_variable
#' @family summarize variable
#' @keywords var_summary data_summary
#' @seealso \code{\link{summarize_dataset}}
#' @importFrom rlang arg_match
#' @importFrom assertr verify
#' @importFrom dplyr select
#' @importFrom tidyselect all_of
#' @export

summarize_variable <- function(.data, var_name, wide_format = FALSE) {
  require(tidyverse)
  require(assertr)
  require(rlang)
  require(labelled)
  require(stringr)

  .data <- set_var_class(.data, var_name)

  summary_result <- summarize_var(
    .data = .data,
    var_name = var_name,
    wide_format = wide_format
  )

  summary_result <- summary_result %>%
    {
      if (wide_format) {
        verify(., nrow(.) == 1)
      } else {
        (.)
      }
    }

  return(summary_result)
}

#' @title Generic function to summarize single variable
#' @description
#'  A generic function to summarize single variable based on variable-specific class type.
#'  This function is used in \code{\link[summarize_variable]()} function.
#' @inheritParams summarize_variable
#' @inherit summarize_variable return details examples importFrom
#' @name summarize_var
#' @family summarize variable

summarize_var <- function(.data, var_name, wide_format = FALSE) {
  UseMethod("summarize_var")
}

## Summarize Factor Variable -------
#' @importFrom labelled get_variable_labels
#' @importFrom rlang arg_match0
#' @importFrom assertr verify
#' @importFrom tibble tibble
#' @importFrom dplyr select pull group_by mutate ungroup distinct
#' @importFrom tidyselect all_of
#' @rdname summarize_var

summarize_var.fct <- function(.data, var_name, wide_format = FALSE) {
  field_name <- field_class <- field_label <- field_values_order <- NULL
  field_values <- field_notes <- NULL
  .data <- remove_var_class(.data)
  var_values <- .data %>%
    select(all_of(var_name)) %>%
    pull()
  var_label <- labelled::get_variable_labels(var_values)
  var_class_type <- class(var_values)
  rlang::arg_match0(arg = var_class_type, values = "factor")
  check_object_type(wide_format, "logical")
  # A description note for roxygen2 document
  var_notes <- str_c(
    "Factor variable with levels: ",
    paste0(levels(var_values), collapse = ", ")
  )
  summary_result <- tibble(
    field_values = levels(var_values),
    field_values_order = seq_along(levels(var_values)),
    field_name = var_name,
    field_class = var_class_type,
    field_label = convert_null_into_na(var_label),
    field_notes = var_notes
  ) %>%
    select(
      field_name, field_class, field_label, field_values_order,
      field_values, field_notes
    ) %>%
    {
      if (wide_format) {
        group_by(., field_name) %>%
          mutate(., field_values = toString(field_values)) %>%
          ungroup(.) %>%
          select(., -field_values_order) %>%
          distinct(.) %>%
          assertr::verify(., nrow(.) == 1)
      } else {
        (.)
      }
    }

  return(summary_result)
}

## Summarize Character Variable -------
#' @importFrom dplyr case_when
#' @importFrom stringr str_detect
#' @rdname summarize_var

summarize_var.character <- function(.data, var_name, wide_format = FALSE) {
  .data <- remove_var_class(.data)
  var_values <- .data %>%
    select(all_of(var_name)) %>%
    pull()
  var_label <- labelled::get_variable_labels(var_values)
  var_class_type <- class(var_values)
  rlang::arg_match0(arg = var_class_type, values = "character")
  check_object_type(wide_format, "logical")
  var_values <- unique(var_values)[!is.na(unique(var_values))]
  id_format_pattern <- "[0-9]{3}\\_s\\_d+|[0-9]{3}\\_S\\_d+|[0-9]{3}-s-d+|[0-9]{3}-S-d+"
  contain_ids <- any(str_detect(string = var_values, pattern = id_format_pattern))
  if (all(length(var_values) <= 10 & length(var_values) > 0 & contain_ids != TRUE)) {
    var_notes <- paste0(
      "Character variable with options: ",
      paste0(var_values, collapse = ", ")
    )
  }
  if (any(length(var_values) == 0 | length(var_values) > 10 | contain_ids == TRUE)) {
    var_values <- NA_character_
    var_notes <- paste0(" ")
  }

  summary_result <- tibble(
    field_values = var_values,
    field_values_order = seq_along(var_values),
    field_name = var_name,
    field_class = var_class_type,
    field_label = convert_null_into_na(var_label),
    field_notes = var_notes
  ) %>%
    mutate(field_values_order = case_when(
      is.na(field_values) ~ NA_real_,
      TRUE ~ field_values_order
    )) %>%
    select(
      field_name, field_class, field_label, field_values_order,
      field_values, field_notes
    ) %>%
    {
      if (wide_format) {
        group_by(., field_name) %>%
          mutate(., field_values = toString(field_values)) %>%
          ungroup(.) %>%
          select(., -field_values_order) %>%
          distinct(.) %>%
          verify(., nrow(.) == 1)
      } else {
        (.)
      }
    }

  return(summary_result)
}


## Summarize Numeric Variable -------
#' @rdname summarize_var

summarize_var.numeric <- function(.data, var_name, wide_format = FALSE) {
  .data <- remove_var_class(.data)
  var_values <- .data %>%
    select(all_of(var_name)) %>%
    pull()
  var_label <- labelled::get_variable_labels(var_values)
  var_class_type <- class(var_values)
  rlang::arg_match0(
    arg = var_class_type,
    values = c("numeric", "double", "integer")
  )
  check_object_type(wide_format, "logical")
  # Minimum and Maximum Values
  range_values <- range(var_values, na.rm = TRUE)
  var_notes <- paste0("Range value: ", range_values[1], ", ..., ", range_values[2])

  summary_result <- tibble::tibble(
    field_values = as.character(range_values),
    summary_type = c("Min", "Max"),
    field_name = var_name,
    field_class = var_class_type,
    field_label = convert_null_into_na(var_label),
    field_notes = var_notes
  ) %>%
    mutate(field_values = case_when(
      field_values %in% c("-Inf", "Inf") ~ NA_character_,
      TRUE ~ field_values
    )) %>%
    select(
      field_name, field_class, field_label, summary_type,
      field_values, field_notes
    ) %>%
    {
      if (wide_format) {
        group_by(., field_name) %>%
          mutate(.,
            field_values = toString(field_values),
            summary_type = toString(summary_type)
          ) %>%
          ungroup(.) %>%
          distinct(.) %>%
          verify(., nrow(.) == 1)
      } else {
        (.)
      }
    }

  return(summary_result)
}

#' @rdname summarize_var
summarize_var.double <-
  summarize_var.integer <-
  summarize_var.numeric

## Summarize Logical/Boolean Variable -------
#' @rdname summarize_var

summarize_var.logical <- function(.data, var_name, wide_format = FALSE) {
  .data <- remove_var_class(.data)
  var_values <- .data %>%
    select(all_of(var_name)) %>%
    pull()
  var_label <- labelled::get_variable_labels(var_values)
  var_class_type <- class(var_values)
  rlang::arg_match0(arg = var_class_type, values = "logical")
  var_notes <- paste0("A Boolean value: TRUE/ FALSE")
  summary_result <- tibble(
    field_values = str_c("TRUE or FALSE"),
    field_values_order = NA_real_,
    field_name = var_name,
    field_class = var_class_type,
    field_label = convert_null_into_na(var_label),
    field_notes = var_notes
  ) %>%
    select(
      field_name, field_class, field_label, field_values_order,
      field_values, field_notes
    ) %>%
    verify(nrow(.) == 1)

  return(summary_result)
}


## Summarize Date Variable -------
#' @rdname summarize_var

summarize_var.Date <- function(.data, var_name, wide_format = FALSE) {
  .data <- remove_var_class(.data)
  var_values <- .data %>%
    select(all_of(var_name)) %>%
    pull()

  var_label <- labelled::get_variable_labels(var_values)

  var_class_type <- class(var_values)
  rlang::arg_match(
    arg = var_class_type,
    values = c("Date", "POSIXct", "POSIXt", "hms", "difftime"),
    multiple = TRUE
  )
  if ("Date" %in% var_class_type) var_notes <- paste0("Date: YYY-MM-DD")
  if (any(c("POSIXct", "POSIXt") %in% var_class_type)) {
    var_notes <- paste0("POSIXct: YYY-MM-DD")
  }
  if (any(c("hms", "difftime") %in% var_class_type)) {
    var_notes <- paste0("hms: HH-MM-SS")
  }

  summary_result <- tibble(
    field_name = var_name,
    field_class = var_class_type[1],
    field_label = convert_null_into_na(var_label),
    field_notes = var_notes
  ) %>%
    select(field_name, field_class, field_label, field_notes) %>%
    verify(nrow(.) == 1)

  return(summary_result)
}

#' @rdname summarize_var
summarize_var.POSIXct <-
  summarize_var.POSIXt <-
  summarize_var.hms <-
  summarize_var.difftime <-
  summarize_var.Date


## Utils functions for summarizing variable ----
#' @title Generate pre-specified variable class type
#' @return A character vector
#' @rdname get_class_list
#' @keywords utils_fun

get_class_list <- function() {
  return(c(
    "fct", "character", "numeric", "double", "integer",
    "Date", "POSIXct", "POSIXt", "hms", "difftime", "logical"
  ))
}


#' @title Remove variable-specific class type from a data.frame structure
#' @inheritParams summarize_variable
#' @return Similar to the input data.frame with out variable-specific class type
#' @seealso
#'  \code{\link[set_var_class]()}
#' @rdname remove_var_class
#' @keywords utils_fun

remove_var_class <- function(.data) {
  data_class <- class(.data)
  other_class <- data_class[!data_class %in% get_class_list()]
  .data <- structure(.data, class = other_class)
  return(.data)
}

#' @title Set Variable Specific Class Type
#' @inheritParams summarize_variable
#' @return A data.frame with additional variable-specific class type structure
#' @examples
#' \dontrun{
#' library(tidyverse)
#' library(ADNIMERGE2)
#'
#' class(ADNIMERGE2::DM)
#'
#' # Add structure class for 'ADNIMERGE2::DM' based on age variable class type
#'
#' result_data <- set_var_class(
#'   .data = ADNIMERGE2::DM,
#'   var_name = "AGE"
#' )
#'
#' class(result_data)
#' }
#' @seealso
#'  \code{\link[remove_var_class]()}
#' @rdname set_var_class
#' @keywords utils_fun
#' @importFrom dplyr rename_with
#' @importFrom tidyselect all_of

set_var_class <- function(.data, var_name) {
  .data <- remove_var_class(.data)
  var_class <- .data %>%
    rename_with(~ paste0("temp_var"), all_of(var_name))
  var_class <- class(var_class$temp_var)
  if (any("factor" %in% var_class)) var_class <- "fct"
  .data <- structure(.data, class = c(class(.data), var_class))
  return(.data)
}

# Summarize A Dataset ----
#' @title Function to summarize all the available variables in a dataset
#' @description
#'  This function is used to summarize a data.frame that could be used to
#'  generate a data dictionary file.
#' @param dataset_name Dataset name/label, Default: NULL
#' @inheritParams summarize_variable
#' @inherit summarize_variable return
#' @details
#' A returned data.frame will also contain the following appended columns:
#'  \item{num_rows}{Number of observations/records}
#'  \item{num_cols}{Number of columns/variables}
#' @examples
#' \dontrun{
#' library(tidyverse)
#' library(ADNIEMRGE2)
#' library(cli)
#'
#' summarize_dataset(
#'   .data = ADNIMERGE2::DM,
#'   dataset_name = "DM",
#'   wide_format = FALSE
#' )
#' }
#' @rdname summarize_variable
#' @keywords data_summary
#' @seealso \code{\link{summarize_variable}}
#' @importFrom dplyr bind_rows mutate relocate select
#' @importFrom cli cli_abort
#' @export

summarize_dataset <- function(.data, dataset_name = NULL, wide_format = FALSE) {
  dd_name <- NULL
  num_rows <- nrow(.data)
  num_cols <- ncol(.data)

  if (is.null(dataset_name)) tbl_name <- "TEMP_TBL" else tbl_name <- dataset_name
  data_dict_dd <- lapply(colnames(.data), function(x) {
    summarize_variable(.data = .data, var_name = x, wide_format = wide_format)
  }) %>%
    bind_rows() %>%
    mutate(
      dd_name = tbl_name,
      num_rows = num_rows,
      num_cols = num_cols
    ) %>%
    relocate(dd_name, num_rows, num_cols) %>%
    {
      if (is.null(dataset_name)) {
        select(., -dd_name)
      } else {
        (.)
      }
    }

  if (wide_format && nrow(data_dict_dd) != length(colnames(.data))) {
    cli::cli_abort(
      message = c(
        "Discrepancy between number of rows in {.val data_dict_dd} and number of columns in {.val .data}. \n",
        "{.clas {nrow(data_dict_dd)}} rows; {.clas {length(colnames(.data))}} columns."
      )
    )
  }
  return(data_dict_dd)
}

# roxygen2 Document ------

#' @title Function to generate roxygen document for single/multiple dataset(s)
#'
#' @description
#' This function is used to generate roxygen2 document for single/multiple dataset based on either
#'  the actual dataset value or pre specified data dictionary.
#'
#' @param data_name Dataset name
#' @param data_name Dataset name
#' @param data_label Dataset label, Default: NULL
#' @param .data Data.frame and only applicable if \code{.data_dict} is missing, Default: NULL
#' @param .data_dict A data dictionary and only applicable if \code{.data} or \code{data_list} is missing, Default: NULL
#' @param source_type Data source type either 'raw', 'derived' or 'external', Default: 'raw'
#' @param field_nameVar Variable name that contains field names and only applicable if \code{.data_dict} is non-missing, Default: NULL
#' @param field_classVar Variable name that contains field class and only applicable if \code{.data_dict} is non-missing, Default: NULL
#' @param field_labelVar Variable name that contains field label and only applicable if \code{.data_dict} is non-missing, Default: NULL
#' @param field_notesVar Variable name that contains field notes and only applicable if \code{.data_dict} is non-missing, Default: NULL
#' @param tag_list
#'   A list object that contains roxygen2 tag names with corresponding values, Default: list()
#'   \code{tag_list} should be a named list object.
#'   For \code{\link{generate_single_dataset_roxygen}()}, \code{tag_list} will be a list object with a format of \code{list(tag_name = tag_value)}.
#'   For \code{\link{}(generate_roxygen_document)}, \code{tag_list} will be a list object with a format of \code{list("data_name" = list(tag_name = tag_value))}.
#' @param output_path Output file path. It must be non-missing (i.e., `NULL`) if the interest is to save the script in local directory.
#' @param output_path Output file path. It must not be missing (i.e., `NULL`) if the interest is to write the roxygen2 script in local directory.
#' @return
#'  Both \code{\link{generate_single_dataset_roxygen}()} and \code{\link{generate_roxygen_document}()} functions return the following values:
#' \itemize{
#'   \item A data.frame of two columns: dataset name (\code{data_name}) and roxygen script (\code{data_doc}) if \code{output_path} is missing (i.e., `NULL`)
#'   \item Otherwise created an \code{output_path} R script in local directory that contains the roxygen document
#'  }
#' @name roxygen2-doc
#' @keywords roxygen_utils
#' @export
NULL


##  Generate roxygen2 document for single dataset ----
#' @rdname roxygen2-doc
#' @examples
#' \dontrun{
#' library(tidyverse)
#' library(ADNIMERGE2)
#'
#' # To create a data dictionary for a dataset
#' temp_data_dict <- summarize_dataset(
#'   .data = ADNIMERGE2::DM,
#'   dataset_name = "DM",
#'   wide_format = TRUE
#' ) %>%
#'   mutate(data_name = "DM", dataset_source_type = "derived")
#' data_doc <- generate_roxygen_single_dataset(
#'   data_name = "DM",
#'   .data_dict = temp_data_dict,
#'   source_type = "derived",
#' )
#'
#' # Save in temporary (local) directory
#' temp_file <- tempfile()
#' generate_roxygen_single_dataset(
#'   data_name = "DM",
#'   .data_dict = temp_data_dict,
#'   source_type = "derived",
#'   output_path = temp_file
#' )
#' }
#' @importFrom rlang arg_match
#' @importFrom stringr str_c str_remove_all str_to_upper
#' @importFrom cli cli_abort
#' @importFrom dplyr select distinct
#' @importFrom roxygen2 tags_list
#' @importFrom tibble tibble
#' @importFrom readr write_lines

generate_roxygen_single_dataset <- function(data_name, data_label = NULL,
                                            .data = NULL, .data_dict = NULL,
                                            source_type = "raw",
                                            field_nameVar = NULL, field_classVar = NULL,
                                            field_labelVar = NULL, field_notesVar = NULL,
                                            tag_list = list(),
                                            output_path = NULL) {
  rlang::arg_match(
    arg = source_type,
    values = c("raw", "derived", "external"),
    multiple = TRUE
  )
  check_object_type(tag_list, "list")

  source_type <- str_to_upper(source_type)
  if (is.null(data_label)) data_label <- data_name
  data_source_label <- ifelse(source_type == "RAW", " ", str_c("[", source_type, "]"))
  if (is.null(.data_dict) && is.null(.data)) {
    cli_abort(
      message = paste0(
        "Either a data.frame of actual data {.val .data} or ",
        "prepared data dictionary {.val .data_dict} must not be missing."
      )
    )
  }

  if (!is.null(.data_dict) && !is.null(.data)) {
    cli_abort("Only one of {.val .data} and {.val .data_dict} must not be missing")
  }

  # To prepared a data dictionary if it is not missing
  ## i.e. based on the actual dataset value
  if (!is.null(.data) && is.null(.data_dict)) {
    # summarize a dataset
    temp_summarized_dd <- summarize_dataset(
      .data = .data,
      dataset_name = data_name,
      wide_format = TRUE
    ) %>%
      select(dd_name, field_name, field_class, field_label, field_notes, num_rows, num_cols) %>%
      distinct()
  }

  # Generate the document based on pre-specified data dictionary dataset
  if (is.null(.data) && !is.null(.data_dict)) {
    if (any(!c("num_rows", "num_cols") %in% colnames(.data_dict))) {
      cli_abort(
        message = "{.val num_rows} and {.val num_cols} are not included in the {.cla .data_dict}"
      )
    }
    temp_summarized_dd <- .data_dict
  }

  # Checking for a single dataset name
  if (length(unique(temp_summarized_dd$dd_name)) != 1) {
    cli_abort(message = "Only one dataset name must be existed.")
  }

  temp_summarized_dd <- data_dict_column_names(
    data_dict = temp_summarized_dd,
    field_nameVar = field_nameVar,
    field_classVar = field_classVar,
    field_labelVar = field_labelVar,
    field_notesVar = field_notesVar
  )

  if (length(tag_list) == 0) {
    # Default values
    tag_list <- list(
      author = NA, description = NA, family = NA,
      rdname = NA, references = NA, seealso = NA,
      source = NA
    )
  }
  check_list_names(tag_list)
  exclude_tags <- c("title", "usage", "docType")
  tag_list <- tag_list[!names(tag_list) %in% exclude_tags]
  tag_list <- tag_list[names(tag_list) %in% roxygen2::tags_list(built_in = FALSE)]

  if (!"keyword" %in% names(tag_list)) {
    tag_list$keywords <- str_c(tolower(source_type), "_dataset")
  }

  format_description <- str_c(
    "#' @format A data frame with ", unique(temp_summarized_dd$num_rows),
    " observations and ", unique(temp_summarized_dd$num_cols), " variables. \n"
  )

  fields_attr <- lapply(temp_summarized_dd$nameVar, function(x) {
    generate_variable_format(
      data_dict = temp_summarized_dd,
      var_name = x,
      field_nameVar = "nameVar",
      field_classVar = "classVar",
      field_labelVar = "labelVar",
      field_notesVar = "notesVar"
    )
  })

  fields_attr <- unlist(fields_attr)
  fields_attr <- str_c(fields_attr, collapse = "\n")

  data_doc <- str_c(
    str_c("#' @title ", data_source_label, " ", str_remove_all(data_label, "\\[|\\]"), "\n"),
    "#'\n",
    str_c("#' @docType data \n"),
    "#'\n",
    str_c("#' @usage data(", data_name, ") \n"),
    "#'\n",
    format_description,
    "#' @details\n",
    "#' \\itemize{\n",
    fields_attr,
    "\n#' } \n",
    collapse = ""
  )

  for (i in seq_along(tag_list)) {
    data_doc <- concat_tag(
      x = data_doc,
      tag_name = names(tag_list[i]),
      tag_value = tag_list[[names(tag_list[i])]],
      error_call = FALSE
    )
  }

  data_doc <- str_c(data_doc, str_c('"', data_name, '"\n'), "\n", collapse = "")

  if (is.na(data_doc)) {
    cli_abort(
      message = "{.val data_doc} must not be missing",
      .call = rlang::caller_env()
    )
  }

  # As a data.frame output
  if (is.null(output_path)) {
    return(
      tibble(
        data_doc = data_doc,
        data_name = data_name
      )
    )
  }

  # To write in script file
  if (!is.null(output_path)) {
    if (file.exists(output_path) == TRUE) readr::write_lines(x = "", output_path)
    cat(str_c(data_doc, collapse = "\n"),
      file = output_path,
      append = FALSE
    )
    return(TRUE)
  }
}


##  Generate roxygen2 document for multiple datasets ----

#' @rdname roxygen2-doc
#' @param data_names List of data names
#' @param roxygen_source
#' An indicator to generate the roxygen document either directly from actual
#'   dataset (\code{data_list}) or pre-generated data dictionary dataset
#'   (\code{data_dict}). Default: "data_list"
#' @param data_list
#'  List object of actual dataset and only applicable if \code{roxygen_source = 'data_list'}, Default: NULL.
#'  To use it, it must be a named list object with corresponding dataset name.
#' @param data_label_list
#'   A list object with dataset label, Default: list()
#'   For \code{roxygen_source = 'data_list'}, \code{data_label_list} must not be missing.
#'   For \code{roxygen_source = 'data_dict'}, dataset labels can be provided either in \code{data_label_list} or included in the data dictionary data.frame with \code{data_label} column.
#' @param source_type_list
#'   A list object with data source type, Default: list()
#'   For \code{roxygen_source = 'data_list'}, \code{source_type_list} must not be missing.
#'   For \code{roxygen_source = 'data_dict'}, data source type can be provided either in \code{source_type_list} or included in the data dictionary data.frame with \code{source_type} column.
#' @param overwrite A Boolean value to overwrite for existing output file \code{output_path}, Default: FALSE
#'
#' @examples
#' \dontrun{
#' # To generate roxygen document directly from actual datasets
#' library(tidyverse)
#' library(ADNIMERGE2)
#'
#' temp_listed_dd <- list(DM = ADNIMERGE2::DM, AE = ADNIMERGE2::AE)
#'
#' dataset_label_list <- list(
#'   DM = "Demographic Characteristic Records",
#'   AE = "Adverse Event Records"
#' )
#'
#' dataset_source_type <- list(DM = "derived", AE = "derived")
#'
#' generate_roxygen_document(
#'   data_names = names(temp_listed_dd),
#'   roxygen_source = "data_list",
#'   data_list = temp_listed_dd,
#'   data_label_list = dataset_label_list,
#'   source_type_list = dataset_source_type,
#'   output_path = NULL,
#'   tag_list = list(
#'     DM = list(),
#'     AE = list()
#'   )
#' )
#' }
#' @importFrom rlang arg_match0 arg_match
#' @importFrom cli cli_abort
#' @importFrom purrr pluck
#' @importFrom dplyr filter bind_rows select
#' @importFrom readr write_lines
#' @export

generate_roxygen_document <- function(data_names,
                                      roxygen_source = "data_list",
                                      data_list = NULL, .data_dict = NULL,
                                      data_label_list = list(),
                                      source_type_list = list(),
                                      field_nameVar = NULL, field_classVar = NULL,
                                      field_labelVar = NULL, field_notesVar = NULL,
                                      output_path = NULL, overwrite = FALSE,
                                      tag_list = list()) {
  arg_match0(arg = roxygen_source, values = c("data_list", "data_dict"))
  check_object_type(overwrite, "logical")
  if (is.null(field_nameVar)) field_nameVar <- "field_name"
  if (is.null(field_classVar)) field_classVar <- "field_class"
  if (is.null(field_labelVar)) field_labelVar <- "field_label"
  if (is.null(field_notesVar)) field_notesVar <- "field_notes"

  # If a list of actual data (data_list) is provided
  if (roxygen_source %in% "data_list") {
    if (is.null(data_list)) cli_abort(message = "{.val data_list} must not be missing")
    # Checking for name of listed dataset
    check_list_names(obj = data_list, list_names = data_names)
    check_list_names(obj = tag_list, list_names = data_names)
    check_list_names(obj = data_label_list, list_names = data_names)
    check_list_names(obj = source_type_list, list_names = data_names)
  }

  # If a data dictionary (data_dict) is provided
  if (roxygen_source %in% "data_dict") {
    if (is.null(.data_dict)) cli_abort(message = c("{.var .data_dict} must not be missing."))

    temp_arg_list <- c("dd_name", field_nameVar, field_classVar, field_labelVar, field_notesVar)
    arg_match(
      arg = temp_arg_list,
      values = colnames(.data_dict),
      multiple = TRUE
    )

    # Default tag values from a data dictionary variables
    if (length(tag_list) == 0) {
      tag_list <- get_tag_list(.data_dict)
    }

    if (length(data_label_list) == 0) {
      data_label_data <- create_vars(.data = .data_dict, var_name = "data_label")
      data_label_list <- get_prep_list(data_dict = data_label_data, var_name = "data_label")
    }

    if (length(source_type_list) == 0) {
      source_type_data <- create_vars(.data = .data_dict, var_name = "source_type")
      source_type_list <- get_prep_list(data_dict = source_type_data, var_name = "source_type")
    }

    check_list_names(obj = tag_list, list_names = data_names)
    check_list_names(obj = data_label_list, list_names = data_names)
    check_list_names(obj = source_type_list, list_names = data_names)
  }

  output_result <- lapply(data_names, function(data_name) {
    if (roxygen_source %in% "data_list") {
      temp_dd <- data_list %>%
        pluck(., data_name)
      field_nameVar <- field_classVar <- field_labelVar <- field_notesVar <- NULL
      temp_data_dict <- NULL
    }

    if (roxygen_source %in% "data_dict") {
      temp_dd <- NULL
      temp_data_dict <- .data_dict %>%
        filter(dd_name %in% data_name)
    }

    # Current data label
    temp_data_label <- data_label_list[[data_name]]
    temp_data_label <- convert_na_into_null(as.character(temp_data_label))
    # Current data source type
    temp_data_source <- source_type_list[[data_name]]
    temp_data_source <- convert_na_into_null(as.character(temp_data_source))

    temp_single_dd <- generate_roxygen_single_dataset(
      data_name = data_name,
      data_label = temp_data_label,
      source_type = temp_data_source,
      .data = temp_dd,
      .data_dict = temp_data_dict,
      field_nameVar = field_nameVar,
      field_classVar = field_classVar,
      field_labelVar = field_labelVar,
      field_notesVar = field_notesVar,
      tag_list = tag_list[[data_name]],
      output_path = NULL
    )
    return(temp_single_dd)
  }) %>%
    bind_rows() %>%
    select(data_doc, data_name)

  # Final Outputs
  if (is.null(output_path)) {
    return(output_result)
  }

  if (!is.null(output_path)) {
    if (!file.exists(output_path)) cli_abort(message = "{.path {output_path}} not found")
    if (overwrite == FALSE) readr::write_lines(x = "", output_path)
    output_scripts <- paste0(output_result$data_doc, collapse = "\n")
    if (is.na(output_scripts)) cli_abort(message = "{.file output_path} has not been updated/created.")
    cat(output_scripts, file = output_path, append = TRUE)
    return(TRUE)
  }
}

## Utils functions for preparing data for roxygen2 document ----

#' @title Get column list with corresponding value(s) from a data dictionary
#' @param data_dict A data dictionary data.frame
#' @param var_name Variable name, only applicable if \code{is_tag_list} is \code{FALSE}
#' @param is_tag_list A Boolean to apply the function for roxygene2 related tag columns
#' @return A list object for data name specific variable name and variable value
#' @rdname get_prep_list
#' @importFrom roxygen2 tags_list
#' @importFrom dplyr select distinct group_by group_nest ungroup mutate
#' @importFrom assertr assert is_uniq
#' @importFrom purrr map
#' @importFrom tidyselect any_of all_of

get_prep_list <- function(data_dict, var_name = NULL, is_tag_list = FALSE,
                          arg = rlang::caller_arg(var_name), call = rlang::caller_env()) {
  data <- NULL
  check_object_type(is_tag_list, "logical")
  if (is_tag_list) {
    col_names <- roxygen2::tags_list(built_in = FALSE)
  } else {
    col_names <- var_name
  }

  if (length(col_names) == 0 || any(is.na(col_names))) {
    cli::cli_abort(
      message = "{.var {arg}} must not be missing",
      call = call
    )
  }

  output_data <- data_dict %>%
    select(any_of(c("dd_name", col_names))) %>%
    distinct() %>%
    assert(is_uniq, all_of("dd_name")) %>%
    group_by(across(all_of("dd_name")), .add = TRUE) %>%
    group_nest() %>%
    ungroup() %>%
    mutate(data = map(data, as.list))

  names(output_data$data) <- output_data$dd_name

  return(output_data$data)
}

#' @title Get tag list from a data dictionary
#' @inheritParams get_prep_list
#' @return A list object for data name specific tag name and tag value
#' @rdname get_tag_list

get_tag_list <- function(data_dict) {
  get_prep_list(data_dict = data_dict, var_name = NULL, is_tag_list = TRUE)
}

#' @title Add Missing Variable
#' @description
#'  A function to a variable with missing value if it does not exist in the dataset.
#' @param .data A data.frame
#' @param var_name Variable name
#' @return A data.frame with appended column of \code{var_name}
#' @examples
#' \dontrun{
#' library(tidyverse)
#' test_data <- tibble(
#'   letter_names = letters[1:12],
#'   letter_order = 1:12
#' )
#' # To add description variable "description"
#' test_data1 <- create_vars(
#'   .data = test_data,
#'   var_name = "description"
#' )
#' names(test_data1)
#'
#' test_data2 <- create_vars(
#'   .data = test_data,
#'   var_name = "letter_order"
#' )
#' names(test_data2)
#' }
#' @seealso
#'  \code{\link[cli]{cli_alert}}
#' @rdname create_vars
#' @export
#' @importFrom cli cli_alert_info
#' @importFrom dplyr mutate rename_with
#' @importFrom tidyselect all_of

create_vars <- function(.data, var_name) {
  temp_var <- NULL
  if (!var_name %in% colnames(.data)) {
    cli::cli_alert_info(c(
      "{.var {var_name}} variable is not found \n",
      "{.val {var_name}} will be added with missing value!"
    ))
  }
  .data <- .data %>%
    {
      if (!var_name %in% colnames(.data)) {
        mutate(., temp_var = NA) %>%
          rename_with(., ~ paste0(var_name), all_of("temp_var"))
      } else {
        (.)
      }
    }
  return(.data)
}

#' @title Concatenate roxygen2 tags
#' @param x Single character
#' @param tag_name Tag name, see \code{roxygen2::tags_list(built_in = FALSE)}
#' @param tag_value Tag value, Default: NULL
#' @param error_call A Boolean to check for missing tag value, Default: TRUE
#' @return A character vector with added tag value
#' @examples
#' \dontrun{
#' x <- paste0("#' @title Demographic Dataset \n")
#' concat_tag(x, tag_name = "keyword", tag_text = "derived")
#' }
#' @seealso
#'  \code{\link[cli]{cli_abort}}
#' @rdname concat_tag
#' @export
#' @importFrom cli cli_abort

concat_tag <- function(x, tag_name, tag_value = NULL, error_call = TRUE) {
  require(stringr)
  if (length(tag_name) != 1) {
    cli::cli_abort(
      message = c(
        "{.var tag_name} must be size of one. \n",
        "{.var tag_name} is a size of {.val {length(tag_name)}} object"
      )
    )
  }
  status <- ifelse(!is.null(tag_value), TRUE, FALSE)
  status <- ifelse(!is.na(tag_value), TRUE, FALSE)

  if (status) {
    x <- str_c(x, str_c("#' @", tag_name, " ", tag_value, "\n"), collapse = "")
  }
  if (status) {
    if (error_call) cli::cli_abort(message = "{.var tag_value} must not be missing.")
  }
  if (is.na(x)) cli::cli_abort(message = "{.var x} must not be missing.")
  return(x)
}

#' @title Function to prepare a data dictionary dataset
#' @description
#'  This function is used to prepare the data data dictionary to generate
#'  variable specific format list in roxygen document.
#' @param data_dict
#'  A data dictionary data.frame that contains variable name, variable class
#'  type, variable label, and associated description about its value
#' @inheritParams roxygen2-doc
#' @return
#'   A data.frame the same as the provided data dictionary `data_dict` with
#'   following renamed columns:
#'   \item{field_nameVar}{ to \strong{nameVar}}
#'   \item{field_classVar}{ to \strong{classVar}}
#'   \item{field_labelVar}{ to \strong{labelVar}}
#'   \item{field_notesVar}{ to \strong{notesVar}}
#' @examples
#' \dontrun{
#' # Generate a data dictionary for single dataset
#' library(tidyverse)
#' library(ADNIMERGE2)
#'
#' temp_data_dict <- summarize_dataset(
#'   .data = ADNIMERGE2::DM,
#'   dataset_name = "DM",
#'   wide_format = TRUE
#' )
#'
#' # Renamed the following pre-specified columns:
#' # i.e.: field_nameVar, field_classVar, field_labelVar, and field_notesVar
#' data_dict_column_names(data_dict = temp_data_dict)
#' }
#' @rdname data_dict_column_names
#' @keywords utils_fun
#' @importFrom tibble tibble
#' @importFrom dplyr mutate rename_with filter across pull
#' @importFrom tidyselect any_of

data_dict_column_names <- function(data_dict,
                                   field_nameVar = NULL, field_classVar = NULL,
                                   field_labelVar = NULL, field_notesVar = NULL) {
  if (is.null(field_nameVar)) field_nameVar <- "field_name"
  if (is.null(field_classVar)) field_classVar <- "field_class"
  if (is.null(field_labelVar)) field_labelVar <- "field_label"
  if (is.null(field_notesVar)) field_notesVar <- "field_notes"
  # Combined all specified name list and corresponding renamed list
  column_list_dd <- tibble(
    specified_name_list = c(field_nameVar, field_classVar, field_labelVar, field_notesVar),
    renamed_list = c("nameVar", "classVar", "labelVar", "notesVar")
  )

  return(
    data_dict %>%
      mutate(across(any_of(column_list_dd$specified_name_list) & where(is.factor), as.character)) %>%
      rename_with(
        ~ paste0(column_list_dd %>% filter(specified_name_list == .x) %>% pull(renamed_list)),
        any_of(column_list_dd$specified_name_list)
      )
  )
}

## Adjust variable format in roxygen files -----
#' @title Function to generate variable format list
#' @description
#'  This function is used to generate the variable format list for roxygen document.
#' @inheritParams data_dict_column_names
#' @param var_name Single character vector of variable name
#' @return A free text that will be used in the roxygen document
#' [\strong{Variable Name}] [\emph{Variable Class Type}]
#'   [Variable Label]  [variable description details]
#' @examples
#' \dontrun{
#' # Generate a data dictionary for single dataset
#' library(tidyverse)
#' library(ADNIMERGE2)
#'
#' temp_data_dict <- summarize_dataset(
#'   .data = ADNIMERGE2::DM,
#'   dataset_name = "DM",
#'   wide_format = TRUE
#' )
#'
#' # Generate a variable format list
#' generate_variable_format(
#'   data_dict = temp_data_dict,
#'   var_name = "SEX"
#' )
#' }
#' @rdname generate_variable_format
#' @keywords utils_fun
#' @importFrom dplyr filter
#' @importFrom cli cli_abort

generate_variable_format <- function(data_dict, var_name,
                                     field_nameVar = NULL, field_classVar = NULL,
                                     field_labelVar = NULL, field_notesVar = NULL) {
  temp_dd <- data_dict_column_names(
    data_dict = data_dict,
    field_nameVar = field_nameVar,
    field_classVar = field_classVar,
    field_labelVar = field_labelVar,
    field_notesVar = field_notesVar
  ) %>%
    filter(nameVar %in% var_name)

  label_value <- ifelse(is.na(temp_dd$labelVar), " ", temp_dd$labelVar)
  note_value <- ifelse(is.na(temp_dd$notesVar), " ", temp_dd$notesVar)
  if (is.na(temp_dd$classVar)) {
    cli_abort(message = "{.var classVar} must not be missing")
  }
  if (is.na(temp_dd$nameVar)) {
    cli_abort(message = "{.var nameVar} must not be missing")
  }

  return(
    paste0("#' \\item \\strong{", temp_dd$nameVar, "}",
      " \\emph{", temp_dd$classVar, "} ", label_value, " ", note_value, "",
      collapse = ""
    )
  )
}

# Common utils functions ----
## Check list object names -----
#' @title Function to check list object names
#' @description This function is used to check whether list names are a valid non-missing names
#' @param obj List object
#' @param list_names
#'  A character vector of list names, Default NULL
#' @param arg see \code{\link{rlang}{caller_arg}}
#' @param call see \code{\link{rlang}{caller_env}}
#' @return
#'  An error message if there is any missing or misspelled names in the list object.
#' @rdname check_list_names
#' @keywords utils_fun
#' @importFrom cli cli_abort
#' @importFrom rlang call_args caller_env

check_list_names <- function(obj, list_names = NULL, arg = rlang::caller_arg(obj), call = rlang::caller_env()) {
  check_object_type(obj, "list")
  # Checking for any unnamed list
  if (any(is.null(names(obj)) | is.na(names(obj)))) {
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
        "{.val {missing_names}} names{?s} {?is/are} are not presented in list names."
      ),
      call = call
    )
  }

  invisible(missing_names)
}

## Convert missing value types ----
#' @title Convert `NULL` into `NA`
#' @param x Input value
#' @return A character vector with updated values.
#' @rdname convert_null_into_na
#' @keywords utils_fun
#' @family convert missing value type

convert_null_into_na <- function(x) {
  x <- ifelse(is.null(x), NA_character_, x)
  return(x)
}

#' @title Convert `NA` into `NULL`
#' @param x Input value
#' @return A character vector with updated values.
#' @rdname convert_na_into_null
#' @keywords utils_fun
#' @family convert missing value type

convert_na_into_null <- function(x) {
  x <- ifelse(is.na(x), NULL, x)
  return(x)
}
