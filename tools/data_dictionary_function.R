# Help Functions ----
#' @title Function to remove NA from a character vector
#' @description This function is used to remove missing `NA` from a character vector
#' @param x Character vector
#' @return Character vector
#' @rdname remove_na
remove_na <- function(x) {
  return(x[!is.na(x)])
}

#' @title Function to return NULL for any missing value existed
#' @param dd_df Data frame
#' @param var_name Variable name
#' @param single_value_check Checking for single unique value, Default: TRUE
#' @return Returned `NULL` if there is any missing value in the variable, Otherwise returned unique value of the variable.
#' @rdname return_null_missing
return_null_missing <- function(dd_df, var_name, single_value_check = TRUE) {
  # If the variable is existed
  if (var_name %in% colnames(dd_df)) {
    var_value_list <- dd_df %>%
      select(all_of(var_name)) %>%
      pull()
  } else {
    var_value_list <- NA_character_
  }

  unique_value_list <- unique(var_value_list)

  if (any(is.na(unique_value_list))) {
    return(NULL)
  }

  if (any(is.na(unique_value_list)) == FALSE) {
    # Checking for single unique value
    if (single_value_check == TRUE & length(unique_value_list) > 1) stop(str_c("There are more than one unique values ", var_name, " variable"))
    return(unique_value_list)
  }
}

#' @title Function to check the name of listed objects
#' @description This function is used to check whether list names are a valid non-missing names
#' @param listed_dd Listed data frame
#' @param listed_names The named of the objects that needed to be checked, Default NULL
#' @return Returned a stopped message if there are missing/NULL/NA names in the listed data frame. Otherwise returned `NULL`.
#' @rdname check_list_names
check_list_names <- function(listed_dd, listed_names = NULL) {
  # If there are any unnamed lists
  if (any(is.null(names(listed_dd)) | is.na(names(listed_dd)))) stop("Check the list name in listed_dd")

  # To check for any missed listed names
  if (all(is.null(listed_names))) missing_names <- NULL

  if (any(!is.null(listed_names))) {
    missing_names <- listed_names[!listed_names %in% names(listed_dd)]
  }

  if (length(missing_names) > 0) stop(str_c("Check for listed names of ", toString(missing_names)))
}
# Function prepare a data dictionary dataset
#' @title Function to prepare a data dictionary dataset
#' @description This function is used to prepare the data data dictionary for generating variable specific format list in roxygen document.
#' @param dd A data dictionary data.frame that contains variable name, variable class type, variable label,  and associated description about its value
#' @param field_nameVar Column name that contains all dataset variable names, Default: NULL
#' @param field_classVar Column name that contains all variable class type, Default: NULL
#' @param field_labelVar Column name that contains all variable labels, Default: NULL
#' @param field_notesVar Column name that contains all variable description notes, Default: NULL
#' @return A data.frame the same as the provided data dictionary `data_dict` with following renamed columns:
#' \itemize{
#'   \item{field_nameVar}{ to \strong{nameVar}}
#'   \item{field_classVar}{ to \strong{classVar}}
#'   \item{field_labelVar}{ to \strong{labelVar}}
#'   \item{field_notesVar}{ to \strong{notesVar}}
#' }
#' @examples
#' \dontrun{
#' # Generate a data dictionary of a dataset
#' temp_data_dict <- summarize_dataset(dd = ADNI4::DM, dataset_name = "DM", wider_format = TRUE)
#' # Renamed pre-specified columns (i.e. field_nameVar, field_classVar, field_labelVar, field_notesVar)
#' data_dict_column_names(data_dict = temp_data_dict)
#' }
#' @rdname data_dict_column_names
data_dict_column_names <- function(data_dict,
                                   field_nameVar = NULL,
                                   field_classVar = NULL,
                                   field_labelVar = NULL,
                                   field_notesVar = NULL) {
  require(tidyverse)

  if (is.null(field_nameVar)) field_nameVar <- "field_name"
  if (is.null(field_classVar)) field_classVar <- "field_class"
  if (is.null(field_labelVar)) field_labelVar <- "field_label"
  if (is.null(field_notesVar)) field_notesVar <- "field_notes"
  # Combined all specified name list and corresponding renamed list
  column_list_dd <- tibble::tibble(
    specified_name_list = c(field_nameVar, field_classVar, field_labelVar, field_notesVar),
    renamed_list = c("nameVar", "classVar", "labelVar", "notesVar")
  )

  return(data_dict %>%
    mutate(across(any_of(column_list_dd$specified_name_list) & where(is.factor), as.character)) %>%
    rename_with(
      ~ str_c(column_list_dd %>% filter(specified_name_list == .x) %>% pull(renamed_list)),
      any_of(column_list_dd$specified_name_list)
    ))
}

## Function that will generate roxygen files
#' @title Function to generate variable format list
#' @description This function is used to generate the variable format list for roxygen document.
#' @param data_dict A data frame that contains variable name, variable class type, variable label,  and associated description about its value
#' @param var_name Current variable name
#' @return A free text that will be used in the roxygen document
#' [\strong{Variable Name}] [\emph{Variable Class Type}]
#'   [Variable Label]  [variable description details]
#' @examples
#' \dontrun{
#' # Generate a data dictionary of a dataset
#' temp_data_dict <- summarize_dataset(dd = ADNI4::DM, dataset_name = "DM", wider_format = TRUE)
#' # Generate a variable format list
#' generate_variable_format_list(data_dict = temp_data_dict, var_name = "TRACK")
#' }
#' @rdname generate_variable_description
generate_variable_format_list <- function(data_dict,
                                          var_name,
                                          field_nameVar = NULL,
                                          field_classVar = NULL,
                                          field_labelVar = NULL,
                                          field_notesVar = NULL) {
  require(tidyverse)
  require(assertr)
  temp_dd <- data_dict_column_names(
    data_dict = data_dict,
    field_nameVar = field_nameVar,
    field_classVar = field_classVar,
    field_labelVar = field_labelVar,
    field_notesVar = field_notesVar
  ) %>%
    filter(nameVar %in% var_name)

  label_value <- ifelse(is.na(temp_dd$labelVar), " ", temp_dd$labelVar)

  return(str_c("#' \\item{\\strong{", temp_dd$nameVar, "} ",
    "\\emph{ ", temp_dd$classVar, "}}",
    "{", label_value, " ", temp_dd$notesVar, "}",
    collapse = ""
  ))
}

# Summary Function ----
## Summarize Factor Variable ----
#' @title Summarize Factor Variable
#' @description This function is used to summarize a factor variable from a data.frame that could be used to generate a data dictionary file.
#' @param dd Data frame
#' @param var_name Variable name
#' @param wider_format Indicator whether to generate the factor level values in a wider or long format, Default: FALSE
#' @return A data.frame that contains the following variables:
#' \itemize{
#'   \item{field_name}{Variable name}
#'   \item{field_class}{Variable class type: 'factor'}
#'   \item{field_label}{Variable label}
#'   \item{field_values_order}{Factor levels order number and only included for long format: wider_format = FALSE}
#'   \item{field_values}{Factor levels}
#'   \item{field_notes}{Text notes that could be used to generate the roxygen document for the dataset}
#' }
#' @examples
#' \dontrun{
#' summarize_factor_variable(dd = ADNI4::DM, var_name = "AMYSTATUSCL", wider_format = FALSE)
#' }
#' @rdname summarize_factor_variable
summarize_factor_variable <- function(dd, var_name, wider_format = FALSE) {
  require(tidyverse)
  require(labelled)
  require(assertr)
  var_values <- dd %>%
    select(all_of(var_name)) %>%
    pull()
  var_label <- labelled::get_variable_labels(var_values)
  var_class_type <- class(var_values)
  rlang::arg_match0(arg = var_class_type, values = "factor")
  # Add description for roxygen document
  var_notes <- str_c(
    "Factor variable with levels: ",
    str_c(levels(var_values), collapse = ", ")
  )
  summary_dataset <- tibble(
    field_values = levels(var_values),
    field_values_order = seq_len(length(levels(var_values))),
    field_name = var_name,
    field_class = var_class_type,
    field_label = ifelse(is.null(var_label), NA_character_, var_label),
    field_notes = var_notes
  ) %>%
    select(field_name, field_class, field_label, field_values_order, field_values, field_notes) %>%
    {
      if (wider_format == TRUE) {
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

  return(summary_dataset)
}

## Summarize Character Variable ----
#' @title Summarize Character Variable
#' @description This function is used to summarize a character variable from a data.frame that could be used to generate a data dictionary file.
#' @param dd Data frame
#' @param var_name  Variable name
#' @param wider_format Indicator whether to generate the character values in a wider or long format if there are less than 10 unique values, Default: FALSE
#' @return A data.frame that contains the following variables:
#' \itemize{
#'   \item{field_name}{Variable name}
#'   \item{field_class}{Variable class type: 'character'}
#'   \item{field_label}{Variable labels}
#'   \item{field_values}{Variable values and only computed if the number of values is less than 10}
#'   \item{field_values_order}{Variable values order number and only included for long format: wider_format = FALSE}
#'   \item{field_notes}{Text notes that could be used to generate the roxygen document for the dataset}
#' }
#' @examples
#' \dontrun{
#' summarize_character_variable(dd = ADNI4::DM, var_name = "TRACK", wider_format = FALSE)
#' }
#' @rdname summarize_character_variable
summarize_character_variable <- function(dd, var_name, wider_format = FALSE) {
  require(tidyverse)
  require(labelled)
  require(assertr)
  var_values <- dd %>%
    select(all_of(var_name)) %>%
    pull()
  var_label <- labelled::get_variable_labels(var_values)
  var_class_type <- class(var_values)
  rlang::arg_match0(arg = var_class_type, values = "character")
  # Warning message if number of item value is more than 10
  var_values <- remove_na(unique(var_values))
  var_notes <- str_c("Character variable with options: ", str_c(var_values, collapse = ", "))
  if (length(var_values) > 10) {
    warning(stringr::str_c("There are more than 10 values in ", var_name, " variable"))
    var_values <- NA_character_
    var_notes <- str_c(" ")
  }

  if (length(var_values) == 0) {
    warning(stringr::str_c("There are none values in ", var_name, " variable"))
    var_values <- NA_character_
    var_notes <- str_c(" ")
  }


  summary_dataset <- tibble(
    field_values = var_values,
    field_values_order = seq_len(length(var_values)),
    field_name = var_name,
    field_class = var_class_type,
    field_label = ifelse(is.null(var_label), NA_character_, var_label),
    field_notes = var_notes
  ) %>%
    mutate(field_values_order = ifelse(is.na(field_values), NA_real_, field_values_order)) %>%
    select(field_name, field_class, field_label, field_values_order, field_values, field_notes) %>%
    {
      if (wider_format == TRUE) {
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

  return(summary_dataset)
}
## Summarize Numeric Variable -----
#' @title Summarize Numeric Variable
#' @description This function is used to summarize a numeric variable from a data.frame that could be used to generate a data dictionary file.
#' @param dd Data frame
#' @param var_name Variable name
#' @param wider_format Indicator whether to generate the minimum and maximum values in a wide or long format, Default: FALSE
#' @return A data.frame that contains the following variables:
#' \itemize{
#'   \item{field_name}{Variable name}
#'   \item{field_class}{Variable class type: 'numeric'}
#'   \item{field_label}{Variable labels}
#'   \item{field_values}{Minimum and maximum values}
#'   \item{summary_type}{Summary type either 'Min' for minimum value or 'Max' for maximum value}
#'   \item{field_notes}{Text notes that could be used to generate the roxygen document for the dataset}
#' }
#' @examples
#' \dontrun{
#' summarize_numeric_variable(dd = ADNI4::DM, var_name = "AGE", wider_format = FALSE)
#' }
#' @rdname summarize_numeric_variable
summarize_numeric_variable <- function(dd, var_name, wider_format = FALSE) {
  require(tidyverse)
  require(labelled)
  require(assertr)
  var_values <- dd %>%
    select(all_of(var_name)) %>%
    pull()
  var_label <- labelled::get_variable_labels(var_values)
  var_class_type <- class(var_values)
  rlang::arg_match0(arg = var_class_type, values = c("numeric", "double", "integer"))

  # Minimum and Maximum Values
  min_value <- min(var_values, na.rm = TRUE)
  max_value <- max(var_values, na.rm = TRUE)

  var_notes <- str_c("Range value: ", min_value, ", ..., ", max_value)

  summary_dataset <- tibble::tibble(
    field_values = as.character(c(min_value, max_value)),
    summary_type = c("Min", "Max"),
    field_name = var_name, field_class = var_class_type,
    field_label = ifelse(is.null(var_label), NA_character_, var_label),
    field_notes = var_notes
  ) %>%
    select(field_name, field_class, field_label, summary_type, field_values, field_notes) %>%
    {
      if (wider_format == TRUE) {
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

  return(summary_dataset)
}

## Summarize Date Variable -----
#' @title Summarize Date Variable
#' @description This function is used to summarize a date variable from a data.frame that could be used to generate a data dictionary file.
#' @param dd Data frame
#' @param var_name Variable name
#' @return A data.frame that contains the following variables:
#' \itemize{
#'   \item{field_name}{Variable name}
#'   \item{field_class}{Variable class type: 'Date', 'POSIXct'. or 'POSIXt'}
#'   \item{field_label}{Variable labels}
#'   \item{field_notes}{Text notes that could be used to generate the roxygen document for the dataset}
#' }
#' @examples
#' \dontrun{
#' summarize_date_variable(
#'   dd = ADNI4::DM %>%
#'     mutate(RFSTDTC = as.Date(RFSTDTC)),
#'   var_name = "RFSTDTC"
#' )
#' }
#' @rdname summarize_date_variable
summarize_date_variable <- function(dd, var_name) {
  require(tidyverse)
  require(labelled)
  require(assertr)
  var_values <- dd %>%
    select(all_of(var_name)) %>%
    pull()

  var_label <- labelled::get_variable_labels(var_values)

  var_class_type <- class(var_values)
  rlang::arg_match(
    arg = var_class_type,
    values = c("Date", "POSIXct", "POSIXt"),
    multiple = TRUE
  )
  if ("Date" %in% var_class_type) var_notes <- str_c("Date: YYY-MM-DD") else var_notes <- str_c("POSIXct: YYY-MM-DD")

  summary_dataset <- tibble::tibble(
    field_name = var_name,
    field_class = var_class_type[1],
    field_label = ifelse(is.null(var_label), NA_character_, var_label),
    field_notes = var_notes
  ) %>%
    select(field_name, field_class, field_label, field_notes) %>%
    verify(nrow(.) == 1)

  return(summary_dataset)
}

## Summarize Logical Variable -----
#' @title Summarize Logical Variable
#' @description This function is used to summarize a logical variable from a data.frame that could be used to generate a data dictionary file.
#' @param dd Data frame
#' @param var_name Variable name
#' @return A data.frame that contains the following variables:
#' \itemize{
#'   \item{field_name}{Variable name}
#'   \item{field_class}{Variable class type: 'logical'}
#'   \item{field_label}{Variable labels}
#'   \item{field_notes}{Text notes that could be used to generate the roxygen document for the dataset}
#' }
#' @examples
#' \dontrun{
#' summarize_logical_variable(
#'   dd = ADNI4::DM %>%
#'     mutate(ENROLLFLAG = case_when(
#'       ENROLLFLAG %in% "Yes" ~ TRUE,
#'       ENROLLFLAG %in% "No" ~ FALSE
#'     )),
#'   var_name = "ENROLLFLAG"
#' )
#' }
#' @rdname summarize_logical_variable
summarize_logical_variable <- function(dd, var_name) {
  require(tidyverse)
  require(labelled)
  require(assertr)
  var_values <- dd %>%
    select(all_of(var_name)) %>%
    pull()
  var_label <- labelled::get_variable_labels(var_values)
  var_class_type <- class(var_values)
  rlang::arg_match0(arg = var_class_type, values = c("logical"))
  var_notes <- str_c("Boolean value: TRUE, FALSE or NA")
  summary_dataset <- tibble::tibble(
    field_values = str_c("TRUE or FALSE"),
    field_values_order = NA_real_,
    field_name = var_name,
    field_class = var_class_type,
    field_label = ifelse(is.null(var_label), NA_character_, var_label),
    field_notes = var_notes
  ) %>%
    select(
      field_name, field_class, field_label, field_values_order, field_values,
      field_notes
    ) %>%
    verify(nrow(.) == 1)


  return(summary_dataset)
}

## Summarize A Single Variable ----
#' @title Summarize A Single Variable
#' @description This function is used to summarize a single variable from a data.frame that could be used to generate a data dictionary file.
#' @param dd Data frame
#' @param var_name Variable name
#' @param wider_format Indicator whether to generate the minimum and maximum values in a wide or long format, Default: FALSE
#' @return A data.frame that contains the at least the following variables:
#' \itemize{
#'   \item{field_name}{Variable name}
#'   \item{field_class}{Variable class type: 'factor', 'character', 'date', 'POSIXct", "POSIXt" or 'logical'}
#'   \item{field_label}{Variable labels}
#'   \item{field_notes}{Text notes that could be used to generate the roxygen document for the dataset}
#' }
#' @examples
#' \dontrun{
#' summarize_variable(dd = ADNI4::DM, var_name = "TRACK", wider_format = FALSE)
#' }
#' @rdname summarize_variable
#' @seealso \code{\link{summarize_dataset}}
#' @export
summarize_variable <- function(dd, var_name, wider_format = FALSE) {
  require(tidyverse)
  require(assertr)

  dd <- dd %>% select(all_of(var_name))
  # Variable class type
  var_class_type <- class(dd %>% pull())
  specified_class_type <- c(
    "factor", "character", "numeric", "double", "integer",
    "Date", "POSIXct", "POSIXt", "logical"
  )
  rlang::arg_match(arg = var_class_type, values = specified_class_type, multiple = TRUE)

  if (any(var_class_type %in% specified_class_type[1])) summary_dd <- summarize_factor_variable(dd = dd, var_name = var_name, wider_format = wider_format)
  if (any(var_class_type %in% specified_class_type[2])) summary_dd <- summarize_character_variable(dd = dd, var_name = var_name, wider_format = wider_format)
  if (any(var_class_type %in% specified_class_type[3:5])) summary_dd <- summarize_numeric_variable(dd = dd, var_name = var_name, wider_format = wider_format)
  if (any(var_class_type %in% specified_class_type[6:8])) summary_dd <- summarize_date_variable(dd = dd, var_name = var_name)
  if (any(var_class_type %in% specified_class_type[9])) summary_dd <- summarize_logical_variable(dd = dd, var_name = var_name)

  summary_dd <- summary_dd %>%
    {
      if (wider_format == TRUE) {
        verify(., nrow(.) == 1)
      } else {
        (.)
      }
    }

  return(summary_dd)
}

## Summarize A Dataset ----
#' @title Function to summarize all the variables of a dataset
#' @description This function is used to summarize a data.frame that could be used to generate a data dictionary file.
#' @param dd Data frame
#' @param dataset_name Dataset label, Default: NULL
#' @param wider_format Indicator whether to generate result in a wide or long format, Default: FALSE
#' @return A data.frame that contains the at least the following variables:
#' \itemize{
#'   \item{dd_name}{Dataset name}
#'   \item{field_name}{Variable names}
#'   \item{field_class}{Variable class type: 'factor', 'character', 'date', 'POSIXct", "POSIXt" or 'logical'}
#'   \item{field_label}{Variable labels}
#'   \item{field_notes}{Text notes that could be used to generate the roxygen document for the dataset}
#' }
#' @examples
#' \dontrun{
#' summarize_variable(dd = ADNI4::DM, var_name = "TRACK", wider_format = FALSE)
#' }
#' @rdname summarize_variable
#' @seealso \code{\link{summarize_variable}}
#' @export
summarize_dataset <- function(dd,
                              dataset_name = NULL,
                              wider_format = FALSE) {
  require(tidyverse)
  data_dict_dd <- lapply(colnames(dd), function(x) {
    summarize_variable(dd = dd, var_name = x, wider_format = wider_format)
  }) %>%
    bind_rows() %>%
    mutate(
      dd_name = dataset_name,
      num_rows = nrow(dd),
      num_cols = ncol(dd)
    ) %>%
    relocate(dd_name, num_rows, num_cols)

  if (wider_format == TRUE && nrow(data_dict_dd) != length(colnames(dd))) stop("Check for the number of rows in data_dict_dd")

  return(data_dict_dd)
}

# Roxygen Documentation ----
## Generate roxygen document for a single dataset ----
#' @title Function to generate roxygen document for a single dataset
#' @description This function is used to generate roxygen document for a single dataset or based on pre specified data dictionary dataset.
#' @param dd Data frame of actual values
#' @param data_dict Prepared data dictionary dataset, Default: NULL
#' @param dataset_name Dataset name
#' @param dataset_label Dataset label, Default: NULL
#' @param dataset_source_type Dataset source type either raw dataset (`raw`), derived dataset (`derived`) or external dataset (`external`), Default: "raw".
#' @param short_description A short description about the dataset.
#' @param add_source To add dataset source description. Default: NULL. See more `@source`
#' @param add_seealso To add related links, Default: NULL. See more `@seealso`
#' @param add_authors To add authors name, Default: NULL. See more `@author`
#' @param add_reference To add reference description, Default: NULL. See more `@references`
#' @param add_rdname_prefix To add rdname prefix, Default: NULL.
#' @param output_file Whether to return string output or save in a local file (i.e. write in a file), Default: NULL
#' @param output_file_name Output file name. Should other than `NULL` if the interest is to store the result in local environment.
#' @return
#'  \itemize{
#'   \item{A data.frame of two columns: data_doc and dataset_name if output_file_name is `NULL`}
#'   \item{Otherwise stored the roxygen document in local directory with the specified file path: `output_file_name`}
#'  }
#' @examples
#' \dontrun{
#' # Generate a data dictionary of a dataset
#' temp_data_dict <- summarize_dataset(dd = ADNI4::DM, dataset_name = "DM", wider_format = TRUE) %>%
#'   mutate(dataset_name = "DM", dataset_source_type = "derived")
#' generate_single_dataset_roxygen(data_dict = temp_data_dict)
#' }
#' @rdname generate_single_dataset_roxygen
generate_single_dataset_roxygen <- function(dd = NULL,
                                            data_dict = NULL,
                                            field_nameVar = NULL, #
                                            field_classVar = NULL, #
                                            field_labelVar = NULL, #
                                            field_notesVar = NULL, #
                                            dataset_name,
                                            dataset_label = NULL,
                                            dataset_source_type = "raw",
                                            short_description = NULL,
                                            add_source = NULL,
                                            add_seealso = NULL,
                                            add_authors = NULL,
                                            add_reference = NULL,
                                            add_rdname_prefix = NULL,
                                            output_file_name = NULL) {
  library(tidyverse)

  rlang::arg_match(dataset_source_type,
    values = c("raw", "derived", "external"),
    multiple = TRUE
  )
  dataset_source_type <- str_to_upper(dataset_source_type)
  if (is.null(dataset_label)) dataset_label <- dataset_name
  data_source_label <- ifelse(dataset_source_type %in% "RAW", " ", str_c("[", dataset_source_type, "]"))

  if (is.null(data_dict) && is.null(dd)) stop("A dataframe of anctual dataset or prepared data dictionary should be provide")

  # Prepared a data dictionary if there is no pre-specified data dictionary daset
  ## i.e. based on the actual values dataset
  if (!is.null(dd)) {
    # Summarized a dataset
    temp_summarized_dd <- summarize_dataset(dd = dd, dataset_name = dataset_name, wider_format = TRUE) %>%
      select(
        dd_name, field_name, field_class, field_label, field_notes,
        num_rows, num_cols
      ) %>%
      distinct()

    # Checking for a single dataset name
    if (length(unique(temp_summarized_dd$dd_name)) > 1) stop("Check the dataset name")

    temp_summarized_dd <- data_dict_column_names(
      data_dict = temp_summarized_dd, field_nameVar = field_nameVar,
      field_classVar = field_classVar, field_labelVar = field_labelVar,
      field_notesVar = field_notesVar
    )
  }

  # Generate the document based on pre-specified data dictionary dataset
  if (is.null(dd)) {
    if (any(!c("num_rows", "num_cols") %in% colnames(data_dict))) stop("num_rows and num_cols are noted included in the data_dict")

    temp_summarized_dd <- data_dict_column_names(
      data_dict = data_dict, field_nameVar = field_nameVar,
      field_classVar = field_classVar, field_labelVar = field_labelVar,
      field_notesVar = field_notesVar
    )
  }

  # if (is.na(num_rows) || is.na(num_colums)) format_description <- str_c("#' @format \n")
  # if (!is.na(num_rows) | !is.na(num_colums)) {
  format_description <- str_c(
    "#' @format A data frame with ", unique(temp_summarized_dd$num_rows),
    " rows and ", unique(temp_summarized_dd$num_cols), " variables: \n"
  )
  # }

  data_doc <- str_c(
    str_c("#' @title ", data_source_label, " ", str_remove_all(dataset_label, "\\[|\\]"), "\n"),
    "#'\n",
    # str_c("#' @description ", short_description, "\n"),
    # "#' \n",
    str_c("#' @docType data \n"),
    "#'\n",
    str_c("#' @usage data(", dataset_name, ") \n"),
    "#'\n",
    str_c("#' @keywords ", dataset_source_type, " datasets \n"),
    "#'\n",
    format_description,
    "#' \\describe{\n",
    str_c(
      unlist(lapply(temp_summarized_dd$nameVar, function(x) {
        generate_variable_format_list(
          data_dict = temp_summarized_dd, var_name = x,
          field_nameVar = "nameVar", field_classVar = "classVar",
          field_labelVar = "labelVar", field_notesVar = "notesVar"
        )
      })),
      collapse = "\n"
    ),
    "\n",
    "#'} \n",
    collapse = ""
  )

  if (!is.null(short_description)) {
    data_doc <- str_c(data_doc, str_c("#' @description ", short_description, "\n"), collapse = "")
  }

  if (!is.null(add_source)) {
    data_doc <- str_c(data_doc, str_c("#' @source ", add_source, "\n"), collapse = "")
  }

  if (!is.null(add_authors)) {
    data_doc <- str_c(data_doc, str_c("#' @author ", add_authors, "\n"), collapse = "")
  }

  if (!is.null(add_reference)) {
    data_doc <- str_c(data_doc, str_c("#' @references ", add_reference, "\n"), collapse = "")
  }

  if (!is.null(add_seealso)) {
    data_doc <- str_c(data_doc, str_c("#' @seealso ", add_seealso, "\n"), collapse = "")
  }

  if (!is.null(add_rdname_prefix)) {
    data_doc <- str_c(data_doc, str_c("#' @rdname ", add_rdname_prefix, "_", dataset_name, "\n"), collapse = "")
  }

  if (is.null(add_rdname_prefix)) {
    data_doc <- str_c(data_doc, str_c("#' @rdname ", dataset_name, "\n"), collapse = "")
  }

  data_doc <- str_c(data_doc, str_c('"', dataset_name, '"\n'), "\n", collapse = "")

  # To write in in A file
  if (is.null(output_file_name)) {
    return(tibble::tibble(
      data_doc = data_doc,
      dataset_name = dataset_name
    ))
  } else {
    if (file.exists(output_file_name) == TRUE) readr::write_lines(x = "", output_file_name)
    cat(str_c(data_doc, collapse = "\n"),
      file = output_file_name, append = FALSE
    )
  }
}


## Generate roxygen document for multiple datasets ----
#' @title Function to generate roxygen document for multiple datasets
#' @description This function is used to generate roxygen document for multiple datasets.
#' @param dataset_name_list Vector of datasets name
#' @param roxygen_source_type Indicator to generate the roxygen document either directly from actual dataset (`actual_dataset`) or prepared data dictionary dataset (`data_dictionary`). Default: actual_dataset
#' @param dd Listed objects of a single actual dataset or multiple actual datasets. Default: NULL
#' @param data_dict Prepared data dictionary dataset. Default: NULL
#' @param ... Other parameters from generate_single_dataset_document and cat() function
#' @param output_file_name Output file name. Should other than `NULL` if the interest is to store the result in local environment.
#' @return
#'  \itemize{
#'   \item{A data.frame of two columns: data_doc and dataset_name if output_file_name is `NULL`}
#'   \item{Otherwise stored the roxygen document in local directory with the specified file path: `output_file_name`}
#'  }
#' @seealso \code{\link{generate_single_dataset_roxygen}} \code{\link{cat}}
#' @examples
#' \dontrun{
#' # Data preparation to generate the roxygen document directly from datasets
#' temp_listed_dd <- list()
#' temp_listed_dd[[1]] <- list(dd = ADNI4::DM, dataset_source_type = "derived")
#' temp_listed_dd[[2]] <- list(dd = ADNI4::TK, dataset_source_type = "derived")
#' names(temp_listed_dd) <- c("DM", "TK")
#' generate_roxygen_document(dd = temp_listed_dd, roxygen_source_type = "actual_dataset")
#' }
#' @export
#' @rdname generate_roxygen_document
generate_roxygen_document <- function(dataset_name_list,
                                      roxygen_source_type = "actual_dataset",
                                      dd = NULL,
                                      data_dict = NULL,
                                      field_nameVar = NULL,
                                      field_classVar = NULL,
                                      field_labelVar = NULL,
                                      field_notesVar = NULL,
                                      output_file_name = NULL) {
  require(tidyverse)
  rlang::arg_match0(arg = roxygen_source_type, values = c("actual_dataset", "data_dictionary"))

  if (is.null(field_nameVar)) field_nameVar <- "field_name"
  if (is.null(field_classVar)) field_classVar <- "field_class"
  if (is.null(field_labelVar)) field_labelVar <- "field_label"
  if (is.null(field_notesVar)) field_notesVar <- "field_notes"


  # CASE I: when actual datasets are only provided
  if (roxygen_source_type %in% "actual_dataset") {
    if (is.null(dd)) stop("Check for the actual dataset")
    if (!is.list(dd)) stop("dd should be a list dataset with names")
    # Checking for listed dataset names
    check_list_names(dd, listed_names = dataset_name_list)
  }

  if (roxygen_source_type %in% "data_dictionary") {
    if (is.null(data_dict)) stop("Check for data_dict dataset")
    data_dict_col_names <- colnames(data_dict)
    arg_lst <- c("dd_name", field_nameVar, field_classVar, field_labelVar, field_notesVar)
    rlang::arg_match(
      arg = arg_lst,
      values = data_dict_col_names,
      multiple = TRUE
    )

    ## Add default values for the variable names that are not presented in the data_dict
    common_col_names <- c(
      "dataset_label",
      "short_description", "add_source", "add_seealso",
      "add_authors", "add_reference", "add_rdname_prefix"
    )
    missing_common_cols <- common_col_names[!common_col_names %in% colnames(data_dict)]
    if (length(missing_common_cols) > 0) {
      data_dict[missing_common_cols] <- NA
    }
  }


  output_result <- lapply(dataset_name_list, function(dataset_name) {
    if (roxygen_source_type %in% "actual_dataset") {
      temp_dd <- dd %>%
        pluck(., dataset_name) %>%
        pluck(., "dd")
      field_nameVar <- field_classVar <- field_labelVar <- field_notesVar <- NULL
      temp_data_dict <- NULL
      temp_dataset_source_type <- dd %>%
        pluck(., dataset_name) %>%
        pluck(., "dataset_source_type")
    }

    if (roxygen_source_type %in% "data_dictionary") {
      temp_dd <- NULL
      temp_data_dict <- data_dict %>%
        filter(dd_name %in% dataset_name)
      temp_dataset_source_type <- return_null_missing(
        dd_df = temp_data_dict,
        var_name = "dataset_source_type"
      )
    }

    temp_single_dd <- generate_single_dataset_roxygen(
      dataset_name = dataset_name,
      dd = temp_dd,
      data_dict = temp_data_dict,
      field_nameVar = field_nameVar,
      field_classVar = field_classVar,
      field_labelVar = field_labelVar,
      field_notesVar = field_notesVar,
      dataset_label = return_null_missing(dd_df = temp_data_dict, var_name = "dataset_label"),
      dataset_source_type = temp_dataset_source_type,
      short_description = return_null_missing(dd_df = temp_data_dict, var_name = "short_description"),
      add_source = return_null_missing(dd_df = temp_data_dict, var_name = "add_source"),
      add_seealso = return_null_missing(dd_df = temp_data_dict, var_name = "add_seealso"),
      add_authors = return_null_missing(dd_df = temp_data_dict, var_name = "add_authors"),
      add_reference = return_null_missing(dd_df = temp_data_dict, var_name = "add_reference"),
      add_rdname_prefix = return_null_missing(dd_df = temp_data_dict, var_name = "add_rdname_prefix"),
      output_file_name = NULL
    )

    return(temp_single_dd)
  }) %>%
    bind_rows() %>%
    select(data_doc, dataset_name)

  # Final Outputs
  if (is.null(output_file_name)) {
    return(output_result)
  }
  if (!is.null(output_file_name)) {
    if (file.exists(output_file_name) == TRUE) readr::write_lines(x = "", output_file_name)
    cat(str_c(output_result$data_doc, collapse = "\n"),
      file = output_file_name, append = TRUE
    )
  }
}
