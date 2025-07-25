# Summary Function ----
## Summarize Factor Variable ----
#' @title Summarize Factor Variable
#' @description
#'  This function is used to summarize a factor variable from a data.frame.
#'  The output result that can be used to create a data dictionary file.
#' @param data Data frame
#' @param var_name Variable name
#' @param wider_format
#'  Indicator whether to generate the factor level values in
#'  a wider or long format, Default: FALSE
#' @return A data.frame that contains the following variables:
#'   \item{field_name}{Variable name}
#'   \item{field_class}{Variable class type: 'factor'}
#'   \item{field_label}{Variable label}
#'   \item{field_values_order}{Factor levels order number and only included for long format: wider_format = FALSE}
#'   \item{field_values}{Factor levels}
#'   \item{field_notes}{Text notes that could be used to generate the roxygen document for the dataset}
#' @examples
#' \dontrun{
#' summarize_factor_variable(
#'   data = ADNIMERGE2::DM,
#'   var_name = "ORIGPROT",
#'   wider_format = FALSE
#' )
#' }
#' @rdname summarize_factor_variable
#' @keywords var_summary
#' @importFrom dplyr select pull group_by ungroup distinct
#' @importFrom labelled get_variable_labels
#' @importFrom rlang arg_match0
#' @importFrom tibble tibble
#' @importFrom assertr verify
summarize_factor_variable <- function(data, var_name, wider_format = FALSE) {
  require(tidyverse)
  require(labelled)
  require(assertr)
  var_values <- data %>%
    select(all_of(var_name)) %>%
    pull()
  var_label <- labelled::get_variable_labels(var_values)
  var_class_type <- class(var_values)
  rlang::arg_match0(arg = var_class_type, values = "factor")
  check_is_logical(wider_format)
  # Add description for roxygen document
  var_notes <- str_c(
    "Factor variable with levels: ",
    paste0(levels(var_values), collapse = ", ")
  )
  summary_result <- tibble(
    field_values = levels(var_values),
    field_values_order = seq_along(levels(var_values)),
    field_name = var_name,
    field_class = var_class_type,
    field_label = convert_null_na(var_label),
    field_notes = var_notes
  ) %>%
    select(
      field_name, field_class, field_label, field_values_order,
      field_values, field_notes
    ) %>%
    {
      if (wider_format) {
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

## Summarize Character Variable ----
#' @title Summarize Character Variable
#' @description
#'  This function is used to summarize a character variable from a data.frame.
#'  The output result can be used to create a data dictionary file.
#' @param data Data frame
#' @param var_name  Variable name
#' @param wider_format
#'  Indicator whether to generate the character values in a wider or long format
#'  if there are less than 10 unique values, Default: FALSE
#' @return A data.frame that contains the following variables:
#'   \item{field_name}{Variable name}
#'   \item{field_class}{Variable class type: 'character'}
#'   \item{field_label}{Variable labels}
#'   \item{field_values}{Variable values and only computed if the number of values is less than 10}
#'   \item{field_values_order}{Variable values order number and only included for long format: wider_format = FALSE}
#'   \item{field_notes}{Text notes that could be used to generate the roxygen document for the dataset}
#' @examples
#' \dontrun{
#' summarize_character_variable(
#'   data = ADNIMERGE2::DM,
#'   var_name = "SEX",
#'   wider_format = FALSE
#' )
#' }
#' @rdname summarize_character_variable
#' @keywords var_summary
#' @importFrom dplyr select pull group_by ungroup distinct mutate
#' @importFrom labelled get_variable_labels
#' @importFrom rlang arg_match0
#' @importFrom tibble tibble
#' @importFrom assertr verify
#' @importFrom stringr str_detect
summarize_character_variable <- function(data, var_name, wider_format = FALSE) {
  require(tidyverse)
  require(labelled)
  require(assertr)
  require(stringr)
  var_values <- data %>%
    select(all_of(var_name)) %>%
    pull()
  var_label <- labelled::get_variable_labels(var_values)
  var_class_type <- class(var_values)
  rlang::arg_match0(arg = var_class_type, values = "character")
  check_is_logical(wider_format)
  # Warning message if number of item value is more than 10
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
    warning(paste0(
      "There are more than ",
      ifelse(length(var_values) == 0, "none", length(var_values)),
      " values in ", var_name, " variable"
    ))
    var_values <- NA_character_
    var_notes <- paste0(" ")
  }

  summary_result <- tibble(
    field_values = var_values,
    field_values_order = seq_along(var_values),
    field_name = var_name,
    field_class = var_class_type,
    field_label = convert_null_na(var_label),
    field_notes = var_notes
  ) %>%
    mutate(field_values_order = ifelse(is.na(field_values), NA_real_,
      field_values_order
    )) %>%
    select(
      field_name, field_class, field_label, field_values_order,
      field_values, field_notes
    ) %>%
    {
      if (wider_format) {
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
## Summarize Numeric Variable -----
#' @title Summarize Numeric Variable
#' @description
#'  This function is used to summarize a numeric variable from a data.frame.
#'  The output result can be used to create a data dictionary file.
#' @param data Data frame
#' @param var_name Variable name
#' @param wider_format
#'  Indicator whether to generate the minimum and maximum values in a wide or
#'  long format, Default: FALSE
#' @return A data.frame that contains the following variables:
#'   \item{field_name}{Variable name}
#'   \item{field_class}{Variable class type: 'numeric'}
#'   \item{field_label}{Variable labels}
#'   \item{field_values}{Minimum and maximum values}
#'   \item{summary_type}{Summary type either 'Min' for minimum value or 'Max' for maximum value}
#'   \item{field_notes}{Text notes that could be used to generate the roxygen document for the dataset}
#' @examples
#' \dontrun{
#' summarize_numeric_variable(
#'   data = ADNIMERGE2::DM,
#'   var_name = "AGE",
#'   wider_format = FALSE
#' )
#' }
#' @rdname summarize_numeric_variable
#' @keywords var_summary
#' @importFrom dplyr select pull group_by ungroup distinct mutate
#' @importFrom labelled get_variable_labels
#' @importFrom rlang arg_match0
#' @importFrom tibble tibble
#' @importFrom assertr verify
summarize_numeric_variable <- function(data, var_name, wider_format = FALSE) {
  require(tidyverse)
  require(labelled)
  require(assertr)
  var_values <- data %>%
    select(all_of(var_name)) %>%
    pull()
  var_label <- labelled::get_variable_labels(var_values)
  var_class_type <- class(var_values)
  rlang::arg_match0(
    arg = var_class_type,
    values = c("numeric", "double", "integer")
  )
  check_is_logical(wider_format)
  # Minimum and Maximum Values
  min_value <- min(var_values, na.rm = TRUE)
  max_value <- max(var_values, na.rm = TRUE)
  var_notes <- paste0("Range value: ", min_value, ", ..., ", max_value)

  summary_result <- tibble::tibble(
    field_values = as.character(c(min_value, max_value)),
    summary_type = c("Min", "Max"),
    field_name = var_name,
    field_class = var_class_type,
    field_label = convert_null_na(var_label),
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
      if (wider_format) {
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

## Summarize Date/Time Variable -----
#' @title Summarize Date/Time Variable
#' @description
#'  This function is used to summarize a date/time variable from a data.frame.
#'   The output result can be used to create a data dictionary file.
#' @param data Data frame
#' @param var_name Variable name
#' @return A data.frame that contains the following variables:
#'   \item{field_name}{Variable name}
#'   \item{field_class}{Variable class type: 'Date', 'POSIXct', 'POSIXt', 'hms' or 'difftime'}
#'   \item{field_label}{Variable labels}
#'   \item{field_notes}{Text notes that could be used to generate the roxygen document for the dataset}
#' @examples
#' \dontrun{
#' summarize_date_variable(
#'   data = ADNIMERGE2::DM %>%
#'     mutate(RFSTDTC = as.Date(RFSTDTC)),
#'   var_name = "RFSTDTC"
#' )
#' }
#' @rdname summarize_date_variable
#' @keywords var_summary
#' @importFrom dplyr select pull
#' @importFrom labelled get_variable_labels
#' @importFrom rlang arg_match
#' @importFrom tibble tibble
#' @importFrom assertr verify
summarize_date_variable <- function(data, var_name) {
  require(tidyverse)
  require(labelled)
  require(assertr)
  require(tibble)
  var_values <- data %>%
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
    field_label = convert_null_na(var_label),
    field_notes = var_notes
  ) %>%
    select(field_name, field_class, field_label, field_notes) %>%
    verify(nrow(.) == 1)

  return(summary_result)
}

## Summarize Logical Variable -----
#' @title Summarize Logical Variable
#' @description
#'  This function is used to summarize a logical variable from a data.frame.
#'  The output result can be used to create a data dictionary file.
#' @param data Data frame
#' @param var_name Variable name
#' @return A data.frame that contains the following variables:
#'   \item{field_name}{Variable name}
#'   \item{field_class}{Variable class type: 'logical'}
#'   \item{field_label}{Variable labels}
#'   \item{field_notes}{Text notes that could be used to generate the roxygen document for the dataset}
#' @examples
#' \dontrun{
#' summarize_logical_variable(
#'   data = ADNIMERGE2::DM %>%
#'     mutate(DTHFL = ifelse(DTHFL %in% "Yes", TRUE, NA)),
#'   var_name = "DTHFL"
#' )
#' }
#' @rdname summarize_logical_variable
#' @keywords var_summary
#' @importFrom dplyr select pull
#' @importFrom labelled get_variable_labels
#' @importFrom rlang arg_match0
#' @importFrom tibble tibble
#' @importFrom assertr verify
summarize_logical_variable <- function(data, var_name) {
  require(tidyverse)
  require(labelled)
  require(assertr)
  require(tibble)
  var_values <- data %>%
    select(all_of(var_name)) %>%
    pull()
  var_label <- labelled::get_variable_labels(var_values)
  var_class_type <- class(var_values)
  rlang::arg_match0(arg = var_class_type, values = c("logical"))
  var_notes <- paste0("A Boolean value: TRUE/ FALSE")
  summary_result <- tibble(
    field_values = str_c("TRUE or FALSE"),
    field_values_order = NA_real_,
    field_name = var_name,
    field_class = var_class_type,
    field_label = convert_null_na(var_label),
    field_notes = var_notes
  ) %>%
    select(
      field_name, field_class, field_label, field_values_order,
      field_values, field_notes
    ) %>%
    verify(nrow(.) == 1)

  return(summary_result)
}

## Summarize A Single Variable ----
#' @title Summarize A Single Variable
#' @description
#'  This function is used to summarize a single variable from a data.frame.
#'  The output result can be used to create a data dictionary file.
#' @param data Data frame
#' @param var_name Variable name
#' @param wider_format
#'  A Boolean value to generate the minimum and maximum values in a wide or
#'  long format, Default: FALSE
#' @return A data.frame that contains the at least the following variables:
#'   \item{field_name}{Variable name}
#'   \item{field_class}{Variable class type: 'factor', 'character', 'date', 'POSIXct", "POSIXt" or 'logical'}
#'   \item{field_label}{Variable labels}
#'   \item{field_notes}{Text notes that could be used to generate the roxygen document for the dataset}
#' @examples
#' \dontrun{
#' summarize_variable(
#'   data = ADNIMERGE2::DM,
#'   var_name = "ORIGPROT",
#'   wider_format = FALSE
#' )
#' }
#' @rdname summarize_variable
#' @keywords var_summary
#' @seealso \code{\link{summarize_dataset}}
#' @importFrom rlang arg_match
#' @importFrom assertr verify
#' @importFrom dplyr select
#' @importFrom tidyselect all_of
#' @export
summarize_variable <- function(data, var_name, wider_format = FALSE) {
  require(tidyverse)
  require(assertr)
  require(rlang)
  data <- data %>% select(all_of(var_name))
  var_class_type <- class(data %>% pull())
  specified_class_type <- c(
    "factor", "character", "numeric", "double", "integer",
    "Date", "POSIXct", "POSIXt", "hms", "difftime", "logical"
  )
  arg_match(arg = var_class_type, values = specified_class_type, multiple = TRUE)
  check_is_logical(wider_format)
  if (any(var_class_type %in% specified_class_type[1])) {
    summary_result <- summarize_factor_variable(data = data, var_name = var_name, wider_format = wider_format)
  }
  if (any(var_class_type %in% specified_class_type[2])) {
    summary_result <- summarize_character_variable(data = data, var_name = var_name, wider_format = wider_format)
  }
  if (any(var_class_type %in% specified_class_type[3:5])) {
    summary_result <- summarize_numeric_variable(data = data, var_name = var_name, wider_format = wider_format)
  }
  if (any(var_class_type %in% specified_class_type[6:10])) {
    summary_result <- summarize_date_variable(data = data, var_name = var_name)
  }
  if (any(var_class_type %in% specified_class_type[11])) {
    summary_result <- summarize_logical_variable(data = data, var_name = var_name)
  }

  summary_result <- summary_result %>%
    {
      if (wider_format) {
        verify(., nrow(.) == 1)
      } else {
        (.)
      }
    }

  return(summary_result)
}

## Summarize A Dataset ----
#' @title Function to summarize all the variables of a dataset
#' @description
#'  This function is used to summarize a data.frame that could be used to
#'   generate a data dictionary file.
#' @param data Data frame
#' @param dataset_name Dataset label, Default: NULL
#' @param wider_format
#'  Indicator whether to generate result in a wide or long format, Default: FALSE
#' @return A data.frame that contains the at least the following variables:
#'   \item{dd_name}{Dataset name}
#'   \item{field_name}{Variable names}
#'   \item{field_class}{Variable class type: 'factor', 'character', 'date', 'POSIXct", "POSIXt" or 'logical'}
#'   \item{field_label}{Variable labels}
#'   \item{field_notes}{Text notes that could be used to generate the roxygen document for the dataset}
#' @examples
#' \dontrun{
#' summarize_variable(
#'   data = ADNIMERGE2::DM,
#'   var_name = "ORIGPROT",
#'   wider_format = FALSE
#' )
#' }
#' @rdname summarize_variable
#' @keywords data_summary
#' @seealso \code{\link{summarize_variable}}
#' @importFrom dplyr bind_rows mutate relocate select
#' @export
summarize_dataset <- function(data, dataset_name = NULL, wider_format = FALSE) {
  require(tidyverse)
  if (is.null(dataset_name)) tbl_name <- "TEMP_TBL" else tbl_name <- dataset_name
  data_dict_dd <- lapply(colnames(data), function(x) {
    summarize_variable(data = data, var_name = x, wider_format = wider_format)
  }) %>%
    bind_rows() %>%
    mutate(
      dd_name = tbl_name,
      num_rows = nrow(data),
      num_cols = ncol(data)
    ) %>%
    relocate(dd_name, num_rows, num_cols) %>%
    {
      if (is.null(dataset_name)) {
        select(., -dd_name)
      } else {
        (.)
      }
    }

  if (wider_format && nrow(data_dict_dd) != length(colnames(data))) {
    cli_abort(
      message = c(
        "Discrepancy between number of numbers in {.val data_dict_dd} and number of columns in {.val data}. \n",
        "{.clas {nrow(data_dict_dd)}} rows; {.clas {length(colnames(data))}} columns."
      )
    )
  }
  return(data_dict_dd)
}

# Roxygen Documentation ----
## Generate roxygen document for a single dataset ----
#' @title Function to generate roxygen document for a single dataset
#' @description
#'  This function is used to generate roxygen document for a single dataset or
#'  based on pre specified data dictionary dataset.
#' @param data Data frame of actual values
#' @param data_dict Prepared data dictionary dataset, Default: NULL
#' @param dataset_name Dataset name
#' @param dataset_label Dataset label, Default: NULL
#' @param dataset_source_type
#'  Dataset source type either raw dataset (`raw`), derived dataset (`derived`)
#'  or external dataset (`external`), Default: "raw".
#' @param short_description A short description about the dataset.
#' @param add_source To add dataset source description. Default: NULL. See more [@source](https://roxygen2.r-lib.org/reference/index.html)
#' @param add_seealso To add related links, Default: NULL. See more [@seealso](https://roxygen2.r-lib.org/reference/index.html)
#' @param add_authors To add authors name, Default: NULL. See more [@author](https://roxygen2.r-lib.org/reference/index.html)
#' @param add_reference To add reference description, Default: NULL. See more [@references](https://roxygen2.r-lib.org/reference/index.html)
#' @param add_rdname_prefix To add rdname prefix, Default: NULL. See more [@rdname](https://roxygen2.r-lib.org/reference/index.html)
#' @param add_keywords To add key words, Default: NULL. See more [@keywords](https://roxygen2.r-lib.org/reference/index.html)
#' @param output_file
#'  A Boolean value to return a string output or write the output in a local
#'  file (i.e. write in a file), Default: NULL
#' @param output_file_name
#'  Output file name. Should other than `NULL` if the interest is to store the
#'  result in local environment.
#' @return
#' \itemize{
#'   \item A data.frame of two columns: data_doc and dataset_name if output_file_name is `NULL`
#'   \item Otherwise stored the roxygen document in local directory with the specified file path: `output_file_name`
#'  }
#' @examples
#' \dontrun{
#' # Generate a data dictionary of a dataset
#' temp_data_dict <- summarize_dataset(
#'   dd = ADNIMERGE2::DM,
#'   dataset_name = "DM",
#'   wider_format = TRUE
#' ) %>%
#'   mutate(dataset_name = "DM", dataset_source_type = "derived")
#' generate_single_dataset_roxygen(data_dict = temp_data_dict)
#' }
#' @rdname generate_roxygen_single_dataset
#' @keywords roxygen_utils
#' @importFrom rlang arg_match
#' @importFrom stringr str_to_upper str_c str_remove_all
#' @importFrom dplyr select distinct
#' @importFrom tibble tibble
#' @importFrom readr write_lines
#' @importFrom cli cli_abort
generate_roxygen_single_dataset <- function(dataset_name, dataset_label = NULL, data = NULL, data_dict = NULL,
                                            dataset_source_type = "raw", short_description = NULL,
                                            field_nameVar = NULL, field_classVar = NULL, field_labelVar = NULL, field_notesVar = NULL,
                                            add_source = NULL, add_seealso = NULL, add_authors = NULL, add_reference = NULL,
                                            add_rdname_prefix = NULL, add_keywords = NULL, output_file_name = NULL) {
  require(tidyverse)
  require(tibble)
  require(rlang)

  arg_match(
    arg = dataset_source_type,
    values = c("raw", "derived", "external"),
    multiple = TRUE
  )
  dataset_source_type <- str_to_upper(dataset_source_type)
  if (is.null(dataset_label)) dataset_label <- dataset_name
  data_source_label <- ifelse(dataset_source_type %in% "RAW", " ",
    str_c("[", dataset_source_type, "]")
  )
  if (is.null(data_dict) && is.null(dd)) {
    cli_abort(
      message = paste0(
        "Either a data.frame of actual data {.val dd} or prepared data",
        " dictionary {.val data_dict} must be provided."
      )
    )
  }

  # To prepared a data dictionary if not provided
  ## i.e. based on the actual values dataset
  if (!is.null(data)) {
    # Summarized a dataset
    temp_summarized_dd <- summarize_dataset(
      data = data,
      dataset_name = dataset_name,
      wider_format = TRUE
    ) %>%
      select(dd_name, field_name, field_class, field_label, field_notes, num_rows, num_cols) %>%
      distinct()

    # Checking for a single dataset name
    if (length(unique(temp_summarized_dd$dd_name)) > 1) {
      cli_abort(message = "At least one dataset name must be existed.")
    }

    temp_summarized_dd <- data_dict_column_names(
      data_dict = temp_summarized_dd, field_nameVar = field_nameVar,
      field_classVar = field_classVar, field_labelVar = field_labelVar,
      field_notesVar = field_notesVar
    )
  }

  # Generate the document based on pre-specified data dictionary dataset
  if (is.null(data)) {
    if (any(!c("num_rows", "num_cols") %in% colnames(data_dict))) {
      cli_abort(
        message = "{.var num_rows} and {.var num_cols} are not included in the {.clas data_dict}"
      )
    }

    temp_summarized_dd <- data_dict_column_names(
      data_dict = data_dict, field_nameVar = field_nameVar,
      field_classVar = field_classVar, field_labelVar = field_labelVar,
      field_notesVar = field_notesVar
    )
  }

  format_description <- str_c(
    "#' @format A data frame with ", unique(temp_summarized_dd$num_rows),
    " observations and ", unique(temp_summarized_dd$num_cols), " variables. \n"
  )

  if (is.null(add_keywords)) {
    add_keywords <- str_c(tolower(dataset_source_type), "_dataset")
  }

  data_doc <- str_c(
    str_c("#' @title ", data_source_label, " ", str_remove_all(dataset_label, "\\[|\\]"), "\n"),
    "#'\n",
    str_c("#' @docType data \n"),
    "#'\n",
    str_c("#' @usage data(", dataset_name, ") \n"),
    "#'\n",
    str_c("#' @keywords ", add_keywords, " \n"),
    "#'\n",
    format_description,
    "#' @details\n",
    "#' \\itemize{\n",
    str_c(
      unlist(lapply(temp_summarized_dd$nameVar, function(x) {
        generate_variable_format(
          data_dict = temp_summarized_dd, var_name = x,
          field_nameVar = "nameVar", field_classVar = "classVar",
          field_labelVar = "labelVar", field_notesVar = "notesVar"
        )
      })),
      collapse = "\n"
    ),
    "\n#' } \n",
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

  if (is.na(data_doc)) {
    cli_abort(
      message = "{.val data_doc} must not be missing"
    )
  }
  # To write in in A file
  if (is.null(output_file_name)) {
    return(tibble(
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
#' @param dataset_name_list Character vector of dataset names
#' @param roxygen_source_type
#'   Indicator to generate the roxygen document either directly from an actual dataset (`actual_dataset`) or
#'   prepared data dictionary dataset (`data_dictionary`). Default: "actual_dataset"
#' @param data_list
#'  Listed objects of a single actual dataset or multiple actual datasets. Default: NULL
#' @param data_dict Prepared data dictionary dataset. Default: NULL
#' @param ... Other parameters from \code{\link{generate_roxygen_single_dataset}()} and \code{\link{cat}} function
#' @param output_file_name
#'  Output file name. Should be a file path if the interest is to store the result in local environment.
#' @param existed_append
#'  A boolen value to overwrite on an existed `output_file_name` files.
#'  Only applicable if a file path is provided to `output_file_name`.
#' @return
#'  A data.frame of two columns: data_doc and dataset_name if output_file_name is `NULL`.
#'  Otherwise write an R script that contains the roxygen documentation in a
#'  local directory with the specified file path: `output_file_name`.
#' @seealso \code{\link{generate_roxygen_single_dataset}()} \code{\link{cat}}
#' @examples
#' \dontrun{
#' # Data preparation to generate the roxygen document directly from datasets
#' temp_listed_dd <- list()
#' temp_listed_dd[[1]] <- list(dd = ADNIMERGE2::DM, dataset_source_type = "derived")
#' temp_listed_dd[[2]] <- list(dd = ADNIMERGE2::AE, dataset_source_type = "derived")
#' names(temp_listed_dd) <- c("DM", "TK")
#' generate_roxygen_document(
#'   data_list = temp_listed_dd,
#'   roxygen_source_type = "actual_dataset"
#' )
#' }
#' @export
#' @rdname generate_roxygen_document
#' @keywords roxygen_utils
#' @importFrom rlang arg_match0 arg_match
#' @importFrom purrr pluck
#' @importFrom stringr str_to_upper str_c str_remove_all
#' @importFrom dplyr filter bind_rows select
#' @importFrom readr write_lines
#' @importFrom cli cli_abort
generate_roxygen_document <- function(dataset_name_list, roxygen_source_type = "actual_dataset", data_list = NULL, data_dict = NULL,
                                      field_nameVar = NULL, field_classVar = NULL, field_labelVar = NULL, field_notesVar = NULL,
                                      output_file_name = NULL, existed_append = FALSE) {
  require(tidyverse)
  require(rlang)
  arg_match0(arg = roxygen_source_type, values = c("actual_dataset", "data_dictionary"))
  check_is_logical(existed_append)
  if (is.null(field_nameVar)) field_nameVar <- "field_name"
  if (is.null(field_classVar)) field_classVar <- "field_class"
  if (is.null(field_labelVar)) field_labelVar <- "field_label"
  if (is.null(field_notesVar)) field_notesVar <- "field_notes"

  # If actual dataset list data_list is provided
  if (roxygen_source_type %in% "actual_dataset") {
    if (is.null(data_list)) cli_abort(message = "{.val data_list} must not be missing")
    if (!is.list(data_list)) {
      cli_abort(
        message = c(
          "{.var data_list} must be a list object.",
          "{.var data_list} is a {.cls {class(data_list)}} object."
        )
      )
    }
    # Checking for listed dataset names
    check_list_names(data_list = data_list, list_names = dataset_name_list)
  }

  # If data dictionary data_dict is provided
  if (roxygen_source_type %in% "data_dictionary") {
    if (is.null(data_dict)) {
      cli_abort(
        message = c("{.var data_dict} must not be missing.")
      )
    }
    data_dict_col_names <- colnames(data_dict)
    arg_lst <- c("dd_name", field_nameVar, field_classVar, field_labelVar, field_notesVar)
    rlang::arg_match(
      arg = arg_lst,
      values = data_dict_col_names,
      multiple = TRUE
    )

    ## Add default values for the variable names that are not presented in the data_dict
    common_col_names <- c(
      "dataset_label", "short_description", "add_source", "add_seealso",
      "add_authors", "add_reference", "add_rdname_prefix", "add_keywords"
    )
    missing_common_cols <- common_col_names[!common_col_names %in% colnames(data_dict)]
    if (length(missing_common_cols) > 0) {
      data_dict[missing_common_cols] <- NA
    }
  }

  output_result <- lapply(dataset_name_list, function(dataset_name) {
    if (roxygen_source_type %in% "actual_dataset") {
      temp_dd <- data_list %>%
        pluck(., dataset_name) %>%
        pluck(., "dd")
      field_nameVar <- field_classVar <- field_labelVar <- field_notesVar <- NULL
      temp_data_dict <- NULL
      temp_dataset_source_type <- data_list %>%
        pluck(., dataset_name) %>%
        pluck(., "dataset_source_type")
    }

    if (roxygen_source_type %in% "data_dictionary") {
      temp_dd <- NULL
      temp_data_dict <- data_dict %>%
        filter(dd_name %in% dataset_name)
      temp_dataset_source_type <- return_null_missing(
        data = temp_data_dict,
        var_name = "dataset_source_type"
      )
    }

    temp_single_dd <- generate_roxygen_single_dataset(
      dataset_name = dataset_name,
      data = temp_dd,
      data_dict = temp_data_dict,
      field_nameVar = field_nameVar,
      field_classVar = field_classVar,
      field_labelVar = field_labelVar,
      field_notesVar = field_notesVar,
      dataset_label = return_null_missing(data = temp_data_dict, var_name = "dataset_label"),
      dataset_source_type = temp_dataset_source_type,
      short_description = return_null_missing(data = temp_data_dict, var_name = "short_description"),
      add_source = return_null_missing(data = temp_data_dict, var_name = "add_source"),
      add_seealso = return_null_missing(data = temp_data_dict, var_name = "add_seealso"),
      add_authors = return_null_missing(data = temp_data_dict, var_name = "add_authors"),
      add_reference = return_null_missing(data = temp_data_dict, var_name = "add_reference"),
      add_rdname_prefix = return_null_missing(data = temp_data_dict, var_name = "add_rdname_prefix"),
      add_keywords = return_null_missing(data = temp_data_dict, var_name = "add_keywords"),
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
    if (file.exists(output_file_name) == TRUE) {
      if (existed_append == FALSE) readr::write_lines(x = "", output_file_name)
    }
    output_scripts <- paste0(output_result$data_doc, collapse = "\n")
    if (is.na(output_scripts)) cli_abort(message = "{.file output_file_name} has not been updated.")
    cat(output_scripts, file = output_file_name, append = TRUE)
  }
}

# Additional Utils Functions ----
## Return `NULL` for missing values -----
#' @title Function to return `NULL` for missing value
#' @description
#'  This function is used to return `NULL` if the provided column contains any missing values.
#' @param data Data frame
#' @param var_name Variable name
#' @param single_value_check Checking for single unique value, Default: TRUE
#' @return
#'  `NULL` if there is the variable contains any missing values.
#'   Otherwise return a unique value of the variable.
#' @rdname return_null_missing
#' @keywords utils_fun
#' @importFrom dplyr select pull
#' @importFrom cli cli_abort
return_null_missing <- function(data, var_name, single_value_check = TRUE) {
  check_is_logical(single_value_check)
  # If the variable is existed
  if (var_name %in% colnames(data)) {
    var_value_list <- data %>%
      select(all_of(var_name)) %>%
      pull()
  } else {
    var_value_list <- NA_character_
  }

  unique_value_list <- unique(var_value_list)

  if (any(is.na(unique_value_list))) {
    return(NULL)
  } else {
    if (single_value_check == TRUE & length(unique_value_list) > 1) {
      cli_abort(message = c(
        "{.var var_name} must be a single character vector. \n",
        "There are {.val {unique_value_list}} values in {.var var_name}"
      ))
    }
    return(unique_value_list)
  }
}

## Checks name of list objects -----
#' @title Function to check the name of listed objects
#' @description This function is used to check whether list names are a valid non-missing names
#' @param data_list List data frame
#' @param list_names
#'  A character vector of list object names that will be checked, Default NULL
#' @return
#'  A stop message if there is any missing/NULL/NA names in the list data.frame.
#'  Otherwise returns `TRUE`.
#' @rdname check_list_names
#' @keywords utils_fun
#' @importFrom cli cli_abort
check_list_names <- function(data_list, list_names = NULL) {
  # If there are any unnamed lists
  if (any(is.null(names(data_list)) | is.na(names(data_list)))) {
    cli_abort(message = c(
      "{.var data_list} must be fully named list object. \n",
      "{.var data_list} contains unnamed {.cls {class(data_list)}} values."
    ))
  }

  # To check for any missed listed names
  if (all(is.null(list_names))) missing_names <- NULL

  if (any(!is.null(list_names))) {
    missing_names <- list_names[!list_names %in% names(data_list)]
  }

  if (length(missing_names) > 0) {
    cli_abort(message = c(
      "{.var data_list} contain unnamed list value. \n",
      "Named list {.val {missing_names}}."
    ))
  }

  return(TRUE)
}

## Prepare a data dictionary dataset ----
#' @title Function to prepare a data dictionary dataset
#' @description
#'  This function is used to prepare the data data dictionary for generating
#'  variable specific format list in roxygen document.
#' @param data_dict
#'  A data dictionary data.frame that contains variable name, variable class
#'  type, variable label,  and associated description about its value
#' @param field_nameVar Column name that contains all dataset variable names, Default: NULL
#' @param field_classVar Column name that contains all variable class type, Default: NULL
#' @param field_labelVar Column name that contains all variable labels, Default: NULL
#' @param field_notesVar Column name that contains all variable description notes, Default: NULL
#' @return
#'   A data.frame the same as the provided data dictionary `data_dict` with
#'   following renamed columns:
#'   \item{field_nameVar}{ to \strong{nameVar}}
#'   \item{field_classVar}{ to \strong{classVar}}
#'   \item{field_labelVar}{ to \strong{labelVar}}
#'   \item{field_notesVar}{ to \strong{notesVar}}
#' @examples
#' \dontrun{
#' # Generate a data dictionary of a dataset
#' temp_data_dict <- summarize_dataset(
#'   data = ADNIMERGE2::DM,
#'   dataset_name = "DM",
#'   wider_format = TRUE
#' )
#' # Renamed pre-specified columns:
#' # i.e.: field_nameVar, field_classVar, field_labelVar, and field_notesVar
#' data_dict_column_names(data_dict = temp_data_dict)
#' }
#' @rdname data_dict_column_names
#' @keywords utils_fun
#' @importFrom tibble tibble
#' @importFrom dplyr mutate rename_with across
#' @importFrom tidyselect any_of
data_dict_column_names <- function(data_dict, field_nameVar = NULL,
                                   field_classVar = NULL, field_labelVar = NULL,
                                   field_notesVar = NULL) {
  require(tidyverse)
  require(tibble)
  if (is.null(field_nameVar)) field_nameVar <- "field_name"
  if (is.null(field_classVar)) field_classVar <- "field_class"
  if (is.null(field_labelVar)) field_labelVar <- "field_label"
  if (is.null(field_notesVar)) field_notesVar <- "field_notes"
  # Combined all specified name list and corresponding renamed list
  column_list_dd <- tibble(
    specified_name_list = c(field_nameVar, field_classVar, field_labelVar, field_notesVar),
    renamed_list = c("nameVar", "classVar", "labelVar", "notesVar")
  )

  return(data_dict %>%
    mutate(across(any_of(column_list_dd$specified_name_list) & where(is.factor), as.character)) %>%
    rename_with(
      ~ paste0(column_list_dd %>% filter(specified_name_list == .x) %>% pull(renamed_list)),
      any_of(column_list_dd$specified_name_list)
    ))
}
## Adjust variable format in roxygen files -----
#' @title Function to generate variable format list
#' @description
#'  This function is used to generate the variable format list for roxygen document.
#' @param data_dict
#'  A data frame that contains variable name, variable class type, variable label,
#'  and associated description about its value
#' @param var_name Current variable name
#' @return A free text that will be used in the roxygen document
#' [\strong{Variable Name}] [\emph{Variable Class Type}]
#'   [Variable Label]  [variable description details]
#' @examples
#' \dontrun{
#' # Generate a data dictionary of a dataset
#' temp_data_dict <- summarize_dataset(
#'   data = ADNIMERGE2::DM,
#'   dataset_name = "DM",
#'   wider_format = TRUE
#' )
#' # Generate a variable format list
#' generate_variable_format_list(data_dict = temp_data_dict, var_name = "TRACK")
#' }
#' @rdname generate_variable_format
#' @keywords utils_fun
#' @importFrom dplyr filter
#' @importFrom cli cli_abort
generate_variable_format <- function(data_dict, var_name, field_nameVar = NULL, field_classVar = NULL,
                                     field_labelVar = NULL, field_notesVar = NULL) {
  require(tidyverse)

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

# Convert `NULL` as Missing Value ----
#' @title Convert `NULL` as Missing Value
#' @description
#'  Convert `NULL` as missing value character 'NA'.
#' @param x Input value
#' @return A character vector with updated values.
#' @rdname convert_null_na
#' @keywords utils_fun
convert_null_na <- function(x) {
  x <- ifelse(is.null(x), NA_character_, x)
  return(x)
}
