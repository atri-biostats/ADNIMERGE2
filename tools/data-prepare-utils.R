# Function to use use_data function ----
#' @title Use usethis::use_data Function
#' @description
#'  This function is used to apply usethis::use_data function for data preparation.
#' @param dataset_name Dataset name
#' @param dataset Actual dataset
#' @param edit_type Edit type, Default: 'create'
#' \itemize{
#'   \item {\code{create}}: To create a new script
#'   \item {\code{modify}}: To modify the existed files
#'  }
#' @param run_script Indicator for running script, Default: TRUE
#' @param add_text
#'  Additional text that will appended in the script, Default: NULL
#' @param include_pipe
#'  A Boolean to include a pipe after the first line of the script, Default: FALSE
#' @param clean If TRUE, remove script file
#' @return
#'  \itemize{
#'    \item A file path if the script is not compiled for \code{run_script = FALSE}
#'    \item Otherwise \code{TRUE} Boolean value
#'  }
#' @examples
#' \dontrun{
#' use_data_modified(
#'   dataset_name = "ADAS",
#'   dataset = ADNIMERGE2::adas_pooled,
#'   edit_type = "create",
#'   run_script = TRUE
#' )
#' }
#' @rdname use_data_modified
#' @family functions related to use_data
#' @keywords use_data
#' @seealso [usethis::use_data()]
#' @importFrom rlang arg_match0
#' @importFrom usethis use_data_raw use_data
#' @importFrom readr read_lines write_lines
#' @importFrom cli cli_abort cli_alert_info
#' @export
use_data_modified <- function(dataset_name, dataset, edit_type = "create",
                              run_script = TRUE, add_text = NULL,
                              include_pipe = FALSE, clean = TRUE) {
  require(usethis)
  require(readr)
  require(rlang)

  arg_match0(edit_type, values = c("create", "modify"))
  check_object_type(run_script, "logical")
  check_object_type(include_pipe, "logical")
  check_object_type(clean, "logical")

  added_script <- paste0(dataset_name, " <- ", dataset_name)

  # Create data-preparation script in 'data-raw/dataset_name.R'
  data_script_path <- file.path(".", "data-raw", paste0(dataset_name, ".R"))

  if (edit_type %in% "create") {
    if (file.exists(data_script_path)) file.remove(data_script_path)
    usethis::use_data_raw(name = dataset_name, open = FALSE)
    prefix_lines <- NULL
    suffix_lines <- NULL
  }

  if (edit_type %in% "modify") {
    cli::cli_alert_info(
      text = "{.path {data_script_path}} script will be modified."
    )
    existed_script <- readr::read_lines(file = data_script_path)
    last_lines <- existed_script[str_detect(existed_script, "usethis::use")]
    if (length(last_lines) == 0) {
      cli::cli_abort(
        message = "{.path {data_script_path}} script does not include {.var usethis::use_data}."
      )
    }
    last_row_index <- seq_along(existed_script)[existed_script == last_lines]
    last_two_row_index <- c(last_row_index - 1, last_row_index)
    prefix_lines <- existed_script[!seq_along(existed_script) %in% last_two_row_index]
    suffix_lines <- existed_script[seq_along(existed_script) %in% last_two_row_index]
    added_script <- NA
  }

  temp_script <- readr::read_lines(file = data_script_path)

  if (is.null(prefix_lines) & is.null(suffix_lines)) {
    prefix_lines <- temp_script[1]
    suffix_lines <- temp_script[c(2:3)]
  }

  if (!is.null(add_text)) {
    if (any(is.na(add_text))) {
      cli::cli_abort(
        message = c(
          "{.var add_text} must not contains missing value. \n",
          "{.var add_text} contains {.val {add_text}}."
        )
      )
    }
    added_script <- c(added_script, add_text)
  }
  added_script <- added_script[!is.na(added_script)]

  if (include_pipe) {
    added_script[seq_along(added_script) == length(added_script)] <- paste0(
      added_script[seq_along(added_script) == length(added_script)],
      " %>%"
    )
  }

  temp_script <- c(prefix_lines, added_script, suffix_lines)
  ## Write the 'dataset_name.R' script in data-raw directory to use quoted objects
  readr::write_lines(
    x = temp_script,
    file = data_script_path,
    sep = "\n",
    append = FALSE
  )

  if (run_script) {
    new_env <- new.env()
    new_env[[dataset_name]] <- dataset_name
    new_env$dataset <- dataset
    assign(dataset_name, dataset, envir = new_env)
    sys.source(file = data_script_path, envir = new_env, chdir = TRUE)
    if (clean) file.remove(data_script_path)
    return(TRUE)
  }

  if (!run_script) {
    return(data_script_path)
  }
}

# Unzip zipped files from a source -----
#' @title Function to unzip zipped files
#' @description This function is used to extracted raw datasets from zip files
#' @param input_dir The directory where the zip file is located.
#' @param file_name Zip file name
#' @param output_dir
#'    The directory where the unzipped file is to be stored, Default: NULL.
#'    Store the file in the same input directory if it is NULL.
#' @param overwrite Indicator to overwrite file, Default: TRUE
#' @return \code{TRUE} if the file is properly unzipped
#' @rdname get_unzip_file
#' @keywords utils_fun
#' @importFrom utils unzip
#' @importFrom cli cli_abort cli_alert_success
#' @export
get_unzip_file <- function(input_dir,
                           file_name,
                           output_dir = ".",
                           overwrite = TRUE) {
  require(utils)
  if (output_dir %in% ".") output_dir <- input_dir
  lapply(c(input_dir, output_dir), check_dir_path)
  output_dir <- file.path(output_dir, gsub(pattern = "\\.zip$", replacement = "", x = file_name))
  if (!dir.exists(output_dir)) dir.create(output_dir)
  file_path <- file.path(input_dir, file_name)
  if (!file.exists(file_path)) {
    cli::cli_abort(
      message = "{.path {file.path(input_dir, file_name)}} file is not existed."
    )
  }

  utils::unzip(
    zipfile = file_path, exdir = output_dir,
    files = NULL, list = FALSE, overwrite = overwrite,
    setTimes = FALSE, unzip = "internal"
  )

  cli::cli_alert_success(
    text = "Extracting data from {.path {file.path(input_dir, file_name)}}"
  )

  return(TRUE)
}

# Rename/ Copy files ----
#' @title Function to rename csv file
#' @description This function is used to rename csv file into certain format
#' @param input_dir Directory location where the file is stored
#' @param output_dir Output directory
#' @param file_extension File extension, Default: ".csv"
#' @param action Either \code{rename} or \code{copy}
#' @param remove_name_pattern
#'   Strings that will be removed from the file name, Default = NULL
#' @return \code{TRUE} if the file action is properly renamed or copied
#' @rdname file_action
#' @keywords utils_fun
#' @importFrom stringr str_remove_all
#' @importFrom rlang arg_match0
#' @export
file_action <- function(input_dir,
                        output_dir = ".",
                        file_extension = ".csv",
                        action = "rename",
                        remove_name_pattern = NULL) {
  require(rlang)
  require(stringr)
  arg_match0(arg = action, values = c("rename", "copy"))
  # file list
  old_file_name <- list.files(path = input_dir, pattern = file_extension, all.files = TRUE)
  new_file_name <- old_file_name
  if (!is.null(remove_name_pattern)) {
    new_file_name <- str_remove_all(old_file_name, pattern = remove_name_pattern)
  }
  if (output_dir %in% ".") output_dir <- input_dir
  lapply(c(input_dir, output_dir), check_dir_path)

  if (action %in% "copy") {
    file.copy(
      from = file.path(input_dir, old_file_name),
      to = file.path(output_dir, new_file_name)
    )
  }

  if (action %in% "rename") {
    file.rename(
      from = file.path(input_dir, old_file_name),
      to = file.path(output_dir, new_file_name)
    )
  }

  return(TRUE)
}

# Generate .rda dataset in `data` directory ----
#' @title Function to create .rda dataset
#' @description This function is used to create .rda file in \code{./data} directory.
#' @param input_dir The directory where the .csv file is located.
#' @param file_extension File extension, Default: ".csv"
#' @return \code{TRUE} if the .rda dataset is created and stored in \code{./data} directory
#' @examples
#' \dontrun{
#' using_use_data(
#'   input_dir = "./data-raw/",
#'   file_extension = ".csv"
#' )
#' }
#' @rdname using_use_data
#' @keywords use_data
#' @family functions related to use_data
#' @importFrom readr read_csv
#' @importFrom stringr str_c str_remove_all
#' @importFrom rlang arg_match0
#' @importFrom cli cli_alert_info cli_status cli_status_update
using_use_data <- function(input_dir, file_extension = ".csv") {
  require(stringr)
  require(readr)
  require(rlang)
  arg_match0(arg = file_extension, values = ".csv")
  file_extension <- str_c("\\", file_extension, "$")
  check_dir_path(input_dir)
  file_list <- list.files(
    path = input_dir,
    pattern = file_extension,
    all.files = TRUE
  )
  if (is.null(file_list)) {
    return("No file is found!")
  }
  csv_data_list <- lapply(file_list, function(x) {
    cli::cli_alert_info(text = "Importing {.path {file.path(input_dir, x)}}")
    readr::read_csv(
      file = file.path(input_dir, x),
      col_names = TRUE,
      show_col_types = FALSE,
      guess_max = Inf
    )
  })
  names(csv_data_list) <- str_remove_all(file_list, pattern = file_extension)

  # Load available dataset into a new environment
  new_env <- new.env()
  list2env(csv_data_list, envir = new_env)
  for (dd_name in names(csv_data_list)) {
    cli::cli_alert_info(
      text = "Applying {.var use_data()} for {.val {dd_name}} data"
    )
    # Using usethis::use_data function
    use_data_modified(
      dataset_name = dd_name,
      dataset = new_env[[dd_name]],
      run_script = TRUE,
      add_text = NULL,
      edit_type = "create",
      include_pipe = FALSE
    )
    rm(list = as.character(dd_name), envir = .GlobalEnv)
  }

  return(TRUE)
}

# Relabel field code and text from DATADIC dataset ----
#' @title Function to adjust for field code and text
#' @description
#'  This function is used to adjust the field code and text from DATADIC
#' @param .datadic A data.frame (i.e. from DATADIC)
#' @param phaseVar Variable name for study phase. Default: "PHASE"
#' @param codeVar Variable name for field code. Default: "CODE"
#' @param textVar Variable name for field text. Default: "TEXT"
#' @return
#'  A data.frame  that contains \code{field_value} and \code{field_label} variables.
#' @rdname adjust_code_labels
#' @keywords adni_datadic_fun
#' @family data dictionary related functions
#' @importFrom tibble tibble
#' @importFrom dplyr mutate rename_with across pull row_number
#' @importFrom tidyselect where all_of
#' @importFrom rlang arg_match0
adjust_code_labels <- function(.datadic, phaseVar = "PHASE", codeVar = "CODE", textVar = "TEXT") {
  require(tidyverse)
  arg_match0(arg = phaseVar, values = colnames(.datadic))
  arg_match0(arg = codeVar, values = colnames(.datadic))
  arg_match0(arg = textVar, values = colnames(.datadic))

  column_list_dd <- tibble::tibble(
    specified_name_list = c(phaseVar, codeVar, textVar),
    renamed_list = c("phase_var", "code_var", "text_var")
  )

  .datadic <- .datadic %>%
    mutate(across(all_of(column_list_dd$specified_name_list) & where(is.factor), as.character)) %>%
    rename_with(
      ~ paste0(
        column_list_dd %>%
          filter(specified_name_list == .x) %>%
          pull(renamed_list)
      ),
      all_of(column_list_dd$specified_name_list)
    )

  if (nrow(.datadic) > 1) {
    unique_rows <- .datadic %>%
      distinct(code_var) %>%
      nrow()
    if (unique_rows == 1) {
      .datadic <- .datadic %>%
        filter(row_number() %in% n())
      temp_code <- .datadic$code_var
      temp_text <- .datadic$text_var
      output_data <- tibble::tibble(
        field_value = temp_code,
        field_label = temp_text
      )
    }

    if (unique_rows > 1) {
      # Last row records for variable description
      temp_text <- .datadic %>%
        distinct(phase_var, text_var) %>%
        filter(row_number() %in% n()) %>%
        pull(text_var)

      # Coded values
      temp_code_list <- .datadic %>%
        group_by(code_var) %>%
        mutate(phase_var = toString(phase_var)) %>%
        ungroup() %>%
        distinct(phase_var, code_var) %>%
        mutate(phase_code_var = case_when(
          !is.na(code_var) ~ paste0("\n#' \\item \\emph{", phase_var, ":} ", code_var, "\n"),
          is.na(code_var) ~ paste0("\n#' \\item \\emph{", phase_var, "} \n")
        ))

      temp_code <- paste0("\n#' \\itemize{", paste0(temp_code_list$phase_code_var, collapse = ""), "#' }")

      output_data <- tibble::tibble(
        field_value = temp_code,
        field_label = temp_text
      )
    }
  }

  if (nrow(.datadic) == 1) {
    output_data <- tibble::tibble(
      field_value = .datadic$code_var,
      field_label = .datadic$text_var
    )
  }

  output_data <- output_data %>%
    mutate(field_label = str_replace_all(field_label, "\\{P\\}", " P"))

  return(output_data)
}

# Add description for common columns in DATADIC ----
#' @title Add description text for common columns in DATADIC
#' @description
#'  This function is used to add description text for common columns in the DATADIC.
#' @param tblname Dataset name (TBNAME)
#' @param .datadic Data dictionary dataset
#' @param fldname Common column names, usually "ORIGPROT" or "CORPORT"
#' @param description Description text
#' @return A data frame the same as \code{./datadic} with appended rows.
#' @examples
#' \dontrun{
#' common_cols_description_datadic(
#'   tblname = "ADAS_ADNIGO123",
#'   .datadic = ADNIMERGE2::DATADIC,
#'   fldname = "CORPORT",
#'   description = "Study protocol of data collection"
#' )
#' }
#' @rdname common_cols_description_datadic
#' @keywords adni_datadic_fun
#' @family data dictionary related functions
#' @importFrom rlang arg_match
#' @importFrom dplyr mutate across filter bind_rows
#' @importFrom assertr verify
#' @importFrom cli cli_abort
#' @export
common_cols_description_datadic <- function(.datadic, tblname, fldname, description) {
  PHASE <- TBLNAME <- CRFNAME <- FLDNAME <- NULL

  check_colnames(
    .data = .datadic,
    col_names = c("PHASE", "TBLNAME", "CRFNAME", "FLDNAME"),
    strict = TRUE,
    stop_message = TRUE
  )

  .datadic <- .datadic %>%
    mutate(across(c(PHASE, TBLNAME, FLDNAME), ~ tolower(.x)))
  tblname <- tolower(tblname)
  fldname <- tolower(fldname)

  rlang::arg_match(
    arg = tblname,
    values = unique(.datadic$TBLNAME),
    multiple = TRUE
  )

  arg_names <- c("fldname", "description")
  checks <- lapply(arg_names, function(i) {
    if (!is.vector(get(i))) {
      cli_abort(
        message = c(
          "{.var {get(i)}} must be a vector character. \n ",
          "{.var {get(i)}} is a {.val {class({get(i)})}} object."
        )
      )
    }
  })

  if (length(description) != length(fldname)) {
    cli_abort(
      message = c(
        "The length of {.var description} and {.var fldname} must be the same. \n",
        "The length of {.var description} is {.val {length(description)}}. \n",
        "The length of {.var fldname} is {.val {length(fldname)}}."
      )
    )
  }
  description <- as.list(description)
  names(description) <- fldname
  temp_data_dict <- .datadic %>%
    filter(TBLNAME %in% tblname) %>%
    verify(nrow(.) > 0)

  tblname_data_dict <- lapply(fldname, function(cur_fldname) {
    fldname_data_dict <- temp_data_dict %>%
      filter(!FLDNAME %in% cur_fldname) %>%
      distinct(PHASE, TBLNAME, CRFNAME) %>%
      mutate(
        FLDNAME = cur_fldname,
        TEXT = description[[cur_fldname]]
      ) %>%
      bind_rows(
        temp_data_dict %>%
          filter(!FLDNAME %in% cur_fldname)
      )
    return(fldname_data_dict)
  }) %>%
    bind_rows()

  result_data_dict <- .datadic %>%
    filter(!TBLNAME %in% tblname) %>%
    bind_rows(tblname_data_dict) %>%
    mutate(across(c(PHASE, TBLNAME, FLDNAME), ~ toupper(.x)))

  return(result_data_dict)
}

# Checks Directory Path Pattern ----
#' @title Checks Directory Path Pattern
#' @description
#'  This function is used check whether the last character of a directory name is "/".
#' @param dir_path Directory path name
#' @return
#'  A stop message if directory is not existed or the last character is "/" .
#'  Otherwise return \code{TRUE}.
#' @rdname check_dir_path
#' @keywords utils_fun
#' @family checks function
#' @importFrom cli cli_abort
check_dir_path <- function(dir_path) {
  if (all(grepl(pattern = "/$", x = dir_path, perl = TRUE))) {
    cli_abort(
      message = c("The last `/` character must be removed from {.path {dir_path}}.")
    )
  }
  if (all(!dir.exists(dir_path))) {
    cli_abort(
      message = c("{.path {dir_path}} not existed.")
    )
  }
  return(TRUE)
}

# Expand DATADIC Across Study Phase ----
#' @title Expand Data Directory Dataset By Study Phase
#' @description
#'  This function is used expand the data dictionary dataset by possible ADNI
#'   study phase if the dataset contains combined study phases.
#' @param .datadic Data Dictionary Dataset
#' @param concat_phase
#'  A character vector that contains study phase that concatenated with
#'  \code{concat_char} character.
#' @param concat_char Concatenate character
#' @return
#'  Updated data directory data frame with the same structure as \code{data_dict}.
#' @rdname expand_data_dict
#' @keywords adni_datadic_fun
#' @family data dictionary related functions
#' @importFrom stringr str_split str_detect
#' @importFrom dplyr filter select bind_rows
#' @importFrom assertr verify
#' @importFrom tidyr expand_grid
#' @importFrom cli cli_abort
expand_data_dict <- function(.datadic, concat_phase, concat_char = ",") {
  require(tidyverse)
  require(stringr)
  require(assertr)
  PHASE <- NULL
  check_colnames(
    .data = .datadic,
    col_names = "PHASE",
    strict = TRUE,
    stop_message = TRUE
  )
  if (all(is.na(concat_phase))) {
    return(data_dict)
  }
  if (any(!str_detect(string = concat_phase, pattern = concat_char))) {
    cli::cli_abort(
      message = "{.val {concat_char} not found."
    )
  }
  concat_phase_list <- str_split(
    string = concat_phase,
    pattern = concat_char,
    simplify = FALSE
  )
  names(concat_phase_list) <- concat_phase

  output_data_dict <- lapply(names(concat_phase_list), function(i) {
    split_phase <- as.character(unlist(concat_phase_list[i]))
    if (!any(str_detect(split_phase, "ADNI"))) {
      cli::cli_abort(
        message = c(
          "At least one `ADNI` prefix is not foound. \n",
          "There are only {.val {split_phase}} values."
        )
      )
    }
    split_phase[!str_detect(split_phase, "ADNI")] <- str_c("ADNI", split_phase[!str_detect(split_phase, "ADNI")])
    return(.datadic %>%
      filter(PHASE %in% i) %>%
      select(-PHASE) %>%
      expand_grid(., PHASE = split_phase) %>%
      verify(nrow(.) >= length(split_phase)))
  }) %>%
    bind_rows()

  output_data_dict <- .datadic %>%
    filter(!PHASE %in% names(concat_phase_list)) %>%
    bind_rows(output_data_dict)
  return(output_data_dict)
}


# Add Prefix for Coded Values Based DATADIC  ----
#' @title Add Prefix for Coded Values Based DATADIC
#' @description
#'  This function is used to add prefix on the data dictionary (\code{DATADIC})
#'  coded values to match values in the actual data.
#' @param .datadic
#'  Data dictionary dataset created using
#'  \code{\link{get_factor_levels_datadict}()} function.
#' @param prefix_char Prefix character, Default: "0"
#' @param nested_value
#'  A Boolean value to indicate the code and decode values are
#'  nested in the \code{data_dict}. Default: TRUE
#' @param position Either in the beginning (first) or in the end (last), Default: "first"
#' @param add_char
#'  Character that will be concatenated with \code{prefix_char} character based on
#'  the provided \code{position}.
#' @return
#'  A same data.frame as \code{data_dict} with additional records if there coded
#'  values that did not contains the specified prefix character.
#' @rdname add_code_prefix
#' @keywords adni_datadic_fun
#' @family data dictionary related functions
#' @importFrom stringr str_split str_detect
#' @importFrom dplyr filter select bind_rows
#' @importFrom assertr verify
#' @importFrom tidyr expand_grid
add_code_prefix <- function(.datadic, prefix_char = "0",
                            nested_value = TRUE, position = "first",
                            add_char = NULL) {
  require(tidyverse)
  require(assertr)
  CODES <- CRFNAME <- TBLNAME <- FLDNAME <- PHASE <- NULL
  prefix_char <- as.character(prefix_char)
  check_object_type(nested_value, "logical")
  is_datadict_tbl(.datadic)
  if (nested_value) add_cols <- "CODES" else add_cols <- c("prefix", "suffix")
  check_colnames(
    .data = .datadic,
    col_names = c("PHASE", "CRFNAME", "TBLNAME", "FLDNAME", add_cols),
    strict = TRUE,
    stop_message = TRUE
  )

  initial_nrow <- nrow(.datadic)

  output_data_dict <- .datadic %>%
    datadict_as_tibble() %>%
    {
      if (nested_value) {
        unnest(., all_of("CODES"))
      } else {
        (.)
      }
    } %>%
    set_datadict_tbl() %>%
    update_code_prefix_char(
      .datadic = .,
      prefix_char = prefix_char,
      position = position,
      add_char = add_char
    ) %>%
    datadict_as_tibble() %>%
    group_by(CRFNAME, TBLNAME, FLDNAME, PHASE) %>%
    {
      if (nested_value) {
        nest(., CODES = all_of(c("prefix", "suffix"))) %>%
          ungroup(.) %>%
          verify(., nrow(.) == initial_nrow)
      } else {
        (.)
      }
    }

  output_data_dict <- set_datadict_tbl(output_data_dict)

  return(output_data_dict)
}

#' @title Update Specific Character in Coded Values (Prefix)
#' @description
#'  This function is used to add prefix on the data dictionary (`DATADIC`)
#'  coded values to match values in the actual data.
#' @param .datadic
#'  Data dictionary dataset created using
#'  \code{\link{get_factor_levels_datadict}()} function in long format (i.e. not in nested format).
#' @param prefix_char Prefix character that will be concatenated with \code{prefix} or \code{CODED} values
#' @param position Either in the beginning (first) or in the end (last), Default: "first"
#' @param add_char
#'  Character that will be concatenated with \code{prefix_char} character based on
#'  the provided `position`.
#' @return A data.frame similar the provided data dictionary dataset \code{data_dict}.
#' @rdname update_code_prefix_char
#' @keywords adni_datadic_fun internal
#' @family data dictionary related internal functions
#' @importFrom dplyr mutate case_when bind_rows
#' @importFrom tibble as_tibble
#' @importFrom assertr assert not_na
#' @importFrom rlang arg_match0
update_code_prefix_char <- function(.datadic, prefix_char, position, add_char = NULL) {
  require(tidyverse)
  require(assertr)
  require(rlang)
  status <- CRFNAME <- TBLNAME <- FLDNAME <- PHASE <- overall_status <- prefix <- NULL
  arg_match0(arg = position, values = c("first", "last"))
  cols_list <- c("PHASE", "CRFNAME", "TBLNAME", "FLDNAME", "prefix", "suffix")
  check_colnames(
    .data = .datadic,
    col_names = cols_list,
    strict = TRUE,
    stop_message = TRUE
  )
  # Identify records that contains `char` in prefix
  .datadic <- .datadic %>%
    datadict_as_tibble() %>%
    mutate(status = case_when(
      prefix %in% prefix_char ~ TRUE,
      !prefix %in% prefix_char ~ FALSE
    )) %>%
    assert(not_na, status)

  if (position %in% "first") first_char <- prefix_char
  if (position %in% "last") last_char <- prefix_char

  if (!is.null(add_char)) {
    if (position %in% "first") first_char <- paste0(add_char, first_char)
    if (position %in% "last") last_char <- paste0(add_char, last_char)
  }

  # Group by Phase, CFRNAME, TBLNAME, FLDNAME,
  data_dict_update <- .datadic %>%
    group_by(CRFNAME, TBLNAME, FLDNAME, PHASE) %>%
    mutate(overall_status = any(status)) %>%
    ungroup() %>%
    filter(overall_status == FALSE)

  if (nrow(data_dict_update) > 0) {
    data_dict_update <- data_dict_update %>%
      {
        if (position %in% "first") {
          mutate(., prefix = paste0(first_char, prefix))
        } else {
          mutate(., prefix = paste0(prefix, last_char))
        }
      } %>%
      select(-any_of("overall_status"))
  }

  result_data_dict <- .datadic %>%
    {
      if (nrow(data_dict_update) > 0) {
        bind_rows(., data_dict_update)
      } else {
        (.)
      }
    } %>%
    select(-any_of("status")) %>%
    arrange(CRFNAME, TBLNAME, FLDNAME, PHASE)
  return(result_data_dict)
}

# Functions to get dataset category/groups ----
#' @title Get Dataset Category/Group
#' @description
#'  This function is used to categorize dataset based on the corresponding
#'  directory location.
#' @param dir.path Directory path
#' @param file_extension_pattern File extension pattern, Default: '\\.csv$'
#' @param recursive \code{\link[base]{list.files}}
#' @return A data.frame that contains the following columns:
#'  \item \code{dir} Top level directory name
#'  \item \code{sub_dir} Sub-directory name
#'  \item \code{full_file_path} Full file path
#'  \item \code{file_list} Abbreviated/short data file name
#'  \item \code{dir_cat} Dataset category
#' @examples
#' \dontrun{
#' get_dataset_category(
#'   dir.path = file.path(".", "data-raw"),
#'   extension_pattern = "\\.csv$"
#' )
#' }
#' @rdname get_dataset_category
#' @family utility function
#' @keywords utils_fun
#' @importFrom stringr str_remove_all str_detect str_to_lower
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows mutate case_when select
#' @importFrom assertr assert within_bounds
#' @export
get_dataset_category <- function(dir.path, file_extension_pattern = "\\.csv$",recursive = TRUE) {
  require(tidyverse)
  dir <- main_dir <- dir_cat <- NULL
  full_file_path <- file_list <- file_list_temp <- num_dash_char <- NULL
  check_dir_path(dir.path)

  file_list <- list.files(
    path = dir.path,
    pattern = file_extension_pattern,
    all.files = TRUE,
    recursive = recursive,
    full.names = FALSE
  )
  if (all(!is.na(file_list))) {
    file_list <- str_remove_all(
      string = file_list,
      pattern = file_extension_pattern
    )
  }

  output_data <- tibble(dir = dir.path, file_list = file_list) %>%
    mutate(file_list_temp = file_list) %>%
    mutate(num_dash_char = str_count(string = file_list, pattern = "/")) %>%
    {
      if (nrow(.) > 0) {
        assertr::assert(., assertr::within_bounds(0, 1), num_dash_char)
      } else {
        (.)
      }
    } %>%
    separate(file_list_temp, into = c("dir_cat", "file_name"), sep = "/") %>%
    mutate(main_dir = case_when(is.na(file_name) & !is.na(dir_cat) ~ "Yes")) %>%
    mutate(
      file_name = ifelse(!is.na(main_dir), dir_cat, file_name),
      dir_cat = ifelse(!is.na(main_dir), NA_character_, dir_cat),
      dir_cat = str_to_lower(str_remove_all(string = dir_cat, pattern = "^\\_"))
    ) %>%
    mutate(across(all_of(c("file_name", "dir_cat")), basename)) %>%
    mutate(
      dir_cat = case_when(
        str_detect(string = dir_cat, pattern = "^table") | is.na(dir_cat) ~ "other_raw_dataset",
        TRUE ~ dir_cat
      ),
      dir_cat = case_when(
        tolower(file_name) %in% tolower(c("DATADIC", "UPDATED_DATADIC")) ~ "data_dict",
        tolower(file_name) %in% tolower("REMOTE_DATADIC") ~ "data_dict, remotely_collected_data",
        TRUE ~ dir_cat
      ),
      full_file_path = file.path(dir, file_list), 
      sub_dir = file.path(dir, str_remove_all(file_list, paste0("/",file_name,"$")))
    ) %>%
    select(dir, sub_dir, full_file_path, file_list = file_name, dir_cat)

  return(output_data)
}

#' @title Get Dataset Category By Study Phase
#' @param .data A data.frame
#' @param phase_vars Study phase variables, Default: NULL
#' @return A data.frame with \code{PHASE} variable
#' @examples
#' \dontrun{
#' get_study_phase_category(.data = ADNIMERGE2::ADAS)
#' }
#' @rdname get_study_phase_category
#' @export
#' @importFrom cli cli_abort
#' @importFrom dplyr rename mutate across select distinct
#' @importFrom tidyselect all_of everything
get_study_phase_category <- function(.data, phase_vars = NULL) {
  require(dplyr)
  # Checking for study phase variable
  if (is.null(phase_vars)) {
    phase_vars <- c("COLPROT", "PHASE", "Phase", "ProtocolID")
  }
  phaseVar <- get_cols_name(.data = .data, col_name = phase_vars)
  if (length(phaseVar) > 1) {
    cli::cli_abort(
      message = paste0(
        "Only one {.val PHASE} variable must be in the data. \n ",
        "{.val {phaseVar}} variable{?s} {?is/are} found in the data."
      )
    )
  }
  if (!is.na(phaseVar)) {
    names(phaseVar) <- "PHASE"
    output_data <- .data %>%
      rename(all_of(phaseVar)) %>%
      mutate(across(all_of(names(phaseVar)), ~ as.character(tolower(.x)))) %>%
      select(all_of(names(phaseVar))) %>%
      distinct()
  } else {
    output_data <- tibble(
      PHASE = "undefined_phase"
    ) %>% 
      mutate(across(everything(), as.character))
  }
  return(output_data)
}

#' @title Create a tibble/data.frame with no rows/records
#' @param col_names Character vector of column names
#' @return A data.frame with the provided columns with no rows/records.
#' @examples
#' \dontrun{
#' create_tibble0(col_names = c("SEX", "AGE"))
#' }
#' @rdname create_tibble0
#' @export
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_wider
create_tibble0 <- function(col_names) {
  return(
    tibble::tibble(
      var = col_names,
      value = NA_character_
    ) %>%
      tidyr::pivot_wider(names_from = var, values_from = value) %>%
      na.omit()
  )
}
