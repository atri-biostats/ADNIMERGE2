# Function to use use_data function ----
#' @title Use usethis::use_data Function
#' @description
#'  This function is used to apply usethis::use_data function for data preparation.
#' @param dataset_name Dataset name
#' @param dataset Actual dataset
#' @param edit_type Edit type, Default: 'create'
#'  \item{create}{To create a new script}
#'  \item{modify}{To modify the existed files}
#' @param run_script Indicator for running script, Default: TRUE
#' @param add_text
#'  Additional text that will appended in the script, Default: NULL
#' @param include_pipe
#'  A boolean to include a pipe after the first line of the script, Default: FALSE
#' @param clean If TRUE, remove script file
#' @return
#'  \itemize{
#'    \item A file path if the script is not compiled for \code{\emph{run_script == FALSE}}
#'    \item Otherwise \emph{TRUE} boolean values
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
#' @export
use_data_modified <- function(dataset_name,
                              dataset,
                              edit_type = "create",
                              run_script = TRUE,
                              add_text = NULL,
                              include_pipe = FALSE,
                              clean = TRUE) {
  require(usethis)
  require(readr)
  require(rlang)

  arg_match0(edit_type, values = c("create", "modify"))
  if (!is.logical(run_script)) stop("`run_script` must be a boolean value.")

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
    existed_script <- readr::read_lines(file = data_script_path)
    last_lines <- existed_script[str_detect(existed_script, "usethis::use") == TRUE]
    if (length(last_lines) == 0) stop("Check the usethis::use_data line")
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
    if (any(is.na(add_text))) stop("added text must not contains missing value")
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

  if (run_script == TRUE) {
    new_env <- new.env()
    new_env[[dataset_name]] <- dataset_name
    new_env$dataset <- dataset
    assign(dataset_name, dataset, envir = new_env)
    sys.source(file = data_script_path, envir = new_env, chdir = TRUE)
    if (clean) file.remove(data_script_path)
    return(TRUE)
  }

  if (run_script == FALSE) {
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
#' @return `TRUE` if the file is properly unzipped
#' @rdname get_unzip_file
#' @keywords utils_fun
#' @importFrom utils unzip
#' @export
get_unzip_file <- function(input_dir,
                           file_name,
                           output_dir = ".",
                           overwrite = TRUE) {
  require(utils)
  if (output_dir %in% ".") output_dir <- input_dir
  if (!output_dir %in% ".") {
    if (dir.exists(output_dir) == FALSE) stop(output_dir, " is not existed.")
  }
  lapply(c(input_dir, output_dir), check_dir_path)
  output_dir <- file.path(output_dir, gsub(pattern = "\\.zip$", replacement = "", x = file_name))
  if (dir.exists(output_dir) == FALSE) dir.create(output_dir)
  file_path <- file.path(input_dir, file_name)
  if (file.exists(file_path) == FALSE) {
    stop(file_name, " zip file is not found in the ", input_dir)
  }
  utils::unzip(
    zipfile = file_path, exdir = output_dir,
    files = NULL, list = FALSE, overwrite = overwrite,
    setTimes = FALSE, unzip = "internal"
  )

  return(TRUE)
}

# Rename/ Copy files ----
#' @title Function to rename csv file
#' @description This function is used to rename csv file into certain format
#' @param input_dir Directory location where the file is stored
#' @param output_dir Output directory
#' @param file_extension File extension, Default: ".csv"
#' @param action Either `rename` or `copy`
#' @param remove_name_pattern
#'   Strings that will be removed from the file name, Default = `NULL`
#' @return `TRUE` if the file action is properly renamed or copied
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
#' @description This function is used to create .rda file in `data` directory.
#' @param input_dir The directory where the .csv file is located.
#' @param file_extension File extension, Default: ".csv"
#' @return `TRUE` if the .rda dataset is created and stored in `data` directory
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
    return("no file is found")
  }

  csv_data_list <- lapply(file_list, function(x) {
    message("Importing ", x, " dataset")
    readr::read_csv(
      file = file.path(input_dir, x),
      col_names = TRUE,
      show_col_types = FALSE
    )
  })
  names(csv_data_list) <- str_remove_all(file_list, pattern = file_extension)

  # Load all the dataset in .GlobalEnv
  list2env(csv_data_list, .GlobalEnv)
  for (dd_name in names(csv_data_list)) {
    message("Applying use_data() for ", dd_name)
    # Using usethis::use_data function
    use_data_modified(
      dataset_name = dd_name,
      dataset = get(dd_name),
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
#' @param data_dict A data.frame (i.e. from DATADIC)
#' @param phaseVar Variable name for study phase. Default: "PHASE"
#' @param codeVar Variable name for field code. Default: "CODE"
#' @param textVar Variable name for field text. Default: "TEXT"
#' @return
#'  A data.frame  that contains `field_value` and `field_label` variables.
#' @rdname adjust_code_labels
#' @keywords adni_datadic_fun
#' @family data dictionary related functions
#' @importFrom tibble tibble
#' @importFrom dplyr mutate rename_with across pull row_number
#' @importFrom tidyselect where all_of
#' @importFrom rlang arg_match0
adjust_code_labels <- function(data_dict,
                               phaseVar = "PHASE",
                               codeVar = "CODE",
                               textVar = "TEXT") {
  require(tidyverse)
  require(rlang)
  require(tibble)
  arg_match0(arg = phaseVar, values = colnames(data_dict))
  arg_match0(arg = codeVar, values = colnames(data_dict))
  arg_match0(arg = textVar, values = colnames(data_dict))

  column_list_dd <- tibble::tibble(
    specified_name_list = c(phaseVar, codeVar, textVar),
    renamed_list = c("phase_var", "code_var", "text_var")
  )

  data_dict <- data_dict %>%
    mutate(across(all_of(column_list_dd$specified_name_list) &
      where(is.factor), as.character)) %>%
    rename_with(
      ~ paste0(column_list_dd %>%
        filter(specified_name_list == .x) %>%
        pull(renamed_list)),
      all_of(column_list_dd$specified_name_list)
    )

  if (nrow(data_dict) > 1) {
    unique_rows <- data_dict %>%
      distinct(code_var) %>%
      nrow()
    if (unique_rows == 1) {
      data_dict <- data_dict %>% filter(row_number() %in% n())
      temp_code <- data_dict$code_var
      temp_text <- data_dict$text_var
      output_data <- tibble::tibble(
        field_value = temp_code,
        field_label = temp_text
      )
    }

    if (unique_rows > 1) {
      data_dict <- data_dict %>%
        mutate(phase_code_var = case_when(
          !is.na(code_var) ~ paste0(
            "\n#' \\item \\emph{",
            phase_var, ":} ", code_var, "\n"
          ),
          is.na(code_var) ~ paste0("\n#' \\item \\emph{", phase_var, "} \n")
        ))
      temp_code <- paste0(
        "\n#' \\itemize{",
        paste0(data_dict$phase_code_var, collapse = ""), "#' }"
      )
      temp_text <- data_dict %>%
        filter(row_number() %in% n()) %>%
        pull(text_var)
      output_data <- tibble::tibble(
        field_value = temp_code,
        field_label = temp_text
      )
    }
  }

  if (nrow(data_dict) == 1) {
    output_data <- tibble::tibble(
      field_value = data_dict$code_var,
      field_label = data_dict$text_var
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
#' @param data_dict Data dictionary dataset
#' @param fldname Common column names (usually "ORIGPROT" or "CORPORT")
#' @param description Description text
#' @return A data frame the same as `data_dict`  with appended rows.
#' @examples
#' \dontrun{
#' common_cols_description_datadic(
#'   tblname = "ADAS_ADNIGO123",
#'   data_dict = ADNIMERGE2::DATADIC,
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
#' @export
common_cols_description_datadic <- function(data_dict, tblname, fldname, description) {
  PHASE <- TBLNAME <- CRFNAME <- FLDNAME <- NULL

  check_colnames(
    data = data_dict,
    col_names = c("PHASE", "TBLNAME", "CRFNAME", "FLDNAME"),
    strict = TRUE,
    stop_message = TRUE
  )

  data_dict <- data_dict %>%
    mutate(across(c(PHASE, TBLNAME, FLDNAME), ~ tolower(.x)))
  tblname <- tolower(tblname)
  fldname <- tolower(fldname)

  rlang::arg_match(
    arg = tblname,
    values = unique(data_dict$TBLNAME),
    multiple = TRUE
  )

  if (!is.vector(fldname)) stop("fldname must be a vector character")
  if (!is.vector(description)) stop("description must be a vector character")
  if (length(description) != length(fldname)) {
    stop("The length of description and FLDNAME must be the same")
  }
  description <- as.list(description)
  names(description) <- fldname

  temp_data_dict <- data_dict %>%
    filter(TBLNAME %in% tblname) %>%
    verify(nrow(.) > 0)

  tblname_data_dict <- lapply(
    fldname,
    function(cur_fldname) {
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
    }
  ) %>%
    bind_rows()

  result_data_dict <- data_dict %>%
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
#'  Otherwise return `TRUE`.
#' @rdname check_dir_path
#' @keywords utils_fun
#' @family checks function
check_dir_path <- function(dir_path) {
  if (all(grepl(pattern = "/$", x = dir_path, perl = TRUE))) {
    stop("The last `/` character must be removed from ", dir_path, ".")
  }
  if (all(dir.exists(dir_path) == FALSE)) {
    stop(dir_path, " directory is not existed!")
  }
  return(TRUE)
}

# Expand DATADIC Across Study Phase ----
#' @title Expand Data Directory Dataset By Study Phase
#' @description
#'  This function is used expand the data dictionary dataset by possible ADNI
#'   study phase if the dataset contains combined study phases.
#' @param data_dict Data Dictionary Dataset
#' @param concat_phase
#'  A character vector that contains study phase that concatenated with
#'  \emph{concat_char} character.
#' @param concat_char Concatenate character
#' @return
#'  Updated data directory data frame with the same structure as \emph{data_dict}.
#' @rdname expand_data_dict
#' @keywords adni_datadic_fun
#' @family data dictionary related functions
#' @importFrom stringr str_split str_detect
#' @importFrom dplyr filter select bind_rows
#' @importFrom assertr verify
#' @importFrom tidyr expand_grid
expand_data_dict <- function(data_dict, concat_phase, concat_char = ",") {
  require(tidyverse)
  require(stringr)
  require(assertr)
  PHASE <- NULL
  check_colnames(
    data = data_dict,
    col_names = "PHASE",
    strict = TRUE,
    stop_message = TRUE
  )
  if (all(is.na(concat_phase))) {
    return(data_dict)
  }
  if (any(!str_detect(string = concat_phase, pattern = concat_char))) {
    stop(concat_char, " must be presented.")
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
      stop("At least one `ADNI` prefix is not presented.")
    }
    split_phase[!str_detect(split_phase, "ADNI")] <- str_c("ADNI", split_phase[!str_detect(split_phase, "ADNI")])
    return(data_dict %>%
      filter(PHASE %in% i) %>%
      select(-PHASE) %>%
      expand_grid(., PHASE = split_phase) %>%
      verify(nrow(.) >= length(split_phase)))
  }) %>%
    bind_rows()

  output_data_dict <- data_dict %>%
    filter(!PHASE %in% names(concat_phase_list)) %>%
    bind_rows(output_data_dict)
  return(output_data_dict)
}


# Functions to get dataset category/groups ----
#' @title Get Dataset Category/Group
#' @description
#'  This function is used to categorize dataset based on the corresponding
#'  directory location.
#' @param dir.path Directory path
#' @param extension_pattern File extension patterns, Default: '\.csv$'
#' @return A data.frame that contains the following columns:
#' @examples
#' \dontrun{
#' get_dataset_cat(
#'   dir.path = file.path(".", "data-raw"),
#'   extension_pattern = "\\.csv$"
#' )
#' }
#' @rdname get_dataset_cat
#' @family utility function
#' @keywords utils_fun
#' @importFrom stringr str_remove_all str_detect str_to_lower
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows mutate case_when select
#' @export
get_dataset_cat <- function(dir.path, extension_pattern = "\\.csv$") {
  require(tidyverse)
  dir <- file_list <- dir_cat <- NULL
  # Function to extract all `*.csv$` files within a single directory
  get_file_list <- function(dir.path, extension_pattern) {
    check_dir_path(dir.path)
    file_list <- list.files(
      path = dir.path,
      pattern = extension_pattern,
      all.files = TRUE
    )
    if (all(!is.na(file_list))) {
      file_list <- str_remove_all(
        string = file_list,
        pattern = extension_pattern
      )
    }
    output_data <- tibble(dir = dir.path, file_list = file_list)
    return(output_data)
  }
  check_dir_path(dir.path)
  # Get directories list
  dir_list <- list.dirs(
    path = dir.path,
    full.names = TRUE,
    recursive = FALSE
  )
  if (all(!is.na(dir_list))) {
    dir_list <- dir_list[str_detect(
      string = dir_list,
      pattern = "/\\_|/Tables\\_"
    )]
  } else {
    dir_list <- NA_character_
  }
  dir_list <- c(dir.path, dir_list)
  dir_list <- dir_list[!is.na(dir_list)]
  dir_list <- unique(dir_list)

  output_data <- lapply(dir_list, function(i) {
    get_file_list(dir.path = i, extension_pattern = extension_pattern)
  }) %>%
    bind_rows() %>%
    mutate(dir_cat = str_remove_all(
      string = dir,
      pattern = str_c(dir.path, "/")
    )) %>%
    mutate(dir_cat = str_remove_all(
      string = dir_cat,
      pattern = "^\\_"
    )) %>%
    mutate(dir_cat = str_to_lower(dir_cat)) %>%
    mutate(dir_cat = case_when(
      str_detect(string = dir_cat, pattern = "^table") |
        dir_cat %in% dir.path ~ "other_raw_dataset",
      TRUE ~ dir_cat
    )) %>%
    select(dir, file_list, dir_cat)

  return(output_data)
}
