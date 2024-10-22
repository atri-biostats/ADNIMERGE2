# Function to use use_data function ----
#' @title Use usethis::use_data Function
#' @description This function is used to apply usethis::use_data function for data preparation
#' @param dataset_name Dataset name
#' @param dataset Actual dataset
#' @param edit_type Edit type: either to create a new script or modify the existed files, Default: 'create'
#' @param run_script Indicator for running script, Default: TRUE
#' @param add_text Additional text that will appended in the script, Default: NULL
#' @param include_pipe Whether to include a pipe after the first line of the script, Default: FALSE
#' @return A file path if the script is not compiled (i.e. `run_script == FALSE`) otherwise `TRUE` value
#' @examples
#' \dontrun{
#' use_data_modified(
#'   dataset_name = "ADAS",
#'   dataset = ADNIMERGE2::adas_pooled,
#'   edit_type = "create",
#'   run_script = TRUE
#' )
#' }
#' @seealso
#'  \code{\link[usethis]{use_data}}
#' @rdname use_data_modified
#' @export
#' @importFrom rlang arg_match0
#' @importFrom usethis use_data_raw use_data
#' @importFrom readr read_lines write_lines
use_data_modified <- function(dataset_name,
                              dataset,
                              edit_type = "create",
                              run_script = TRUE,
                              add_text = NULL,
                              include_pipe = FALSE) {
  require(usethis)
  require(readr)
  require(rlang)

  rlang::arg_match0(edit_type, values = c("create", "modify"))
  if (!is.logical(run_script)) stop("run_script must be a boolean value")

  added_script <- paste0(dataset_name, " <- ", dataset_name)

  # Create data-preparation script in 'data-raw/dataset_name.R'
  raw_dataset_path <- file.path(".", "data-raw", paste0(dataset_name, ".R"))

  if (edit_type %in% "create") {
    if (file.exists(raw_dataset_path)) file.remove(raw_dataset_path)
    usethis::use_data_raw(name = dataset_name, open = FALSE)
    prefix_lines <- NULL
    suffix_lines <- NULL
  }

  if (edit_type %in% "modify") {
    existed_script <- readr::read_lines(file = raw_dataset_path)
    last_lines <- existed_script[str_detect(existed_script, "usethis::use") == TRUE]
    if (length(last_lines) == 0) stop("Check the usethis::use_data line")
    last_row_index <- seq_along(existed_script)[existed_script == last_lines]
    last_two_row_index <- c(last_row_index - 1, last_row_index)
    prefix_lines <- existed_script[!seq_along(existed_script) %in% last_two_row_index]
    suffix_lines <- existed_script[seq_along(existed_script) %in% last_two_row_index]
    added_script <- NA
  }

  temp_script <- readr::read_lines(file = raw_dataset_path)

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
  readr::write_lines(x = temp_script, file = raw_dataset_path, sep = "\n", append = FALSE)

  if (run_script == TRUE) {
    new_env <- new.env()
    new_env[[dataset_name]] <- dataset_name
    new_env$dataset <- dataset
    assign(dataset_name, dataset, envir = new_env)
    sys.source(file = raw_dataset_path, envir = new_env, chdir = TRUE)
    return(TRUE)
  }

  if (run_script == FALSE) {
    return(raw_dataset_path)
  }
}

# Unzip zipped files from a source -----
#' @title Function to unzip zipped files
#' @description This function is used to extracted raw datasets from zip files
#' @param input_dir The directory where the zip file is located.
#' @param file_name Zip file name
#' @param output_dir The directory where the unzipped file is to be stored, Default: NULL.
#'                   Stored the file in the same input directory if it is NULL.
#' @param overwrite Indicator to overwrite file, Default: TRUE
#' @return `TRUE` if the file is properly unzipped
#' @rdname get_unzip_file
#' @export
get_unzip_file <- function(input_dir,
                           file_name,
                           output_dir = ".",
                           overwrite = TRUE) {
  require(utils)

  if (output_dir %in% ".") output_dir <- input_dir

  if (!output_dir %in% ".") {
    if (dir.exists(output_dir) == FALSE) stop(output_dir, " is not existed")
  }

  output_dir <- paste0(output_dir, gsub(pattern = ".zip", replacement = "", x = file_name))
  if (dir.exists(output_dir) == FALSE) dir.create(output_dir)

  file_path <- paste0(input_dir, file_name)
  if (file.exists(file_path) == FALSE) stop(file_name, " zip File is not found in ", input_dir)
  utils::unzip(
    zipfile = file_path, exdir = output_dir,
    files = NULL, list = FALSE, overwrite = overwrite,
    setTimes = FALSE, unzip = "internal"
  )

  return(TRUE)
}

# Renaming all csv files ----
#' @title Function to rename csv file
#' @description This function is used to rename csv file into certain format
#' @param input_dir The directory where the .csv file is stored
#' @param output_dir The directory where all renamed csv file is to be stored.
#' @param file_extension File extension, Default: ".csv"
#' @param removed_strings Strings that would be removed from the file name
#' @param file_action Either `file_rename` to renamed the file name or `file_copy` to make a copy of the file
#' @return `TRUE` if the file is properly renamed or copied
#' @rdname rename_file
#' @export
rename_file <- function(input_dir,
                        output_dir = ".",
                        file_extension = ".csv",
                        removed_strings,
                        file_action = "file_rename") {
  rlang::arg_match0(arg = file_action, values = c("file_rename", "file_copy"))
  # csv files
  old_files_name <- list.files(path = input_dir, pattern = file_extension, all.files = TRUE)
  new_files_name <- stringr::str_remove_all(old_files_name, pattern = removed_strings)

  if (output_dir %in% ".") output_dir <- input_dir
  if (!output_dir %in% ".") {
    if (dir.exists(output_dir) == FALSE) stop(output_dir, " directory is not existed")
  }

  if (file_action %in% "file_copy") {
    file.copy(
      from = paste0(input_dir, old_files_name),
      to = paste0(output_dir, new_files_name)
    )
  }

  if (file_action %in% "file_rename") {
    file.rename(
      from = paste0(input_dir, old_files_name),
      to = paste0(output_dir, new_files_name)
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
#' @rdname using_use_data
#' @seealso
#'  \code{\link[ADNIMERGE2]{use_data_modified}}
#' @examples
#' \dontrun{
#' using_use_data(
#'   input_dir = "./data-raw/",
#'   file_extension = ".csv"
#' )
#' }
using_use_data <- function(input_dir,
                           file_extension = ".csv") {
  require(stringr)

  all_files <- list.files(path = input_dir, pattern = file_extension, all.files = TRUE)
  if (is.null(all_files)) {
    return("no file is found")
  }
  if (!file_extension %in% ".csv") stop("Check for file extensions")

  all_csv_data <- lapply(all_files, function(x) {
    message("Importing ", x, " dataset")
    read_csv(file = str_c(input_dir, "/", x), col_names = TRUE)
  })
  names(all_csv_data) <- str_remove_all(all_files, pattern = file_extension)

  # Load all the dataset in .GlobalEnv
  list2env(all_csv_data, .GlobalEnv)
  for (dd_name in names(all_csv_data)) {
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

# Function to relabel field code and text from DATADIC dataset
#' @title Function to adjust for field code and text
#' @description This function is used to adjust the field code and text from DATADIC
#' @param dd A data.frame (i.e. from DATADIC)
#' @param phaseVar Variable name for study phase. Default: "PHASE"
#' @param codeVar Variable name for field code. Default: "CODE"
#' @param textVar Variable name for field text. Default: "TEXT"
#' @return Returned one row data.frame  that contains `field_value` and `field_label` variables.
#' @rdname adjust_code_lables
adjust_code_lables <- function(dd,
                               phaseVar = "PHASE",
                               codeVar = "CODE",
                               textVar = "TEXT") {
  require(tidyverse)

  column_list_dd <- tibble::tibble(
    specified_name_list = c(phaseVar, codeVar, textVar),
    renamed_list = c("phase_var", "code_var", "text_var")
  )

  dd <- dd %>%
    mutate(across(all_of(column_list_dd$specified_name_list) & where(is.factor), as.character)) %>%
    rename_with(
      ~ str_c(column_list_dd %>% filter(specified_name_list == .x) %>% pull(renamed_list)),
      all_of(column_list_dd$specified_name_list)
    )

  if (nrow(dd) > 1) {
    unique_rows <- dd %>%
      distinct(code_var) %>%
      nrow()
    if (unique_rows == 1) {
      temp_dd <- dd %>% filter(row_number() %in% n())
      temp_code <- temp_dd$code_var
      temp_text <- temp_dd$text_var
      output_data <- tibble::tibble(field_value = temp_code, field_label = temp_text)
    }

    if (unique_rows > 1) {
      temp_dd <- dd %>%
        mutate(phase_code_var = str_c("\n #' \\code{\\link{", phase_var, "}}: ", code_var, "\n "))
      temp_code <- str_c(str_c(temp_dd$phase_code_var, collapse = ""), "#' ")
      temp_text <- temp_dd %>%
        filter(row_number() %in% n()) %>%
        pull(text_var)
      output_data <- tibble::tibble(field_value = temp_code, field_label = temp_text)
    }
  }

  if (nrow(dd) == 1) {
    output_data <- tibble::tibble(field_value = dd$code_var, field_label = dd$text_var)
  }

  return(output_data)
}
