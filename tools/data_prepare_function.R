# Function to extracted raw datasets from zip files
# Unzip zipped files from a source
#' @title Function to unzip zipped files
#' @description This function is used to unzip the downloaded  zip files
#' @param input_dir The directory where the zip file is located.
#' @param file_name Zip file name
#' @param output_dir The directory where the unzipped file is to be stored. If it is NULL, it will stored in the same input directory. Default: NULL
#' @return `TRUE` if the file is properly unzipped
#' @rdname get_unzip_file
get_unzip_file <- function(input_dir,
                           file_name,
                           output_dir = ".") {
  require(stringr)
 
  if (output_dir %in% ".") output_dir <- input_dir
  
  if (!output_dir %in% ".") {
    if (dir.exists(output_dir) == FALSE) stop(str_c(output_dir, " is not existed"))
  }

  output_dir <- str_c(output_dir, "/", str_remove(file_name, ".zip"))
  if (dir.exists(output_dir) == FALSE) dir.create(output_dir)

  file_path <- str_c(input_dir, "/", file_name)
  if (file.exists(file_path) == FALSE) stop(str_c(file_name, " zip File is not found in ", input_dir))
  unzip(
    zipfile = file_path, exdir = output_dir,
    files = NULL, list = FALSE, overwrite = FALSE,
    setTimes = FALSE, unzip = "internal"
  )

  return(TRUE)
}

# Renaming all csv files
#' @title Function to rename csv file
#' @description This function is used to rename csv file into certain format
#' @param input_dir The directory where the .csv file is stored
#' @param output_dir The directory where all renamed csv file is to be stored.
#' @param file_extension File extension, Default: ".csv"
#' @param removed_strings Strings that would be removed from the file name
#' @param file_action Whether to renamed the file name or make a copy of the file
#' @return `TRUE` if the file is properly renamed or copied
#' @rdname rename_file
rename_file <- function(input_dir,
                        output_dir = ".",
                        file_extension = ".csv",
                        removed_strings,
                        file_action = "file_rename") {
  require(stringr)

  rlang::arg_match0(arg = file_action, values = c("file_rename", "file_copy"))
  # csv files
  old_files_name <- list.files(path = input_dir, pattern = file_extension, all.files = TRUE)
  new_files_name <- str_remove_all(old_files_name, pattern = removed_strings)

  if (output_dir %in% ".") output_dir <- input_dir
  if (!output_dir %in% ".") {
    if (dir.exists(output_dir) == FALSE) stop(str_c(output_dir, " directory is not existed"))
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

# Change all csv files into rdata file
#' @title Function to convert csv file format to .rdata file format
#' @description This function is used to convert csv file to .rdata file format
#' @param input_dir The directory where the .csv file is located.
#' @param output_dir The directory where .rdata file is to be stored.
#' @param file_extension File extension, Default: ".csv"
#' @return `TRUE` if the .rdata file format is created and stored in the specified `output_dir`
#' @rdname convert_rda_file
convert_rda_file <- function(input_dir,
                               output_dir = ".",
                               file_extension = ".csv") {
  require(tidyverse)

  all_files <- list.files(path = input_dir, pattern = file_extension, all.files = TRUE)
  if (is.null(all_files)) {
    return("no file is found")
  }
  if (!file_extension %in% ".csv") stop("Check for file extensions")

  all_csv_data <- lapply(all_files, function(x) {
    message("Importing ", x, " file")
    read_csv(file = paste0(input_dir, "/", x), col_names = TRUE)
  })
  names(all_csv_data) <- str_remove_all(all_files, pattern = file_extension)

  if (output_dir %in% ".") output_dir <- input_dir
  if (dir.exists(output_dir) == FALSE) {
    message(output_dir, " directory was created")
    dir.create(output_dir)
  }

  # Load all data.frame in global environment
  list2env(all_csv_data, .GlobalEnv)
  for (i in seq_len(length(names(all_csv_data)))) {
    cur_file <- names(all_csv_data)[i]
    message("Converting ", cur_file, ".csv to ", cur_file, ".rda format")
    save(list = c(cur_file), file = str_c(output_dir, cur_file, ".rda"), 
         compress = "bzip2", version = 2)
    rm(list = as.character(cur_file), envir = .GlobalEnv)
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
    mutate(across(any_of(column_list_dd$specified_name_list) & where(is.factor), as.character)) %>%
    rename_with(
      ~ str_c(column_list_dd %>% filter(specified_name_list == .x) %>% pull(renamed_list)),
      any_of(column_list_dd$specified_name_list)
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
