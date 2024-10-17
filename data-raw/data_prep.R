# Data preparation script setup ----
source(file.path(".", "tools", "data-prepare-utils.R"))
source(file.path(".", "R", "utils.R"))
# Libraries ----
library(tidyverse)

# Data downloaded date ----
data_downloaded_date <- as.Date("2024-10-04")
DATA_DOWNLOADED_DATE <- as.Date(data_downloaded_date)
usethis::use_data(DATA_DOWNLOADED_DATE, overwrite = TRUE)

# Extract raw datasets from zip file ----
list_zip_files <- list.files(
  path = "./data-raw", pattern = ".zip", full.names = TRUE,
  all.files = TRUE, recursive = TRUE
)
if (length(list_zip_files) > 0) {
  EXISTED_ZIPFILE <- TRUE
} else {
  EXISTED_ZIPFILE <- FALSE
  warning("No existed zip file existed in data-raw directory")
}

if (EXISTED_ZIPFILE) {
  zip_files_prefix <- str_remove_all(list_zip_files, pattern = "Tables.zip|.zip|./data-raw/")

  removed_zip_strings <- str_c(c(
    zip_files_prefix,
    str_c("_", format(data_downloaded_date, "%d%b%Y")), "v_"
  ), collapse = "|")

  lapply(list_zip_files, function(zip_file) {
    ### Unzipped file ----
    get_unzip_file(
      input_dir = "./data-raw/",
      file_name = str_remove_all(string = zip_file, pattern = "./data-raw/"),
      output_dir = "."
    )
    ### Renamed csv files ----
    rawdata_csv_path <- str_c(str_remove_all(string = zip_file, pattern = ".zip"), "/")
    rename_file(
      input_dir = rawdata_csv_path,
      output_dir = ".",
      file_extension = ".csv",
      removed_strings = removed_zip_strings,
      file_action = "file_rename"
    )
    ### Create .rda dataset ----
   using_use_data(
      input_dir = rawdata_csv_path,
      file_extension = ".csv"
    )
  })
}

# Convert raw .csv dataset into .rda file format ----
list_csv_files <- list.files(
  path = "./data-raw", pattern = ".csv",
  all.files = TRUE, full.names = FALSE, recursive = FALSE
)

if (length(list_csv_files) > 0) {
  EXISTED_CSVFILE <- TRUE
} else {
  EXISTED_CSVFILE <- FALSE
  warning("No existed csv file existed in data-raw directory")
}

if (EXISTED_CSVFILE) {
  # Removing date stamp from file name
  csv_removed_strings <- str_c("_", format(data_downloaded_date, "%d%b%Y"))
  rename_file(
    input_dir = list_csv_files,
    output_dir = ".",
    file_extension = ".csv",
    removed_strings = csv_removed_strings,
    file_action = "file_rename"
  )
  # Store datasets in "./data" folder
  using_use_data(
    input_dir = "./data-raw/",
    file_extension = ".csv"
  )
}

# Adding common columns and replacing `-4` as missing value ----
dataset_list_files <- list.files(
  path = "./data", pattern = ".rda", full.names = TRUE,
  all.files = TRUE, recursive = TRUE
)
data_dict_file <- file.path(".", "data", "DATADIC.rda")
dataset_list_files <- dataset_list_files[!dataset_list_files == data_dict_file]

if (length(dataset_list_files) > 0) {
  UPDATE_MISSING_VALUE <- TRUE
} else {
  UPDATE_MISSING_VALUE <- FALSE
  warning("No existed datasets in data directory")
}

if (UPDATE_MISSING_VALUE) {
  file_path_patterns <- c("./data/|.rda")
  prefix_patterns <- c(str_c("adni", 1:4, "_"), "adni_")
  sufix_patterns <- c("_pooled", "_harmonized")
  string_removed_pattern <- str_c(c(file_path_patterns, prefix_patterns, sufix_patterns),
    collapse = "|"
  )

  tblname_list_dd <- tibble(full_tblname = dataset_list_files) %>%
    mutate(
      short_tblname = str_remove_all(string = full_tblname, pattern = file_path_patterns),
      tblname = str_to_upper(str_remove_all(string = full_tblname, pattern = string_removed_pattern))
    )

  for (tb in tblname_list_dd$short_tblname) {
    ## Load all the datasets to .GlobalEnv
    lapply(
      tblname_list_dd %>% filter(short_tblname == tb) %>% pull(full_tblname),
      load, .GlobalEnv
    )
    assign("dd", get(tb))

    if ("RID" %in% names(dd)) check_RID_col <- TRUE else check_RID_col <- FALSE
    ## Adding common columns -----
    if (check_RID_col) {
      message("Adding ORGCOL and CORPOL variables in ", tb)
      dd <- dd %>%
        create_col_protocol(dd = ., phaseVar = c("Phase", "PHASE")) %>%
        create_orig_protocol(dd = .)
    } else {
      message("ORGCOL and CURPOL are not addedd in ", tb)
    }
    # Replacing `-4` as missing value -----
    message("Making -4 values as missing value for ", tb)
    dd <- make_missing_value(dd = dd, col_name = names(dd), value = "-4", missing_char = NA)

    use_data_modified(
      dataset_name = tb,
      dataset = dd,
      edit_type = "create",
      run_script = TRUE
    )

    ## Remove objects from the .GlobalEnv
    rm(list = c("tb", "dd"))
  }

  rm(list = c(
    "tblname_list_dd", "file_path_patterns", "prefix_patterns",
    "sufix_patterns", "string_removed_pattern", "check_RID_col",
    "DATA_DOWNLOADED_DATE"
  ))
}
rm(list = c(
  "dataset_list_files", "data_dict_file",
  "data_downloaded_date", "list_zip_files"
))
