# Script setup ----
library(tidyverse)
source(file.path("..", "tools", "data_prepare_function.R"))
data_download_date <- as.Date("2024-09-26")
if (dir.exists("../data") == TRUE) unlink(x = "../data/", recursive = TRUE)
dir.create("../data")

# Downloaded Zip file ----
all_zip_files <- list.files(
  path = "../data-raw", pattern = ".zip", full.names = TRUE,
  all.files = TRUE, recursive = TRUE
)
if (length(all_zip_files) > 0) {
  zip_files_prefix <- str_remove_all(all_zip_files, pattern = "Tables.zip|.zip|../data-raw/")

  removed_zip_strings <- str_c(c(
    zip_files_prefix,
    str_c("_", format(data_download_date, "%d%b%Y")), "v_"
  ), collapse = "|")

  lapply(all_zip_files, function(zip_files) {
    ### Unzipped file ----
    get_unzip_file(
      input_dir = "../data-raw",
      file_name = zip_files,
      output_dir = "."
    )
    ### Renamed csv files ----
    rawdata_csv_path <- str_c(str_remove_all(string = zip_files, pattern = ".zip"), "/")
    rename_file(
      input_dir = rawdata_csv_path,
      output_dir = ".",
      file_extension = ".csv",
      removed_strings = removed_zip_strings,
      file_action = "file_rename"
    )
    ### Convert csv files to rda file format ----
    convert_rda_file(
      input_dir = rawdata_csv_path,
      file_extension = ".csv",
      output_dir = "../data/"
    )
  })
} else {
  warning("No downloaded zip file existed in data-raw directory")
}

# Downloaded csv file ----
downlaoded_csv_files <- list.files(
  path = "../data-raw", pattern = ".csv",
  all.files = TRUE, full.names = FALSE, recursive = FALSE
)

if (length(downlaoded_csv_files) > 0) {
  csv_removed_strings <- str_c("_", format(data_download_date, "%d%b%Y"))
  ## Convert DATADIC.csv to rda file format ----
  convert_rda_file(
    input_dir = "../data-raw/",
    file_extension = ".csv",
    output_dir = "../data/"
  )
} else {
  warning("No downloaded csv file existed in data-raw directory")
}
