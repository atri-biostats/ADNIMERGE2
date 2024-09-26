# Script setup ----
library(tidyverse)
source("./tools/data_prepare_function.R")

## All the datasets can be downloaded from loni website with the name "all_clinical_biomaker" file names
## And the zip file should be stored in local project directory "./ADNIMERGE2/raw_data_source"
zip_file_name <- "all_clinical_biomarker_Tables.zip" 
data_download_date <- Sys.Date() # The date of data download
removed_strings <- str_c(c(
  str_remove_all(string = zip_file_name, pattern = "Tables.zip"),
  str_c("_", format(data_download_date, "%d%b%Y")), "v_"
), collapse = "|")
## Unzip downloaded files ----
## The zip file should be stored in raw_data_source folders
get_unzip_file(
  package_name = "ADNIMERGE2",
  input_dir = "./raw_data_source",
  file_name = zip_file_name,
  output_dir = NULL
)
## Renamed csv files ----
rawdata_path <- str_c("./ADNIMERGE2/rawdata/", 
                      str_remove_all(string = zip_file_name, pattern = ".zip"), "/")
rename_file(
  input_dir = raw_data_path,
  output_dir = ".",
  file_extension = ".csv",
  removed_strings = removed_strings,
  file_action = "file_rename"
)
## Convert csv files to rdata file format ----
convert_rdata_file(
  input_dir = raw_data_path,
  file_extension = ".csv",
  output_dir = "./ADNIMERGE2/data/"
)

# Prepare Data Dictionary ----
## Copy DATADIC.csv file ----
rename_file(
  input_dir = "./raw_data_source/",
  output_dir = "./ADNIMERGE2/rawdata/",
  file_extension = ".csv",
  removed_strings = removed_strings,
  file_action = "file_copy"
)
## Convert DATADIC.csv to rdata file format ----
convert_rdata_file(
  input_dir = "./ADNIMERGE2/rawdata/",
  file_extension = ".csv",
  output_dir = "./ADNIMERGE2/data/"
)
