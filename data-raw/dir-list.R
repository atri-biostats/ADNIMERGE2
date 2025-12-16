# Raw data directory ----
raw_data_dir <- "./data-raw"
# Sub-directories ----
updated_datadic_dir <- file.path(raw_data_dir, "updated_datadic")
date_stamped_dir <- file.path(raw_data_dir, "date_stamped")
common_columns_dir <- file.path(raw_data_dir, "common_columns")
dataset_cat_dir <- file.path(raw_data_dir, "dataset_cat")
coded_record_dir <- file.path(raw_data_dir, "coded_records")
derived_datadic_dir <- file.path(raw_data_dir, "derived-datadic")
# Data directory ----
data_dir <- "./data"
dir_list <- c(
  data_dir, updated_datadic_dir, date_stamped_dir, common_columns_dir,
  dataset_cat_dir, coded_record_dir, derived_datadic_dir
)
