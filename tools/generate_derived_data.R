# Libraries ----
library(tidyverse)
library(devtools)
library(knitr)
library(rlang)

# Input parameter - derived/analysis datasets ----
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 1) {
  DERIVED_DATASET_LIST <- str_remove_all(
    string = args,
    pattern = '[\\(\\)]|\\"|^c'
  ) %>%
    str_split(string = ., pattern = ",") %>%
    unlist() %>%
    str_trim(string = ., side = "both")
  if (all(DERIVED_DATASET_LIST %in% "NULL")) {
    stop("`DERIVED_DATASET_LIST` must not be missing.")
  }
} else {
  stop("`DERIVED_DATASET_LIST` must not be missing.")
}

# Generate derived and analysis datasets using metadata specs ----
devtools::load_all("./")

## Derived dataset -----
temp_file <- tempfile()
derived_vignette_file_path <- file.path(".", "vignettes", "ADNIMERGE2-Derived-Data.Rmd")
knitr::purl(input = derived_vignette_file_path, output = temp_file)
source(file = temp_file)

## Metadata specs for analysis dataset ----
metaspecs_vignette_file_path <- file.path(".", "vignettes", "ADNIMERGE2-Analysis-Meta-Specs.Rmd")
knitr::purl(input = metaspecs_vignette_file_path, output = temp_file)
source(file = temp_file)

## Analysis dataset ----
analysis_vignette_file_path <- file.path(".", "vignettes", "ADNIMERGE2-Analysis-Data.Rmd")
knitr::purl(input = analysis_vignette_file_path, output = temp_file)
source(file = temp_file)

## Store all derived objects in `./data` directory ----
### using use_data_modified() function
source(file = file.path(".", "tools", "data-prepare-utils.R"))
save_derived_data <- lapply(DERIVED_DATASET_LIST, function(tbl_name) {
  use_data_modified(
    dataset_name = tbl_name,
    dataset = get(tbl_name),
    edit_type = "create",
    run_script = TRUE
  )
  rm(list = c("tbl_name"))
})

save_derived_data <- unlist(save_derived_data)
if (any(!is.null(save_derived_data))) {
  stop("One of the derived dataset has not been stored in `./data` directory.")
}

# Derived/Analysis data dictionary ----
DERIVED_DATASET_LIST <- DERIVED_DATASET_LIST[!DERIVED_DATASET_LIST %in% "METACORES"]
DERIVED_DATASET_LIST <- str_to_lower(DERIVED_DATASET_LIST)
derived_data_dic <- ls()[str_detect(string = ls(), pattern = "_data_dic")]
derived_data_dic_patterns <- str_c("^", DERIVED_DATASET_LIST, "_", collapse = "|")
derived_data_dic <- derived_data_dic[str_detect(string = derived_data_dic, pattern = derived_data_dic_patterns)]
DERIVED_DATADIC <- bind_rows(mget(derived_data_dic))

derived_datadic_dir_path <- file.path(".", "data-raw", "derived-datadic")
if (dir.exists(derived_datadic_dir_path)) {
  unlink(derived_datadic_dir_path, recursive = TRUE)
  warning("`", derived_datadic_dir_path, "` directory is removed!")
}
dir.create(derived_datadic_dir_path)
save(
  list = "DERIVED_DATADIC",
  file = file.path(derived_datadic_dir_path, "DERIVED_DATADIC.rda")
)
message("Completed generating derived/analysis datasets and data dictionary")
