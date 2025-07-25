# Libraries ----
library(tidyverse)
library(devtools)
library(knitr)
library(rlang)
library(cli)

# Input parameter - derived/analysis datasets ----
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1) {
  cli::cli_abort(
    message = c(
      "Input argument {.val DERIVED_DATASET_LIST} must be size of 1. \n",
      "{.val arg} is a length of {.val {length(DERIVED_DATASET_LIST)}}"
    )
  )
}
DERIVED_DATASET_LIST <- str_remove_all(string = args, pattern = '[\\(\\)]|\\"|^c') %>%
  str_split(string = ., pattern = ",") %>%
  unlist() %>%
  str_trim(string = ., side = "both")
if (all(DERIVED_DATASET_LIST %in% "NULL") | any(is.na(DERIVED_DATASET_LIST))) {
  cli::cli_abort(
    message = c(
      "{.val DERIVED_DATASET_LIST} must not be missing. \n",
      "The value of {.var DERIVED_DATASET_LIST} is {.val {DERIVED_DATASET_LIST}}."
    )
  )
}

# Generate derived and analysis datasets using metadata specs ----
devtools::load_all("./")

# Load system files\ utils function ----
utils_file_list <- list.files(
  path = "./inst", pattern = "\\.R$", all.files = TRUE,
  full.names = TRUE, recursive = FALSE
)
load_result <- lapply(utils_file_list, source)

## Derived dataset -----
vignette_dir <- file.path(".", "vignettes")

temp_file <- tempfile()
derived_vignette_path <- file.path(vignette_dir, "ADNIMERGE2-Derived-Data.Rmd")
knitr::purl(input = derived_vignette_path, output = temp_file)
source(file = temp_file)

## Metadata specs for analysis dataset ----
metaspecs_vignette_path <- file.path(vignette_dir, "ADNIMERGE2-Analysis-Meta-Specs.Rmd")
knitr::purl(input = metaspecs_vignette_path, output = temp_file)
source(file = temp_file)

## Analysis dataset ----
analysis_vignette_path <- file.path(vignette_dir, "ADNIMERGE2-Analysis-Data.Rmd")
knitr::purl(input = analysis_vignette_path, output = temp_file)
source(file = temp_file)

## Store all derived objects in `./data` directory ----
### using use_data_modified() function
source(file = file.path(".", "tools", "data-prepare-utils.R"))
save_derived_data <- lapply(
  DERIVED_DATASET_LIST,
  function(tbl_name) {
    use_data_modified(
      dataset_name = tbl_name,
      dataset = get(tbl_name),
      edit_type = "create",
      run_script = TRUE
    )
  }
)

save_derived_data <- unlist(save_derived_data)
if (!all(save_derived_data == TRUE)) {
  cli::cli_abort(
    message = c(
      paste0(
        "At least one the derived datasets {.val {DERIVED_DATASET_LIST}} ",
        "has not been saved in the `./data` directory."
      )
    )
  )
}

# Derived/Analysis data dictionary ----
DERIVED_DATASET_LIST <- DERIVED_DATASET_LIST[!DERIVED_DATASET_LIST %in% "METACORES"]
DERIVED_DATASET_LIST <- str_to_lower(DERIVED_DATASET_LIST)
derived_data_dic <- ls()[str_detect(string = ls(), pattern = "_data_dic")]
derived_data_dic_patterns <- str_c("^", DERIVED_DATASET_LIST, "_", collapse = "|")
derived_data_dic <- derived_data_dic[str_detect(string = derived_data_dic, pattern = derived_data_dic_patterns)]
DERIVED_DATADIC <- bind_rows(mget(derived_data_dic)) %>%
  select(all_of(c("TBLNAME", "CRFNAME", "FLDNAME", "LABEL", "TEXT"))) %>%
  mutate(TEXT = ifelse(is.na(TEXT), " ", TEXT))

derived_datadic_dir_path <- file.path(".", "data-raw", "derived-datadic")
if (dir.exists(derived_datadic_dir_path)) {
  unlink(derived_datadic_dir_path, recursive = TRUE)
  cli::cli_alert_warning(
    text = "{.val {derived_datadic_dir_path}} directory is removed"
  )
}
dir.create(derived_datadic_dir_path)
save(
  list = "DERIVED_DATADIC",
  file = file.path(derived_datadic_dir_path, "DERIVED_DATADIC.rda")
)
# Save in the `./data` directory
use_data_modified(
  dataset_name = "DERIVED_DATADIC",
  dataset = DERIVED_DATADIC,
  edit_type = "create",
  run_script = TRUE
)
cli::cli_alert_success(
  text = "Completed generating derived/analysis datasets and data dictionary"
)
