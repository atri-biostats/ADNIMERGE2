# Package Build ----
## Libraries ----
library(devtools)
library(tidyverse)
library(callr)
library(knitr)

## Set working directory ----
## To set the project directory as working directory
## cur_script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
## cur_script_dir <- str_remove(string = cur_script_dir, pattern = "/tools$")
## setwd(cur_script_dir)

## Data preparation ----
# Data downloaded date
DATA_DOWNLOADED_DATE <- "2025-02-25"
callr::rscript(
  script = "./data-raw/data_prep.R",
  wd = ".",
  cmdargs = list(DATA_DOWNLOADED_DATE = DATA_DOWNLOADED_DATE)
)

DECODE_VALUE <- TRUE

if (DECODE_VALUE) {
  # A boolean indicator to use the updated data dictionary.
  # Only applicable if the UPDATED_DATADIC is created.
  # Please see line 385 in the `./data-raw/data_prep.R`.
  USE_UPDATED_DATADIC <- TRUE
  callr::rscript(
    script = "./data-raw/data_prep_recode.R",
    wd = ".",
    cmdargs = list(USE_UPDATED_DATADIC = USE_UPDATED_DATADIC)
  )
} else {
  USE_UPDATED_DATADIC <- FALSE
}

## Generate derived/analysis dataset ----
INCLUDE_DERIVED_DATASET <- TRUE

if (INCLUDE_DERIVED_DATASET) {
  # List of derived dataset
  DERIVED_DATASET_LIST <- c(
    "DM", "AE", "QS", "SC", "RS", "NV", "BS","VS",
    "ADSL", "ADAE", "ADQS", "METACORES"
  )
  callr::rscript(
    script = "./tools/generate_derived_data.R",
    wd = ".",
    cmdargs = list(DERIVED_DATASET_LIST = DERIVED_DATASET_LIST)
  )
} else {
  DERIVED_DATASET_LIST <- NULL
}

## Generate documentations ----
callr::rscript(
  script = "./tools/document.R",
  wd = ".",
  cmdargs = list(
    DERIVED_DATASET_LIST = DERIVED_DATASET_LIST,
    USE_UPDATED_DATADIC = USE_UPDATED_DATADIC
  )
)

## Finalize package building ----
devtools::load_all("./")
devtools::document()
devtools::check(error_on = "error", vignettes = INCLUDE_DERIVED_DATASET)
pkg_dir <- devtools::build(vignettes = INCLUDE_DERIVED_DATASET)
install.packages(pkg_dir)

# Build README.md ----
devtools::build_readme()

# Build website ----
# run once:
# For non manual website development:
# # usethis::use_pkgdown() # overwrite any existed `_pkgdown.yml` file
# For manual website development: required to modify the pre-existed `_pkgdown.yml` as needed
# pkgdown::build_site()
# publish online:
# # pkgdown::deploy_to_branch()
