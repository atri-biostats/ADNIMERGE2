# Package Build ----
## Libraries ----
library(devtools)
library(tidyverse)
library(callr)

## Set working directory ----
## To set active project directory as a working directory
setwd(rstudioapi::getActiveProject())

## Data preparation ----
DATA_DOWNLOADED_DATE <- "2026-01-05" # Data downloaded date YYYY-MM-DD format
UPDATE_DATADIC <- TRUE # Please see line 611 in the `./data-raw/data_prep.R`
callr::rscript(
  script = "./data-raw/data-prep.R",
  wd = ".",
  cmdargs = list(
    DATA_DOWNLOADED_DATE = DATA_DOWNLOADED_DATE,
    UPDATE_DATADIC = UPDATE_DATADIC
  )
)

### Replace coded values with actual values ----
DECODE_VALUE <- TRUE
USE_UPDATED_DATADIC <- UPDATE_DATADIC
if (DECODE_VALUE) {
  callr::rscript(
    script = "./data-raw/data-prep-recode.R",
    wd = ".",
    cmdargs = list(USE_UPDATED_DATADIC = USE_UPDATED_DATADIC)
  )
}

### Create data category for pkgdown -----
# Recommended to run this line for creating a website using pkgdown
CREATE_DATA_CATEGORY <- TRUE
if (CREATE_DATA_CATEGORY) {
  callr::rscript(
    script = "./data-raw/data-prep-category-pkgdown.R",
    wd = "."
  )
}

# Generate PACC score input data ----
VIGNETTE_DIR <- file.path(".", "vignettes")
TOOLS_DIR <- file.path(".", "tools")
TEST_DIR <- file.path(".", "tests", "testthat")

INCLUDE_PACC_DERIVED_DATA <- TRUE
# NOTE:
#  Required to install the latest version of `ADNIMERGE` and `ADNI4 ` R packages
#  `ADNI4` R package is only available internally
if (INCLUDE_PACC_DERIVED_DATA) {
  callr::rscript(
    script = "./tools/generate-pacc-input-data.R",
    wd = ".",
    cmdargs = list(DATA_DOWNLOADED_DATE = DATA_DOWNLOADED_DATE)
  )
} else {
  # Transfer PACC scoring article from "./vignettes" to "./tools"
  # when PACC input raw data are not generated.
  callr::rscript(
    script = "./tools/remove-files.R",
    wd = ".",
    cmdargs = list(
      INPUT_DIR = VIGNETTE_DIR,
      OUTPUT_DIR = TOOLS_DIR,
      PATTERN = "ADNIMERGE2-PACC\\.Rmd$"
    )
  )
  # Transfer PACC unit test file
  callr::rscript(
    script = "./tools/remove-files.R",
    wd = ".",
    cmdargs = list(
      INPUT_DIR = TEST_DIR,
      OUTPUT_DIR = TOOLS_DIR,
      PATTERN = "test-baseline-pacc\\.R$"
    )
  )
}

## Generate derived/analysis dataset ----
INCLUDE_DERIVED_DATASET <- TRUE

## Modify default PACC score related params YAML in vignettes
# Required to modify the default `INCLUDE_PACC` param YAML if:
#  I. PACC score input raw data are not generated or
#  II. To generate some derived dataset other than PACC score
MODIFY_PACC_PARAM <- FALSE
MODIFY_PACC_PARAM <- (INCLUDE_DERIVED_DATASET & !INCLUDE_PACC_DERIVED_DATA)
if (MODIFY_PACC_PARAM) {
  callr::rscript(
    script = "./tools/vignettes-yaml.R",
    wd = ".",
    cmdargs = list(
      INPUT_DIR = VIGNETTE_DIR,
      CURRENT_PACC_PARAM = "INCLUDE_PACC: TRUE",
      NEW_PACC_PARAM = "INCLUDE_PACC: FALSE"
    )
  )
} else {
  cli::cli_alert_info(
    text = "Not required to modify PACC score param YAML value in vignettes!"
  )
}

if (INCLUDE_DERIVED_DATASET) {
  # List of derived dataset
  DERIVED_DATASET_LIST <- c(
    "DM", "AE", "QS", "SC", "RS", "NV", "LB", "VS", "GF",
    "ADSL", "ADAE", "ADQS", "ADRS", "METACORES"
  )
  if (INCLUDE_PACC_DERIVED_DATA) {
    DERIVED_DATASET_LIST <- c("PACC", DERIVED_DATASET_LIST)
  }
  callr::rscript(
    script = "./tools/generate-derived-data.R",
    wd = ".",
    cmdargs = list(DERIVED_DATASET_LIST = DERIVED_DATASET_LIST)
  )
} else {
  DERIVED_DATASET_LIST <- NULL
  callr::rscript(
    script = "./tools/remove-files.R",
    wd = ".",
    cmdargs = list(
      INPUT_DIR = VIGNETTE_DIR,
      OUTPUT_DIR = TOOLS_DIR,
      PATTERN = "\\.Rmd$"
    )
  )
  # Transfer corresponding unit tests to "./tools"
  callr::rscript(
    script = "./tools/remove-files.R",
    wd = ".",
    cmdargs = list(
      INPUT_DIR = TEST_DIR,
      OUTPUT_DIR = TOOLS_DIR,
      PATTERN = "test-derived|test-PET"
    )
  )
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

## Additional package related files ----
# # run once:
# # Package license
# usethis::use_mit_license()
# # Package news
# # Caution of overwriting any existing `NEWS.md` file
# usethis::use_news_md()

## Finalize package building ----
devtools::load_all("./")
devtools::document()
devtools::check(error_on = "error", vignettes = INCLUDE_DERIVED_DATASET)
pkg_dir <- devtools::build(vignettes = INCLUDE_DERIVED_DATASET)
install.packages(pkgs = pkg_dir, repos = NULL)

# Build README.md ----
devtools::build_readme()

# # Build website ----
# # run once:
# # To clean any existing site on local machine
# pkgdown::clean_site()
# # Caution of overwriting any existing `_pkgdown.yml` file
# usethis::use_pkgdown()
# pkgdown::check_pkgdown()
# pkgdown::build_site()
# # Publish website online ----
# # To publish a site online via GitHub repo: PUBLISH_SITE = TRUE
# # PUBLISH_SITE <- FALSE
# if (PUBLISH_SITE) {
#   pkgdown::deploy_to_branch()
# }
