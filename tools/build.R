# Package Build ----
## Libraries ----
library(devtools)
library(tidyverse)
library(callr)

## Set working directory ----
## To set active project directory as a working directory
setwd(rstudioapi::getActiveProject())

## Data preparation ----
DATA_DOWNLOADED_DATE <- "2025-07-10" # Data downloaded date YYYY-MM-DD format
UPDATE_DATADIC <- TRUE # Please see line 630 in the `./data-raw/data_prep.R`
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


# Generate PACC score input data ----
INCLUDE_PACC_DERIVED_DATA <- TRUE
# NOTE:
#  Required to install the latest version of `ADNIMERGE` and `ADNI4 `study R packages
#  `ADNI4` R data package is only available internally
if (INCLUDE_PACC_DERIVED_DATA) {
  callr::rscript(
    script = "./tools/generate-pacc-input-data.R",
    wd = ".",
    cmdargs = list(DATA_DOWNLOADED_DATE = DATA_DOWNLOADED_DATE)
  )
}

## Generate derived/analysis dataset ----
INCLUDE_DERIVED_DATASET <- TRUE

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
