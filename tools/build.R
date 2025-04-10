# Package Build ----
## Libraries ----
library(devtools)
library(tidyverse)
library(callr)

## Set working directory ----
## To set the project directory as a working directory
# cur_script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
# cur_script_dir <- str_remove(string = cur_script_dir, pattern = "/tools$")
# setwd(cur_script_dir)

## Data preparation ----
DATA_DOWNLOADED_DATE <- "2025-02-25" # Data downloaded date
UPDATE_DATADIC <- TRUE # Please see line 490 in the `./data-raw/data_prep.R`
callr::rscript(
  script = "./data-raw/data_prep.R",
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
    script = "./data-raw/data_prep_recode.R",
    wd = ".",
    cmdargs = list(USE_UPDATED_DATADIC = USE_UPDATED_DATADIC)
  )
}

## Generate derived/analysis dataset ----
INCLUDE_DERIVED_DATASET <- TRUE

if (INCLUDE_DERIVED_DATASET) {
  # List of derived dataset
  DERIVED_DATASET_LIST <- c(
    "DM", "AE", "QS", "SC", "RS", "NV", "BS", "VS",
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

# # Build website ----
# # run once:
# # To clean any existing site on local machine
# pkgdown::clean_site()
# # Caution about overwriting any existing `_pkgdown.yml` file
# usethis::use_pkgdown()
# pkgdown::check_pkgdown()
# # Initialize site in order to add `ADNI Data Use Agreement` document
# #  to "./docs/articles" directory
# pkgdown::init_site()
# docs_article_dir <- "docs/articles"
# if (!dir.exists(paths = file.path(".", docs_article_dir))) {
#   dir.create(path = file.path(".", docs_article_dir), recursive = TRUE)
# }
# rmarkdown::render(
#   input = file.path("vignettes", "ADNI-Study.Rmd"),
#   params = list(download_file_dir = file.path("..", docs_article_dir)),
#   quiet = TRUE
# )
# pkgdown::build_site()
# # Publish website online: ----
# # To publish a site: PUBLISH_SITE = TRUE
# PUBLISH_SITE <- FALSE
# if (PUBLISH_SITE) {
#   pkgdown::deploy_to_branch()
# }
