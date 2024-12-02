# Package Build ----
## Libraries ----
library(devtools)
library(tidyverse)
library(callr)
library(knitr)

## Set working directory ----
## To set the project directory as working directory
## current_script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
## current_script_dir <- str_remove(current_script_dir, "/tools")
## setwd(current_script_dir)

## Data preparation ----
callr::rscript(
  script = "./data-raw/data_prep.R",
  wd = "."
)

RECODE_VALUE <- TRUE

if (RECODE_VALUE) {
  callr::rscript(
    script = "./data-raw/data_prep_recode.R",
    wd = "."
  )
}

UPDATE_DERIVED_DATASET <- TRUE

if (UPDATE_DERIVED_DATASET) {
  # Generate derived dataset and stored in `./data` directory -----
  devtools::load_all("./")
  temporary_file <- tempfile()
  vignette_file_path <- file.path(".", "vignettes", "ADNIMERGE2-Derived-Data.Rmd")
  knitr::purl(input = vignette_file_path, output = temporary_file)
  source(file = temporary_file)
  DERIVED_DATASET_LIST <- c("DM", "AE", "QS")

  # To apply use_data() function: use_data_modified()
  source(file = file.path(".", "tools", "data-prepare-utils.R"))
  lapply(DERIVED_DATASET_LIST, function(tbl_name) {
    assign("dd", get(tbl_name))
    # using use_data function
    use_data_modified(
      dataset_name = tbl_name,
      dataset = dd,
      edit_type = "create",
      run_script = TRUE
    )
    rm(list = c("dd", "tbl_name"))
  })

  # Derived data dictionary
  derived_data_dic <- ls()[str_detect(string = ls(), pattern = "_data_dic")]
  derived_data_dic_patterns <- str_c("^",
    str_to_lower(DERIVED_DATASET_LIST),
    "_",
    collapse = "|"
  )
  derived_data_dic <- derived_data_dic[str_detect(
    string = derived_data_dic,
    pattern = derived_data_dic_patterns
  )]

  DERIVED_DATADIC <- mget(derived_data_dic) %>%
    bind_rows()

  derived_datadic_dir_path <- file.path(".", "data-raw", "derived-datadic")
  unlink(derived_datadic_dir_path, recursive = TRUE)
  dir.create(derived_datadic_dir_path)
  save(
    list = "DERIVED_DATADIC",
    file = file.path(derived_datadic_dir_path, "DERIVED_DATADIC.rda")
  )
} else {
  DERIVED_DATASET_LIST <- NULL
}

## Generate documentations ----
callr::rscript(
  script = "./tools/document.R",
  wd = ".",
  cmdargs = list(DERIVED_DATASET_LIST = DERIVED_DATASET_LIST)
)

## Finalized package building ----
devtools::load_all("./")
devtools::document()
devtools::check(error_on = "error", vignettes = UPDATE_DERIVED_DATASET)
pkg_dir <- devtools::build(vignettes = UPDATE_DERIVED_DATASET)

# Build README.md ----
build_readme()

# Build website ----
# run once:
# usethis::use_pkgdown()
# pkgdown::build_site()
# publish online:
# pkgdown::deploy_to_branch()
