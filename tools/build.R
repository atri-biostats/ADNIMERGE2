# Package Build ----
## Libraries ----
library(devtools)
library(tidyverse)
library(callr)

## Set working directory ----
## To set the project directory as working directory
## current_script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
## current_script_dir <- str_remove(current_script_dir, "/tools")
## setwd(current_script_dir)

## Data preparation ----
callr::rscript(
  script = "./data-raw/data_prep.R", 
  wd = '.'
)

RECODE_VALUE <- TRUE

if (RECODE_VALUE) {
  callr::rscript(
    script = "./data-raw/data_prep_recode.R", 
    wd = '.'
  )
}

## Generate documentations ----
callr::rscript(
  script = "./tools/document.R", 
  wd = '.'
)

## Finalized package building ----
devtools::load_all("./")
devtools::document()
devtools::check(error_on = "error", vignettes = FALSE)
pkg_dir <- devtools::build(vignettes = FALSE)

# Build README.md ----
build_readme()
