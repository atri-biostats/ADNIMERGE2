# ADNIMERGE data package build ----
library(devtools); library(tidyverse)

devtools::load_all('../')
source("../data-raw/data_prep.R")
source('document.R')
document()
check(error_on = 'error')
pkg_dir <- devtools::build(vignettes = FALSE)

