# Modify the default param YAML of `INCLUDE_PACC` in vignettes -----
## This allows to modify the default/current rmarkdown param yaml using script rather than manual update

## Util-functions ----
source(file.path(".", "tools", "yaml-utils.R"))
source(file.path(".", "tools", "data-prepare-utils.R"))
source(file.path(".", "tools", "prepare-datadict.R"))

## Input args ----
arg_list <- commandArgs(trailingOnly = TRUE)
check_arg(arg_list, 3)
INPUT_DIR <- arg_list[1]
CURRENT_PACC_PARAM <- arg_list[2]
NEW_PACC_PARAM <- arg_list[3]

## Modify param YAML -----
modify_rmd_param_yaml(
  dir_path = INPUT_DIR,
  file_path = NULL,
  current_param = CURRENT_PACC_PARAM,
  new_param = NEW_PACC_PARAM, 
  value_as_logical = TRUE
)