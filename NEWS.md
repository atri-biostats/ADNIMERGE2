# NOTE 

* Because `ADNIMERGE2` is an R data package with frequent content updates, the change log focuses exclusively on major changes to R functions and package infrastructure rather than routine data refreshes.

# ADNIMERGE2 0.1.2

* New Features: 

   - `list_derived_data()` function to list all available derived datasets in `ADNIMERGE2` R package
   
   - Set `check_list_names()` and `convert_f_viscode_to_sc()` as an exported function. Both functions were relocated from package build configurations.
   
   - `PACC` scores related util functions: `get_common_value()`, `deframe_as_list()` and `get_common_viscode2()`.

* Minor Changes/Bugs Fix

   - Fix #21 bugs in `get_required_dataset_list`. Thanks @jwang-lilly.
   
   - Minimize dependency R packages
   
   - Rename `rescale_trialsB` argument to `rescale_trailsB` in `compute_pacc_score` function.
   
   - Update PACC-scores generating steps to include `PTID` and `VISCODE2` variables
   
   - Fix measurement unit of ratio biomarkers

* Documentations

   - Update package vignettes content
   
   - Update roxygen2 syntax to allow `markdown` style

# ADNIMERGE2 0.1.1

* Initial data package.
