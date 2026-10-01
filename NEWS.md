# NOTE 

* Because `ADNIMERGE2` is an R data package with frequent content updates, the change log focuses exclusively on major changes to R functions and package infrastructure rather than routine data refreshes.

# ADNIMERGE2 0.1.3

* New Features: 
   
   - `derive-pacc-data()` function to generate `PACC` score data using `ADNIMERGE2-PACC` vignettes #35, #36
     
     + `list_pacc_dataset()` internal function to list all required datasets to generate `PACC` score data using `derive-pacc-data()` wrapper function #
   
   - Include TEAM-ADNI study dataset in package build
     
     + `keep_teamadni()` and `filter_out_teamadni()` internal functions to keep or filter out records that are associated or collected in TEAM-ADNI study phase, respectively. 7c3f0c2 #32 
   
* Minor Changes/Bugs Fix

  - `compute_pacc_score()` 
  
  - Fix bugs minor bugs in `extract_codelist_datadict()`, `derive_blfl_adni()` and `left_fuzzy_join()`
  
  - Expand unit test coverage for internal functions
  
* Documentations 
  
  - Update package vignettes format

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
