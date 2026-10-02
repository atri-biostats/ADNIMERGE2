# NOTE 

* Because `ADNIMERGE2` is an R data package with frequent content updates, the change log focuses exclusively on major changes to R functions and package infrastructure rather than routine data refreshes.

# ADNIMERGE2 (development version)

* Breaking Changes:

   - `TBM22` dataset is renamed to `TBM`. The date stamped file name `TBM_03_17_2022` was previously truncated by the date stamp pattern, which only supported two-digit years.
   
   - The ADSP-PHC merged data dictionary (`ADSP_PHC_MERGED_DATADIC_<YYYYMMDD>`) and ADOPIC MUSE volumetrics dictionary (`MUSE_volume_ADNI123_Dictionary`) are converted into the `DATADIC` layout, renamed as `ADSP_PHC_DATADIC` and `MUSE_volume_ADNI123_DATADIC`, and included in `DATADIC`.

* Minor Changes/Bugs Fix

  - `compute_pacc_score()`: fix missing `cur_column()` import, Trails B negative value check (now applied only with `rescale_trailsB = TRUE` and ignores missing values), correlation check with an all-missing component, duplicated rows in long format output, the warning about pre-existing z-score columns, and the deprecated `rescale_trialsB` argument. A `LOG_<TRABSCOR>` row in `bl.summary` is now used when available, and the log-scale requirement of `bl.summary` is documented.
  
  - `derive_pacc_data()`: fix default `data_source` argument, `DATA_SOURCE_DATE` field name and `PACC_DATADICT` output name.
  
  - `get_adni_screen_date()` and `get_adni_blscreen_dxsum()`: allow multiple study phases. `get_adni_screen_date()` returns `VISCODE` only if `multiple_screen_visit = TRUE`, as documented.
  
  - `original_study_protocol()`: `RID` 12000 is assigned to `TEAM` phase.
  
  - `detect_baseline_score()`: handle missing record dates.
  
  - Package build: fix fuzzy join with floating point string distances in `left_fuzzy_join()`, duplicated records in the updated `DATADIC`, `ORIGPROT`/`COLPROT` descriptions in `DATADIC`, decoding of single-phase datasets without a phase column, and removal of previously extracted raw data files.
  
  - Require `dplyr (>= 1.2.0)` and skip `sdtmchecks` based tests if it is not installed. Add unit tests for `compute_pacc_score()`.

# ADNIMERGE2 0.1.3

* New Features: 
   
   - `derive-pacc-data()` function to generate `PACC` score data using `ADNIMERGE2-PACC` vignettes #35, #36
     
     + `list_pacc_dataset()` internal function to list all required datasets to generate `PACC` score data using `derive-pacc-data()` wrapper function 
   
   - Include TEAM-ADNI study dataset in package build
     
     + `keep_teamadni()` and `filter_out_teamadni()` internal functions to keep or filter out records that are associated or collected in TEAM-ADNI study phase, respectively. 7c3f0c2 #32 
   
* Minor Changes/Bugs Fix

  - `componentVars` argument in `compute_pacc_score()` is now required a named list object in order to make sure to compute correct total `PACC` score 88fd67e
  
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
