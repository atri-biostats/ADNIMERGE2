# Building ADNIMERGE2 from csv files <a href="https://adni.loni.usc.edu/"><img src="../man/figures/logo.png" align="right" height="138" /></a>

The document describes the procedure to build an R data package from source `.csv` files with similar workflow. Assumed the following pre-conditions are existed/completed in the beginning.

 a. Local R package working directory: If not existed, it can be created using [usethis::create_package](https://usethis.r-lib.org/reference/create_package.html) function.
 
 b. Package metadata `DESCRIPTION` file (similar to [this](https://github.com/atri-biostats/ADNIMERGE2/blob/main/DESCRIPTION)): It can be created using [usethis::use_description](https://usethis.r-lib.org/reference/use_description.html) function and modified as necessary. 

To build a package from source `.csv` files with similar workflow, clone the [https://github.com/atri-biostats/ADNIMERGE2](https://github.com/atri-biostats/ADNIMERGE2) repository. This will create the following directories:

   + `data-raw`: to store raw-data
   + `R`: to store package-specific defined functions that includes utils function
   + `vignettes`: to add guidance document/article about the package if necessary
   + `tests`: to store package related test and retest scripts.
   + `tools`: to store all user defined additional utils script/function that includes package building script.

* Download the ADNI study data from the data-shared platform at [https://adni.loni.usc.edu/data-samples/adni-data/](https://adni.loni.usc.edu/data-samples/adni-data/) either in `*.zip` or `*.csv` file format, and store the files in [`./data-raw`](https://github.com/atri-biostats/ADNIMERGE2/tree/main/data-raw) directory
   
   + Required to downloaded a data dictionary `*.csv` file
   
   + It is recommend to download files from the data-shared platform on the same date and without adding any file name prefix.

* Copy all pre-defined scripts/functions as necessary within the above-mentioned directory of the [ADNIMERGE2 github repository](https://github.com/atri-biostats/ADNIMERGE2) with the same file path

* Run `source('tools/build.R')` to prepare dataset, generate documentations and build R package. More details about the main procedures in [`build.R`](https://github.com/atri-biostats/ADNIMERGE2/tree/main//tools/build.R) script are presented as follows: 

  + Data preparation: 
    
    - [`./data-raw/data_prep.R`](https://github.com/atri-biostats/ADNIMERGE2/tree/main/data-raw/data_prep.R): 
         
         + To store all dataset in *`./data`* directory using [`usethis::use_data()`](https://usethis.r-lib.org/reference/use_data.html)
         
         + Some additional data preparation, please see  [here](https://github.com/atri-biostats/ADNIMERGE2/tree/main/data-raw/data_prep.R) for more information. 
         
         + Required the date of data downloaded (`DATA_DOWNLOADED_DATE`) as an input argument
         
    - [`./data-raw/data_prep_recode.R`](https://github.com/atri-biostats/ADNIMERGE2/tree/main/data-raw/data_prep_recode.R): 
         
         + To decode dataset values based on a data dictionary as necessary
         
         + Required a boolean input argument regarding whether to use an UPDATED data dictionary (`USE_UPDATED_DATADIC`)
         
    - [`./tools/generate_derived_data.R`](https://github.com/atri-biostats/ADNIMERGE2/tree/main/tools/generate_derived_data.R):  
         + To generate derived dataset
         
         + Required list of derived dataset (`DERIVED_DATASET_LIST`) as an input argument and also all `.Rmd` files in the [`vignettes`](https://github.com/atri-biostats/ADNIMERGE2/tree/main/vignettes) directory
      
  + Generate data-related documentations:
    
    - Based on available data in the [`./data`](https://github.com/atri-biostats/ADNIMERGE2/tree/main/data) directory including a data dictionary using user-defined function.
    
    - [`./tools/document.R`](https://github.com/atri-biostats/ADNIMERGE2/tree/main/tools/document.R):
        
        + Using actual dataset values and a data dictionary 
        
        + Required two input arguments: a boolean indicator to use an updated data dictionary (`USE_UPDATED_DATADIC`) and list of derived dataset (`DERIVED_DATASET_LIST`)
          
  + Build the data package: using [`devtools` R package](https://devtools.r-lib.org/)
