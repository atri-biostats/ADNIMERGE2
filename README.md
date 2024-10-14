# ADNIMERGE2 <a href="https://adni.loni.usc.edu/"><img src="man/figures/logo.png" align="right" height="138" /></a>

This is an R data package for the Alzheimer's Disease Neuroimaging Initiative (ADNI) study. All datasets are included
as shared on <https://adni.loni.usc.edu/>.


# Installation 

To build and install the package:  
  * Download the ADNI study data from [https://adni.loni.usc.edu/data-samples/adni-data/](https://adni.loni.usc.edu/data-samples/adni-data/) either in `*.zip` or `*.csv` file format, and copy to [data-raw](https://github.com/atrihub/ADNIMERGE2/tree/main/data-raw)
  * `source('tools/build.R', chidr = TRUE)` to prepare datasets, generate documentations and build R package
     - Data preparation: 
         + [`source("../data-raw/data_prep.R", chidr =TRUE)`](https://github.com/atrihub/ADNIMERGE2/tree/main/data-raw/data_prep.R) is used to extract `*.csv` files from `data-raw/*.zip` files and to store all `data-raw/*.csv` files in `data/*.rda` using [`use_data`](https://usethis.r-lib.org/reference/use_data.html)
       
        + [`source("../data-raw/data_prep_recode.R", chidr =TRUE)`](https://github.com/atrihub/ADNIMERGE2/tree/main/data-raw/data_prep_recode.R) is used to replace recoded values of a dataset using data dictionary file.
     - Generate documentations: [`source('document.R')`](https://github.com/atrihub/ADNIMERGE2/tree/main/tools/document.R) is used to generate documentations based on the `ADNIMERGE::DATADIC` and actual dataset value
    
  * To install the package locally `install.packages(ADNIMERGE2, repos = NULL, type = "source")`
