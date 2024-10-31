# Building A4LEARN from csv files <a href="https://www.a4studydata.org/"><img src="../man/figures/logo.png" align="right" height="138" /></a>

To generate the package from source csv files:

* Download the ADNI study data from [https://adni.loni.usc.edu/data-samples/adni-data/](https://adni.loni.usc.edu/data-samples/adni-data/) either in `*.zip` or `*.csv` file format, and copy to [data-raw](https://github.com/atrihub/ADNIMERGE2/tree/main/data-raw)
* `source('./tools/build.R')` to prepare datasets, generate documentations and build R package
* [`source("./data-raw/data_prep.R", chdir = TRUE)`](https://github.com/atrihub/ADNIMERGE2/tree/main/data-raw/data_prep.R) is used to extract `*.csv` files from `data-raw/*.zip` files and to store all `data-raw/*.csv` files in `data/*.rda` using [`use_data`](https://usethis.r-lib.org/reference/use_data.html)
* [`source("./data-raw/data_prep_recode.R", chdir = TRUE)`](https://github.com/atrihub/ADNIMERGE2/tree/main/data-raw/data_prep_recode.R) is used to replace recoded values of a dataset using data dictionary file `ADNIMERGE2::DATADIC`.
* Generate documentation: [`source('./tools/document.R', chdir = TRUE)`](https://github.com/atrihub/ADNIMERGE2/tree/main/tools/document.R) is used to generate documentations based on the `ADNIMERGE2::DATADIC` and actual dataset value
