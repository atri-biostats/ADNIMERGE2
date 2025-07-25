library(testthat)
library(sdtmchecks)

pkg <- "ADNIMERGE2"
source(system.file("derived-dataset-check-script.R", package = pkg))

test_that("Check run_check_derived_dataset function", {
  run_check_derived_dataset(
    pkg = pkg,
    dataset_name = c("DM", "AE", "QS", "RS", "LB", "VS"),
    export_report = FALSE,
    apply_testthat = FALSE,
    ncores = 1
  )
})
