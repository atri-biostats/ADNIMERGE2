library(testthat)
library(sdtmchecks)

# Testing single derived dataset
test_that("Check DM derived dataset", {
  check_status <- sdtmchecks::check_dm_usubjid_dup(DM = DM)
  expect_identical(
    object = check_status,
    expected = TRUE,
    info = "Check unique `USUBJID` in DM derived dataset"
  )
})
