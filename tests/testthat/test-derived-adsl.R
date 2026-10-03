library(testthat)

test_that("Check ADSL derived dataset", {
  adsl_orig_prot <- as.character(ADSL$ORIGPROT)
  adsl_orig_prot <- unique(adsl_orig_prot)
  adsl_orig_prot <- sort(adsl_orig_prot)
  expect_identical(
    object = adsl_orig_prot,
    expected = sort(adni_phase()),
    info = "Check study phase in ADSL"
  )
})
