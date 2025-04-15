library(testthat)
library(tidyverse)
library(assertr)

# PET Scan Tracer Type in ADNI ----
test_that("Check adni_enrollment function", {
  # Enrollment summary based on registry
  enroll_summary <- adni_enrollment(data_registry = REGISTRY) %>%
    verify(ORIGPROT == COLPROT) %>%
    group_by(ORIGPROT) %>%
    count() %>%
    ungroup() %>%
    arrange(ORIGPROT)

  # Enrollment based on DM
  enroll_summary_dm <- DM %>%
    mutate(ORIGPROT = as.character(ORIGPROT)) %>%
    filter(!is.na(RFSTDTC)) %>%
    group_by(ORIGPROT) %>%
    count() %>%
    ungroup() %>%
    arrange(ORIGPROT)

  # Enrollment based on ADSL
  enroll_summary_adsl <- ADSL %>%
    mutate(ORIGPROT = as.character(ORIGPROT)) %>%
    filter(ENRLFL %in% "Yes") %>%
    group_by(ORIGPROT) %>%
    count() %>%
    ungroup() %>%
    arrange(ORIGPROT)

  expect_identical(
    object = enroll_summary_dm,
    expected = enroll_summary,
    info = "Check adni_enrollment function based on registry and DM records"
  )

  expect_identical(
    object = enroll_summary_adsl,
    expected = enroll_summary,
    info = "Check adni_enrollment function based on registry and ADSL records"
  )

})
