library(testthat)
library(tidyverse)

# Test derived dataset ----
pkg <- "ADNIMERGE2"
source(system.file("derived-labdata-utils.R", package = pkg))

# Batch list ----
# Batch values handled in `upennbiomk_value_limits()`
test_that("Check BATCH values in UPENNBIOMK_ROCHE_ELECSYS data", {
  possible_batch_list <- c(
    "ADNI1/GO/2 batch", "ADNI3 1st batch",
    "ADNI3 2nd batch", "ADNI3 3rd & 4th batches"
  )
  possible_batch_list <- sort(possible_batch_list)
  actual_batch_list <- UPENNBIOMK_ROCHE_ELECSYS %>%
    distinct(BATCH) %>%
    arrange(BATCH) %>%
    pull(BATCH)

  expect_identical(
    object = possible_batch_list,
    expected = actual_batch_list,
    info = paste0(
      "Check batch list in `UPENNBIOMK_ROCHE_ELECSYS` ",
      "data that are handled in upennbiomk_value_limits"
    )
  )
})

# Comment field ----
# Comment values handled in `adjust_lab_comment()`
test_that("Check COMMENT values in UPENNBIOMK_ROCHE_ELECSYS data", {
  possible_value <- tolower(c(
    "Abeta42>1700", "Abeta42<200", "Tau>1300, PTau>120", "Tau<80, PTau<8",
    "Ptau<8", "PTau>120", "Ptau<8"
  ))
  possible_value <- unique(possible_value)
  possible_value <- sort(possible_value)

  actual_comment <- UPENNBIOMK_ROCHE_ELECSYS %>%
    mutate(
      COMMENT = str_remove_all(COMMENT, ", recalculation failed|Sample Hemolyzed"),
      COMMENT = tolower(COMMENT)
    ) %>%
    distinct(COMMENT) %>%
    arrange(COMMENT) %>%
    filter(!is.na(COMMENT)) %>%
    pull(COMMENT)
  actual_comment <- actual_comment[!actual_comment %in% ""]
  actual_comment <- tolower(actual_comment)

  expect_identical(
    object = possible_value,
    expected = actual_comment,
    info = paste0(
      "Check comment field `UPENNBIOMK_ROCHE_ELECSYS` ",
      "data that are handled in adjust_lab_comment"
    )
  )
})
