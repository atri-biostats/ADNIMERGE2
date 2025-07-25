library(testthat)
library(tidyverse)
library(assertr)
library(labelled)

# Replace Multiple Values ----
test_that("Check replace_multiple_values function", {
  # Values that are listed here are testing purpose only
  # Test 1 ----
  input_string1 <- c("-2", "1", "1", "-1")
  code1 <- c("null", "1", "-1", "-2")
  decode1 <- c(
    "pending enrollment", "randomized - assigned a scan category",
    "screen failed", "screen failed after randomization"
  )
  replaced_values1 <- replace_multiple_values(
    input_string = input_string1,
    code = code1,
    decode = decode1
  )
  pre_values1 <- c(
    "screen failed after randomization", "randomized - assigned a scan category",
    "randomized - assigned a scan category", "screen failed"
  )
  expect_identical(
    object = replaced_values1,
    expected = pre_values1,
    info = "Check replace_multiple_values function: test-1"
  )

  # Test 2 ----
  data_dict <- get_factor_levels_datadict(
    .datadic = DATADIC,
    tbl_name = "ADAS"
  ) %>%
    datadict_as_tibble() %>%
    filter(FLDNAME %in% "COT1LIST") %>%
    verify(nrow(.) == 10)

  input_string2 <- c("1:2:3:6:8:10")
  replaced_values2 <- replace_multiple_values(
    input_string = input_string2,
    code = data_dict$prefix,
    decode = data_dict$suffix
  )
  pre_values2 <- paste0(data_dict$suffix[c(1, 2, 3, 6, 8, 10)], collapse = ":")
  expect_identical(
    object = replaced_values2,
    expected = pre_values2,
    info = "Check replace_multiple_values function: test-2"
  )

  # Test 3 ----
  input_string3 <- c("0:2:3:08:10")
  replaced_values3 <- replace_multiple_values(
    input_string = input_string3,
    code = data_dict$prefix,
    decode = data_dict$suffix
  )
  pre_values3 <- paste0(c("0", data_dict$suffix[c(2, 3, 8, 10)]), collapse = ":")
  pre_values3 <- str_replace_all(string = pre_values3, pattern = "TICKET", replacement = "08")
  expect_identical(
    object = replaced_values3,
    expected = pre_values3,
    info = "Check replace_multiple_values function: test-3"
  )

  # Test 4 ----
  code4 <- c("1", "2", "3")
  decode4 <- c(
    "4 frames, 5 min/frame (4x300s)",
    "2 scans, 10 min each (2x600s) (only for BioGraph scanners without list-mode)",
    "1 frame, 20 min (1x1200s) (only for the oldest BioGraph scanners:  Models 1023 or 1024)"
  )
  input_string4 <- c("1", "2:1", "3")
  replaced_values4 <- replace_multiple_values(
    input_string = input_string4,
    code = code4,
    decode = decode4
  )
  pre_values4 <- c(
    "4 frames, 5 min/frame (4x300s)",
    "2 scans, 10 min each (2x600s) (only for BioGraph scanners without list-mode):4 frames, 5 min/frame (4x300s)",
    "1 frame, 20 min (1x1200s) (only for the oldest BioGraph scanners:  Models 1023 or 1024)"
  )
  expect_identical(
    object = replaced_values4,
    expected = pre_values4,
    info = "Check replace_multiple_values function: test-4"
  )

  # Test 5 ----
  data_dict2 <- get_factor_levels_datadict(
    .datadic = DATADIC,
    tbl_name = "ADAS"
  ) %>%
    datadict_as_tibble() %>%
    filter(FLDNAME %in% "Q9TASK") %>%
    filter(PHASE %in% "ADNIGO") %>%
    verify(nrow(.) == 6) %>%
    arrange(prefix)

  code5 <- as.character(seq(0, 5, by = 1))
  decode5 <- data_dict2$suffix
  input_string5 <- c("1", "2;1", "3", "5")
  replaced_values5 <- replace_multiple_values(
    input_string = input_string5,
    code = code5,
    decode = decode5
  )
  pre_values5 <- c(
    "Very mild: forgets once (1)",
    "Mild: must be reminded two times (2);Very mild: forgets once (1)",
    "Moderate: must be reminded 3-4 times",
    "Severe: must be reminded 7 or more times."
  )
  expect_identical(
    object = replaced_values5,
    expected = pre_values5,
    info = "Check replace_multiple_values function: test-5"
  )
})
