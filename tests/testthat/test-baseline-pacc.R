library(testthat)
library(tidyverse)
library(assertr)

set_as_dataframe <- function(.data) {
  .data <- .data %>%
    data.frame() %>%
    as_tibble() %>%
    mutate(across(everything(), as.character))
  return(.data)
}

lapply(c("mPACCtrailsB", "mPACCdigit"), function(score_var) {
  test_that("Check baseline modified-PACC score", {
    # Enrollment data
    enrolled_data <- get_adni_enrollment(.registry = REGISTRY) %>%
      filter(ENRLFG %in% "Yes") %>%
      select(RID, ORIGPROT) %>%
      arrange(RID, ORIGPROT) %>%
      as_tibble()

    # Baseline modified PACC score based on PACC
    bl_paccscore_pacc <- enrolled_data %>%
      left_join(
        PACC %>%
          set_as_dataframe() %>%
          filter(ENRLFG %in% "Yes") %>%
          filter(ORIGPROT == COLPROT) %>%
          # mPACCdigit only in ADNI1 phase
          {
            if (score_var %in% "mPACCdigit") {
              filter(., ORIGPROT == adni_phase()[1])
            } else {
              (.)
            }
          } %>%
          filter(VISCODE %in% get_baseline_vistcode()) %>%
          assert_uniq(RID) %>%
          mutate(across(all_of(score_var), ~ as.numeric(.x), .names = toupper(score_var))) %>%
          mutate(
            RID = as.numeric(RID)
          ) %>%
          select(all_of(c("RID", "ORIGPROT", toupper(score_var)))),
        by = c("RID", "ORIGPROT")
      )

    # Baseline modified PACC score based on ADQS
    bl_paccscore_adqs <- enrolled_data %>%
      left_join(
        ADQS %>%
          set_as_dataframe() %>%
          filter(PARAMCD %in% toupper(score_var)) %>%
          filter(ABLFL %in% "Y" & ENRLFL %in% "Y") %>%
          mutate(
            RID = convert_usubjid_to_rid(USUBJID),
            ORIGPROT = original_study_protocol(RID)
          ) %>%
          {
            if (score_var %in% "mPACCdigit") {
              filter(., ORIGPROT == adni_phase()[1])
            } else {
              (.)
            }
          } %>%
          mutate(across(AVAL, ~ as.numeric(.x), .names = toupper(score_var))) %>%
          select(all_of(c("RID", "ORIGPROT", toupper(score_var)))),
        by = c("RID", "ORIGPROT")
      )

    expect_identical(
      object = bl_paccscore_pacc,
      expected = bl_paccscore_adqs,
      info = paste0("Check baseline ", score_var, "score based on `PACC` and `ADQS` records")
    )
  })
})
