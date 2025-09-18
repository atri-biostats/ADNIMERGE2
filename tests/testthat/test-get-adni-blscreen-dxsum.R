library(testthat)
library(tidyverse)
library(assertr)

test_that("Check get_adni_blscreen_dxsum function", {
  set_as_dataframe <- function(.data) {
    .data <- .data %>%
      data.frame() %>%
      as_tibble() %>%
      mutate(across(everything(), as.character))
    return(.data)
  }

  # Enrollment data
  enrolled_data <- get_adni_enrollment(.registry = REGISTRY) %>%
    filter(ENRLFG %in% "Yes") %>%
    select(RID, ORIGPROT) %>%
    arrange(RID, ORIGPROT) %>%
    as_tibble()

  # Baseline DX status based on the function
  bl_dxsum <- enrolled_data %>%
    left_join(
      get_adni_blscreen_dxsum(
        .dxsum = DXSUM,
        visit_type = "baseline",
        phase = "Overall"
      ) %>%
        mutate(DX = ifelse(DIAGNOSIS %in% "Dementia", "DEM", DIAGNOSIS)) %>%
        select(RID, ORIGPROT, DX),
      by = c("RID", "ORIGPROT")
    )

  # Baseline DX status based on ADSL
  bl_dxsum_adsl <- ADSL %>%
    set_as_dataframe() %>%
    filter(ENRLFL %in% "Y") %>%
    mutate(RID = convert_usubjid_to_rid(USUBJID)) %>%
    arrange(RID, ORIGPROT) %>%
    select(RID, ORIGPROT, DX)

  # Baseline DX status based on RS data
  bl_dxsum_rs <- enrolled_data %>%
    left_join(
      RS %>%
        set_as_dataframe() %>%
        filter(RSBLFL %in% "Y") %>%
        mutate(
          RID = convert_usubjid_to_rid(USUBJID),
          COLPROT = RSGRPID,
          DX = RSORRES
        ) %>%
        select(RID, COLPROT, DX),
      by = c("RID" = "RID", "ORIGPROT" = "COLPROT")
    )

  expect_identical(
    object = bl_dxsum_adsl,
    expected = bl_dxsum,
    info = "Check get_adni_blscreen_dxsum function based on `DXSUM` and `ADSL` records"
  )

  expect_identical(
    object = bl_dxsum_rs,
    expected = bl_dxsum,
    info = "Check get_adni_blscreen_dxsum function based on `DXSUM` and `RS` records"
  )
})
