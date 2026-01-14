# Libraries ----
## Required R packages ----
# Install two packages from local directory
# run once --
# install.packages("./ADNIMERGE_0.0.1.tar.gz")
# # `ADNI4` R package is available for internal use-only
# install.packages("./ADNI4_0.0.1.tar.gz")

library(tidyverse)
library(assertr)
library(cli)

# Input Argument ----
args <- commandArgs(trailingOnly = TRUE)
source(file.path(".", "tools", "data-prepare-utils.R"))
check_arg(args, 1)
DATA_DOWNLOADED_DATE <- as.Date(args[1])
check_arg_date(DATA_DOWNLOADED_DATE)
# Compared with raw source dataset date
if (ADNI4::data_dump_date < DATA_DOWNLOADED_DATE) {
  cli::cli_abort(
    message = c(
      "{.var ADNI4} package must be downloaded on {.val {DATA_DOWNLOADED_DATE}}. \n",
      "{.var ADNI4} R package download date is {.val {ADNI4::data_dump_date}}."
    )
  )
}

devtools::load_all("./")

# Utility functions ------
#' @title Check for Unique Records
#' @description
#' This function is used to check unique records across the combination of
#' \code{COLPROT}, \code{RID} and \code{VISCODE} columns.
#' @param .data A data.frame
#' @return A data.frame
#' @rdname check_unique_record

check_unique_record <- function(.data) {
  .data <- .data %>%
    assert_uniq(all_of(c("COLPROT", "RID", "VISCODE")))
  return(.data)
}

#' @title Adjust Screening Visit Code in ADNI1 Phase
#' @description
#' This function is used to convert screen failure visit code \code{f} into
#' screening visit \code{sc} for records in ADNI phase.
#' @param .data A data.frame
#' @return A data.frame
#' @rdname convert_f_viscode_to_sc
#' @importFrom dplyr mutate case_when across
#' @importFrom tidyselect all_of

convert_f_viscode_to_sc <- function(.data, visitVar = "VISCODE") {
  require(dplyr)
  require(tidyselect)
  .data <- .data %>%
    mutate(across(all_of(visitVar), ~ case_when(.x %in% "f" ~ "sc", TRUE ~ .x)))
  return(.data)
}

# Prepare input data -----
## Get ADAS - Delayed Word Recall Score/Q4 sub-score -----
### For ADNI1-3 phases
adas_q4score_adni13 <- ADNIMERGE::adas %>%
  select(
    COLPROT, RID, SITEID,
    VISCODE2 = VISCODE, DONE, NDREASON,
    DATE, Q4TASK, Q4UNABLE, Q4SCORE
  ) %>%
  set_as_tibble() %>%
  # Trying to remap visitcode2 to the original visit code
  left_join(
    ADNIMERGE2::ADAS %>%
      select(COLPROT, RID, VISCODE2, VISCODE, VISDATE) %>%
      set_as_tibble(),
    by = c("COLPROT", "RID", "VISCODE2")
  ) %>%
  mutate(
    VISCODE = ifelse(is.na(VISCODE), VISCODE2, VISCODE),
    DATE = ifelse(is.na(DATE), VISDATE, DATE)
  ) %>%
  select(-VISDATE)

## For ANDI4 phase
adni4_adas_q4score <- ADNI4::adas_score %>%
  ADNI4::create_common_cols() %>%
  ADNI4::derive_site_id() %>%
  select(COLPROT, RID, VISCODE, done, ndreason, date, q4task, q4unable, q4score) %>%
  set_as_tibble() %>%
  left_join(
    ADNI4::registry %>%
      ADNI4::create_common_cols() %>%
      set_as_tibble() %>%
      select(RID, VISCODE, EXAMDATE),
    by = c("RID", "VISCODE")
  ) %>%
  mutate(DATE = ifelse(is.na(DATE), EXAMDATE, DATE)) %>%
  select(-EXAMDATE)

# Bind across study phases
pacc_adas_q4score <- bind_rows(adas_q4score_adni13, adni4_adas_q4score) %>%
  rename("VISDATE" = DATE) %>%
  rename_with(~ paste0("ADAS_", .x), any_of(c("DONE", "NDREASON"))) %>%
  check_unique_record()

pacc_adas_q4score_long <- pacc_adas_q4score %>%
  mutate(
    SCORE = Q4SCORE,
    SCORE_SOURCE = "ADASQ4SCORE"
  ) %>%
  select(COLPROT, RID, VISCODE, VISDATE, SCORE, SCORE_SOURCE)

## MMSE from ADNIMERGE2 -----
pacc_mmse <- MMSE %>%
  select(COLPROT, RID, VISCODE, VISDATE, DONE, NDREASON, MMSCORE) %>%
  set_as_tibble() %>%
  convert_f_viscode_to_sc() %>%
  rename_with(~ paste0("MMSE_", .x), any_of(c("DONE", "NDREASON"))) %>%
  check_unique_record()

pacc_mmse_long <- pacc_mmse %>%
  mutate(
    SCORE = MMSCORE,
    SCORE_SOURCE = "MMSE"
  ) %>%
  select(COLPROT, RID, VISCODE, VISDATE, SCORE, SCORE_SOURCE)

## NEUROBAT from ADNIMERGE2 -----
# Includes: Trial B Score: `TRABSCOR`
#           Logical Memory IIa Delayed Recall Score: `LDELTOTL`
#           Digit Symbol Substitution Test Score: `DIGITSCR`

pacc_neurobat <- NEUROBAT %>%
  select(COLPROT, RID, VISCODE, VISDATE, LDELTOTL = LDELTOTAL, DIGITSCR = DIGITSCOR, TRABSCOR) %>%
  set_as_tibble() %>%
  convert_f_viscode_to_sc() %>%
  check_unique_record()

pacc_neurobat_long <- pacc_neurobat %>%
  select(COLPROT, RID, VISCODE, VISDATE, LDELTOTL, DIGITSCR, TRABSCOR) %>%
  pivot_longer(
    cols = all_of(c("LDELTOTL", "DIGITSCR", "TRABSCOR")),
    values_to = "SCORE",
    names_to = "SCORE_SOURCE"
  )

# Save input data in `./inst/extradata/pacc-raw` directory ------
pacc_raw_data_dir <- file.path("./inst", "extradata", "pacc-raw-input")
if (dir.exists(pacc_raw_data_dir)) {
  unlink(pacc_raw_data_dir, recursive = TRUE)
  cli_alert_warning(text = "{.path {pacc_raw_data_dir}} is removed")
}
dir.create(pacc_raw_data_dir, recursive = TRUE)
pacc_input_data_names <- c(
  "pacc_adas_q4score_long", "pacc_mmse_long", "pacc_neurobat_long"
)
save_csv <- lapply(pacc_input_data_names, function(x) {
  readr::write_csv(
    x = get(x),
    file = file.path(pacc_raw_data_dir, paste0(x, ".csv"))
  )
})
cli_alert_success(text = "Completed generating raw-data for PACC score")
