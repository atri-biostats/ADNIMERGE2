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

# Source util functions ----
source(file.path(".", "tools", "data-prepare-utils.R"))
devtools::load_all("./")

# Input Argument ----
args <- commandArgs(trailingOnly = TRUE)
check_arg(x = args, size = 1)
DATA_DOWNLOADED_DATE <- as.Date(args[1])
check_arg_date(x = DATA_DOWNLOADED_DATE)
verify_pkg_install(pkg = c("ADNI4", "ADNIMERGE"))
# Compared with raw source dataset date
if (ADNI4::data_dump_date < DATA_DOWNLOADED_DATE) {
  cli::cli_abort(
    message = c(
      "{.var ADNI4} package must be downloaded on {.val {DATA_DOWNLOADED_DATE}}. \n",
      "{.var ADNI4} R package was downloaded on {.val {ADNI4::data_dump_date}}."
    )
  )
}

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

# Prepare input data -----
## Get ADAS - Delayed Word Recall Score/Q4 sub-score -----
### For ADNI1-3 phases
adas_q4score_adni13 <- ADNIMERGE::adas %>%
  select(
    COLPROT, RID, SITEID, DONE, NDREASON,
    DATE, Q4TASK, Q4UNABLE, Q4SCORE,
    VISCODE2 = VISCODE
  ) %>%
  # To add PTID
  left_join(
    ADNIMERGE2::ADAS %>%
      distinct(PTID, RID),
    relationship = "many-to-one",
    by = "RID"
  ) %>%
  set_as_tibble() %>%
  # Trying to remap visitcode2 to the original visit code
  left_join(
    ADNIMERGE2::ADAS %>%
      select(COLPROT, RID, VISCODE2, VISCODE, VISDATE) %>%
      set_as_tibble(),
    by = c("COLPROT", "RID", "VISCODE2")
  ) %>%
  # Adjust for missing VISCODE - that are not done
  mutate(
    VISCODE = ifelse(is.na(VISCODE), VISCODE2, VISCODE),
    DATE = ifelse(is.na(DATE), VISDATE, DATE)
  ) %>%
  select(-VISDATE)

## For ANDI4 phase
adni4_adas_q4score <- ADNI4::adas_score %>%
  ADNI4::create_common_cols() %>%
  ADNI4::derive_site_id() %>%
  ADNI4::remove_site_records() %>%
  select(COLPROT, PTID, RID, VISCODE, done, ndreason, date, q4task, q4unable, q4score) %>%
  set_as_tibble() %>%
  left_join(
    ADNI4::registry %>%
      ADNI4::create_common_cols() %>%
      set_as_tibble() %>%
      select(RID, VISCODE, EXAMDATE),
    by = c("RID", "VISCODE")
  ) %>%
  mutate(DATE = ifelse(is.na(DATE), EXAMDATE, DATE)) %>%
  select(-EXAMDATE) %>%
  # Add VISCODE2 from current ADAS score data
  left_join(
    ADNIMERGE2::ADAS %>%
      select(COLPROT, RID, VISCODE2, VISCODE) %>%
      set_as_tibble(),
    by = c("COLPROT", "RID", "VISCODE")
  )

# Bind across study phases
pacc_adas_q4score <- bind_rows(adas_q4score_adni13, adni4_adas_q4score) %>%
  rename("VISDATE" = DATE) %>%
  rename_with(~ paste0("ADAS_", .x), any_of(c("DONE", "NDREASON"))) %>%
  check_unique_record() %>%
  assert_non_missing(all_of(c("PTID", "VISCODE")))

pacc_adas_q4score_long <- pacc_adas_q4score %>%
  mutate(
    SCORE = Q4SCORE,
    SCORE_SOURCE = "ADASQ4SCORE"
  ) %>%
  select(COLPROT, PTID, RID, VISCODE, VISCODE2, VISDATE, SCORE, SCORE_SOURCE)

## MMSE from ADNIMERGE2 -----
pacc_mmse <- ADNIMERGE2::MMSE %>%
  select(COLPROT, PTID, RID, VISCODE, VISCODE2, VISDATE, DONE, NDREASON, MMSCORE) %>%
  set_as_tibble() %>%
  convert_f_viscode_to_sc(
    .data = .,
    code_var = c("VISCODE", "VISCODE2")
  ) %>%
  rename_with(~ paste0("MMSE_", .x), any_of(c("DONE", "NDREASON"))) %>%
  check_unique_record()

pacc_mmse_long <- pacc_mmse %>%
  mutate(
    SCORE = MMSCORE,
    SCORE_SOURCE = "MMSE"
  ) %>%
  select(COLPROT, PTID, RID, VISCODE, VISCODE2, VISDATE, SCORE, SCORE_SOURCE)

## NEUROBAT from ADNIMERGE2 -----
# Includes: Trial B Score: `TRABSCOR`
#           Logical Memory IIa Delayed Recall Score: `LDELTOTL`
#           Digit Symbol Substitution Test Score: `DIGITSCR`

pacc_neurobat <- ADNIMERGE2::NEUROBAT %>%
  select(
    COLPROT, PTID, RID, VISCODE, VISCODE2, VISDATE,
    LDELTOTL = LDELTOTAL, DIGITSCR = DIGITSCOR, TRABSCOR
  ) %>%
  set_as_tibble() %>%
  convert_f_viscode_to_sc(
    .data = .,
    code_var = c("VISCODE", "VISCODE2")
  ) %>%
  check_unique_record()

pacc_neurobat_long <- pacc_neurobat %>%
  select(COLPROT, PTID, RID, VISCODE, VISCODE2, VISDATE, LDELTOTL, DIGITSCR, TRABSCOR) %>%
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
