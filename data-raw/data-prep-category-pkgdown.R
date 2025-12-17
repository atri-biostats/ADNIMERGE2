# Generate dataset categories for pkgdown -----
source(file.path(".", "tools", "data-prepare-utils.R"))
source(file.path(".", "R", "utils.R"))

# Libraries -----
library(tidyverse)

# Pre-specified directories name ----
source(file.path(".", "data-raw", "dir-list.R"))

# Create dataset category/groups for pkgdown ----
if (dir.exists(dataset_cat_dir)) unlink(dataset_cat_dir, recursive = TRUE)
dir.create(dataset_cat_dir)

## Get dataset category based on file path ----
dataset_category_raw <- get_dataset_category(
  dir.path = raw_data_dir,
  file_extension_pattern = "\\.csv$",
  recursive = TRUE
)

dataset_category_raw <- dataset_category_raw %>%
  # Removing pre-generated dataset in './data-prep.R'
  filter(!sub_dir %in% dir_list) %>%
  select(-any_of("sub_dir")) %>%
  # Adjust for Neuropathology and Data Dictionary category
  mutate(dir_cat = case_when(
    file_list %in% "NEUROPATH" & dir_cat %in% "other_raw_dataset" ~ "neuropath",
    str_detect(file_list, "^DATADIC$|^DATADIC|DATADIC$") & dir_cat %in% "other_raw_dataset" ~ "data_dict",
    TRUE ~ dir_cat
  ))

### Adjust for date stamped file names -----
date_stamped_file_path <- file.path(date_stamped_dir, "dataset_list_date_stamped.csv")
if (file.exists(date_stamped_file_path)) {
  dataset_date_stamped <- readr::read_csv(
    file = date_stamped_file_path,
    col_names = TRUE,
    show_col_types = FALSE,
    guess_max = Inf
  ) %>%
    select(PREVIOUS_TBLNAME, UPDATED_TBLNAME)
} else {
  dataset_date_stamped <- create_tibble0(c("PREVIOUS_TBLNAME", "UPDATED_TBLNAME"))
}

dataset_category_raw <- dataset_category_raw %>%
  left_join(
    dataset_date_stamped %>%
      select(PREVIOUS_TBLNAME, UPDATED_TBLNAME),
    by = c("file_list" = "PREVIOUS_TBLNAME")
  ) %>%
  mutate(TBLNAME = case_when(
    !is.na(UPDATED_TBLNAME) ~ UPDATED_TBLNAME,
    TRUE ~ file_list
  ))

## Get dataset category based on study phase ----
data_path_list <- list.files(
  path = data_dir,
  pattern = "\\.rda$",
  full.names = TRUE,
  all.files = TRUE,
  recursive = FALSE
)
data_dic_path <- file.path(data_dir, "DATADIC.rda")
data_path_list <- data_path_list[!data_path_list %in% data_dic_path]

### Based on study phase columns ----
dataset_category_phase <- lapply(data_path_list, function(x) {
  dataset_name <- str_remove(basename(x), "\\.rda$")
  # Load dataset in new environment
  new_env <- new.env()
  load(file = x, envir = new_env)
  get_study_phase_category(
    .data = new_env %>%
      pluck(., dataset_name),
    phase_vars = NULL
  ) %>%
    rename("dir_cat" = PHASE) %>%
    mutate(
      dir = raw_data_dir, # For simplicity
      full_file_path = x, # Full file path
      file_list = dataset_name,
    ) %>%
    # Adjust for remotely collected dataset
    mutate(dir_cat = case_when(
      str_detect(file_list, "$RMT\\_") ~ tolower(adni_phase()[5]),
      TRUE ~ dir_cat
    )) %>%
    relocate(dir_cat, .after = last_col())
}) %>%
  bind_rows()

dataset_category_phase <- dataset_category_phase %>%
  filter(!is.na(dir_cat) & !dir_cat %in% "nv")

### Based on file name prefix/suffix -----
dataset_category_file_name <- dataset_category_phase %>%
  filter(str_detect(tolower(file_list), "adni[1-9]|adnigo")) %>%
  mutate(dir_cat = str_extract(tolower(file_list), "adni[1-9]|adnigo")) %>%
  filter(str_detect(dir_cat, "^adni")) %>%
  filter(!is.na(dir_cat))

dataset_category_phase <- bind_rows(dataset_category_phase, dataset_category_file_name) %>%
  group_by(file_list) %>%
  filter(
    (n() == 1 & row_number() == 1) |
      (n() > 1 & any(dir_cat %in% "undefined_phase") & !dir_cat %in% "undefined_phase") |
      (n() > 1 & all(!dir_cat %in% "undefined_phase"))
  ) %>%
  ungroup() %>%
  distinct() %>%
  mutate(TBLNAME = file_list)

## Finalized dataset category list -----
# Combine dataset category based on file path and study phase/file name -----
dataset_category <- bind_rows(dataset_category_raw, dataset_category_phase) %>%
  group_by(TBLNAME) %>%
  mutate(dir_cat = toString(unique(dir_cat))) %>%
  ungroup() %>%
  distinct() %>%
  # Adjust the category for remotely collected data in ADNI4
  mutate(across(
    dir_cat,
    ~ case_when(
      str_detect(file_list, "^RMT\\_") & !str_detect(.x, "adni4") ~ paste0(.x, ", adni4"),
      TRUE ~ .x
    )
  ))

# Save dataset category ----
readr::write_csv(
  x = dataset_category,
  file = file.path(dataset_cat_dir, "dataset_category.csv")
)

cli::cli_alert_info(text = "Completed generating dataset category for pkdown!")
