# Documentations for the raw datasets ----
message("Generating documentations for raw datasets")
source(file.path(".", "tools", "data-prepare-utils.R"))
source(file.path(".", "tools", "data-dictionary-utils.R"))

# Libraries ----
library(tidyverse)
library(assertr)

# Load .rda dataset to .GlobalEnv from data directory ----
list_rda_files <- list.files(
  path = file.path(".", "data"),
  pattern = ".rda", all.files = TRUE,
  full.names = TRUE, recursive = TRUE
)
lapply(list_rda_files, load, .GlobalEnv)

## Exclude DATADIC dataset from the list
data_dictionary_path <- file.path(".", "data", "DATADIC.rda")
data_downloaded_date_path <- file.path(".", "data", "DATA_DOWNLOADED_DATE.rda")
list_rda_files <- list_rda_files[!list_rda_files %in% c(data_dictionary_path, data_downloaded_date_path)]
combined_datasets <- mget(str_remove_all(
  string = list_rda_files,
  pattern = ".rda|./data/"
))
rm(list = as.character(str_remove_all(
  string = list_rda_files,
  pattern = ".rda|./data/"
)))
prefix_patterns <- c(str_c("adni", 1:4, "_"), "adni_")
sufix_patterns <- c("_pooled", "_harmonized")
string_removed_pattern <- str_c(c(prefix_patterns, sufix_patterns), collapse = "|")

# Common texts ----
## Data source link -----
loni_data_link <- str_c(
  "\\href{https://adni.loni.usc.edu/data-samples/adni-data/}",
  "{https://adni.loni.usc.edu/data-samples/adni-data/}"
)
## Common data description ----
common_description <- str_c(
  "data from the electronic case report form (eCRF).",
  " More information is available at ", loni_data_link
)
## Authors ----
authors <- "\\href{adni-data@googlegroups.com}{adni-data@googlegroups.com}"

# Prepare data dictionary ----
## Generate data dictionary from actual dataset ----
temp_data_dict <- lapply(names(combined_datasets), function(tbl_name) {
  summarize_dataset(
    dd = combined_datasets %>% pluck(., tbl_name),
    dataset_name = tbl_name,
    wider_format = TRUE
  )
}) %>%
  bind_rows() %>%
  mutate(tblname = str_remove_all(
    string = str_to_lower(dd_name),
    pattern = string_removed_pattern
  ))

unique_tblname <- unique(temp_data_dict$tblname)

## Get field code and labels from DATADIC dataset ----
temp_field_codetext <- DATADIC %>%
  as_tibble() %>%
  mutate(TBLNAME = str_to_lower(TBLNAME)) %>%
  filter(TBLNAME %in% unique_tblname) %>%
  distinct(PHASE, TBLNAME, FLDNAME, TEXT, CODE) %>%
  filter(!str_detect(CODE, "crfname|\\<display")) %>%
  mutate(across(c(TEXT, CODE), 
                ~str_remove_all(string = .x, 
                                pattern = "\n|\\<br\\>|\\<br /\\>|\\<!--|--\\>$"))) %>%
  group_by(TBLNAME, FLDNAME) %>%
  mutate(num_records = n()) %>%
  nest() %>%
  ungroup() %>%
  mutate(adjust_fldcodes = map(data, ~ adjust_code_lables(dd = .x))) %>%
  unnest(cols = adjust_fldcodes)

temp_data_dict <- temp_data_dict %>%
  # Add dataset (crf) labels
  left_join(
    DATADIC %>%
      distinct(CRFNAME, TBLNAME) %>%
      mutate(TBLNAME = str_to_lower(TBLNAME)) %>%
      group_by(TBLNAME) %>%
      mutate(CRFNAME = as.character(toString(CRFNAME))) %>%
      ungroup() %>%
      distinct(TBLNAME, CRFNAME) %>%
      assert(is_uniq, TBLNAME) %>%
      select(TBLNAME, CRFNAME),
    by = c("tblname" = "TBLNAME")
  ) %>%
  verify(nrow(.) == nrow(temp_data_dict)) %>%
  filter(!is.na(CRFNAME)) %>%
  mutate(
    prefix_char = str_to_upper(str_extract(string = dd_name, pattern = str_c(prefix_patterns, collapse = "|"))),
    sufix_char = str_to_title(str_extract(string = dd_name, pattern = str_c(sufix_patterns, collapse = "|")))
  ) %>%
  mutate(across(c(prefix_char, sufix_char), ~ str_remove(string = .x, pattern = "_"))) %>%
  mutate(
    dataset_label = case_when(
      !is.na(prefix_char) & !is.na(sufix_char) ~ str_c(prefix_char, CRFNAME, sufix_char, sep = " - "),
      !is.na(prefix_char) & is.na(sufix_char) ~ str_c(prefix_char, CRFNAME, sep = " - "),
      is.na(prefix_char) & !is.na(sufix_char) ~ str_c(CRFNAME, sufix_char, sep = " - "),
      is.na(prefix_char) & is.na(sufix_char) ~ CRFNAME
    ),
    add_authors = authors,
    short_description = str_c(CRFNAME, common_description, sep = " "),
    dataset_source_type = "raw",
    add_source = loni_data_link
  ) %>%
  # Add field code and labels from DATADIC dataset
  left_join(
    temp_field_codetext %>%
      mutate(tbl_fld_name = str_c(TBLNAME, FLDNAME)) %>%
      assert(is_uniq, tbl_fld_name) %>%
      select(TBLNAME, FLDNAME, field_value, data_field_label = field_label),
    by = c("tblname" = "TBLNAME", "field_name" = "FLDNAME")
  ) %>%
  # Adjust field code and text
  mutate(
    field_notes = case_when(
      is.na(field_value) ~ field_notes,
      !is.na(field_value) ~ field_value
    ),
    field_label = case_when(
      is.na(field_label) ~ data_field_label,
      !is.na(field_label) ~ field_label
    )
  ) %>%
  select(
    dd_name, num_rows, num_cols, field_name, field_class, field_label, field_values, field_notes,
    dataset_label, add_authors, short_description, dataset_source_type, add_source
  )

# Generate documentations ------
if (dir.exists(file.path(".", "R")) == FALSE) dir.create(file.path("..", "R"))
data_document_filepath <- file.path(".", "R", "data.R")
if (file.exists(data_document_filepath) == TRUE) readr::write_lines(x = "", data_document_filepath)
generate_roxygen_document(
  dataset_name_list = unique(temp_data_dict$dd_name),
  dd = NULL, data_dict = temp_data_dict,
  roxygen_source_type = "data_dictionary",
  output_file_name = data_document_filepath
)
message("Completed generating documentations for raw datasets")
