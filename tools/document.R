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
  full.names = TRUE, recursive = FALSE
)

## Exclude DATADIC dataset from the list
data_downloaded_date_path <- file.path(".", "data", "DATA_DOWNLOADED_DATE.rda")
data_dict_file_path <- file.path(".", "data", "DATADIC.rda")

# Updated DATADIC file
updated_data_dict_file_path <- file.path(".", "data-raw", "updated_datadic", "UPDATED_DATADIC.rda")

USE_UPDATED_DATADIC <- TRUE

if (USE_UPDATED_DATADIC) {
  lapply(list_rda_files[!list_rda_files %in% c(data_dict_file_path)], load, .GlobalEnv)

  if (file.exists(updated_data_dict_file_path)) load(updated_data_dict_file_path)
  DATADIC <- UPDATED_DATADIC
}

list_rda_files <- list_rda_files[!list_rda_files %in% c(data_downloaded_date_path)]
combined_datasets <- mget(str_remove_all(
  string = list_rda_files,
  pattern = "\\.rda|\\./data/"
))
rm(list = as.character(str_remove_all(
  string = list_rda_files[!list_rda_files %in% data_dict_file_path],
  pattern = "\\.rda|.\\/data/"
)))
prefix_patterns <- "^adni\\_"
string_removed_pattern <- str_c(c(prefix_patterns), collapse = "|")

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
  mutate(across(
    c(TEXT, CODE),
    ~ str_remove_all(
      string = .x,
      pattern = "\n|\\<br\\>|\\<br /\\>|\\<!--|--\\>$"
    )
  )) %>%
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
      # Add CFRNAME manually for "ADNI2_VISITID" and "VISITS" datasets
      bind_rows(tibble(
        TBLNAME = "ADNI2_VISITID",
        CRFNAME = "ADNI2 Visit Code Mapping List"
      )) %>%
      mutate(CRFNAME = case_when(
        is.na(CRFNAME) & TBLNAME %in% "VISITS" ~ "Combined list of phase specific visit code",
        TRUE ~ CRFNAME
      )) %>%
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
    prefix_char = str_to_upper(str_extract(
      string = dd_name,
      pattern = str_c(prefix_patterns, collapse = "|")
    ))
  ) %>%
  mutate(prefix_char = str_remove(string = prefix_char, pattern = "_")) %>%
  mutate(
    dataset_label = case_when(
      !is.na(prefix_char) ~ str_c(prefix_char, CRFNAME, sep = " - "),
      is.na(prefix_char) ~ CRFNAME
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
    dd_name, num_rows, num_cols, field_name, field_class, field_label,
    field_values, field_notes, dataset_label, add_authors, short_description,
    dataset_source_type, add_source
  )

# Generate documentations ------
if (dir.exists(file.path(".", "R")) == FALSE) dir.create(file.path("..", "R"))
data_document_file_path <- file.path(".", "R", "data.R")
if (file.exists(data_document_file_path) == TRUE) {
  readr::write_lines(x = "", data_document_file_path)
} else {
  file.create(data_document_file_path, showWarnings = TRUE)
}

generate_roxygen_document(
  dataset_name_list = unique(temp_data_dict$dd_name),
  dd = NULL, data_dict = temp_data_dict,
  roxygen_source_type = "data_dictionary",
  output_file_name = data_document_file_path
)
message("Completed generating documentations for raw datasets")

# Add documentation for DATA_DOWNLOADED_DATE dataset
cat("#' ADNI data download date",
  "#'",
  paste0(
    "#' The date when data in this package were downloaded from ",
    loni_data_link, "."
  ),
  "#'",
  "#' @docType data",
  "#' @keywords datasets",
  "#' @name DATA_DOWNLOADED_DATE",
  "#' @usage data(DATA_DOWNLOADED_DATE)",
  "#' @format A `Date` class object.",
  "#' @examples",
  "#' \\dontrun{",
  "#' ADNIMERGE2::DATA_DOWNLOADED_DATE",
  "#' }",
  "NULL\n\n",
  file = data_document_file_path, sep = "\n", append = TRUE
)
