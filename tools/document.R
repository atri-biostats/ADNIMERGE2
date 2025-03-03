# Libraries ----
library(tidyverse)
library(assertr)

# Input args ----
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 2) stop("The impute argument must be size of 2.")
if (length(args) > 0) {
  DERIVED_DATASET_LIST <- str_remove_all(
    string = args[1],
    pattern = '[\\(\\)]|\\"|^c'
  ) %>%
    str_split(string = ., pattern = ",") %>%
    unlist() %>%
    str_trim(string = ., side = "both")
  if (all(DERIVED_DATASET_LIST %in% "NULL")) DERIVED_DATASET_LIST <- NULL
  USE_UPDATED_DATADIC <- as.logical(args[2])
}

# Load all data from "./data" to .GlobalEnv ----
data_dir <- "./data"
data_path <- list.files(
  path = data_dir, pattern = "\\.rda$", all.files = TRUE,
  full.names = TRUE, recursive = FALSE
)

derived_data_path <- file.path(data_dir, str_c(DERIVED_DATASET_LIST, ".rda"))
data_downloaded_date_path <- file.path(data_dir, "DATA_DOWNLOADED_DATE.rda")
metadata_specs_path <- file.path(data_dir, "METACORES.rda")
data_dict_path <- file.path(data_dir, "DATADIC.rda")
# Updated DATADIC file
updated_data_dict_path <- file.path("./data-raw/updated_datadic", "UPDATED_DATADIC.rda")
dataset_date_stamped_file_path <- "./data-raw/date_stamped/dataset_list_date_stamped.csv"

if (file.exists(dataset_date_stamped_file_path)) {
  dataset_list_date_stamped <- readr::read_csv(dataset_date_stamped_file_path) %>%
    mutate(
      TBLNAME = str_to_lower(UPDATED_TBLNAME),
      STAMPED_DATE = as.character(STAMPED_DATE)
    ) %>%
    select(TBLNAME, STAMPED_DATE)
} else {
  dataset_list_date_stamped <- tibble(STAMPED_DATE = NA_character_, TBLNAME = NA_character_) %>%
    na.omit()
}

if (USE_UPDATED_DATADIC) {
  lapply(data_path[!data_path %in% c(data_dict_path)], load, .GlobalEnv)

  if (file.exists(updated_data_dict_path)) load(updated_data_dict_path, .GlobalEnv)
  DATADIC <- UPDATED_DATADIC
}

## Exclude `DATA_DOWNLOADED_DATE`, `DATADIC`, `METACORES` data from the list
raw_data_path <- data_path[!data_path %in% c(
  data_downloaded_date_path,
  derived_data_path,
  metadata_specs_path
)]
derived_data_path <- data_path[data_path %in% derived_data_path]
raw_data_list <- mget(str_remove_all(
  string = raw_data_path,
  pattern = "\\.rda$|^\\./data/"
))
if (length(derived_data_path) > 0) {
  derived_data_list <- mget(str_remove_all(
    string = derived_data_path,
    pattern = "\\.rda$|^\\./data/"
  ))
}
rm(list = as.character(str_remove_all(
  string = data_path[!data_path %in% data_dict_path],
  pattern = "\\.rda$|^.\\/data/"
)))
prefix_patterns <- "^adni\\_"
string_removed_pattern <- str_c(c(prefix_patterns), collapse = "|")


# Documentations for the raw datasets ----
message("Generating documentations for raw datasets")
source(file.path(".", "tools", "data-prepare-utils.R"))
source(file.path(".", "tools", "data-dictionary-utils.R"))
source(file.path(".", "R", "checks-assert.R"))

## Common texts ----
### Data source link
loni_url_link <- str_c(
  "\\href{https://adni.loni.usc.edu/data-samples/adni-data/}",
  "{https://adni.loni.usc.edu/data-samples/adni-data/}"
)
### Common data description
common_description <- str_c(
  "data. More information is available at ", loni_url_link
)
### Authors
authors <- "\\href{adni-data@googlegroups.com}{adni-data@googlegroups.com}"

## Prepare data dictionary for raw dataset ----
### Generate data dictionary from actual raw dataset ----
temp_data_dict <- lapply(names(raw_data_list), function(tb) {
  summarize_dataset(
    data = raw_data_list %>% pluck(., tb),
    dataset_name = tb,
    wider_format = TRUE
  )
}) %>%
  bind_rows() %>%
  mutate(tblname = str_remove_all(
    string = str_to_lower(dd_name),
    pattern = string_removed_pattern
  ))

unique_tblname <- unique(temp_data_dict$tblname)

### Get field code and labels from DATADIC dataset ----
exc_code <- c('crfname|\\"indexes\\"|\\<display\\>|\"Pass;|\"OTF Lumos;|\"a; b; c; d|select distinct ')
exc_code_text <- "\n|\\<br\\>|\\<br /\\>|\\<!--|--\\>$"
temp_field_codetext <- DATADIC %>%
  as_tibble() %>%
  mutate(TBLNAME = str_to_lower(TBLNAME)) %>%
  filter(TBLNAME %in% unique_tblname) %>%
  distinct(PHASE, TBLNAME, FLDNAME, TEXT, CODE) %>%
  mutate(across(c(TEXT, CODE), ~ ifelse(.x %in% "-4", NA_character_, .x))) %>%
  mutate(removed_records = str_detect(CODE, exc_code)) %>%
  filter(removed_records == FALSE | is.na(removed_records)) %>%
  mutate(across(c(TEXT, CODE), ~ str_remove_all(string = .x, pattern = exc_code_text))) %>%
  group_by(TBLNAME, FLDNAME) %>%
  mutate(num_records = n()) %>%
  nest() %>%
  ungroup() %>%
  mutate(adjust_fldcodes = map(data, ~ adjust_code_labels(data_dict = .x))) %>%
  unnest(cols = adjust_fldcodes)

temp_data_dict <- temp_data_dict %>%
  # Add dataset label
  left_join(
    DATADIC %>%
      distinct(CRFNAME, TBLNAME) %>%
      # Add dataset label manually for "ADNI2_VISITID" and "VISITS"
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
      assert_uniq(TBLNAME) %>%
      select(TBLNAME, CRFNAME),
    by = c("tblname" = "TBLNAME")
  ) %>%
  # Add date stamp for dataset with a date stamped file extension
  left_join(dataset_list_date_stamped, by = c("tblname" = "TBLNAME")) %>%
  verify(nrow(.) == nrow(temp_data_dict)) %>%
  filter(!is.na(CRFNAME)) %>%
  mutate(CRFNAME = case_when(
    !is.na(STAMPED_DATE) ~ str_c(CRFNAME, ": ", STAMPED_DATE),
    TRUE ~ CRFNAME
  )) %>%
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
    short_description = case_when(
      tblname %in% str_to_lower("DATADIC") ~ str_c("Data dictionary dataset. More information is available at ", loni_url_link, "."),
      tblname %in% str_to_lower("VISITS") ~ str_c(CRFNAME, ". More information is available at ", loni_url_link, "."),
      TRUE ~ str_c(CRFNAME, common_description, sep = " ")
    ),
    dataset_source_type = "raw",
    add_source = loni_url_link
  ) %>%
  # Add field code and labels from DATADIC dataset
  left_join(
    temp_field_codetext %>%
      mutate(tbl_fld_name = str_c(TBLNAME, FLDNAME)) %>%
      assert_uniq(tbl_fld_name) %>%
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

## Finalize documentations ------
# if (dir.exists(file.path(".", "R")) == FALSE) dir.create(file.path("..", "R"))
data_document_path <- file.path(".", "R", "data.R")
if (file.exists(data_document_path) == TRUE) {
  readr::write_lines(x = "", data_document_path)
} else {
  file.create(data_document_path, showWarnings = TRUE)
}
generate_roxygen_document(
  dataset_name_list = unique(temp_data_dict$dd_name),
  data_list = NULL,
  data_dict = temp_data_dict,
  roxygen_source_type = "data_dictionary",
  output_file_name = data_document_path
)

# Documentation for DATA_DOWNLOADED_DATE dataset ----
cat("#' ADNI data download date",
  "#'",
  paste0(
    "#' The date when data in this package were downloaded from ",
    loni_url_link, "."
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
  file = data_document_path, sep = "\n", append = TRUE
)
message("Completed generating documentation for raw datasets")

# Documentations for the derived dataset ----
## Prepare data dictionary for derived dataset ----

if (exists("derived_data_list")) {
  ### Generate data dictionary from actual derived dataset ----
  common_description_derived_data <- paste0(
    "derived data. More information is available at `browseVignettes('ADNIMERGE2')`."
  )
  derived_data_dict_path <- file.path(
    "./data-raw/derived-datadic", "DERIVED_DATADIC.rda"
  )
  if (!file.exists(derived_data_dict_path)) stop(derived_data_dict_path, " is not existed!")
  load(derived_data_dict_path, .GlobalEnv)

  derived_data_name <- names(derived_data_list)
  derived_data_name <- derived_data_name[!derived_data_name %in% "METACORES"]

  temp_data_dict_derived <- lapply(derived_data_name, function(tb) {
    summarize_dataset(
      data = derived_data_list %>% pluck(., tb),
      dataset_name = tb,
      wider_format = TRUE
    )
  }) %>%
    bind_rows() %>%
    mutate(tblname = str_remove_all(
      string = dd_name,
      pattern = string_removed_pattern
    )) %>%
    left_join(
      DERIVED_DATADIC %>%
        select(TBLNAME, FLDNAME, TEXT, CRFNAME),
      by = c("dd_name" = "TBLNAME", "field_name" = "FLDNAME")
    ) %>%
    assert_uniq(dd_name, field_name) %>% 
    assert_non_missing(dd_name, field_name, CRFNAME) %>%
    mutate(
      add_authors = authors,
      short_description = str_c(
        str_to_sentence(
          string = str_remove_all(
            string = CRFNAME,
            pattern = "\\[ Derived \\]"
          )
        ),
        common_description_derived_data,
        sep = " "
      ),
      dataset_source_type = "derived",
      add_source = "browseVignettes('ADNIMERGE2')",
      field_notes = case_when(
        TEXT %in% " " | is.na(TEXT) ~ field_notes,
        TRUE ~ str_c(TEXT, "; ", field_notes)
      )
    ) %>%
    select(
      dd_name, num_rows, num_cols, field_name, field_class, field_label,
      field_values, field_notes, add_authors, short_description,
      dataset_source_type, add_source
    ) %>%
    assert_non_missing(field_label, field_notes)

  # derived_data_document_path <- file.path(".", "R", "derived-data.R")
  # if (file.exists(derived_data_document_path) == TRUE) {
  #   readr::write_lines(x = "", derived_data_document_path)
  # } else {
  #   file.create(derived_data_document_path, showWarnings = TRUE)
  # }

  ## Finalize documentations ----
  generate_roxygen_document(
    dataset_name_list = unique(temp_data_dict_derived$dd_name),
    data_list = NULL,
    data_dict = temp_data_dict_derived,
    roxygen_source_type = "data_dictionary",
    output_file_name = data_document_path,
    existed_append = TRUE
  )
  message("Completed generating documentations for derived datasets")
}
