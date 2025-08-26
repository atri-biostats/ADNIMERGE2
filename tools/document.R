# Libraries ----
library(tidyverse)
library(assertr)
library(cli)

# Input args ----
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2) {
  cli::cli_abort(
    message = c(
      "Input argument {.val arg} must be size of 2. \n",
      "{.val arg} is a length of contains {.val {length(arg)}} vector."
    )
  )
}
DERIVED_DATASET_LIST <- str_remove_all(string = args[1], pattern = '[\\(\\)]|\\"|^c') %>%
  str_split(string = ., pattern = ",") %>%
  unlist() %>%
  str_trim(string = ., side = "both")
if (all(DERIVED_DATASET_LIST %in% "NULL")) DERIVED_DATASET_LIST <- NULL
USE_UPDATED_DATADIC <- as.logical(args[2])
if (!is.logical(USE_UPDATED_DATADIC) | is.na(USE_UPDATED_DATADIC)) {
  cli::cli_abort(
    message = c(
      "{.var USE_UPDATED_DATADIC} must be a Boolean value. \n",
      "The value of {.var USE_UPDATED_DATADIC} is {.val {USE_UPDATED_DATADIC}}."
    )
  )
}

# Load all data from "./data" to .GlobalEnv ----
data_dir <- "./data"
data_path <- list.files(
  path = data_dir, pattern = "\\.rda$", all.files = TRUE,
  full.names = TRUE, recursive = FALSE
)
derived_data_path <- file.path(data_dir, str_c(c(DERIVED_DATASET_LIST, "DERIVED_DATADIC"), ".rda"))
data_downloaded_date_path <- file.path(data_dir, "DATA_DOWNLOADED_DATE.rda")
metadata_specs_path <- file.path(data_dir, "METACORES.rda")
data_dict_path <- file.path(data_dir, "DATADIC.rda")
data_dic_path_remote <- file.path(data_dir, "REMOTE_DATADIC.rda")

# Updated DATADIC file
updated_data_dict_path <- file.path(
  "./data-raw", "updated_datadic",
  "UPDATED_DATADIC.rda"
)
dataset_date_stamped_file_path <- file.path(
  "./data-raw", "date_stamped",
  "dataset_list_date_stamped.csv"
)

if (file.exists(dataset_date_stamped_file_path)) {
  dataset_list_date_stamped <- readr::read_csv(
    file = dataset_date_stamped_file_path,
    show_col_types = FALSE,
    guess_max = Inf
  ) %>%
    mutate(
      TBLNAME = str_to_lower(UPDATED_TBLNAME),
      STAMPED_DATE = as.character(STAMPED_DATE)
    ) %>%
    select(TBLNAME, STAMPED_DATE)
} else {
  dataset_list_date_stamped <- create_tibble0(c("STAMPED_DATE", "TBLNAME"))
}

if (USE_UPDATED_DATADIC) {
  lapply(data_path[!data_path %in% c(data_dict_path)], load, .GlobalEnv)

  if (file.exists(updated_data_dict_path)) load(updated_data_dict_path, .GlobalEnv)
  DATADIC <- UPDATED_DATADIC
}

if (file.exists(data_dic_path_remote)) load(data_dic_path_remote, .GlobalEnv)
if (exists("REMOTE_DATADIC")) {
  REMOTE_DATADIC <- REMOTE_DATADIC %>%
    mutate(across(everything(), as.character))
} else {
  REMOTE_DATADIC <- create_tibble0("PHASE")
}
## Exclude `DATA_DOWNLOADED_DATE`, `DATADIC`, `DERIVED_DATADIC`, `METACORES` data from the list
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
  string = data_path[!data_path %in% c(data_dict_path, data_dic_path_remote)],
  pattern = "\\.rda$|^.\\/data/"
)))
prefix_patterns <- "^adni\\_"
string_removed_pattern <- str_c(c(prefix_patterns), collapse = "|")

# Documentations for the raw datasets ----
cli::cli_alert_info(
  text = "Generating documentations for raw datasets"
)
source(file.path(".", "tools", "data-prepare-utils.R"))
source(file.path(".", "tools", "data-dictionary-utils.R"))
source(file.path(".", "R", "checks-assert.R"))

## Common texts ----
### Data source link
loni_url_link <- paste0(
  "\\href{https://adni.loni.usc.edu/data-samples/adni-data/}",
  "{https://adni.loni.usc.edu/data-samples/adni-data/}"
)
### Common data description
common_description <- str_c("data. More information is available at ", loni_url_link)
### Authors
authors <- paste0(
  "\\href{adni-data@googlegroups.com}",
  "{adni-data@googlegroups.com}"
)

adjust_value_datadict <- function(.data, datadict_name = c("DATADIC", "REMOTE_DATADIC", "DERIVED_DATADIC")) {
  col_names <- c("field_values", "field_notes")
  .data <- .data %>%
    mutate(across(any_of(col_names), ~ case_when(
      dd_name %in% datadict_name ~ " ",
      TRUE ~ .x
    )))
  return(.data)
}

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
  adjust_value_datadict() %>%
  mutate(tblname = str_remove_all(str_to_lower(dd_name), string_removed_pattern))

unique_tblname <- unique(temp_data_dict$tblname)

### Get field code and labels from DATADIC dataset ----
exc_code <- c('crfname|\\"indexes\\"|\\<display\\>|\"Pass;|\"OTF Lumos;|\"a; b; c; d|select distinct |^-4$')
exc_code_text <- "\n|\\<br\\>|\\<br /\\>|\\<!--|--\\>$"
temp_field_codetext <- bind_rows(DATADIC, REMOTE_DATADIC) %>%
  filter(!TBLNAME %in% c("DATADIC", "REMOTE_DATADIC")) %>%
  as_tibble() %>%
  mutate(TBLNAME = str_to_lower(TBLNAME)) %>%
  filter(TBLNAME %in% unique_tblname) %>%
  mutate(
    CODE = str_replace_all(str_replace_all(CODE, "; ", ";"), ";", "; "),
    CODE = str_replace_all(str_replace_all(CODE, " ;", ";"), ";", " ;")
  ) %>%
  mutate(across(c(CODE, TEXT), ~ ifelse(.x %in% "-4", NA_character_, as.character(.x)))) %>%
  mutate(across(c(CODE, TEXT), ~ str_trim(.x))) %>%
  distinct(PHASE, TBLNAME, FLDNAME, TEXT, CODE) %>%
  mutate(removed_records = str_detect(CODE, exc_code)) %>%
  filter(removed_records == FALSE | is.na(removed_records)) %>%
  mutate(across(c(TEXT, CODE), ~ str_remove_all(.x, exc_code_text))) %>%
  group_by(TBLNAME, FLDNAME) %>%
  nest() %>%
  ungroup() %>%
  mutate(adjust_fldcodes = map(data, ~ adjust_code_labels(data_dict = .x))) %>%
  unnest(cols = adjust_fldcodes) %>%
  assert_uniq(TBLNAME, FLDNAME)

common_data_names <- str_to_lower(c("DATADIC", "REMOTE_DATADIC", "VISITS", "ADNI2_VISITID"))

temp_data_dict <- temp_data_dict %>%
  # Add dataset labels
  left_join(
    bind_rows(DATADIC, REMOTE_DATADIC) %>%
      distinct(CRFNAME, TBLNAME, STATUS) %>%
      group_by(TBLNAME) %>%
      filter(
        (n() == 1 & row_number() == 1) |
          (n() > 1 & any(STATUS %in% "Archived") & !STATUS %in% "Archived" & row_number() == 1) |
          (n() > 1 & all(STATUS %in% "Archived") & row_number() == 1) |
          (n() > 1 & all(!STATUS %in% "Archived") & row_number() == 1) |
          (n() > 1 & all(!is.na(STATUS)) & row_number() == 1)
      ) %>%
      ungroup() %>%
      assert_uniq(TBLNAME) %>%
      mutate(
        CRFNAME = ifelse(CRFNAME == "-4", NA_character_, CRFNAME),
        tblname = str_to_lower(TBLNAME)
      ) %>%
      distinct(tblname, CRFNAME),
    by = c("tblname" = "tblname")
  ) %>%
  # Add date stamp for dataset with a date stamped file extension
  left_join(dataset_list_date_stamped,
    by = c("tblname" = "TBLNAME")
  ) %>%
  verify(nrow(.) == nrow(temp_data_dict)) %>%
  mutate(CRFNAME = case_when(
    is.na(CRFNAME) ~ toupper(tblname),
    !is.na(CRFNAME) ~ CRFNAME
  )) %>%
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
      !tblname %in% common_data_names ~ str_c(CRFNAME, common_description, sep = " "),
      tblname %in% common_data_names ~ str_c(CRFNAME, str_remove(common_description, "^data"), sep = " ")
    ),
    dataset_source_type = "raw",
    add_source = loni_url_link
  ) %>%
  # Add field code and labels from DATADIC dataset
  left_join(
    temp_field_codetext %>%
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
    ),
    field_class = case_when(
      field_class %in% c("Date", "POSIXct", "POSIXt", "hms", "difftime") ~ " ",
      TRUE ~ field_class
    )
  ) %>%
  mutate(across(c(field_label, field_notes, field_value), ~ str_replace_all(.x, "\\%", "\\\\%"))) %>%
  mutate(field_notes = str_replace_all(field_notes, "Character variable with", ", with")) %>%
  mutate(field_notes = str_replace_all(field_notes, "Factor variable with levels", ", with levels")) %>%
  select(
    dd_name, num_rows, num_cols, field_name, field_class, field_label,
    field_values, field_notes, dataset_label, add_authors, short_description,
    dataset_source_type, add_source
  )

### Add dataset category/keywords ----
dataset_cat_path <- file.path(".", "data-raw", "dataset_cat", "dataset_category.csv")
if (file.exists(dataset_cat_path)) {
  dataset_cat <- readr::read_csv(
    file = dataset_cat_path,
    col_names = TRUE,
    show_col_types = FALSE,
    guess_max = Inf
  ) %>%
    select(dir_cat, TBLNAME)
} else {
  dataset_cat <- create_tibble0(c("dir_cat", "TBLNAME"))
}

temp_data_dict <- temp_data_dict %>%
  left_join(
    dataset_cat %>%
      mutate(dir_cat = str_remove_all(string = dir_cat, ",")) %>%
      select(TBLNAME, dir_cat) %>%
      distinct(),
    by = c("dd_name" = "TBLNAME")
  ) %>%
  verify(nrow(.) == nrow(temp_data_dict)) %>%
  mutate(add_keywords = case_when(
    is.na(dir_cat) ~ "other_raw_dataset",
    !is.na(dir_cat) ~ dir_cat
  )) %>%
  select(-dir_cat)

### Adjust for coded FLDNAME records ----
coded_records_path <- file.path(".", "data-raw", "coded_records", "coded_records.csv")
if (file.exists(coded_records_path)) {
  coded_records <- readr::read_csv(
    file = coded_records_path,
    show_col_types = FALSE,
    guess_max = Inf
  ) %>%
    as_tibble() %>%
    filter(STATUS == "Yes") %>%
    select(TBLNAME, FLDNAME, CODED = STATUS) %>%
    distinct()
} else {
  coded_records <- create_tibble0(c("TBLNAME", "FLDNAME", "CODED"))
}

temp_data_dict <- temp_data_dict %>%
  # Add coded values status
  {
    if (nrow(coded_records) > 0) {
      left_join(.,
        coded_records %>%
          select(TBLNAME, FLDNAME, CODED),
        by = c("dd_name" = "TBLNAME", "field_name" = "FLDNAME")
      ) %>%
        verify(., nrow(.) == nrow(temp_data_dict))
    } else {
      mutate(., CODED = NA_character_)
    }
  } %>%
  mutate(field_notes = case_when(
    CODED %in% "Yes" ~ str_c(field_notes, "\n#' \\emph{Decoded Value: }\\strong{Yes}"),
    TRUE ~ field_notes
  ))

## Finalize documentations ------
if (dir.exists(file.path(".", "R")) == FALSE) {
  cli::cli_abort(
    message = "{.val {file.path('.', 'R')}} directory is not existed."
  )
}
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
  output_file_name = data_document_path,
  existed_append = FALSE
)

# Documentation for DATA_DOWNLOADED_DATE data ----
cat("#' ADNI Study Data Downloaded Date",
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
  "#' @keywords other_raw_dataset",
  "#' @format A `Date` class object.",
  "#' @examples",
  "#' \\dontrun{",
  "#' ADNIMERGE2::DATA_DOWNLOADED_DATE",
  "#' }",
  "NULL\n\n",
  file = data_document_path, sep = "\n", append = TRUE
)
cli::cli_alert_success(
  text = "Completed generating documentations for raw datasets"
)

# Documentations for the derived dataset ----
## Prepare data dictionary for derived dataset ----

if (exists("derived_data_list")) {
  ### Generate data dictionary from actual derived dataset ----
  # Helper function to link data source with vignettes document
  source_link <- function(tblname) {
    vignette_link <- case_when(
      nchar(tblname) == 2 ~ "ADNIMERGE2-Derived-Data",
      nchar(tblname) >2 & tolower(tblname) %in% "pacc" ~ "ADNIMERGE2-PACC",
      nchar(tblname) > 2 ~ "ADNIMERGE2-Analysis-Data"
    )
    output_link <- c()
    for (i in seq_len(length(vignette_link))) {
      temp_vignette_link <- paste0(
        "\\code{vignette(topic = '", vignette_link[i],
        "', package = 'ADNIMERGE2')}"
      )
      output_link[i] <- paste0(
        "For more details see the help vignette: \n#' ",
        temp_vignette_link
      )
    }
    return(output_link)
  }

  derived_data_dict_path <- file.path(data_dir, "DERIVED_DATADIC.rda")
  if (!file.exists(derived_data_dict_path)) {
    cli::cli_abort(
      message = "{.val {derived_data_dict_path}} is not existed."
    )
  }
  load(derived_data_dict_path, .GlobalEnv)

  # Add description for `DERIVED_DATADIC`
  DERIVED_DATADIC <- DERIVED_DATADIC %>%
    bind_rows(
      tibble(
        TBLNAME = "DERIVED_DATADIC",
        CRFNAME = "Data dictionary for derived dataset",
        FLDNAME = colnames(DERIVED_DATADIC)
      )
    )

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
    adjust_value_datadict() %>%
    mutate(tblname = str_remove_all(dd_name, string_removed_pattern)) %>%
    left_join(
      DERIVED_DATADIC %>%
        select(TBLNAME, FLDNAME, TEXT, CRFNAME),
      by = c("dd_name" = "TBLNAME", "field_name" = "FLDNAME")
    ) %>%
    assert_uniq(dd_name, field_name) %>%
    assert_non_missing(dd_name, field_name, CRFNAME) %>%
    mutate(
      dataset_label = CRFNAME,
      add_authors = authors,
      short_description = str_c(
        str_to_sentence(str_remove_all(CRFNAME, "\\[ Derived \\]")),
        " derived dataset."
      ),
      dataset_source_type = "derived",
      add_source = case_when(
        dd_name %in% "DERIVED_DATADIC" ~ "For more details see the help vignettes",
        TRUE ~ source_link(tblname = dd_name)
      ),
      field_notes = case_when(
        TEXT %in% " " | is.na(TEXT) ~ field_notes,
        TRUE ~ paste0(TEXT, "; ", field_notes)
      ),
      add_keywords = "derived_dataset"
    ) %>%
    mutate(across(
      c(field_label, field_notes, field_values),
      ~ str_replace_all(.x, "\\%", "\\\\%")
    )) %>%
    mutate(
      field_label = ifelse(is.na(field_label), " ", field_label),
      field_notes = str_replace_all(field_notes, "Character variable with", ", with character"),
      field_notes = str_replace_all(field_notes, "Factor variable with levels", ", with factor levels")
    ) %>%
    select(
      dd_name, dataset_label, num_rows, num_cols, field_name, field_class,
      field_label, field_values, field_notes, add_authors, short_description,
      dataset_source_type, add_source, add_keywords
    ) %>%
    assert_non_missing(field_label, field_notes)

  ## Finalize documentations ----
  generate_roxygen_document(
    dataset_name_list = unique(temp_data_dict_derived$dd_name),
    data_list = NULL,
    data_dict = temp_data_dict_derived,
    roxygen_source_type = "data_dictionary",
    output_file_name = data_document_path,
    existed_append = TRUE
  )

  # Documentation for METACORES meta-specs ----
  cat("#' ADNI Metadata-Specs",
    "#'",
    paste0(
      "#' Metadata specifications for the ADNI study. It is generated to create ",
      "analysis ready dataset using \\href{https://pharmaverse.org/}{PHARMAVERSE} ",
      "workflow for illustration purpose."
    ),
    "#'",
    "#' @docType data",
    "#' @keywords metadata specs derived",
    "#' @name METACORES",
    "#' @usage data(METACORES)",
    "#' @keywords derived_dataset",
    "#' @format A R6-class wrapper object created using \\code{\\link[metacore]{metacore}} function.",
    paste0(
      "#' @source For more details about the metadata-specs see the help vignette: \n",
      "#' \\code{vignette(topic = 'ADNIMERGE2-Analysis-Meta-Specs', package = 'ADNIMERGE2')}."
    ),
    "#' @examples",
    "#' \\dontrun{",
    "#' ADNIMERGE2::METACORES",
    "#' }",
    "NULL\n\n",
    file = data_document_path, sep = "\n", append = TRUE
  )
  cli::cli_alert_success(
    text = "Completed generating documentations for derived datasets"
  )
}
