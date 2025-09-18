# Data preparation script setup ----
source(file.path(".", "tools", "data-prepare-utils.R"))
source(file.path(".", "R", "utils.R"))
source(file.path(".", "R", "checks-assert.R"))
# Libraries ----
library(tidyverse)
library(assertr)
library(cli)

# Directory ----
data_dir <- "./data"
# All data from "./data" directory, except DATADIC if it is existed
data_path_list <- list.files(
  path = data_dir,
  pattern = "\\.rda$",
  full.names = TRUE,
  all.files = TRUE,
  recursive = FALSE
)
data_dic_path <- file.path(data_dir, "DATADIC.rda")
data_dic_path_remote <- file.path(data_dir, "REMOTE_DATADIC.rda")
updated_data_dic_path <- file.path("./data-raw/updated_datadic", "UPDATED_DATADIC.rda")
data_path_list <- data_path_list[!data_path_list == data_dic_path]

# Input Args ----
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1) {
  cli::cli_abort(
    message = c(
      "Input argument {.val arg} must be size of 1. \n",
      "{.val arg} is a length of contains {.val {length(arg)}} vector."
    )
  )
}
USE_UPDATED_DATADIC <- as.logical(args)
if (!is.logical(USE_UPDATED_DATADIC) | is.na(USE_UPDATED_DATADIC)) {
  cli::cli_abort(
    message = c(
      "{.var USE_UPDATED_DATADIC} must be a Boolean value. \n",
      "The value of {.var USE_UPDATED_DATADIC} is {.val {USE_UPDATED_DATADIC}}."
    )
  )
}

if (USE_UPDATED_DATADIC) {
  # To use manually updated data dictionary, see line 390-434 & 479-489 in `./data-raw/data_prep.R`
  cur_data_dict_path <- updated_data_dic_path
} else {
  # To use the actual DATADIC that was downloaded directly from the data sharing platform
  cur_data_dict_path <- data_dic_path
}

if (file.exists(cur_data_dict_path)) {
  EXISTED_DATADTIC <- TRUE
  # Load the existed data dictionary DATDIC into .GlobalEnv
  load(file = cur_data_dict_path, envir = .GlobalEnv)
  if (exists("UPDATED_DATADIC", envir = .GlobalEnv)) {
    DATADIC <- UPDATED_DATADIC
    rm(list = "UPDATED_DATADIC", envir = .GlobalEnv)
  }
  load(file = data_dic_path_remote, envir = .GlobalEnv)
  if (!exists("REMOTE_DATADIC")) {
    REMOTE_DATADIC <- tibble(PHASE = NA_character_) %>%
      na.omit()
  }
} else {
  EXISTED_DATADTIC <- FALSE
  cli_alert_warning(
    text = "No existing data dictionary with file name {.val {cur_data_dict_path}}"
  )
  DECODE_VALUE <- FALSE
}

# Replace coded values ----
# If the DATADIC file is existed
if (EXISTED_DATADTIC) {
  # Expand DATADIC for combined phases
  DATADIC <- DATADIC %>%
    # Add a DATADIC of the remote digital cohort
    bind_rows(REMOTE_DATADIC) %>%
    mutate(PHASE = str_remove_all(string = PHASE, pattern = "\\[|\\]"))

  concat_phase <- unique(DATADIC$PHASE)[!is.na(unique(DATADIC$PHASE))]
  concat_phase <- concat_phase[str_detect(concat_phase, ",")]
  DATADIC <- expand_data_dict(
    .datadic = DATADIC,
    concat_phase = concat_phase,
    concat_char = ","
  )

  # File name extension patterns
  file_path_pattern <- c("^\\./data/|\\.rda$")
  prefix_pattern <- "^adni\\_"
  string_removed_pattern <- str_c(c(file_path_pattern, prefix_pattern), collapse = "|")

  tblname_list_dd <- tibble(file_path = data_path_list) %>%
    mutate(
      short_tblname = str_remove_all(string = file_path, pattern = file_path_pattern),
      tblname = str_remove_all(string = file_path, pattern = string_removed_pattern),
      tblname = str_to_upper(tblname)
    )

  data_dict <- get_factor_levels_datadict(
    .datadic = DATADIC,
    nested_value = TRUE
  ) %>%
    # Add "0" prefix character for FLDNAME coded value that contains "0" value
    add_code_prefix(
      .datadic = .,
      prefix_char = "0",
      nested_value = TRUE,
      position = "first",
      add_char = NULL
    ) %>%
    add_code_prefix(
      .datadic = .,
      prefix_char = "0",
      nested_value = TRUE,
      position = "last",
      add_char = "."
    ) %>%
    datadict_as_tibble() %>%
    filter(TBLNAME %in% tblname_list_dd$tblname) %>%
    filter(class_type %in% "factor") %>%
    # ?? Required to confirm the coded values for the following tblnames/fldnames:
    mutate(excluded_fld_name = case_when(
      (TBLNAME %in% c("RECCMEDS", "TREATDIS") |
        (TBLNAME %in% "MRIPROT" & FLDNAME %in% "PASS") |
        (TBLNAME %in% "NPIQ" & FLDNAME %in% "NPIJ") |
        (TBLNAME %in% "PETQC" & FLDNAME %in% c("PQPROERR", "PQISSUES")) |
        (TBLNAME %in% "TAUMETA" & FLDNAME %in% "TRACERISS")
      ) ~ "Yes"
    ))

  dataset_data_dict <- data_dict %>%
    filter(is.na(excluded_fld_name)) %>%
    set_datadict_tbl()

  unique_adni_phase <- unique(dataset_data_dict$PHASE)[!is.na(unique(dataset_data_dict$PHASE))]

  if (length(unique_adni_phase) > 0 & any(!unique_adni_phase %in% adni_phase())) {
    cli_abort(
      message = c(
        "Additional ADNI phase coded values is presented in {.var DATADIC} data dictionary. \n",
        "The value must be {.val {adni_phase()}}."
      )
    )
  }

  if (nrow(dataset_data_dict) > 0) {
    DECODE_VALUE <- TRUE
  } else {
    DECODE_VALUE <- FALSE
    cli_alert_warning(
      text = "No existing data conatins variables with coded values"
    )
  }
}

# Apply algorithm to decode values ----
if (DECODE_VALUE) {
  coded_tblname <- tblname_list_dd %>%
    filter(tblname %in% dataset_data_dict$TBLNAME)

  coded_fldname_records <- tibble(
    TBLNAME = NA_character_,
    FLDNAME = NA_character_,
    STATUS = NA_character_,
    PHASE_CODES = list()
  )

  # Replace with function ?
  for (tb in seq_len(nrow(coded_tblname))) {
    note_prefix <- cli::col_blue(str_c(tb, "/", nrow(coded_tblname), ":"))
    cur_tblname_dd <- coded_tblname %>% filter(row_number() == tb)
    cur_tblname_full <- cur_tblname_dd %>% pull(file_path)
    cur_tblname_short <- cur_tblname_dd %>% pull(short_tblname)
    cur_tblname <- cur_tblname_dd %>% pull(tblname)

    # Load current dataset into .GlobalEnv
    lapply(cur_tblname_full, load, .GlobalEnv)

    unique_tb_fldname <- get_factor_fldname(
      .datadic = dataset_data_dict,
      tbl_name = cur_tblname,
      dd_fldnames = colnames(get(cur_tblname_short))
    )

    # Update the code fldname records
    coded_fldname_records <- bind_rows(
      coded_fldname_records,
      tibble(
        TBLNAME = cur_tblname,
        FLDNAME = unique_tb_fldname,
        STATUS = "Yes"
      )
    )

    if (length(unique_tb_fldname) == 0) {
      cli_alert_info(
        text = paste0(
          "{.val {.emph {note_prefix}}} No existing unique columns with ",
          "coded values in {.val {cur_tblname_short}} data"
        )
      )
      # Update the code fldname records
      coded_fldname_records <- coded_fldname_records %>%
        mutate(STATUS = case_when(
          TBLNAME %in% cur_tblname ~ "No",
          TRUE ~ as.character(STATUS)
        ))
      rm(list = c("cur_tblname_short", "cur_tblname_short", "cur_tblname_full", "cur_tblname"))
    }

    if (length(unique_tb_fldname) > 0) {
      assign("dd", get(cur_tblname_short))
      codelist <- collect_value_mapping(
        .datadic = dataset_data_dict,
        tbl_name = cur_tblname,
        all_fld_name = unique_tb_fldname
      )

      # Convert listed code values into data.frame
      convert_codelist <- convert_value_mapping(
        coded_values = codelist,
        tbl_name = cur_tblname
      ) %>%
        unnest(CODES) %>%
        group_by(TBLNAME, FLDNAME) %>%
        nest(PHASE_CODES = everything() & -TBLNAME & -FLDNAME) %>%
        ungroup() %>%
        mutate(STATUS = "Yes")

      # Update the code fldname records
      coded_fldname_records <- coded_fldname_records %>%
        filter(!TBLNAME %in% cur_tblname) %>%
        bind_rows(convert_codelist)

      pre_phase_vars <- c("COLPROT", "PHASE", "Phase", "ProtocolID")
      phaseVar <- get_cols_name(
        .data = dd,
        col_name = pre_phase_vars
      )

      if (!is.na(phaseVar)) {
        cli_alert_info(
          text = paste0(
            "{.val {.emph {note_prefix}}} Start replacing coded values in ",
            "{.val {cur_tblname_short}} data"
          )
        )
        dd <- replace_values_dataset(
          .data = dd,
          phaseVar = phaseVar,
          input_values = codelist
        )
        cli_alert_success(
          text = paste0(
            "{.val {.emph {note_prefix}}} Completed replacing coded values in ",
            "{.val {cur_tblname_short}} data"
          )
        )
      }

      if (is.na(phaseVar)) {
        cli_alert_warning(
          text = paste0(
            "{.val {.emph {note_prefix}}} No existing phase column in ",
            "{.val {cur_tblname_short}} data"
          )
        )
        cli_alert_info(
          text = paste0("{.val {cur_tblname_short}} data has not been updated")
        )
        # Update the code fldname records
        coded_fldname_records <- coded_fldname_records %>%
          mutate(STATUS = case_when(
            TBLNAME %in% cur_tblname ~ "Not decoded - phase",
            TRUE ~ as.character(STATUS)
          ))
      }

      # Update data
      data_update_status <- use_data_modified(
        dataset_name = cur_tblname_short,
        dataset = dd,
        edit_type = "create",
        run_script = TRUE
      )
      if (data_update_status != TRUE) cli::cli_abort(message = "{.val {cur_tblname_short}} has not been updated")
      cli_alert_success(
        text = paste0("{.val {cur_tblname_short}} data has been removed from .GlobalEnv.")
      )
    }

    rm(list = c("dd", "codelist", "cur_tblname_dd", "cur_tblname_short", "cur_tblname_full", "cur_tblname"))
  }

  # Update the code fldname records
  coded_fldname_records <- coded_fldname_records %>%
    unnest(PHASE_CODES) %>%
    bind_rows(
      data_dict %>%
        filter(excluded_fld_name == "Yes") %>%
        select(TBLNAME, FLDNAME, PHASE, CODES) %>%
        unnest(cols = CODES) %>%
        mutate(STATUS = "Not decoded - data type")
    ) %>%
    filter(if_any(.cols = everything(), ~ !is.na(.)))

  # Store the procedure summary in csv files
  coded_records_dir <- file.path("./data-raw", "coded_records")
  if (dir.exists(coded_records_dir)) {
    cli_alert_warning(
      text = paste0("Overwriting {.val {coded_records_dir}}")
    )
    unlink(x = coded_records_dir, recursive = FALSE)
  }
  dir.create(coded_records_dir)
  readr::write_csv(
    x = coded_fldname_records,
    file = file.path(coded_records_dir, "coded_records.csv")
  )

  rm(list = c("UPDATED_DATADIC", "DATADIC", "dataset_data_dict", "coded_tblname", "tblname_list_dd"))
}

cli_alert_success(text = paste0("Completed mapping coded values"))
