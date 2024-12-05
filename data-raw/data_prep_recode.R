# Data preparation script setup ----
source(file.path(".", "tools", "data-prepare-utils.R"))
source(file.path(".", "r", "utils.R"))
# Libraries ----
library(tidyverse)

dataset_list_files <- list.files(
  path = "./data", pattern = ".rda", full.names = TRUE,
  all.files = TRUE, recursive = FALSE
)

data_dict_file_path <- file.path(".", "data", "DATADIC.rda")
updated_data_dict_file_path <- file.path(".", "data-raw", "updated_datadic", "UPDATED_DATADIC.rda")

USE_UPDATED_DATADIC <- TRUE

if (USE_UPDATED_DATADIC) {
  # To use the updated DATADIC that was generated in "./data-raw/data_prep.R"
  if (file.exists(updated_data_dict_file_path)) {
    EXISTED_DATADTIC <- TRUE
  } else {
    EXISTED_DATADTIC <- FALSE
    warning("No existed updated DATADTIC dataset in `./data-raw/updated_datadic/` directory")
  }
} else {
  # To use the actual DATADIC that was downloaded directly from the data sharing platform
  # If the DATADIC file is existed
  if (data_dict_file_path %in% dataset_list_files == TRUE) {
    EXISTED_DATADTIC <- TRUE
  } else {
    EXISTED_DATADTIC <- FALSE
    warning("No existed DATADTIC dataset in ./data directory")
  }
}

# Re-coding values ----
if (EXISTED_DATADTIC) {
  file_path_patterns <- c("./data/|.rda")
  prefix_patterns <- "^adni\\_"
  string_removed_pattern <- str_c(c(file_path_patterns, prefix_patterns), collapse = "|")

  if (USE_UPDATED_DATADIC) {
    # Load the data dictionary DATDIC in .GlobalEnv
    lapply(updated_data_dict_file_path, load, .GlobalEnv)
    DATADIC <- UPDATED_DATADIC
    rm(list = "UPDATED_DATADIC")
  } else {
    ## To use `DATADIC` data dictionary that was downloaded directly from the data sharing platform
    lapply(data_dict_file_path, load, .GlobalEnv)
  }

  dataset_list_files <- dataset_list_files[!dataset_list_files == data_dict_file_path]
  tblname_list_dd <- tibble(full_tblname = dataset_list_files) %>%
    mutate(
      short_tblname = str_remove_all(string = full_tblname, pattern = file_path_patterns),
      tblname = str_to_upper(str_remove_all(string = full_tblname, pattern = string_removed_pattern))
    )

  dataset_data_dict <- get_factor_levels_datadict(
    data_dict = DATADIC,
    nested_value = TRUE
  ) %>%
    filter(TBLNAME %in% c(tblname_list_dd$tblname)) %>%
    filter(class_type == "factor") %>%
    mutate(PHASE = str_remove_all(string = PHASE, pattern = "\\[|\\]")) %>%
    # Required to confirm the coded values for the following tblnames/fldnames:
    mutate(excluded_fld_name = case_when(
      TBLNAME %in% "RECCMEDS" |
        c(TBLNAME %in% "MRIPROT" & FLDNAME %in% c("PASS")) |
        c(TBLNAME %in% "PTDEMOG" & FLDNAME %in% c("PTLANGWR6")) |
        c(TBLNAME %in% "VITALS" & FLDNAME %in% c("VSHGTSC")) ~ "Yes"
    )) %>%
    filter(is.na(excluded_fld_name))

  unique_adni_phase_list <- unique(dataset_data_dict$PHASE)[!is.na(unique(dataset_data_dict$PHASE))]

  if (length(unique_adni_phase_list) > 0 & any(!unique_adni_phase_list %in% adni_phase())) {
    stop("Additional ADNI phase coded value is presented in the DATADIC file")
  }

  if (nrow(dataset_data_dict) > 0) {
    UPDATE <- TRUE
  } else {
    UPDATE <- FALSE
    warning("No existed dataset name with recode values in the data dictionary DATADIC")
  }

  # Apply re-coding algorithm ----
  if (UPDATE) {
    existed_tblname_list <- tblname_list_dd %>%
      filter(tblname %in% dataset_data_dict$TBLNAME)

    num_existed_tb <- nrow(existed_tblname_list)

    for (tb in seq_len(num_existed_tb)) {
      message_note_prefix <- str_c(tb, "/", num_existed_tb, ": ")
      cur_tblname_dd <- existed_tblname_list %>% filter(row_number() == tb)
      cur_tblname_full <- cur_tblname_dd %>% pull(full_tblname)
      cur_tblname_short <- cur_tblname_dd %>% pull(short_tblname)
      cur_tbname <- cur_tblname_dd %>% pull(tblname)

      # Load current specific dataset
      lapply(cur_tblname_full, load, .GlobalEnv)

      tbl_unique_fldname <- get_factor_fldname(
        data_dict = dataset_data_dict,
        tbl_name = cur_tbname,
        dd_fldnames = colnames(get(cur_tblname_short))
      )

      if (length(tbl_unique_fldname) == 0) {
        message(
          message_note_prefix, "No existed unique column with coded value in ",
          cur_tblname_short, " dataset"
        )
        rm(list = c("cur_tblname_short", "cur_tblname_short", "cur_tblname_full", "cur_tbname"))
      } else {
        assign("dd", get(cur_tblname_short))
        coded_values <- collect_values(
          data_dict = dataset_data_dict,
          tbl_name = cur_tbname,
          all_fld_name = tbl_unique_fldname
        )

        cur_phase_var <- extract_cols(dd, col_name = c("COLPROT", "PHASE", "Phase"))

        if (!is.na(cur_phase_var)) {
          message(message_note_prefix, "Start replacing values of dataset: ", cur_tblname_short)
          dd <- data_value_replacement(
            dd = dd,
            phaseVar = cur_phase_var,
            input_values = coded_values
          )

          message(
            " > ", message_note_prefix,
            "Replaced values of dataset: ", cur_tblname_short
          )
        } else {
          warning(
            message_note_prefix,
            "No existed COLPROT/PHASE/Phase column in the ", cur_tblname_short, " dataset"
          )
          message("The ", cur_tblname_short, " has not been updated!")
        }
        # Using usethis::use_data()
        data_update_status <- use_data_modified(
          dataset_name = cur_tblname_short,
          dataset = dd,
          edit_type = "create",
          run_script = TRUE
        )
        if (data_update_status != TRUE) stop("The ", cur_tblname_short, " has not been updated!")
      }

      message(" >> ", cur_tblname_short, " dataset has been removed from .GlobalEnv")

      rm(list = c(
        "dd", "coded_values", "cur_tblname_dd", "cur_tblname_short",
        "cur_tblname_full", "cur_tbname"
      ))
    }

    rm(list = c(
      "UPDATED_DATADIC", "DATADIC", "dataset_data_dict", "existed_tblname_list",
      "tblname_list_dd", "num_existed_tb"
    ))
  }
  rm(list = c(dataset_list_files))
}
