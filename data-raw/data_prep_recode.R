# Data preparation script setup ----
source(file.path(".", "tools", "data-prepare-utils.R"))
source(file.path(".", "r", "utils.R"))
# Libraries ----
library(tidyverse)

# Directory ----
data_dir <- "./data"
# All data from "./data" directory, except DATADIC if it is existed
data_path_list <- list.files(
  path = data_dir, pattern = "\\.rda$", full.names = TRUE,
  all.files = TRUE, recursive = FALSE
)
data_dic_path <- file.path(data_dir, "DATADIC.rda")
updated_data_dic_path <- file.path("./data-raw/updated_datadic", "UPDATED_DATADIC.rda")
data_path_list <- data_path_list[!data_path_list == data_dic_path]

# Input Args ----
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  USE_UPDATED_DATADIC <- as.logical(args)
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
} else {
  EXISTED_DATADTIC <- FALSE
  warning("No existed data dictionary `", cur_data_dict_path, "`!")
  DECODE_VALUE <- FALSE
}

# Replace coded values ----
# If the DATADIC file is existed
if (EXISTED_DATADTIC) {
  # Expand DATADIC for combined phases
  DATADIC <- DATADIC %>%
    mutate(PHASE = str_remove_all(
      string = PHASE,
      pattern = "\\[|\\]"
    ))

  concat_phase <- unique(DATADIC$PHASE)[!is.na(unique(DATADIC$PHASE))]
  concat_phase <- concat_phase[str_detect(concat_phase, ",")]
  DATADIC <- expand_data_dict(
    data_dict = DATADIC,
    concat_phase = concat_phase,
    concat_char = ","
  )

  # File name extension patterns
  file_path_pattern <- c("^\\./data/|\\.rda$")
  prefix_pattern <- "^adni\\_"
  string_removed_pattern <- str_c(c(file_path_pattern, prefix_pattern), collapse = "|")

  tblname_list_dd <- tibble(file_path = data_path_list) %>%
    mutate(
      short_tblname = str_remove_all(
        string = file_path,
        pattern = file_path_pattern
      ),
      tblname = str_remove_all(
        string = file_path,
        pattern = string_removed_pattern
      ),
      tblname = str_to_upper(tblname)
    )

  # Extract all FLDNAMEs for existed TBLNAMEs with coded value
  adverse_event_col <- c(
    str_c("AESEV", 3:10), str_c("AEDATE", 3:10),
    str_c("AECHRON", 3:10), str_c("AECHANGE", 3:10)
  )

  data_dict <- get_factor_levels_datadict(
    data_dict = DATADIC,
    nested_value = TRUE
  ) %>%
    filter(TBLNAME %in% tblname_list_dd$tblname) %>%
    filter(class_type == "factor") %>%
    # ?? Required to confirm the coded values for the following tblnames/fldnames:
    mutate(excluded_fld_name = case_when(
      (TBLNAME %in% c("ADAS_ADNI1", "MRINCLUSIO", "RECCMEDS", "TREATDIS") |
        (TBLNAME %in% "ADAS_ADNIGO23" & FLDNAME %in% c("Q9TASK", "Q10TASK", "Q11TASK", "Q12TASK")) |
        (TBLNAME %in% "ADVERSE" & FLDNAME %in% adverse_event_col) |
        (TBLNAME %in% "AV45META" & FLDNAME %in% "PMFRAME") |
        (TBLNAME %in% "BIOMARK" & FLDNAME %in% "BILPOTPROC") |
        (TBLNAME %in% "DXSUM" & FLDNAME %in% c("DXMDES", "DXAPOSS")) |
        (TBLNAME %in% "MRIPROT" & FLDNAME %in% "PASS") |
        (TBLNAME %in% "NPIQ" & FLDNAME %in% "NPIJ") |
        (TBLNAME %in% "PETQC" & FLDNAME %in% c("PQREASON", "PQPROERR", "PQISSUES")) |
        (TBLNAME %in% "PTDEMOG" & FLDNAME %in% c("PTASIAN", "PTLANGWR6")) |
        (TBLNAME %in% "TAUMETA" & FLDNAME %in% "TRACERISS")
      ) ~ "Yes"
    ))

  dataset_data_dict <- data_dict %>%
    filter(is.na(excluded_fld_name))

  unique_adni_phase <- unique(dataset_data_dict$PHASE)[!is.na(unique(dataset_data_dict$PHASE))]

  if (length(unique_adni_phase) > 0 & any(!unique_adni_phase %in% adni_phase())) {
    stop("Additional ADNI phase coded values is presented in the data dictionary `DATADIC`.")
  }

  if (nrow(dataset_data_dict) > 0) {
    DECODE_VALUE <- TRUE
  } else {
    DECODE_VALUE <- FALSE
    warning("No existed dataset name with coded values in the data dictionary `DATADIC`.")
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

  for (tb in seq_len(nrow(coded_tblname))) {
    note_prefix <- str_c(tb, "/", nrow(coded_tblname), ": ")
    cur_tblname_dd <- coded_tblname %>% filter(row_number() == tb)
    cur_tblname_full <- cur_tblname_dd %>% pull(file_path)
    cur_tblname_short <- cur_tblname_dd %>% pull(short_tblname)
    cur_tblname <- cur_tblname_dd %>% pull(tblname)

    # Load current dataset into .GlobalEnv
    lapply(cur_tblname_full, load, .GlobalEnv)

    unique_tb_fldname <- get_factor_fldname(
      data_dict = dataset_data_dict,
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
      message(note_prefix, "No existed unique columns with coded value in ", cur_tblname_short, " dataset.")
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
      codelist <- collect_values(
        data_dict = dataset_data_dict,
        tbl_name = cur_tblname,
        all_fld_name = unique_tb_fldname
      )

      # Convert listed code values into data.frame
      convert_codelist <- convert_collect_values(
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

      phaseVar <- get_cols_name(
        data = dd,
        col_name = c("COLPROT", "PHASE", "Phase", "ProtocolID")
      )

      if (!is.na(phaseVar)) {
        message(note_prefix, "Start replacing values of dataset: ", cur_tblname_short)
        dd <- data_value_replacement(
          data = dd,
          phaseVar = phaseVar,
          input_values = codelist
        )
        message(" > ", note_prefix, "Replaced values of dataset: ", cur_tblname_short)
      }

      if (is.na(phaseVar)) {
        warning(note_prefix, "No existed `COLPROT/PHASE/Phase/ProtocolID` columns in the ", cur_tblname_short, " dataset.")
        message("The ", cur_tblname_short, " has not been updated!")
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
      if (data_update_status != TRUE) stop("The ", cur_tblname_short, " has not been updated!")

      message(" >> ", cur_tblname_short, " dataset has been removed from .GlobalEnv.")
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
    warning(coded_records_dir, " directory over-written!")
    unlink(x = coded_records_dir, recursive = FALSE)
  }
  dir.create(coded_records_dir)
  readr::write_csv(
    x = coded_fldname_records,
    file = file.path(coded_records_dir, "coded_records.csv")
  )

  rm(list = c("UPDATED_DATADIC", "DATADIC", "dataset_data_dict", "coded_tblname", "tblname_list_dd"))
}
