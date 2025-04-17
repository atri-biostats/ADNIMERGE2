# Data preparation script setup ----
source(file.path(".", "tools", "data-prepare-utils.R"))
source(file.path(".", "R", "utils.R"))
source(file.path(".", "R", "checks-assert.R"))

# Libraries ----
library(tidyverse)
library(readr)
library(assertr)
library(rlang)

# Directories ----
raw_data_dir <- "./data-raw"
# Clear up the following directories ----
updated_datadic_dir <- file.path(raw_data_dir, "updated_datadic")
date_stamped_dir <- file.path(raw_data_dir, "date_stamped")
common_columns_dir <- file.path(raw_data_dir, "common_columns")
dataset_cat_dir <- file.path(raw_data_dir, "dataset_cat")
data_dir <- "./data"
specified_dir <- c(
  data_dir, updated_datadic_dir, date_stamped_dir,
  common_columns_dir, dataset_cat_dir
)
lapply(specified_dir, function(i) {
  if (dir.exists(i) == TRUE) unlink(i, recursive = TRUE)
  message(i, " directory has been removed!")
})

# Data downloaded date arg parameter ----
# Input arg parameter ----
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2) stop("Input argument must be size of 2.")
DATA_DOWNLOADED_DATE <- as.character(args[1])
UPDATE_DATADIC <- as.logical(args[2])

DATA_DOWNLOADED_DATE <- as.Date(DATA_DOWNLOADED_DATE)
usethis::use_data(DATA_DOWNLOADED_DATE, overwrite = TRUE)

prefix_pattern <- "^adni\\_"
date_stamped_suffix <- str_c("_", format(DATA_DOWNLOADED_DATE, "%d%b%Y"))
file_path_pattern <- c("^\\./data/|\\.rda$")

# Extract raw datasets from zip file ----
zip_file_list <- list.files(
  path = raw_data_dir,
  pattern = "\\.zip$",
  full.names = TRUE,
  all.files = TRUE,
  recursive = FALSE
)
if (length(zip_file_list) > 0) {
  EXISTED_ZIPFILE <- TRUE
} else {
  EXISTED_ZIPFILE <- FALSE
  warning("No existed zip file existed in data-raw directory")
}

if (EXISTED_ZIPFILE) {
  zip_files_prefix <- str_remove_all(
    string = zip_file_list,
    pattern = "Tables\\.zip$|\\.zip$|^\\./data-raw/"
  )

  zip_name_pattern <- str_c(c(
    zip_files_prefix,
    date_stamped_suffix,
    str_to_upper(prefix_pattern),
    "v_"
  ), collapse = "|")

  lapply(zip_file_list, function(zip_file) {
    ### Unzipped file ----
    unzipped_file_status <- get_unzip_file(
      input_dir = raw_data_dir,
      file_name = str_remove_all(
        string = zip_file,
        pattern = str_c("^\\", raw_data_dir, "/")
      ),
      output_dir = ".",
      overwrite = TRUE
    )
    if (unzipped_file_status != TRUE) stop("Check the ", zip_file, " file")
    ### Renamed csv files ----
    rawdata_csv_path <- str_remove_all(
      string = zip_file,
      pattern = "\\.zip$"
    )
    rename_file_status <- file_action(
      input_dir = rawdata_csv_path,
      output_dir = ".",
      file_extension = "\\.csv$",
      remove_name_pattern = zip_name_pattern,
      action = "rename"
    )
    if (rename_file_status != TRUE) stop("Check renaming files in ", zip_file, " file")
    ### Create .rda dataset ----
    data_create_status <- using_use_data(
      input_dir = rawdata_csv_path,
      file_extension = ".csv"
    )
    if (data_create_status != TRUE) stop("The .rda files are not created!")
    rm(list = c("data_create_status", "rename_file_status"))
  })
}
rm(list = c("zip_file_list", "zip_name_pattern"))

# Convert raw .csv dataset into .rda file format ----
csv_file_list <- list.files(
  path = raw_data_dir,
  pattern = "\\.csv$",
  all.files = TRUE,
  full.names = FALSE,
  recursive = FALSE
)

if (length(csv_file_list) > 0) {
  EXISTED_CSVFILE <- TRUE
} else {
  EXISTED_CSVFILE <- FALSE
  warning("No existed csv file existed in data-raw directory")
}

if (EXISTED_CSVFILE) {
  # Removing the common date stamped file extension;
  # And any files started with ADNI prefix
  csv_name_pattern <- str_c(c(str_to_upper(prefix_pattern), date_stamped_suffix), collapse = "|")

  rename_file_status <- file_action(
    input_dir = raw_data_dir,
    output_dir = ".",
    file_extension = "\\.csv$",
    remove_name_pattern = csv_name_pattern,
    action = "rename"
  )
  if (rename_file_status != TRUE) stop("Check renaming files in ", csv_file_list, " file")
  # Store datasets in "./data" folder
  data_create_status <- using_use_data(
    input_dir = raw_data_dir,
    file_extension = ".csv"
  )
  if (data_create_status != TRUE) stop("The .rda files are not created!")
  rm(list = c("data_create_status", "rename_file_status"))
}
rm(list = c("date_stamped_suffix", "csv_file_list", "csv_name_pattern"))

# Create dataset category/groups ----
if (dir.exists(dataset_cat_dir)) unlink(dataset_cat_dir)
dir.create(dataset_cat_dir)
dataset_cat <- get_dataset_cat(
  dir.path = raw_data_dir,
  extension_pattern = "\\.csv$"
) %>%
  group_by(file_list) %>%
  filter((n() == 1 & row_number() == 1) |
    (n() > 1 & any(dir_cat %in% "raw_other") & !dir_cat %in% "raw_other") |
    (n() > 1 & all(!dir_cat %in% "raw_other"))) %>%
  mutate(dir_cat = toString(dir_cat)) %>%
  ungroup()

# See line 360

# Adding common columns and converting `-4` & `-1` value as missing values ----
data_path_list <- list.files(
  path = data_dir,
  pattern = "\\.rda$",
  full.names = TRUE,
  all.files = TRUE,
  recursive = FALSE
)
data_dic_path <- file.path(data_dir, "DATADIC.rda")
data_downloaded_date_path <- file.path(data_dir, "DATA_DOWNLOADED_DATE.rda")
data_path_list <- data_path_list[!data_path_list %in% c(
  data_dic_path,
  data_downloaded_date_path
)]

if (length(data_path_list) > 0) {
  UPDATE_MISSING_VALUE <- TRUE
} else {
  UPDATE_MISSING_VALUE <- FALSE
  warning("No existed datasets in data directory")
}

if (UPDATE_MISSING_VALUE) {
  string_removed_pattern <- str_c(c(prefix_pattern), collapse = "|")

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

  for (tb in tblname_list_dd$short_tblname) {
    ## Load all the dataset to .GlobalEnv
    lapply(
      tblname_list_dd %>%
        filter(short_tblname == tb) %>%
        pull(file_path),
      load, .GlobalEnv
    )
    assign("dd", get(tb))

    if ("RID" %in% names(dd)) check_RID_col <- TRUE else check_RID_col <- FALSE
    ## Adding common columns -----
    if (check_RID_col) {
      message("Adding ORIGPROT and COLPROT variables in ", tb)
      num_missing_rid <- dd %>%
        filter(is.na(RID)) %>%
        nrow()
      if (num_missing_rid > 0) {
        message(tb, " dataset have ", num_missing_rid, " records with missing RID")
        warning("These IDs will be replaced from PTID if it is existed in the dataset.")
      }
      dd <- dd %>%
        create_col_protocol(data = ., phaseVar = c("Phase", "PHASE", "ProtocolID")) %>%
        # ?? If there any missing RID but have PTID
        {
          if (all(num_missing_rid > 0 & "PTID" %in% colnames(.))) {
            mutate(., RID = case_when(
              is.na(RID) ~ as.numeric(str_remove_all(string = PTID, pattern = "^[0-9]{3}\\_S\\_")),
              !is.na(RID) ~ as.numeric(RID)
            ))
          } else {
            (.)
          }
        } %>%
        create_orig_protocol(data = .)
    } else {
      message("ORIGPROT and COLPROT are not addedd in ", tb)
    }
    # Replacing `-4` as missing value -----
    message("Convert `-4` values as missing values in ", tb)
    dd <- convert_to_missing_value(
      data = dd,
      col_name = names(dd),
      value = "-4",
      missing_char = NA,
      phase = NULL
    )

    message("Convert `-1` values as missing values in ", tb, " for ADNI1 phase")
    dd <- convert_to_missing_value(
      data = dd,
      col_name = names(dd),
      value = "-1",
      missing_char = NA,
      phase = "ADNI1"
    )

    data_update_status <- use_data_modified(
      dataset_name = tb,
      dataset = dd,
      edit_type = "create",
      run_script = TRUE
    )
    if (data_update_status != TRUE) stop("The ", tb, " has not been updated!")

    rm(list = c("tb", "dd"))
  }

  rm(list = c(
    "tblname_list_dd", "string_removed_pattern", "check_RID_col",
    "DATA_DOWNLOADED_DATE", "data_update_status",
    tblname_list_dd$short_tblname
  ))
}

# Remove date stamped file extension -----
## Some of the imaging dataset might have a date stamped file extension
## These dataset will be copied to `./data-raw/date_stamped` directory
data_path_list <- list.files(
  path = data_dir,
  pattern = "\\.rda$",
  full.names = TRUE,
  all.files = TRUE,
  recursive = FALSE
)
data_path_list <- data_path_list[!data_path_list %in% c(
  data_dic_path,
  data_downloaded_date_path
)]

date_stamped_pattern <- "\\_[0-9]{2}\\_[0-9]{2}\\_[0-9]{2}"
dataset_list_dd <- tibble(file_path = data_path_list) %>%
  mutate(short_tblname = str_remove_all(string = file_path, pattern = file_path_pattern)) %>%
  filter(str_detect(string = file_path, pattern = date_stamped_pattern) == TRUE) %>%
  {
    if (nrow(.) > 1) {
      mutate(.,
        updated_file_path = str_remove_all(string = file_path, pattern = date_stamped_pattern),
        stamped_date = str_extract(string = file_path, pattern = date_stamped_pattern)
      ) %>%
        mutate(., stamped_date = str_sub(string = stamped_date, start = 2)) %>%
        mutate(., stamped_date = str_replace_all(string = stamped_date, pattern = "\\_", "-")) %>%
        mutate(., stamped_date = as.Date(stamped_date, "%m-%d-%y")) %>%
        # To add version extension for the dataset with multiple truncation
        group_by(., updated_file_path) %>%
        arrange(., stamped_date) %>%
        mutate(.,
          version_order = row_number(),
          num_records = n()
        ) %>%
        ungroup(.) %>%
        mutate(., version_extension = str_c("_V", version_order, ".rda")) %>%
        mutate(., updated_file_path = case_when(
          num_records > 1 ~ str_replace(
            string = updated_file_path,
            pattern = "\\.rda$",
            replacement = version_extension
          ),
          TRUE ~ updated_file_path
        )) %>%
        mutate(., updated_short_tblname = str_remove_all(
          string = updated_file_path,
          pattern = str_c(c(file_path_pattern, "ADNI\\_"), collapse = "|")
        )) %>%
        assert_uniq(., updated_short_tblname) %>%
        assert_uniq(., short_tblname)
    } else {
      (.)
    }
  }

## Save the list of dataset with date stamped file extension ----
dir.create(date_stamped_dir)
dataset_stamped_date <- dataset_list_dd %>%
  mutate(
    ID = row_number(),
    PREVIOUS_TBLNAME = short_tblname,
    STAMPED_DATE = stamped_date,
    UPDATED_TBLNAME = updated_short_tblname
  ) %>%
  select(ID, PREVIOUS_TBLNAME, STAMPED_DATE, UPDATED_TBLNAME)
readr::write_csv(
  x = dataset_stamped_date,
  file = file.path(date_stamped_dir, "dataset_list_date_stamped.csv")
)
dataset_cat <- dataset_cat %>%
  left_join(
    dataset_stamped_date %>%
      select(PREVIOUS_TBLNAME, UPDATED_TBLNAME),
    by = c("file_list" = "PREVIOUS_TBLNAME")
  ) %>%
  mutate(TBLNAME = case_when(
    !is.na(UPDATED_TBLNAME) ~ UPDATED_TBLNAME,
    TRUE ~ file_list
  ))

## Save dataset category ----
readr::write_csv(
  x = dataset_cat,
  file = file.path(dataset_cat_dir, "dataset_category.csv")
)

## Save the updated dataset with date stamped ----
if (nrow(dataset_list_dd) > 0) {
  updated_data_path_list <- str_remove_all(
    string = data_path_list,
    pattern = "^\\./data/"
  )
  if (any(dataset_list_dd$updated_short_tblname %in% updated_data_path_list)) {
    stop("Check for duplicated files in `./data` before adjusting date stamped extension!")
  }

  for (tbl_name in dataset_list_dd$file_path) {
    cur_file_path <- dataset_list_dd %>%
      filter(file_path == tbl_name) %>%
      pull(file_path)

    new_file_path <- str_replace(
      string = cur_file_path,
      pattern = "^\\./data",
      replacement = date_stamped_dir
    )

    cur_short_tblname <- dataset_list_dd %>%
      filter(file_path == tbl_name) %>%
      pull(short_tblname)

    cur_updated_short_tblname <- dataset_list_dd %>%
      filter(file_path == tbl_name) %>%
      pull(updated_short_tblname)

    # Load to .GlobalEnv
    lapply(cur_file_path, load, .GlobalEnv)
    assign("dd", get(cur_short_tblname))

    data_update_status <- use_data_modified(
      dataset_name = cur_updated_short_tblname,
      dataset = dd,
      edit_type = "create",
      run_script = TRUE
    )
    if (data_update_status != TRUE) {
      stop("The date stamped file extension has not been removed from ", tbl_name)
    }

    # Copy dataset with a date stamped file extension from "./data" to "./data-raw/date_stamped/" folder
    # file.copy(from = cur_file_path, to = new_file_path, overwrite = TRUE)
    message("Removed date stamped file extension from ", cur_short_tblname)
    ## Remove objects from the .GlobalEnv
    rm(list = c(
      "tb", "dd", "cur_file_path", "new_file_path", "cur_short_tblname",
      "cur_updated_short_tblname", "data_update_status",
      cur_short_tblname
    ))
  }

  # Remove already stored dataset with a date stamped file extension from './data/' directory
  file.remove(dataset_list_dd$file_path)
}
rm(list = c("date_stamped_pattern", "dataset_list_dd"))

# Checking for common columns "COLPROT" and "ORIGPROT" across all dataset -----
data_path_list <- list.files(
  path = data_dir,
  pattern = "\\.rda$",
  full.names = TRUE,
  all.files = TRUE,
  recursive = FALSE
)

data_path_list <- data_path_list[!data_path_list %in% c(
  data_dic_path,
  data_downloaded_date_path
)]

if (length(data_path_list) > 0) {
  CHECK_COMMON_COL <- TRUE
} else {
  CHECK_COMMON_COL <- FALSE
}
if (CHECK_COMMON_COL) {
  dataset_not_contains_common_col <- lapply(data_path_list, function(tblname) {
    common_cols <- c("ORIGPROT", "COLPROT")
    short_tblname <- str_remove_all(
      string = tblname,
      pattern = file_path_pattern
    )
    lapply(tblname, load, .GlobalEnv)
    # message("Checking for common cols (ORIGPROT and COLPROT) in ", short_tblname)
    status <- check_colnames(
      data = get(short_tblname),
      col_names = common_cols,
      strict = TRUE,
      stop_message = FALSE
    )
    if (status != TRUE) {
      result <- str_c(short_tblname, " = ", status)
    } else {
      result <- str_c(short_tblname, " = ", "NA")
    }
    rm(list = short_tblname, envir = .GlobalEnv)
    return(result)
  })

  result_table <- tibble(tblname = unlist(unlist(dataset_not_contains_common_col))) %>%
    separate(col = tblname, into = c("tblname", "colname_list"), sep = " = ") %>%
    filter(!colname_list %in% "NA" & !is.na(colname_list)) %>%
    mutate(
      contains_colport_status = case_when(
        str_detect(string = colname_list, pattern = "COLPROT") == TRUE ~ "No"
      ),
      contains_origport_status = case_when(
        str_detect(string = colname_list, pattern = "ORIGPROT") == TRUE ~ "No"
      )
    )

  dataset_list_dd <- tibble(file_path = data_path_list) %>%
    mutate(short_tblname = str_remove_all(
      string = file_path,
      pattern = file_path_pattern
    )) %>%
    assert_uniq(short_tblname) %>%
    left_join(
      result_table %>%
        select(tblname, contains_colport_status, contains_origport_status),
      by = c("short_tblname" = "tblname")
    ) %>%
    assert_uniq(short_tblname) %>%
    mutate(across(
      all_of(c("contains_colport_status", "contains_origport_status")),
      ~ case_when(is.na(.x) ~ "Yes", TRUE ~ "No")
    )) %>%
    mutate(contain_common_cols = case_when(
      contains_colport_status == "Yes" & contains_origport_status == "Yes" ~ "Both COLPROT and ORIGPROT",
      contains_colport_status == "Yes" & contains_origport_status == "No" ~ "Only COLPROT",
      contains_colport_status == "No" & contains_origport_status == "Yes" ~ "Only ORIGPROT",
      contains_colport_status == "No" & contains_origport_status == "No" ~ "None"
    ))

  # Save the dataset name with corresponding common columns status
  dir.create(common_columns_dir)
  readr::write_csv(
    x = dataset_list_dd %>%
      select(TBLNAME = short_tblname, CONTAIN_COMMON_COLS = contain_common_cols),
    file = file.path(common_columns_dir, "dataset_list_common_columns.csv")
  )
}

# Update DATADIC -----
## Prepare the data dictionary dataset ----
### Some of the datasets might have an additional file extension or they are study phase-specific
### E.g. "_V1$", "_V2$", "^ADNI2_", "_ADNI1$", "_ADNIG023$"
### Currently to update the DATADIC file manually (required confirmation!)
if (exists("DATADIC") == FALSE) {
  load(data_dic_path)
  # Adjust for coded values of diagnostics summary in ADNI1GO2
  temp_DATADIC_dxsum <- bind_rows(
    tibble(
      PHASE = c("ADNI1", "ADNI2", "ADNIGO"),
      DATADIC %>%
        filter(TBLNAME %in% "DXSUM" & FLDNAME %in% "DIAGNOSIS") %>%
        filter(PHASE %in% "ADNI3") %>%
        select(-PHASE)
    ),
    # Add dataset label manually for "ADNI2_VISITID", "VISITS" and "DATADIC"
    DATADIC %>%
      filter(TBLNAME %in% c("VISITS", "DATADIC")) %>%
      mutate(CRFNAME = case_when(
        is.na(CRFNAME) & TBLNAME %in% "VISITS" ~ "ADNI study visit code across phases",
        is.na(CRFNAME) & TBLNAME %in% "DATADIC" ~ "Data Dictionary Dataset",
        TRUE ~ CRFNAME
      )) %>%
      filter(TBLNAME %in% "VISITS" |
        (TBLNAME %in% "DATADIC" & FLDNAME %in% c(names(DATADIC)))),
    tibble(
      TBLNAME = "ADNI2_VISITID",
      CRFNAME = "ADNI2 Visit Code Mapping List"
    )
  )
  DATADIC <- DATADIC %>%
    filter(!TBLNAME %in% "DATADIC") %>%
    bind_rows(temp_DATADIC_dxsum)
} else {
  message("`DATADIC` is not found and UPDATED_DATADIC will not be created.")
  UPDATE_DATADIC <- FALSE
}

if (UPDATE_DATADIC) {
  # Assumed the same data dictionary for those paired dataset
  # ?? Required confirmation
  temp_DATADIC <- DATADIC %>%
    mutate(TBLNAME = case_when(
      TBLNAME %in% "ADAS" & PHASE %in% "ADNI1" ~ "ADAS_ADNI1",
      TBLNAME %in% "ADAS" & PHASE %in% c("ADNIGO", "ADNI2", "ADNI3") ~ "ADAS_ADNIGO23",
      TBLNAME %in% "ECG" & PHASE %in% "ADNI2" ~ "ADNI2_ECG",
      TBLNAME %in% "OTELGTAU" & PHASE %in% "ADNI2" ~ "ADNI2_OTELGTAU",
      TBLNAME %in% "PICSLASHS" ~ "PICSL_ASHS",
      TBLNAME %in% "UCD_WMH" ~ "UCD_WMH_V1",
      TBLNAME %in% "TAUMETA" & PHASE %in% "ADNI2" ~ "TAUMETA2",
      TBLNAME %in% "TAUMETA" & PHASE %in% "ADNI3" ~ "TAUMETA3",
      TBLNAME %in% "TAUQC" & PHASE %in% "ADNI3" ~ "TAUQC3",
      TBLNAME %in% "TBM" ~ "TBM22",
      TBLNAME %in% "UCSFASLFS" ~ "UCSFASLFS_V2",
      TBLNAME %in% "PETMETA" & PHASE %in% "ADNI1" ~ "PETMETA_ADNI1",
      TBLNAME %in% "PETMETA" & PHASE %in% c("ADNIGO", "ADNI2") ~ "PETMETA_ADNIGO2",
      TBLNAME %in% "PETMETA" & PHASE %in% "ADNI3" ~ "PETMETA3"
    )) %>%
    filter(!is.na(TBLNAME))

  UPDATED_DATADIC <- DATADIC %>%
    filter(!(TBLNAME %in% c("ECG", "OTELGTAU") & PHASE %in% "ADNI2")) %>%
    bind_rows(
      temp_DATADIC,
      temp_DATADIC %>%
        filter(TBLNAME %in% "UCD_WMH_V1") %>%
        mutate(TBLNAME = "UCD_WMH_V2")
    )
}

## Add a description for common columns: "ORIGPROT" or "COLPROT" ----
if (CHECK_COMMON_COL == TRUE & UPDATE_DATADIC == TRUE) {
  dataset_list_dd <- dataset_list_dd %>%
    # Long format
    pivot_longer(
      cols = c(contains_colport_status, contains_origport_status),
      names_to = "common_cols",
      values_to = "status"
    ) %>%
    filter(status == "Yes") %>%
    mutate(common_cols = case_when(
      str_detect(common_cols, "contains_colport_status") == TRUE ~ "COLPROT",
      str_detect(common_cols, "contains_origport_status") == TRUE ~ "ORIGPROT"
    )) %>%
    # Only TBLNAME that are existed in the UPDATED_DATADIC
    filter(short_tblname %in% unique(UPDATED_DATADIC$TBLNAME))

  # Description of common cols
  common_description_text <- list(
    ORIGPROT = "Original study protocol",
    COLPROT = "Study protocol of data collection"
  )

  for (tblname in unique(dataset_list_dd$short_tblname)) {
    tblname_common_col <- dataset_list_dd %>%
      filter(short_tblname %in% tblname) %>%
      assert_uniq(common_cols) %>%
      assert_non_missing(common_cols) %>%
      pull(common_cols)

    description_text <- lapply(tblname_common_col, function(fldname) {
      common_description_text[[fldname]]
    }) %>%
      unlist()

    UPDATED_DATADIC <- common_cols_description_datadic(
      tblname = tblname,
      data_dict = UPDATED_DATADIC,
      fldname = tblname_common_col,
      description = description_text
    )
  }
}

## Save the UPDATED DATADIC ----
## UPDATED DATADIC will be stored in the "data-raw/updated_datadic" directory
if (UPDATE_DATADIC) {
  dir.create(updated_datadic_dir)
  readr::write_csv(
    x = UPDATED_DATADIC,
    file = file.path(updated_datadic_dir, "UPDATED_DATADIC.csv")
  )
  save(
    list = "UPDATED_DATADIC",
    file = file.path(updated_datadic_dir, "UPDATED_DATADIC.rda")
  )
}
