# Data preparation script setup ----
source(file.path(".", "tools", "data-prepare-utils.R"))
source(file.path(".", "R", "utils.R"))
# Libraries ----
library(tidyverse)
library(assertr)

# Clear up "./data" directory
if (dir.exists("./data") == TRUE) unlink("./data", recursive = TRUE)

# Data downloaded date ----
DATA_DOWNLOADED_DATE <- as.Date("2024-10-30")
usethis::use_data(DATA_DOWNLOADED_DATE, overwrite = TRUE)

prefix_patterns <- "^adni\\_"
date_stamped_suffix <- paste0("_", format(DATA_DOWNLOADED_DATE, "%d%b%Y"))

# Extract raw datasets from zip file ----
zip_file_list <- list.files(
  path = "./data-raw", pattern = ".zip", full.names = TRUE,
  all.files = TRUE, recursive = FALSE
)
if (length(zip_file_list) > 0) {
  EXISTED_ZIPFILE <- TRUE
} else {
  EXISTED_ZIPFILE <- FALSE
  warning("No existed zip file existed in data-raw directory")
}

if (EXISTED_ZIPFILE) {
  zip_files_prefix <- str_remove_all(zip_file_list, pattern = "Tables.zip|.zip|./data-raw/")

  removed_zip_strings <- str_c(c(
    zip_files_prefix,
    date_stamped_suffix,
    str_to_upper(prefix_patterns),
    "v_"
  ), collapse = "|")

  lapply(zip_file_list, function(zip_file) {
    ### Unzipped file ----
    unzipped_file_status <- get_unzip_file(
      input_dir = "./data-raw/",
      file_name = str_remove_all(string = zip_file, pattern = "./data-raw/"),
      output_dir = ".",
      overwrite = TRUE
    )
    if (unzipped_file_status != TRUE) stop("Check the ", zip_file, " file")
    ### Renamed csv files ----
    rawdata_csv_path <- str_c(str_remove_all(string = zip_file, pattern = ".zip"), "/")
    rename_file_status <- rename_file(
      input_dir = rawdata_csv_path,
      output_dir = ".",
      file_extension = ".csv",
      removed_strings = removed_zip_strings,
      file_action = "file_rename"
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
rm(list = c("zip_file_list", "removed_zip_strings"))

# Convert raw .csv dataset into .rda file format ----
csv_file_list <- list.files(
  path = "./data-raw", pattern = ".csv",
  all.files = TRUE, full.names = FALSE, recursive = FALSE
)

if (length(csv_file_list) > 0) {
  EXISTED_CSVFILE <- TRUE
} else {
  EXISTED_CSVFILE <- FALSE
  warning("No existed csv file existed in data-raw directory")
}

if (EXISTED_CSVFILE) {
  # Removing the common date stamp from file name;
  ## Any files started with ADNI prefix
  csv_removed_strings <- str_c(c(str_to_upper(prefix_patterns), date_stamped_suffix), collapse = "|")

  rename_file_status <- rename_file(
    input_dir = "./data-raw/",
    output_dir = ".",
    file_extension = ".csv",
    removed_strings = csv_removed_strings,
    file_action = "file_rename"
  )
  if (rename_file_status != TRUE) stop("Check renaming files in ", csv_file_list, " file")
  # Store datasets in "./data" folder
  data_create_status <- using_use_data(
    input_dir = "./data-raw/",
    file_extension = ".csv"
  )
  if (data_create_status != TRUE) stop("The .rda files are not created!")
  rm(list = c("data_create_status", "rename_file_status"))
}
rm(list = c("date_stamped_suffix", "csv_file_list", "csv_removed_strings"))

# Adding common columns and replacing `-4` as missing value ----
dataset_list_files <- list.files(
  path = "./data", pattern = ".rda", full.names = TRUE,
  all.files = TRUE, recursive = FALSE
)
data_dict_file_path <- file.path(".", "data", "DATADIC.rda")
data_downloaded_date_file <- file.path(".", "data", "DATA_DOWNLOADED_DATE.rda")
dataset_list_files <- dataset_list_files[!dataset_list_files %in% c(
  data_dict_file_path,
  data_downloaded_date_file
)]

if (length(dataset_list_files) > 0) {
  UPDATE_MISSING_VALUE <- TRUE
} else {
  UPDATE_MISSING_VALUE <- FALSE
  warning("No existed datasets in data directory")
}

if (UPDATE_MISSING_VALUE) {
  file_path_patterns <- c("\\./data/|\\.rda")
  string_removed_pattern <- str_c(c(prefix_patterns), collapse = "|")

  tblname_list_dd <- tibble(file_list_path = dataset_list_files) %>%
    mutate(
      short_tblname = str_remove_all(string = file_list_path, pattern = file_path_patterns),
      tblname = str_to_upper(str_remove_all(string = file_list_path, pattern = string_removed_pattern))
    )

  for (tb in tblname_list_dd$short_tblname) {
    ## Load all the datasets to .GlobalEnv
    lapply(
      tblname_list_dd %>% filter(short_tblname == tb) %>% pull(file_list_path),
      load, .GlobalEnv
    )
    assign("dd", get(tb))

    if ("RID" %in% names(dd)) check_RID_col <- TRUE else check_RID_col <- FALSE
    ## Adding common columns -----
    if (check_RID_col) {
      message("Adding ORIGPROT and COLPROT variables in ", tb)
      dd <- dd %>%
        create_col_protocol(dd = ., phaseVar = c("Phase", "PHASE", "ProtocolID")) %>%
        create_orig_protocol(dd = .)
    } else {
      message("ORIGPROT and COLPROT are not addedd in ", tb)
    }
    # Replacing `-4` as missing value -----
    message("Making -4 values as missing value for ", tb)
    dd <- make_missing_value(dd = dd, col_name = names(dd), value = "-4", missing_char = NA, phase = NULL)
   
    message("Making -1 values as missing value for ", tb, " in ADNI1 phase")
    dd <- make_missing_value(dd = dd, col_name = names(dd), value = "-1", missing_char = NA, phase = "ADNI1")
    
    data_update_status <- use_data_modified(
      dataset_name = tb,
      dataset = dd,
      edit_type = "create",
      run_script = TRUE
    )
    if (data_update_status != TRUE) stop("The ", tb, " has not been updated!")
    ## Remove objects from the .GlobalEnv
    rm(list = c("tb", "dd"))
  }

  rm(list = c(
    "tblname_list_dd", "file_path_patterns", "string_removed_pattern", "check_RID_col",
    "DATA_DOWNLOADED_DATE", "data_update_status",
    tblname_list_dd$short_tblname
  ))
}

# Remove date stamped file extension from "*.rda" dataset name -----
## Some of the imaging datasets are named with a date stamped extension
dataset_list_files <- list.files(
  path = "./data", pattern = ".rda", full.names = TRUE,
  all.files = TRUE, recursive = FALSE
)
dataset_list_files <- dataset_list_files[!dataset_list_files %in% c(
  data_dict_file_path,
  data_downloaded_date_file
)]

date_stamped_pattern <- "\\_[0-9]{2}\\_[0-9]{2}\\_[0-9]{2}"
file_path_patterns <- c("\\./data/|\\.rda")
dataset_list_dd <- tibble(file_list_path = dataset_list_files) %>%
  mutate(short_tblname = str_remove_all(string = file_list_path, pattern = file_path_patterns)) %>%
  filter(str_detect(string = file_list_path, pattern = date_stamped_pattern)) %>%
  {
    if (nrow(.) > 1) {
      mutate(.,
        updated_file_list_path = str_remove_all(
          string = file_list_path,
          pattern = date_stamped_pattern
        ),
        stamped_date = str_extract_all(
          string = file_list_path,
          pattern = date_stamped_pattern,
          simplify = TRUE
        )
      ) %>%
        mutate(., stamped_date = str_replace_all(
          string = str_sub(string = stamped_date, start = 2),
          pattern = "\\_", "-"
        )) %>%
        mutate(., stamped_date = as.Date(stamped_date, "%m-%d-%y")) %>%
        # To add version extension for the dataset with multiple truncation
        group_by(., updated_file_list_path) %>%
        arrange(., stamped_date) %>%
        mutate(.,
          version_order = row_number(),
          num_records = n()
        ) %>%
        ungroup(.) %>%
        mutate(., version_extension = str_c("_V", version_order, ".rda")) %>%
        mutate(., updated_file_list_path = case_when(
          num_records > 1 ~ str_c(str_replace(
            string = updated_file_list_path,
            pattern = "\\.rda",
            replacement = version_extension
          )),
          TRUE ~ updated_file_list_path
        )) %>%
        mutate(., updated_short_tblname = str_remove_all(
          string = updated_file_list_path,
          pattern = str_c(c(file_path_patterns, "ADNI\\_"), collapse = "|")
        )) %>%
        assert(., is_uniq, updated_short_tblname) %>%
        assert(., is_uniq, short_tblname)
    } else {
      (.)
    }
  }

if (nrow(dataset_list_dd) > 0) {
  updated_dataset_list_files <- str_remove_all(string = dataset_list_files, pattern = "\\./data/")
  if (any(dataset_list_dd$updated_short_tblname %in% updated_dataset_list_files)) stop("Check for duplicated files in `./data` before adjusting date stamped extension")

  date_stamped_folder <- "./data-raw/date_stamped"
  if (dir.exists(date_stamped_folder) == TRUE) unlink(date_stamped_folder, recursive = TRUE)
  dir.create(date_stamped_folder)

  for (tb in dataset_list_dd$file_list_path) {
    cur_file_path <- dataset_list_dd %>%
      filter(file_list_path == tb) %>%
      pull(file_list_path)

    new_file_path <- str_replace(
      string = cur_file_path,
      pattern = "\\./data",
      replacement = date_stamped_folder
    )

    cur_short_tblname <- dataset_list_dd %>%
      filter(file_list_path == tb) %>%
      pull(short_tblname)

    cur_updated_short_tblname <- dataset_list_dd %>%
      filter(file_list_path == tb) %>%
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
    if (data_update_status != TRUE) stop("The date satmped file extension for ", tb, " has not been removed!")

    # Copy dataset with a date stamped file extension from "./data" to "./data-raw/date_stamped/" folder
    file.copy(from = cur_file_path, to = new_file_path, overwrite = TRUE)
    message("Removed date stamped file extension from ", cur_short_tblname)
    ## Remove objects from the .GlobalEnv
    rm(list = c(
      "tb", "dd", "cur_file_path", "new_file_path", "cur_short_tblname",
      "cur_updated_short_tblname", "data_update_status",
      cur_short_tblname
    ))
  }
  # Removing already stored dataset with a date stamped extension from './data/' directory
  file.remove(dataset_list_dd$file_list_path)
  message("Datasets with date stamped extension have been removed from `./data` directory")
}
rm(list = c("date_stamped_pattern", "dataset_list_dd"))

# Checking for common columns "COLPROT" and "ORIGPROT" across all dataset -----
dataset_list_files <- list.files(
  path = "./data", pattern = ".rda", full.names = TRUE,
  all.files = TRUE, recursive = FALSE
)

dataset_list_files <- dataset_list_files[!dataset_list_files %in% c(
  data_dict_file_path,
  data_downloaded_date_file
)]

if (length(dataset_list_files) > 0) {
  CHECK_COMMON_COL <- TRUE
} else {
  CHECK_COMMON_COL <- FALSE
}
if (CHECK_COMMON_COL) {
  dataset_not_contains_common_col <- lapply(
    dataset_list_files,
    function(tblname) {
      common_cols <- c("ORIGPROT", "COLPROT")
      short_tblname <- str_remove_all(string = tblname, pattern = file_path_patterns)
      lapply(tblname, load, .GlobalEnv)
      assign("dd_df", get(short_tblname))

      message("Checking for common cols (ORIGPROT and COLPROT) in ", short_tblname)
      status <- check_colnames(dd = dd_df, col_names = common_cols, strict = TRUE, stop_message = FALSE)
      if (status != TRUE) result <- paste0(short_tblname, " = ", status) else result <- paste0(short_tblname, " = ", "NA")

      data_update_status <- use_data_modified(
        dataset_name = short_tblname,
        dataset = dd_df,
        edit_type = "create",
        run_script = TRUE
      )
      if (data_update_status != TRUE) stop("The common cols in the ", tb, " have not been added!")
      return(result)
    }
  )

  result_table <- tibble(tblname = unlist(unlist(dataset_not_contains_common_col))) %>%
    separate(col = tblname, into = c("tblname", "colname_list"), sep = " = ") %>%
    filter(!colname_list %in% "NA" & !is.na(colname_list)) %>%
    {
      if (nrow(.) > 0) {
        mutate(.,
          contains_colport_status = case_when(str_detect(string = colname_list, pattern = "COLPROT") ~ "No"),
          contains_origport_status = case_when(str_detect(string = colname_list, pattern = "ORIGPROT") ~ "No")
        )
      } else {
        (.)
      }
    }

  # This could be replaced with a stop message to make sure the common cols are added for all dataset
  if (nrow(result_table) > 0) {
    warning("At least one dataset does not contains the common columns : `ORIGPROT` or `COLPROT`")
  }
}

# Update DATADIC -----
## Prepare the data dictionary dataset ----
### Some of the dataset might have an additional file extension or are study phase specific
### E.g. "_V1$", "_V2$", "^ADNI2_", "_ADNI1$", "_ADNIG023$"
### Currently to update the DATADIC file manually (required confirmation!)
if (exists("DATADIC") == FALSE) {
  load(data_dict_file_path)
  CREATE_UPDATED_DATADIC <- TRUE
  # Adjust for coded values of diagnostics summary in ADNI1GO2
  temp_DATADIC_dxsum <- tibble(
    PHASE = c("ADNI1", "ADNI2", "ADNIGO"),
    DATADIC %>%
      filter(TBLNAME %in% "DXSUM" & FLDNAME %in% "DIAGNOSIS") %>%
      filter(PHASE %in% "ADNI3") %>%
      select(-PHASE)
  )
  DATADIC <- DATADIC %>%
    bind_rows(temp_DATADIC_dxsum)
} else {
  message("`DATADIC` is not found and UPDATED_DATADIC will not be created.")
  CREATE_UPDATED_DATADIC <- FALSE
}

if (CREATE_UPDATED_DATADIC) {
  temp_DATADIC <- DATADIC %>%
    mutate(TBLNAME = case_when(
      TBLNAME %in% "ADAS" & PHASE %in% "ADNI1" ~ "ADAS_ADNI1",
      TBLNAME %in% "ADAS" & PHASE %in% c("ADNI1", "ADNIGO", "ADNI2", "ADNI3") ~ "ADAS_ADNIGO23",
      TBLNAME %in% "ECG" & PHASE %in% "ADNI2" ~ "ADNI2_ECG",
      TBLNAME %in% "OTELGTAU" & PHASE %in% "ADNI2" ~ "ADNI2_OTELGTAU",
      TBLNAME %in% "PICSLASHS" ~ "PICSL_ASHS",
      TBLNAME %in% "UCD_WMH" ~ "UCD_WMH_V1",
      TBLNAME %in% "TAUMETA" & PHASE %in% "ADNI2" ~ "TAUMETA2",
      TBLNAME %in% "TAUMETA" & PHASE %in% "ADNI3" ~ "TAUMETA3",
      TBLNAME %in% "TAUQC" & PHASE %in% "ADNI3" ~ "TAUQC3",
      TBLNAME %in% "TBM" ~ "TBM22",
      TBLNAME %in% "UCSFASLFS" ~ "UCSFASLFS_V2"
    )) %>%
    filter(!is.na(TBLNAME))

  UPDATED_DATADIC <- DATADIC %>%
    bind_rows(
      temp_DATADIC,
      temp_DATADIC %>%
        filter(TBLNAME %in% "UCD_WMH_V1") %>%
        mutate(TBLNAME = "UCD_WMH_V2")
    )
}
## Add a description for common columns: "ORIGPROT" or "COLPROT" ----
if (CHECK_COMMON_COL == TRUE & CREATE_UPDATED_DATADIC == TRUE) {
  dataset_list_dd <- tibble(file_list_path = dataset_list_files) %>%
    mutate(short_tblname = str_remove_all(
      string = file_list_path,
      pattern = file_path_patterns
    )) %>%
    assert(is_uniq, short_tblname)

  if (exists("result_table")) {
    if (any(!result_table$tblname %in% c(dataset_list_dd$short_tblname))) stop("List of dataset file does not contains all tblname")
    dataset_list_dd <- dataset_list_dd %>%
      left_join(
        result_table %>%
          select(tblname, contains_colport_status, contains_origport_status),
        by = c("short_tblname" = "tblname")
      ) %>%
      assert(is_uniq, short_tblname)
  }

  dataset_list_dd <- dataset_list_dd %>%
    {
      if (!"contains_colport_status" %in% colnames(.)) {
        mutate(., contains_colport_status = NA)
      } else {
        (.)
      }
    } %>%
    {
      if (!"contains_origport_status" %in% colnames(.)) {
        mutate(., contains_origport_status = NA)
      } else {
        (.)
      }
    } %>%
    mutate(
      contains_colport_status = case_when(
        is.na(contains_colport_status) ~ "Yes",
        TRUE ~ contains_colport_status
      ),
      contains_origport_status = case_when(
        is.na(contains_origport_status) ~ "Yes",
        TRUE ~ "No"
      )
    ) %>%
    # Long format
    pivot_longer(
      cols = c(contains_colport_status, contains_origport_status),
      names_to = "common_cols",
      values_to = "status"
    ) %>%
    filter(status == "Yes") %>%
    mutate(common_cols = case_when(
      str_detect(common_cols, "contains_colport_status") ~ "COLPROT",
      str_detect(common_cols, "contains_origport_status") ~ "ORIGPROT"
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
      assert(is_uniq, common_cols) %>%
      assert_rows(num_row_NAs, within_bounds(0, 0.01), common_cols) %>%
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

## Stored the UPDATED DATADIC ----
## Temporary stored in the "data-raw/updated_datadic" directory
if (CREATE_UPDATED_DATADIC) {
  updated_datadic_folder <- "./data-raw/updated_datadic"
  if (dir.exists(updated_datadic_folder) == TRUE) unlink(updated_datadic_folder, recursive = TRUE)
  dir.create(updated_datadic_folder)
  write.csv(
    x = UPDATED_DATADIC,
    file = paste0(updated_datadic_folder, "/UPDATED_DATADIC.csv"),
    row.names = FALSE
  )
  save(
    list = "UPDATED_DATADIC",
    file = paste0(updated_datadic_folder, "/UPDATED_DATADIC.rda")
  )
}
