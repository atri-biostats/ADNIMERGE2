# Update Main Data Dictionary -----
# Rows are replaced/added in place so that the function is idempotent,
# i.e., calling it more than once does not duplicate any records.
update_main_datadict <- function(.datadict) {
  .datadict <- .datadict %>%
    create_adni2_visitid_datadict() %>%
    create_visit_datadict() %>%
    update_dxsum_datadict() %>%
    update_adni4_ptdemog_datadict() %>%
    # Only keep main DATADIC description records of existing DATADIC columns
    filter(!(TBLNAME %in% "DATADIC" & !FLDNAME %in% names(.datadict))) %>%
    mutate(CRFNAME = case_when(
      TBLNAME %in% "DATADIC" ~ "ADNI Study Data Dictionary",
      TRUE ~ CRFNAME
    ))
  return(.datadict)
}

# Create a data dictionary for ADNI2_VISITID -----
create_adni2_visitid_datadict <- function(.datadict) {
  if (any(.datadict$TBLNAME %in% "ADNI2_VISITID")) {
    return(.datadict)
  }
  bind_rows(
    .datadict,
    tibble(
      TBLNAME = "ADNI2_VISITID",
      CRFNAME = "ADNI2 Visit Code Mapping List"
    )
  )
}

# Create a data dictionary for VISITS -----
create_visit_datadict <- function(.datadict) {
  .datadict %>%
    mutate(CRFNAME = case_when(
      is.na(CRFNAME) & TBLNAME %in% "VISITS" ~ "ADNI study visit code across study phases",
      TRUE ~ CRFNAME
    ))
}

# Update DX$DIAGNOSIS data dictionary -----
# DIAGNOSIS field codes are only listed for ADNI3 phase,
# and are copied to ADNI1, ADNIGO and ADNI2 phases if they do not exist.
update_dxsum_datadict <- function(.datadict) {
  add_phases <- c("ADNI1", "ADNI2", "ADNIGO")
  is_dxsum_diagnosis <- .datadict$TBLNAME %in% "DXSUM" & .datadict$FLDNAME %in% "DIAGNOSIS"
  adni3_record <- .datadict %>%
    dplyr::filter(is_dxsum_diagnosis & PHASE %in% "ADNI3") %>%
    dplyr::slice_head(n = 1)
  add_phases <- add_phases[!add_phases %in% .datadict$PHASE[is_dxsum_diagnosis]]
  if (nrow(adni3_record) == 0 || length(add_phases) == 0) {
    return(.datadict)
  }
  bind_rows(
    .datadict,
    tidyr::crossing(
      PHASE = add_phases,
      adni3_record %>% dplyr::select(-PHASE)
    )
  )
}

# Update ptdemog field data dictionary ------
# Only race and other ethnicity categories:
# additional (legacy) codes that are not listed in the ADNI4 DATADIC records.
# These records are appended once, in addition to the existing records.
update_adni4_ptdemog_datadict <- function(.datadict) {
  # Based on internal source
  extra_codes <- c(
    # PTRACCAT
    PTRACCAT = "3 = Native Hawaiian or Other Pacific Islander",
    # PTETHCATH
    PTETHCATH = "1 = Mexican, Mexican Am., Chicano; 4 = Another Hispanic, Latino, or Spanish origin"
  )
  extra_datadict <- lapply(names(extra_codes), function(fld) {
    is_fld <- .datadict$TBLNAME %in% "PTDEMOG" & .datadict$FLDNAME %in% fld & .datadict$PHASE %in% "ADNI4"
    if (any(.datadict$CODE[is_fld] %in% extra_codes[[fld]])) {
      return(NULL)
    }
    .datadict[is_fld, ] %>%
      dplyr::slice_head(n = 1) %>%
      mutate(CODE = extra_codes[[fld]])
  })
  bind_rows(.datadict, extra_datadict)
}

# Update phase specific data dictionary -----
update_phase_specific_datadict <- function(.datadict) {
  temp_main_datadict <- .datadict %>%
    mutate(TBLNAME = case_when(
      TBLNAME %in% "ECG" & PHASE %in% "ADNI2" ~ "ADNI2_ECG",
      TBLNAME %in% "OTELGTAU" & PHASE %in% "ADNI2" ~ "ADNI2_OTELGTAU",
      TBLNAME %in% "UCSFASLFS" ~ "UCSFASLFS_V2",
      TBLNAME %in% "PETMETA" & PHASE %in% "ADNI1" ~ "PETMETA_ADNI1",
      TBLNAME %in% "PETMETA" & PHASE %in% c("ADNIGO", "ADNI2") ~ "PETMETA_ADNIGO2",
      TBLNAME %in% "PETMETA" & PHASE %in% "ADNI3" ~ "PETMETA3"
    )) %>%
    filter(!is.na(TBLNAME))

  update_datadict <- .datadict %>%
    filter(!(TBLNAME %in% c("ECG", "OTELGTAU") & PHASE %in% "ADNI2")) %>%
    bind_rows(temp_main_datadict)

  return(update_datadict)
}

# External data dictionary -----
# Some data dictionaries (e.g., ADSP-PHC merged data dictionary and ADOPIC MUSE
# volumetrics dictionary) are shared in a different layout than the main DATADIC:
# DS_NAME, DS_DSCR, VARNAME, VARDSCR, VARTYPE, FLD_LEN, DECML, UNITS, CODES, ...

#' @title Check External Data Dictionary Layout
#' @param .data A data.frame
#' @return A Boolean value
#' @rdname is_external_datadict
#' @keywords internal
is_external_datadict <- function(.data) {
  is.data.frame(.data) && all(c("DS_NAME", "VARNAME", "VARDSCR") %in% names(.data))
}

#' @title Rename External Data Dictionary Name
#' @description
#'  Standardize an external data dictionary name to end with \code{_DATADIC},
#'  e.g., \code{ADSP_PHC_MERGED_DATADIC_20260521} to \code{ADSP_PHC_DATADIC} and
#'  \code{MUSE_volume_ADNI123_Dictionary} to \code{MUSE_volume_ADNI123_DATADIC}.
#' @param x Character vector of dataset names
#' @return A character vector
#' @rdname rename_external_datadict
#' @keywords internal
rename_external_datadict <- function(x) {
  stringr::str_replace(
    string = x,
    pattern = "(\\_MERGED)?\\_(DATADIC|Dictionary|DICTIONARY)(\\_[0-9]{8})?$",
    replacement = "_DATADIC"
  )
}

#' @title Convert External Data Dictionary Layout
#' @description
#'  Convert an external data dictionary into the main \code{DATADIC} layout.
#' @param .data An external data dictionary data.frame
#' @return A data.frame with \code{PHASE}, \code{CRFNAME}, \code{TBLNAME},
#'  \code{FLDNAME}, \code{TEXT}, \code{TYPE}, \code{LENGTH}, \code{CODE},
#'  \code{UNITS}, \code{NOTES} and \code{update_stamp} columns.
#' @rdname convert_external_datadict
#' @keywords internal
convert_external_datadict <- function(.data) {
  if (!is_external_datadict(.data)) {
    cli::cli_abort(message = "{.var .data} is not an external data dictionary.")
  }
  empty_to_na <- function(x) {
    x <- as.character(x)
    x[!is.na(x) & stringr::str_trim(x) %in% ""] <- NA_character_
    x
  }
  get_col <- function(col) {
    if (col %in% names(.data)) empty_to_na(.data[[col]]) else NA_character_
  }
  tibble::tibble(
    PHASE = NA_character_,
    CRFNAME = get_col("DS_DSCR"),
    TBLNAME = get_col("DS_NAME"),
    FLDNAME = get_col("VARNAME"),
    TEXT = get_col("VARDSCR"),
    TYPE = get_col("VARTYPE"),
    LENGTH = get_col("FLD_LEN"),
    CODE = get_col("CODES"),
    UNITS = get_col("UNITS"),
    NOTES = get_col("NOTES"),
    update_stamp = get_col("update_stamp")
  )
}

# Utils functions -------
#' @title Bind Data Dictionary Description
#' @param .datadict A data dictionary
#' @param code Short name/code
#' @param label Label
#' @return A data.frame bind with data dictionary description
#' @rdname bind_datadict_description
#' @export
#' @importFrom tibble tibble
#' @importFrom dplyr mutate across bind_rows
#' @importFrom dplyr everything

bind_datadict_description <- function(.datadict, code, label) {
  .datadict <- .datadict %>%
    mutate(across(everything(), as.character))
  desc_data <- tibble::tibble(
    CRFNAME = label,
    TBLNAME = code,
    FLDNAME = names(.datadict)
  )
  .datadict <- bind_rows(.datadict, desc_data)
  .datadict
}

# Manual dictionary labels ----
edit_datadict_labels <- function(.datadict_code) {
  if (.datadict_code %in% "DATADIC") {
    text_label <- "ADNI Study Data Dictionary"
  } else if (.datadict_code %in% "REMOTE_DATADIC") {
    text_label <- "Data Dictionary For Remotely Collected Data In ADNI4 Study Phase"
  } else {
    text_label <- .datadict_code
  }
  return(text_label)
}


#' @title Check Load Inputs
#' @param dir_path Input directory arg
#' @param full_file_path Full file path arg
#' @return A error message
#' @rdname check_load_input
#' @keywords utils_fun internal
#' @family load files
#' @importFrom cli cli_abort

check_load_input <- function(dir_path, full_file_path) {
  if (!is.null(dir_path) & !is.null(full_file_path)) {
    cli::cli_abort(
      message = "Only one of {.var dir_path} and {.var full_file_path} must be provided."
    )
  }
  if (is.null(dir_path) & is.null(full_file_path)) {
    cli::cli_abort(
      message = paste0(
        "At least one of {.var dir_path} and ",
        "{.var full_file_path} must not be missing."
      )
    )
  }
  invisible(TRUE)
}

#' @title A Wrapper Function For Listing Files
#' @inheritParams base::list.files
#' @param dir_path File directory
#' @param pattern Pattern
#' @inheritSection base::list.files return
#' @examples
#' \dontrun{
#' # List all available data dictionary files from "./data"
#' datadict_file_list <- get_full_file_path(
#'   dir_path = "./data",
#'   pattern = "DATADIC\\.rda$"
#' )
#' datadict_file_list
#' }
#' @rdname get_full_file_path
#' @keywords utils_fun

get_full_file_path <- function(dir_path, pattern) {
  full_file_path <- list.files(
    path = dir_path,
    pattern = pattern,
    full.names = TRUE,
    all.files = FALSE,
    recursive = FALSE
  )
  return(full_file_path)
}

#' @title Convert File Path As A List Object
#' @param x File path
#' @param input_dir File directory
#' @return A list object
#' @examples
#' \dontrun{
#' dir_path <- "./data"
#' datadict_file_list <- get_full_file_path(
#'   dir_path = dir_path,
#'   pattern = "DATADIC\\.rda$"
#' )
#' datadict_file_list <- convert_file_path_aslist(
#'   x = datadict_file_list,
#'   dir_path = dir_path
#' )
#' datadict_file_list
#' }
#' @rdname convert_file_path_aslist
#' @keywords utils_fun

convert_file_path_aslist <- function(x, dir_path) {
  check_dir_path(dir_path)
  remove_chars <- c(
    paste0("^", dir_path, "/"),
    paste0("\\.rda$")
  )
  remove_chars <- paste0(remove_chars, collapse = "|")
  names(x) <- gsub(remove_chars, "", x)
  x <- as.list(x)
  return(x)
}

#' @title Get Multiple Data As Listed Object
#' @inheritParams convert_file_path_aslist
#' @return A list object that contains a data.frame
#' @examples
#' \dontrun{
#' # To get all available data dictionary file from "./data"
#' multiple_datadict <- get_listed_data(
#'   dir_path = "./data",
#'   pattern = "DATADIC\\.rda$"
#' )
#' is.list(multiple_datadict)
#' }
#' @seealso \code{\link[load_rda]()}
#' @rdname get_listed_data
#' @keywords utils_fun
#' @export

get_listed_data <- function(dir_path, pattern) {
  full_file_path <- get_full_file_path(dir_path = dir_path, pattern = pattern)
  full_file_path <- convert_file_path_aslist(x = full_file_path, dir_path = dir_path)
  # Load data in new environments
  .envir <- new.env()
  load_rda(
    dir_path = NULL,
    full_file_path = full_file_path,
    pattern = NULL,
    .envir = .envir,
    quiet = TRUE
  )
  output_data <- mget(names(full_file_path), envir = .envir)
  return(output_data)
}

#' @title Load multiple '.rda' files to specific environments
#' @param dir_path File directory, Default: NULL
#' @param full_file_path File path, Default: NULL
#' @param pattern Pattern, Default: 'DATADIC\\.rda$'
#'        Only applicable if \code{full_file_path} is missing
#' @param .envir Environment, Default: NULL
#' @param quiet A Boolean value to hide message
#' @return Load data into specific environment
#' @examples
#' \dontrun{
#' # To load all available data dictionary file into .GlobalEnv
#' load_rda(
#'   dir_path = "./data",
#'   pattern = "DATADIC\\.rda$",
#'   .envir = .GlobalEnv
#' )
#' }
#' @seealso \code{\link[get_listed_data]()}
#' @rdname load_rda
#' @keywords utils_fun
#' @export
#' @importFrom rlang caller_env
#' @importFrom cli cli_alert_success

load_rda <- function(dir_path = NULL,
                     full_file_path = NULL,
                     pattern = "DATADIC\\.rda$",
                     .envir = NULL,
                     quiet = FALSE) {
  check_load_input(dir_path, full_file_path)
  check_object_type(quiet, "logical")
  if (!is.null(dir_path)) {
    full_file_path <- get_full_file_path(dir_path = dir_path, pattern = pattern)
    full_file_path <- convert_file_path_aslist(x = full_file_path, dir_path = dir_path)
  }
  if (is.null(.envir)) .envir <- rlang::caller_env()
  lapply(full_file_path, load, .envir)
  success_text <- sprintf(
    paste0(
      "Load {.val {names(full_file_path)[%d]}} to ",
      "{.cls {rlang::env_name(.envir)}} environment. \n"
    ),
    seq_along(names(full_file_path))
  )
  if (!quiet) {
    cli::cli_alert_success(text = success_text)
  }
  invisible(TRUE)
}
