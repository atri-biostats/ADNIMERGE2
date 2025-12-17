# Update Main Data Dictionary -----
update_main_datadict <- function(.datadict) {
  .datadict <- bind_rows(
    .datadict,
    create_adni2_visitid_datadict(),
    create_visit_datadict(.datadict),
    update_dxsum_datadict(.datadict)
  )
  temp_main_datadict <- .datadict %>%
    filter(TBLNAME %in% "DATADIC" & FLDNAME %in% c(names(.datadict)))
  .datadict <- .datadict %>%
    filter(!TBLNAME %in% "DATADIC") %>%
    bind_rows(temp_main_datadict)
  return(.datadict)
}

# Create a data dictionary for ADNI2_VISITID -----
create_adni2_visitid_datadict <- function() {
  tibble(
    TBLNAME = "ADNI2_VISITID",
    CRFNAME = "ADNI2 Visit Code Mapping List"
  )
}

# Create a data dictionary for VISITS -----
create_visit_datadict <- function(.datadict) {
  .datadict %>%
    filter(TBLNAME %in% "VISITS") %>%
    mutate(CRFNAME = case_when(
      is.na(CRFNAME) & TBLNAME %in% "VISITS" ~ "ADNI study visit code across phases",
      TRUE ~ CRFNAME
    ))
}

# Update DX$DIAGNOSIS data dictionary -----
update_dxsum_datadict <- function(.datadict) {
  tibble::tibble(
    PHASE = c("ADNI1", "ADNI2", "ADNIGO"),
    .datadict %>%
      dplyr::filter(TBLNAME %in% "DXSUM" & FLDNAME %in% "DIAGNOSIS") %>%
      dplyr::filter(PHASE %in% "ADNI3") %>%
      dplyr::select(-PHASE)
  )
}

# Update phase specific datad dictionary -----
update_phase_specific_datadict <- function(.datadict) {
  temp_main_datadict <- .datadict %>%
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

  update_datadict <- .datadict %>%
    filter(!(TBLNAME %in% c("ECG", "OTELGTAU") & PHASE %in% "ADNI2")) %>%
    bind_rows(
      temp_main_datadict,
      temp_main_datadict %>%
        filter(TBLNAME %in% "UCD_WMH_V1") %>%
        mutate(TBLNAME = "UCD_WMH_V2")
    )

  return(update_datadict)
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
#' @importFrom tidyselect everything

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
      message = "Only one of {.var dir_path} and {.var full_file_path} must be provide."
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
  invisible()
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
#'   pattern = "DATADICT\\.rda$"
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
#'   pattern = "DATADICT\\.rda$"
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
    quite = TRUE
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
#' @param quite A Boolean value to hide message
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
                     quite = FALSE) {
  check_load_input(dir_path, full_file_path)
  check_object_type(quite, "logical")
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
  if (!quite) {
    cli::cli_alert_success(text = success_text)
  }
  invisible()
}
