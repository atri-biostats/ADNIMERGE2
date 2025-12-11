# Convert R6-class object into tibble object -----
#' @title Convert Metacore R6-Wrapper Object into Tibble Object
#' @description
#'  This function is used to convert a metacore R6-Wrapper Object into tibble object.
#' @param .metacore Metacore R6-wrapper object
#' @param dataset_name Dataset name, Default: NULL
#' @return
#'  A tibble object version of the input R6-wrapper object and could be specific dataset attribute.
#' @examples
#' \dontrun{
#' convert_metacore_to_tibble(ADNIMERGE2::METACORES)
#' }
#' @seealso
#'  \code{\link[metacore]{select_dataset}}
#' @rdname convert_metacore_to_tibble
#' @importFrom cli cli_abort
#' @importFrom purrr map
#' @importFrom metacore select_dataset is_metacore
#' @importFrom dplyr bind_rows left_join select filter
#' @importFrom tibble as_tibble
#' @export
convert_metacore_to_tibble <- function(.metacore, dataset_name = NULL) {
  require(metacore)
  require(purrr)
  require(dplyr)
  require(tibble)
  require(cli)
  if (!is_metacore(.metacore)) {
    cli_abort(
      c(
        "{.var .metacore} must be a metacore R6-class wrapper object. \n",
        "{.var .metacore} is a {.cls {class(.metacore)}} object."
      )
    )
  }
  dataset_list <- .metacore$ds_spec$dataset

  metadata <- map(dataset_list, ~ .metacore %>%
    metacore::select_dataset(dataset = .x, simplify = TRUE)) %>%
    bind_rows() %>%
    left_join(
      .metacore$ds_spec %>%
        select(dataset, structure, dataset.label = label),
      by = "dataset"
    ) %>%
    {
      if (!is.null(dataset_name)) {
        filter(., dataset %in% dataset_name)
      } else {
        (.)
      }
    } %>%
    as_tibble()

  return(metadata)
}

# Build dataset from a single data based on multiple inputs -----
#' @title Build from Single Derived Dataset
#' @description
#'  This function is used to build analysis ready dataset from a single input
#'  derived dataset based on specific metacore R6-wrapper object.
#' @inheritParams metatools::build_from_derived
#' @param metacore A metacore R6-wrapper object
#' @return
#'  A data.frame that build based on \code{\link[metatools]{build_from_derived}} function.
#' @examples
#' \dontrun{
#' single_build_from_derived(
#'   metacore = ADNIMERGE2::METACORES,
#'   dataset_name = "ADQS",
#'   ds_list = list("QS" = QS),
#'   predecessor_only = TRUE,
#'   keep = TRUE
#' )
#' }
#' @seealso
#'  \code{\link[metacore]{metacore}}
#'  \code{\link[metatools]{build_from_derived}}
#' @rdname single_build_from_derived
#' @importFrom cli cli_abort
#' @importFrom metacore metacore select_dataset
#' @importFrom metatools build_from_derived
#' @importFrom dplyr filter
#' @importFrom tibble as_tibble
#' @importFrom stringr str_detect
#' @export
single_build_from_derived <- function(metacore,
                                      ds_list,
                                      dataset_name = NULL,
                                      predecessor_only = TRUE,
                                      keep = FALSE) {
  require(metacore)
  require(metatools)
  require(dplyr)
  require(stringr)

  if (!is.list(ds_list)) {
    cli_abort(
      c(
        "{.var ds_list} must be a list object. \n",
        "{.var ds_list} is a {.cls {class(ds_list)}} object."
      )
    )
  }
  if (length(names(ds_list)) != 1) {
    cli_abort(
      c(
        "{.var ds_list} must be a single list object. \n",
        "{.var ds_list} is a list object with {.clas {length(names(ds_list))} name{?s}."
      )
    )
  }
  input_dataset_name <- names(ds_list)
  input_dataset_name <- toupper(input_dataset_name)
  metacore <- metacore %>%
    select_dataset(.data = ., dataset = dataset_name, simplify = FALSE)

  # Predecessor from a single dataset
  derivations <- metacore$derivations %>%
    filter(str_detect(string = derivation, pattern = paste0("^", input_dataset_name, "\\.")))

  if (nrow(metacore$supp) == 0) {
    supp <- tibble(
      dataset = character(),
      variable = character(),
      idvar = character(),
      qeval = character()
    )
  } else {
    supp <- metacore$supp
  }
  # Internal metacores - modified
  internal_metacore <- metacore::metacore(
    ds_spec = metacore$ds_spec,
    supp = supp,
    codelist = metacore$codelist,
    derivations = derivations,
    value_spec = metacore$value_spec,
    var_spec = metacore$var_spec,
    ds_vars = metacore$ds_vars
  )
  .data <- metatools::build_from_derived(
    metacore = internal_metacore,
    ds_list = ds_list,
    dataset_name = dataset_name,
    predecessor_only = predecessor_only,
    keep = keep
  )
  return(.data)
}

# Use `convert_var_to_fct` for multiple variables -----
#' @title Convert Multiple Variables into Factor Type - Wrapper Function
#' @description
#'  A wrapper function to apply \code{\link[metatools]{convert_var_to_fct}}
#'  for multiple variables simultaneously.
#' @param .data Data.frame
#' @inheritParams metatools::convert_var_to_fct
#' @param var Character vector of variable name(s)
#' @return A data.frame as of \code{\link[metatools]{convert_var_to_fct}}.
#' @examples
#' \dontrun{
#' # See study package vignette
#' vignette("ADNIMERGE2-Analysis-Data", package = "ADNIMERGE2")
#' }
#' @seealso
#'  \code{\link[metatools]{convert_var_to_fct}}
#'  \code{\link[rlang]{sym}}
#' @rdname convert_var_to_fct_wrapper
#' @export
#' @importFrom metatools convert_var_to_fct
#' @importFrom rlang sym
convert_var_to_fct_wrapper <- function(.data, metacore, var) {
  require(metatools)
  require(rlang)
  for (var_name in var) {
    .data <- metatools::convert_var_to_fct(
      data = .data,
      metacore = metacore,
      var = !!rlang::sym(var_name)
    )
  }
  return(.data)
}

#' @title Get Common Values Across Variables
#' @param .data A data.frame
#' @param var_tracer_name Variable name
#' @param split_parm Split parameters, Default: '0'
#' @return A data.frame that contains the following columns
#'   \item \code{USUBJID} Subject ID
#'   \item \code{SELECT_COL} Variable name where the value is obtained
#'   \item \code{UNIQUE_VALUE} Unique value
#' @rdname get_common_vars_value
#' @importFrom dplyr pivot_longer group_by mutate ungroup case_when filter distinct select
#' @importFrom assertr verify
#' @importFrom stringr str_detect
#' @importFrom tidyselect all_of any_of starts_with ends_with

get_common_vars_value <- function(.data, var_tracer_name, split_parm = "0") {
  USUBJID <- VARNAME <- VARVALUE <- UNIQUE_VALUE <- SELECT_COL <- NULL
  image_resolu <- c("6MM", "8MM", "NONE")
  .data_long <- .data %>%
    assert_uniq(USUBJID) %>%
    pivot_longer(
      cols = starts_with(var_tracer_name),
      names_to = "VARNAME",
      values_to = "VARVALUE"
    ) %>%
    group_by(USUBJID) %>%
    mutate(UNIQUE_VALUE = toString(unique(VARVALUE)[!is.na(unique(VARVALUE))])) %>%
    mutate(SELECT_COL = case_when(UNIQUE_VALUE == VARVALUE ~ VARNAME)) %>%
    ungroup() %>%
    mutate(across(all_of(c("SELECT_COL", "UNIQUE_VALUE")), as.character)) %>%
    verify(!str_detect(UNIQUE_VALUE, ",")) %>%
    filter(!is.na(SELECT_COL)) %>%
    select(USUBJID, SELECT_COL, UNIQUE_VALUE) %>%
    distinct() %>%
    assert_uniq(USUBJID)
  return(.data_long)
}

#' @title Bind Amyloid Status From Multiple Image Tracers
#' @param .data_list A list of data.frame
#' @param var_initial Variable initial
#' @inheritParams get_common_vars_value
#' @return A data.frame contains the following columns
#'  \item \code{USUBJID} Subject ID
#'  \item \code{AMYSTAT_COL} Amyloid status
#'  \item \code{TRACER_COL} Type of image tracer,
#'  \item \code{IMAGERES_COL} Image resolutions either \code{6MM}, \code{8MM} or \code{NONE}
#'  \item \code{AMYSTAT_SOURCE} Source type the same as the \code{var_initial}
#' @rdname bind_amystatus_tracer
#' @importFrom tidyr separate_wider_delim
#' @importFrom stringr str_remove_all

bind_amystatus_tracer <- function(.data_list, var_initial, split_parm = "0") {
  AMYSTAT_COL <- TRACER <- AMYSTAT_SOURCE <- NULL
  check_object_type(.data_list, "list")
  var_initial <- paste0(var_initial, split_parm)
  .data_list <- .data_list[str_detect(names(.data_list), var_initial)]
  combined_data <- bind_rows(.data_list) %>%
    assert_uniq(USUBJID) %>%
    mutate(
      AMYSTAT_COL = UNIQUE_VALUE,
      TRACER = str_remove_all(SELECT_COL, paste0("^", var_initial))
    ) %>%
    separate_wider_delim(TRACER, names = c("TRACER_COL", "IMAGERES_COL"), delim = "_") %>%
    verify(all(IMAGERES_COL %in% c("6MM", "8MM", "NONE", NA))) %>%
    mutate(AMYSTAT_SOURCE = str_remove(var_initial, split_parm)) %>%
    select(all_of(c("USUBJID", "AMYSTAT_COL", "TRACER_COL", "IMAGERES_COL", "AMYSTAT_SOURCE")))
  return(combined_data)
}

#' @title Create common amyloid status variable from different image tracer and resolutions
#' @inheritParams get_common_vars_value
#' @return A data.frame with following appended columns
#'  \item \code{AMYSTAT} Amyloid status based on centiloids that normalized by
#'  \item \code{AMYSTATC} Amyloid status based on centiloids using composite reference
#'  \item \code{TRACER}/\code{_TRACER}, Type of image tracer,
#'  \item \code{IMAGERES}/\code{_IMAGERES} Image resolutions either \code{6MM}, \code{8MM} or \code{NONE}
#' @rdname create_amystatus_cols
#' @importFrom dplyr pivot_wider rename_with left_join
#' @importFrom cli cli_abort
#'
create_amystatus_cols <- function(.data,
                                  var_tracer_names = c(
                                    "AMYSTAT0FBP", "AMYSTAT0FBB", "AMYSTAT0NAV",
                                    "AMYSTATC0FBP", "AMYSTATC0FBB", "AMYSTATC0NAV"
                                  ),
                                  split_parm = "0") {
  if (!all(str_detect(var_tracer_names, split_parm) == TRUE)) {
    cli::cli_abort("{.var split_parm} must be found in all {.val {{var_tracer_names}}}")
  }
  num_rows <- nrow(.data)
  amystat_col_data <- lapply(var_tracer_names, function(x) {
    get_common_vars_value(.data = .data, var_tracer_name = x, split_parm = split_parm)
  })
  names(amystat_col_data) <- var_tracer_names

  var_initial_list <- c("AMYSTAT", "AMYSTATC")
  full_amystat_data <- lapply(var_initial_list, function(x) {
    bind_amystatus_tracer(.data_list = amystat_col_data, var_initial = x, split_parm = split_parm)
  })
  names(full_amystat_data) <- var_initial_list

  full_amystat_data <- bind_rows(full_amystat_data, .id = "AMYSTAT_SOURCE") %>%
    pivot_wider(
      names_from = all_of("AMYSTAT_SOURCE"),
      values_from = all_of(c("AMYSTAT_COL", "TRACER_COL", "IMAGERES_COL"))
    ) %>%
    rename_with(~ str_remove_all(.x, "^AMYSTAT_COL_"), starts_with("AMYSTAT_COL")) %>%
    mutate(
      SAME_TRACER = TRACER_COL_AMYSTAT == TRACER_COL_AMYSTATC,
      SAME_IMAGERES = IMAGERES_COL_AMYSTAT == IMAGERES_COL_AMYSTATC
    )

  tracer_status <- all(full_amystat_data$SAME_TRACER == TRUE)
  imageres_status <- all(full_amystat_data$SAME_IMAGERES == TRUE)
  if (nrow(full_amystat_data) == 0) {
    tracer_status <- imageres_status <- TRUE
  }
  full_amystat_data <- full_amystat_data %>%
    select(-starts_with("SAME_"))
  if (tracer_status) {
    full_amystat_data <- full_amystat_data %>%
      mutate(TRACER = TRACER_COL_AMYSTAT) %>%
      select(-starts_with("TRACER_COL_AMYSTAT"))
  } else {
    full_amystat_data <- full_amystat_data %>%
      rename_with(
        ~ paste0(str_remove_all(.x, "TRACER_COL_"), "_TRACER"),
        starts_with("TRACER_COL_AMYSTAT")
      )
  }
  if (imageres_status) {
    full_amystat_data <- full_amystat_data %>%
      mutate(IMAGERES = IMAGERES_COL_AMYSTAT) %>%
      select(-starts_with("IMAGERES_COL_"))
  } else {
    full_amystat_data <- full_amystat_data %>%
      rename_with(
        ~ paste0(str_remove_all(.x, "IMAGERES_COL_"), "_IMAGERES"),
        starts_with("IMAGERES_COL_AMYSTAT")
      )
  }
  .data <- .data %>%
    left_join(full_amystat_data, by = "USUBJID") %>%
    assert_uniq(all_of("USUBJID")) %>%
    verify(nrow(.) == num_rows)

  return(.data)
}
