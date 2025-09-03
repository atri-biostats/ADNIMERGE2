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
      message = c(
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
      message = c(
        "{.var ds_list} must be a list object. \n",
        "{.var ds_list} is a {.cls {class(ds_list)}} object."
      )
    )
  }
  if (length(names(ds_list)) != 1) {
    cli_abort(
      message = c(
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
