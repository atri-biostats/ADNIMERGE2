## Get common VISCODE2 -----
#' @title Get Common VISCODE2 Value
#'
#' @description
#'  Function to create a common \code{VISCODE2} value in a long format
#'  data across ID columns.
#'
#' @param .data A data.frame
#' @param id_cols ID columns
#' @param col_order Ordered names in a long format data
#' @param quiet
#'    A Boolean value to show message.
#'    By default, it is \code{quiet = FALSE}.
#'
#' @return
#' A data.frame with the following appended column:
#'
#'   \item {\code{COM_VISCODE2}}: Column that contains common VISCODE2 value
#'
#' @examples
#'
#' # Please see an example in
#' # \code{vignette(topic = "ADNIMERGE2-PACC-SCORE", package = "ADNIMERGE2")}.
#'
#' @seealso
#'  [create_list_object()] [get_common_value()]
#'
#' @rdname get_common_viscode2
#' @keywords adni_utils
#' @export
#' @importFrom rlang arg_match
#' @importFrom cli cli_alert_info cli_inform
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate row_number group_by across n_distinct case_when filter
#' @importFrom dplyr ungroup left_join select rename_with
#' @importFrom tidyr nest unnest
#' @importFrom assertr verify

get_common_viscode2 <- function(.data, id_cols, col_order, quiet = FALSE) {
  tmp_id <- num_unique_value <- status <- nest_data <- NULL
  list_value <- common_viscode2 <- prev_viscode2 <- NULL
  check_object_type(id_cols, "character")
  check_object_type(quiet, "logical")
  col_list <- c(id_cols, "COLPROT", "PTID", "VISCODE2", "VISDATE", "SCORE", "SCORE_SOURCE")
  col_list <- unique(col_list)
  check_colnames(
    .data = .data,
    col_names = col_list,
    strict = TRUE
  )
  score_name <- c("ADASQ4SCORE", "MMSE", "LDELTOTL", "DIGITSCR", "TRABSCOR")
  rlang::arg_match(
    arg = col_order,
    values = score_name,
    multiple = TRUE
  )
  # Checking component score names within dataset
  ext_score_name <- unique(.data$SCORE_SOURCE)
  rlang::arg_match(
    arg = ext_score_name,
    values = score_name,
    multiple = TRUE
  )

  viscode2_col <- "VISCODE2"
  nest_col <- c("SCORE_SOURCE", viscode2_col)
  non_common_col <- c("SCORE", "VISDATE", "tmp_id")
  prev_cols <- colnames(.data)
  # Temporary created vars
  temp_vars <- c(
    "tmp_id", "num_unique_value", "status", "nest_data",
    "list_value", "common_viscode2", "prev_viscode2", "COM_VISCODE2"
  )
  if (any(temp_vars %in% prev_cols)) {
    ext_tmp_col <- temp_vars[temp_vars %in% prev_cols]
    cli_alert_info(
      text = "{.val {ext_tmp_col}} columns will be overwritten."
    )
  }
  # Create inner record ids
  .data <- .data %>%
    as_tibble() %>%
    mutate(tmp_id = row_number())

  # Summarize unique number of VISCODE2 across ID cols
  tmp_long <- .data %>%
    group_by(across(all_of(id_cols))) %>%
    mutate(
      num_unique_value = n_distinct(across(all_of(viscode2_col))),
    ) %>%
    ungroup() %>%
    # Flag records with further data wrangling
    mutate(status = case_when(num_unique_value > 1 ~ "Yes"))

  num_flagged_records <- tmp_long %>%
    filter(status == "Yes") %>%
    nrow()

  if (num_flagged_records > 0) {
    if (quiet == FALSE) {
      cli_alert_info(
        text = paste0(
          "{num_flagged_records} record{?s} with multiple ",
          "{.val {'VISCODE2'}} value across {.cls {id_cols}}"
        )
      )
    }
    tmp_long_group <- tmp_long %>%
      # Only records with flags
      filter(status == "Yes") %>%
      group_by(across(all_of(id_cols))) %>%
      nest(nest_data = all_of(c(nest_col, non_common_col))) %>%
      ungroup() %>%
      assert_uniq(all_of(id_cols))

    # Create a list value per group rows
    tmp_long_group$list_value <- apply(tmp_long_group, MARGIN = 1, FUN = function(x) {
      create_list_object(
        .data = as_tibble(x[["nest_data"]]),
        col_names = nest_col
      )
    })
    # Get common VISCODE2
    tmp_long_group$common_viscode2 <- apply(tmp_long_group, MARGIN = 1, FUN = function(x) {
      get_common_value(
        list_value = x[["list_value"]],
        col_order = col_order
      )
    })

    tmp_long_group <- tmp_long_group %>%
      unnest(cols = all_of("nest_data"))

    tmp_long <- tmp_long %>%
      left_join(
        tmp_long_group %>%
          select(all_of(c(id_cols, "tmp_id", "common_viscode2", "list_value"))),
        relationship = "one-to-one",
        by = c(id_cols, "tmp_id")
      ) %>%
      verify(nrow(.) == nrow(tmp_long)) %>%
      mutate(across(all_of(viscode2_col), ~., .names = "prev_viscode2")) %>%
      mutate(
        common_viscode2 = case_when(
          is.na(common_viscode2) ~ prev_viscode2,
          .default = common_viscode2
        )
      )
  } else {
    if (quiet == FALSE) {
      cli_inform(
        message = c(
          "i" = paste0(
            "Can't find any records with multiple {.val {'VISCODE2'}} ",
            "value across {.cls {id_cols}} column{?s}. \n"
          ),
          "i" = "Not required to apply {.val {'VISCODE2'}} adjustment."
        )
      )
    }
    tmp_long <- tmp_long %>%
      mutate(across(all_of(viscode2_col), ~., .names = "prev_viscode2")) %>%
      mutate(common_viscode2 = prev_viscode2)
  }

  # Rename column
  output_data <- tmp_long %>%
    # Rename new VISCODE2 as 'COM_VISCODE2'
    rename_with(~ paste0("COM_", viscode2_col), all_of("common_viscode2"))

  # Drop temporary created columns
  output_data <- output_data %>%
    select(all_of(c(prev_cols, "COM_VISCODE2")))

  # Adjust screen fail visit code of ADNI1 phase
  output_data <- convert_f_viscode_to_sc(
    .data = output_data,
    code_var = "COM_VISCODE2"
  )

  output_data
}

# Create list object -----
#' @title Create list object
#'
#' @description
#'  This function is used to create a list object by converting two-column data frames.
#'  The list object will be named based the first column values.
#'
#' @param .data A data.frame
#' @param col_names Column names
#'
#' @return A named list object
#'
#' @details
#' Please see [tibble::deframe()] for more information.
#'
#' @examples
#'
#' # Example data
#' example_data <- tibble::tibble(
#'   SCORE_SOURCE = c("ADASQ4SCORE", "MMSE", "LDELTOTL", "DIGITSCR", "TRABSCOR"),
#'   VISCODE2 = c("bl", "sc", "bl", "bl", "bl")
#' )
#'
#' # Create a list object
#' create_list_object(
#'   .data = example_data,
#'   col_names = c("SCORE_SOURCE", "VISCODE2")
#' )
#'
#' @seealso [get_common_value()]
#'
#' @rdname create_list_object
#' @keywords pacc_score_utils_fun adni_utils
#' @importFrom rlang arg_match
#' @importFrom dplyr select
#' @importFrom tidyselect all_of
#' @importFrom tibble deframe

create_list_object <- function(.data, col_names) {
  check_object_type(.data, "data.frame")
  pre_cols <- c("SCORE_SOURCE", "VISCODE2")
  rlang::arg_match(
    arg = col_names,
    values = pre_cols,
    multiple = TRUE
  )
  list_value <- .data %>%
    dplyr::select(tidyselect::all_of(pre_cols)) %>%
    tibble::deframe()
  list_value <- as.list(list_value)
  list_value
}

# Get common value -----
#' @title Get common value
#'
#' @description
#'  This function is used to extract a common value based on names order.
#'  Please see \code{Details} section for more information.
#'
#' @param list_value A named list object
#' @param col_order An ordered column names
#'
#' @return A character vector
#'
#' @details
#'  This function is used to extract a common value from a list object based on
#'  pre-specified name order. The first non-missing value will be considered as
#'  a common value. Otherwise, a missing value \code{'NA'} will be assigned.
#'
#' @examples
#'
#' # Example data
#' col_order <- c("ADASQ4SCORE", "MMSE", "LDELTOTL", "DIGITSCR", "TRABSCOR")
#' example_data <- tibble::tibble(
#'   SCORE_SOURCE = col_order,
#'   VISCODE2 = c("bl", "sc", "bl", "bl", "bl")
#' )
#'
#' list_value <- create_list_object(
#'   .data = example_data,
#'   col_names = c("SCORE_SOURCE", "VISCODE2")
#' )
#'
#' get_common_value(
#'   list_value = list_value,
#'   col_order = col_order
#' )
#'
#' # List with missing value
#' list_value2 <- list(
#'   ADASQ4SCORE = "bl",
#'   MMSE = NA,
#'   LDELTOTL = "bl",
#'   DIGITSCR = NA,
#'   TRABSCOR = "bl"
#' )
#'
#' get_common_value(
#'   list_value = list_value2,
#'   col_order = col_order
#' )
#'
#' @seealso [create_list_object()]
#' @rdname get_common_value
#' @keywords pacc_score_utils_fun utils
#' @importFrom cli cli_abort

get_common_value <- function(list_value, col_order) {
  check_list_names(obj = list_value)
  if (length(list_value) > length(col_order)) {
    cli_abort(
      message = c(
        "x" = "Found more values in {.var list_value} than {.var col_order} argument.\n",
        "i" = "Length of {.var col_order}: {.val length(col_order)}\n",
        "i" = "Length of {.var list_value}: {.val length(list_value)}"
      )
    )
  }
  ext_col_order <- col_order[col_order %in% names(list_value)]
  if (length(ext_col_order) == 0) {
    non_ext_col <- col_order[!col_order %in% names(list_value)]
    cli_abort(
      message = c(
        "x" = "Can't get at least one col in the list value. \n",
        "i" = "Can't find column names {.val {non_ext_col}} in {.var list_value} \n",
        "i" = "Ordered columns: {.val {col_order}} \n",
        "i" = "Column names in list value: {.val {names(list_value)}}"
      )
    )
  }
  # Order list value based on existed col_order
  list_value <- list_value[ext_col_order]
  list_value <- list_value[!is.na(list_value)]
  # NULL value if everything is missing
  # Otherwise, the first record
  if (length(list_value) == 0) {
    result <- NA_character_
  } else {
    result <- list_value[[1]]
  }
  result
}
