# Removing files from a specific directory
# Libraries ----
library(cli)

# Input argument ----
arg_list <- commandArgs(trailingOnly = TRUE)
source(file.path(".", "tools", "data-prepare-utils.R"))
source(file.path(".", "R", "checks-assert.R"))
check_arg(x = arg_list, size = 3)
INPUT_DIR <- arg_list[1]
OUTPUT_DIR <- arg_list[2]
PATTERN <- arg_list[3]
source_file_list <- list.files(
  path = INPUT_DIR,
  pattern = PATTERN,
  full.names = TRUE,
  recursive = FALSE
)
SOURCE_FILE_STATUS <- any(!is.na(source_file_list))
if (SOURCE_FILE_STATUS == TRUE) {
  # Copy files to `output` directory ----
  copy_status <- file_action(
    input_dir = INPUT_DIR,
    output_dir = OUTPUT_DIR,
    file_extension = PATTERN,
    action = "copy",
    show_message = TRUE
  )
  # Only remove the source files if all files were copied
  if (!isTRUE(copy_status)) {
    cli::cli_abort(
      message = "Failed to copy files from {.path {INPUT_DIR}} to {.path {OUTPUT_DIR}}. No file is removed."
    )
  }
  # Remove files from previous directory ------
  remove_status <- file_action(
    input_dir = INPUT_DIR,
    output_dir = OUTPUT_DIR,
    file_extension = PATTERN,
    action = "remove",
    show_message = TRUE
  )
  if (!isTRUE(remove_status)) {
    cli::cli_abort(message = "Failed to remove files from {.path {INPUT_DIR}}.")
  }
  source_file_list <- source_file_list[!is.na(source_file_list)]
  cli::cli_inform(
    c(
      "i" = paste0(
        "{.val {basename(source_file_list)}} file{?s} {?is/are} transferred",
        " from {.path {INPUT_DIR}} to {.path {OUTPUT_DIR}}"
      )
    )
  )
} else {
  cli::cli_inform(
    c(
      "i" = "No file with {.val {PATTERN}} pattern exists in {.path {INPUT_DIR}}. \n",
      "!" = paste0(
        "No file {.path {PATTERN}} with pattern is transferred from ",
        "{.path {INPUT_DIR}} to {.path {OUTPUT_DIR}}."
      )
    )
  )
}
