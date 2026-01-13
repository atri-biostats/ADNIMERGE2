# Removing files from a specific directory
# Libraries ----
library(cli)

# Input argument ----
args <- commandArgs(trailingOnly = TRUE)
source(file.path(".", "tools", "data-prepare-utils.R"))
check_arg(args, 3)
INPUT_DIR <- args[1]
OUTPUT_DIR <- args[2]
PATTERN <- args[3]
soure_file_list <- list.files(
  path = INPUT_DIR,
  pattern = PATTERN,
  full.names = TRUE,
  recursive = FALSE
)
SOURCE_FILE_STATUS <- any(!is.na(soure_file_list))
if (SOURCE_FILE_STATUS == TRUE) {
  # Copy files to `output` directory ----
  file_action(
    input_dir = INPUT_DIR,
    output_dir = OUTPUT_DIR,
    file_extension = PATTERN,
    action = "copy",
    show_message = TRUE
  )
  # Remove files from previous directory ------
  file_action(
    input_dir = INPUT_DIR,
    output_dir = OUTPUT_DIR,
    file_extension = PATTERN,
    action = "remove",
    show_message = TRUE
  )
  soure_file_list <- soure_file_list[!is.na(soure_file_list)]
  cli::cli_inform(
    c(
      "i" = paste0(
        "{.val {basename(soure_file_list)}} file{?s} {?is/are} transferred",
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