# Single files
# Input directory
# Output directory
# File name
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3) {
  cli::cli_abort(
    message = c(
      "Input argument {.val args} must be size of 3. \n",
      "{.val args} is a length of {.val {length(args)}} vector."
    )
  )
}
INPUT_DIR <- args[1]
OUTPUT_DIR <- args[2]
PATTERN <- args[3]
source(file.path(".", "tools", "data-prepare-utils.R"))
library(cli)

# Copy files to `output` directory ----
file_action(
  input_dir = INPUT_DIR,
  output_dir = OUTPUT_DIR,
  file_extension = PATTERN,
  action = "copy", 
  show_message = FALSE
)

# Remove files from previous directory ------
file_action(
  input_dir = INPUT_DIR,
  output_dir = OUTPUT_DIR,
  file_extension = PATTERN,
  action = "remove", 
  show_message = TRUE
)
