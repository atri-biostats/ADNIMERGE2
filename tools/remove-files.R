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
