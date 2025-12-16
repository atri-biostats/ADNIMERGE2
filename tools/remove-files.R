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
input_dir <- args[1]
output_dir <- args[2]
file_extension <- args[3]
source(file.path(".", "tools", "data-prepare-utils.R"))
library(cli)

# Copy files to `output` directory ----
file_action(
  input_dir = input_dir,
  output_dir = output_dir,
  file_extension = file_extension,
  action = "copy", 
  show_message = FALSE
)

# Remove files from previous directory ------
file_action(
  input_dir = input_dir,
  output_dir = output_dir,
  file_extension = file_extension,
  action = "remove", 
  show_message = TRUE
)
