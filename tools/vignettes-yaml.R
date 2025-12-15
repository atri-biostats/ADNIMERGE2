# Changing the default params of INCLUDE_PACC in vignettes -----
## This allows changing a rmarkdown yaml using a script rather a manual update

# Vignettes files ----
vignette_dir <- file.path(".", "vignettes")
rmd_file_list <- list.files(
  path = vignette_dir,
  pattern = "\\.Rmd$",
  full.names = TRUE,
  all.files = TRUE,
  recursive = FALSE
)

# Change the default yaml values of params$INCLUDE_PACC
default_params <- " INCLUDE_PACC: true| INCLUDE_PACC: TRUE"
update_params <- " INCLUDE_PACC: false"

for (i in seq_along(rmd_file_list)) {
  status <- xfun::gsub_file(rmd_file_list[i], default_params, update_params, fixed = TRUE)
  if (!is.null(status)) {
    cli::cli_alert_info(text = c(
      "Default params in {.path {rmd_file_list[i]}} is changed \n",
      "{.field {default_params}} -> {.field {update_params}}"
    ))
  } else {
    cli::cli_alert_info(text = "{.path {rmd_file_list[i]}} is not changed!")
  }
}
