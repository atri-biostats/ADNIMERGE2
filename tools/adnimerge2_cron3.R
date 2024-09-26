# ADNIMERGE2 package build ----
# Set package directory
setwd("./ADNIMERGE2")
library(devtools); library(tidyverse)
load_all()
document()
check(error_on = 'error')
pkg_dir <- build(vignettes = FALSE)

# Install the package locally
install.packages(pkg_dir, repos = NULL)