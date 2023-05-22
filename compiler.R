## ---- WORKING DIR ------------------------------------------------------------
rm(list = ls())
## ---- Compile PACKAGE --------------------------------------------------------

package_name <- "shinySbm"

## Verifie la version installee
packages <- installed.packages(fields = c("Package", "Version"))
local_version <- packages[packages[, 1] == package_name, "Version"]
## Create new version name
future_version <- gsub(
  "-", ".",
  gsub(
    " ", ".",
    gsub(":", ".", Sys.time())
  )
)
## Change description file
description <- readLines("DESCRIPTION")
description[grep("Version:", description)] <- paste("Version:", future_version)
cat(description, file = "DESCRIPTION", sep = "\n")

## Set names for old and new package
fn <- paste0("../",package_name,"_", c(local_version, future_version), ".tar.gz")

## Package compilation and test
roxygen2::roxygenise()
devtools::document()
devtools::check(document = FALSE)
devtools::build()
## Package Instalation
unloadNamespace(package_name)
install.packages(fn[[length(fn)]])

invisible(purrr::map(
  fn,
  ~ if (file.exists(.x)) {
    # Delete package file if it exists
    file.remove(.x)
  }
))

rm(packages, local_version, fn, description, future_version, package_name)
