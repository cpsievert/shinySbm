## Script qui recompile le package si la version n'est pas à jour

package_name <- "shinySbm"

packages <- installed.packages(fields = c("Package", "Version"))

local_version <- packages[packages[, 1] == package_name, "Version"]
shared_version <- gsub(
  "Version: ", "",
  grep("Version:",
       readLines(paste0(package_name,"/DESCRIPTION")),
       value = T
  )
)
fn <- character()

if (length(local_version) == 0 || local_version != shared_version) {
  fn <- paste0(package_name,"_", c(local_version, shared_version), ".tar.gz")
  roxygen2::roxygenise(paste0(package_name,"/"))
  devtools::document(paste0(package_name,"/"))
  devtools::check(paste0(package_name,"/"), document = FALSE)
  devtools::build(paste0(package_name,"/"))
  unloadNamespace(package_name)
  install.packages(fn[[length(fn)]])
} else {
  message(paste("package",package_name,"already up to date"))
}

invisible(purrr::map(
  fn,
  ~ if (file.exists(.x)) {
    # Delete package file if it exists
    file.remove(.x)
  }
))

rm(packages, local_version, shared_version, fn,package_name)

