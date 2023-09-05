## To launch to achieve a CRAN submission

## Checks before :

devtools::build(args = c('--resave-data'),vignettes = TRUE)

dir_filtered <- grep("^shinySbm_.*.tar.gz$",dir(".."),value = T)
last_package <- paste0("../",dir_filtered[length(dir_filtered)])

devtools::check_built(last_package)
install.packages(last_package)


## Send to CRAN
devtools::submit_cran(args = c('--resave-data'))
3
