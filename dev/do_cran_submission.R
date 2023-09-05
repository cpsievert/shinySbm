rm(list = ls(all.names = T))

source("dev/fct_cran_submission.R")

## To launch to achieve a CRAN submission

## Checks before :
check_built()
install_built(build = FALSE, delete = TRUE)


## Send to CRAN :
submit_package()

