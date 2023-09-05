rm(list = ls(all.names = T))

source("fct_cran_submission.R")

## To launch to achieve a CRAN submission

## Checks before :
check_built()
install_built(build = FALSE, delete = TRUE)
delete_package()


## Send to CRAN :
submit_package()
111


