FROM rocker/verse:4.3.0
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(renv.config.pak.enabled = TRUE, repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site

RUN R -e 'install.packages(c("config","data.table","dplyr","DT","fresh","ggplot2","golem","igraph","magrittr","purrr","reshape2","sbm","shiny","shinyalert","shinydashboard","stringr","visNetwork","remotes"))'
RUN R -e 'remotes::install_github("Jo-Theo/shinySbm",upgrade = "never")'

# Run the application
EXPOSE 3838
CMD R -e "options('shiny.port'=3838,shiny.host='0.0.0.0');library(shinySbm);shinySbm::run_app(options = list(launch.browser = F))"


