FROM rocker/verse:4.3.0
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(renv.config.pak.enabled = TRUE, repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site

RUN R -e 'install.packages("config")'
RUN R -e 'install.packages("data.table")'
RUN R -e 'install.packages("dplyr")'
RUN R -e 'install.packages("DT")'
RUN R -e 'install.packages("fresh")'
RUN R -e 'install.packages("ggplot2")'
RUN R -e 'install.packages("golem")'
RUN R -e 'install.packages("igraph")'
RUN R -e 'install.packages("magrittr")'
RUN R -e 'install.packages("purrr")'
RUN R -e 'install.packages("reshape2")'
RUN R -e 'install.packages("sbm")'
RUN R -e 'install.packages("shiny")'
RUN R -e 'install.packages("shinyalert")'
RUN R -e 'install.packages("shinydashboard")'
RUN R -e 'install.packages("stringr")'
RUN R -e 'install.packages("visNetwork")'
RUN R -e 'install.packages("remotes")'
RUN R -e 'remotes::install_github("Jo-Theo/shinySbm")'

# Run the application
EXPOSE 38
CMD R -e "options('shiny.port'=38,shiny.host='0.0.0.0');library(shinySbm);shinySbm::run_app(options = list(launch.browser = F))"


