FROM rocker/verse:4.3.0
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(renv.config.pak.enabled = TRUE, repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site

RUN R -e 'install.packages(c("colourpicker","config","data.table","dplyr","DT","flextable","fresh","ggplot2","golem","magrittr","patchwork","purrr","rmarkdown","sbm","shiny","shinyalert","shinydashboard","stringr","visNetwork"))'
RUN tlmgr update --all --self && tlmgr install amsmath latex-amsmath-dev texlive-scripts kvoptions ltxcmds kvsetkeys etoolbox xcolor geometry booktabs mdwtools auxhook bigintcalc bitset etexcmds gettitlestring hycolor hyperref intcalc kvdefinekeys letltxmacro pdfescape refcount rerunfilecheck stringenc uniquecounter zapfding pdftexcmds infwarerr epstopdf-pkg ec colortbl wrapfig float multirow
RUN R -e 'remotes::install_github("Jo-Theo/shinySbm",upgrade = "never")'

# Run the application
EXPOSE 3838
CMD R -e "options('shiny.port'=3838,shiny.host='0.0.0.0');library(shinySbm);shinySbm::run_app(options = list(launch.browser = F))"


