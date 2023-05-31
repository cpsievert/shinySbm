FROM shinysbm_base
COPY renv.lock.prod renv.lock
RUN R -e 'renv::restore()'
COPY shinySbm_*.tar.gz /app.tar.gz
RUN R -e 'remotes::install_local("/app.tar.gz",upgrade="never")'
RUN rm /app.tar.gz
EXPOSE 38
CMD R -e "options('shiny.port'=38,shiny.host='0.0.0.0');library(shinySbm);shinySbm::run_app()"
