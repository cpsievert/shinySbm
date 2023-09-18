FROM registry.forgemia.inra.fr/theodore.vanrenterghem/shinysbm-docker-base:latest

RUN R -e 'install.packages("shinySbm")'

# Run the application
EXPOSE 3838
CMD R -e "options('shiny.port'=3838,shiny.host='0.0.0.0');library(shinySbm);shinySbm::shinySbmApp(nbCore_control = FALSE, console_verbosity = FALSE,options = list(launch.browser = F))"


