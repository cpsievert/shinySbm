FROM ubuntu:18.04

# Update
RUN apt update && apt upgrade -y

# Configuration tzdata
RUN export DEBIAN_FRONTEND=noninteractive
RUN apt install tzdata -y
RUN ln -fs /usr/share/zoneinfo/Europe/Paris /etc/localtime
RUN dpkg-reconfigure --frontend noninteractive tzdata

# Installation de R et des dépendances
RUN apt-get install git software-properties-common curl libcurl4-openssl-dev libssl-dev ca-certificates wget -y
RUN gpg --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
RUN gpg -a --export E298A3A825C0D65DFD57CBB651716619E084DAB9 | apt-key add -

RUN add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu bionic-cran35/'
RUN apt install r-base -y


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
CMD R -e "options('shiny.port'=38,shiny.host='0.0.0.0');library(shinySbm);shinySbm::run_app()"


