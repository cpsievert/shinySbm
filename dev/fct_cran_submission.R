
build_package <- function(vignettes = TRUE, resave_data = TRUE){
  if(resave_data){
    arg <- c('--resave-data')
  }else{
    arg <- NULL
  }
  devtools::build(args = arg, vignettes = vignettes)
}

get_tar_name <- function(){
  pack <- 'Package: '
  ver <- 'Version: '
  file <- readLines("DESCRIPTION")
  package_name <- gsub(pack,'', grep(pack, file, value = T))
  version <- gsub(ver,'',grep(ver, file, value = T))
  last_package <- paste0(package_name,"_",version,".tar.gz")
  return(data.frame(package = package_name, source = last_package))
}

delete_package <- function(version = 'current'){
  package <- get_tar_name()
  package_name <- package$package
  if(version[[1]] == 'current'){
    version <- paste0(package$source)
  }else{
    version <- paste0(package_name,"_",version,".tag.gz")
  }
  version <- version[version %in% dir('..')]
  if(length(version) > 0){
    file.remove(paste0("../",version))
  }else{
    warning("Can't find any ", package_name, " corresponding versions in parent directory")
  }
}

check_built <- function(build = TRUE, delete = FALSE){
  if(delete){
    on.exit(delete_package())
  }
  if(build){
    build_package()
  }
  package <- get_tar_name()
  package$source
  on.exit(unloadNamespace(package$package),add = TRUE)

  if(package$source %in% dir("..",recursive = T)){
    loc_source <- paste0("../",package$source)
    unloadNamespace(package$package)
    devtools::check_built(loc_source)
  }else{
    warning("Can't find ", package$source, " in parent directory try build or check description file")
  }
}

install_built <- function(build = TRUE, delete = FALSE){
  if(delete){
    on.exit(delete_package())
  }
  if(build){
    build_package()
  }
  package <- get_tar_name()
  package$source
  on.exit(unloadNamespace(package$package),add = TRUE)

  if(package$source %in% dir("..",recursive = T)){
    loc_source <- paste0("../",package$source)
    unloadNamespace(package$package)
    install.packages(loc_source)
  }else{
    warning("Can't find ", package$source, " in parent directory try build or check description file")
  }
}

submit_package <- function(resave_data = TRUE){
  if(resave_data){
    arg <- c('--resave-data')
  }else{
    arg <- NULL
  }
  devtools::submit_cran(args = arg)
}


