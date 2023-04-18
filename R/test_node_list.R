# library(tidyverse)
my_list <- read.table(file = "../../../test nodes list/UNAMED_numerical_oriented_unipartite.csv",header = F,sep =",")
my_list2 <- read.table(file = "../../../test nodes list/numerical_oriented_unipartite.csv",header = T,sep =",")
my_list3 <- read.table(file = "../../../test nodes list/oriented_unipartite.csv",header = T,sep =",")

edges_to_adjacency <- function(my_list, type = c('unipartite',"bipartite"), oriented = T){
  ## Rename columns of the pair of node list by 'from', 'to' and 'value' (if needed)
  if(dim(my_list)[2] == 2){
    names(my_list) <- c('from',"to")
  }else if(dim(my_list)[2] == 3){
    names(my_list) <- c('from',"to","value")
  }else{
    message('my_list should be a data.frame of 2 or 3 columns')
  }

  ## According to the type of network while define differently the nodes names
  if(type[1] == 'unipartite'){
    # Unipartite case the nodes are the same for cols and rows
    name_row <- name_col <- sort(unique(c(my_list$from,my_list$to)))
  }else if(type[1] == 'bipartite'){
    # bipartite differnt names for from/rows and to:cols
    name_row <- sort(unique(my_list$from))
    name_col <- sort(unique(my_list$to))
  }else{
    stop("type should be 'bipartite' or 'unipartite'")
  }

  # Empty named matrix
  mat <- matrix(0,length(name_row),length(name_col))
  rownames(mat) <- name_row
  colnames(mat) <- name_col
  # Changing from and to colums with positions into the matrix
  my_list <-  as.matrix(dplyr::mutate(my_list,
                                      from = match(from,name_row),
                                      to = match(to,name_col)))
  # Set values in right positions
  mat[my_list[,1:2]] <- ifelse(rep(dim(my_list)[2] == 2,dim(my_list)[1]),
                               1,my_list[,3])
  # Specific case of unipartite network with symmetrical connections,
  # just doing the same but reversing to and from columns
  if(type[1] == 'unipartite' & !oriented){
    mat[my_list[,2:1]] <- ifelse(rep(dim(my_list)[2] == 2,dim(my_list)[1]),
                                 1,my_list[,3])
  }
  return(as.data.frame(mat))
}

test <- my_list3
edges_to_adjacency(test,type = "unipartite",oriented = F)
edges_to_adjacency(test,type = "unipartite",oriented = T)
edges_to_adjacency(test,type = "bipartite")


# Good for bipartite network
# reshape2::acast(my_list2,From~To,fill = 0,drop = T)
# reshape2::acast(my_list3,From~To,fill = 0,drop = T)

# tel <- igraph::graph.data.frame(my_list)
# igraph::get.adjacency(tel,sparse=FALSE,edges = Value)
#
# tidyr::pivot_wider(my_list2,names_from = 'To',values_from = "Value",values_fill =0,id_expand = T)
