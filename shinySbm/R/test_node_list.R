# my_list1 <- read.table(file = "../../../sbm_train/UNAMED_numerical_oriented_unipartite.csv",header = F,sep =",")
# my_list2 <- read.table(file = "../../../sbm_train/numerical_oriented_unipartite.csv",header = T,sep =",")
# my_list3 <- read.table(file = "../../../sbm_train/oriented_unipartite.csv",header = T,sep =",")
#
# source("R/fct_linkListMatrix.R")
# source("R/fct_sbmMatrixClass.R")
#
# my_sbm1 <- buildSbmMatrix(edges_to_adjacency(my_list1,type = "unipartite",oriented = F))
# my_sbm2 <- buildSbmMatrix(edges_to_adjacency(my_list2,type = "unipartite",oriented = T))
# my_sbm3 <- buildSbmMatrix(edges_to_adjacency(my_list3,type = "bipartite"))
#
# adjacency_to_edges <- function(my_mat, type = c('unipartite',"bipartite"), oriented = T){
#   if(type[1] == 'bipartite'){
#     ## Check if format is ok for
#
#   }else if(type[1] == 'unipartite'){
#
#   }else{
#     stop("type should be 'bipartite' or 'unipartite'")
#   }
# }
#
#
#
#
#
#
