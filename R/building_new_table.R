# remember to change my ref from row and col to first and scnd col


# get_datatable <- function(model,matrix, type = c('raw','ordered','predicted')){
#   UseMethod("get_datatable",model)
# }
#
# get_datatable.default <- function(model,matrix, type = c('raw','ordered','predicted')){
#   return(NULL)
# }
#
# ### still need to make table and build them as dataframe as doing this for simple sbm
# get_datatable.BipartiteSBM <- function(model,matrix, type = c('raw','ordered','predicted')){
#   if(type[[1]] == 'raw'){
#     return(shinySbm:::as.data.frame.sbmMatrix(matrix))
#   }else{
#     clustering <- setNames(model$memberships, c("row", "col"))
#     oRow <- order(clustering$row, decreasing = TRUE)
#     oCol <- order(clustering$col)
#     uRow <- cumsum(table(clustering$row)) + 0.5
#     uCol <- cumsum(table(clustering$col)) + 0.5
#     if(type[[1]] == 'ordered'){
#       oMat <- model$networkData[oRow, oCol] %>%
#         `colnames<-`(matrix$nodes_names$col[oCol]) %>%
#         `rownames<-`(matrix$nodes_names$row[oRow]) %>%
#         as.data.frame()
#       return(oMat)
#     }else{
#       pMat <- model$connectParam$mean[clustering$row, clustering$col][oRow, oCol] %>%
#         `colnames<-`(matrix$nodes_names$col[oCol]) %>%
#         `rownames<-`(matrix$nodes_names$row[oRow]) %>%
#         as.data.frame()
#       return(pMat)
#     }
#   }
# }
#
#
# get_datatable.SimpleSBM <- function(model,matrix, type = c('raw','ordered','predicted')){
#   if(type[[1]] == 'raw'){
#     return(shinySbm:::as.data.frame.sbmMatrix(matrix))
#   }else{
#     clustering <- list(row = model$memberships, col = model$memberships)
#     if (ordered) {
#       oRow <- order(clustering$row)
#       uRow <- cumsum(table(clustering$row)) + 0.5
#       uCol <- uRow[-length(uRow)]
#       uRow <- uRow[length(uRow)] - uRow[-length(uRow)] + 0.5
#       nb_rows <- dim(model$networkData)[1]
#       if(type[[1]] == 'ordered'){
#         oMat <- model$networkData[oRow, oRow][nb_rows:1, ] %>%
#           `colnames<-`(matrix$nodes_names[oRow]) %>%
#           `rownames<-`(matrix$nodes_names[oRow][nb_rows:1]) %>%
#           as.data.frame()
#         return(oMat)
#       }else{
#         pMat <-  model$connectParam$mean[clustering$row, clustering$col][oRow, oRow][nb_rows:1, ] %>%
#           `colnames<-`(matrix$nodes_names[oRow]) %>%
#           `rownames<-`(matrix$nodes_names[oRow][nb_rows:1]) %>%
#           as.data.frame()
#         return(pMat)
#       }
#     }
#   }
# }
#
#
#
# library(shinySbm)
#
# binetmat <- fungusTreeNetwork$fungus_tree %>%
#   `colnames<-`(fungusTreeNetwork$tree_names) %>%
#   `rownames<-`(fungusTreeNetwork$fungus_names)
#
# bisbmmat <- binetmat %>%
#   as.data.frame() %>%
#   shinySbm:::buildSbmMatrix()
#
# bisbm <- estimateBipartiteSBM(netMat = bisbmmat$matrix, model = 'bernoulli',
#                               estimOptions = list(plot = T, verbosity = 3))
#
#
#
#
# uninetmat <- fungusTreeNetwork$tree_tree %>%
#   `colnames<-`(fungusTreeNetwork$tree_names) %>%
#   `rownames<-`(fungusTreeNetwork$tree_names)
#
# unisbmmat <- uninetmat %>%
#   as.data.frame() %>%
#   shinySbm:::buildSbmMatrix()
#
# unisbm <- estimateBipartiteSBM(netMat = unisbmmat$matrix, model = 'poisson',
#                                estimOptions = list(plot = T, verbosity = 3))
#
#
# get_datatable(bisbm,bisbmmat, type = c('raw','ordered','predicted')[3]) %>% View
#
# get_datatable(unisbm,unisbmmat, type = c('raw','ordered','predicted')[3]) %>% View
#
#
#
#
