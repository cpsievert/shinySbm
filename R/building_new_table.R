library(shinySbm)
myNetworkMatrix <- fungusTreeNetwork$fungus_tree %>%
  `colnames<-`(fungusTreeNetwork$tree_names) %>%
  `rownames<-`(fungusTreeNetwork$fungus_names)

sbmMat <- fungusTreeNetwork$fungus_tree %>%
  `colnames<-`(fungusTreeNetwork$tree_names) %>%
  `rownames<-`(fungusTreeNetwork$fungus_names) %>%
  as.data.frame() %>%
  shinySbm:::buildSbmMatrix()

mySbmModel <- estimateBipartiteSBM(netMat = sbmMat$matrix, model = 'bernoulli',
                                   estimOptions = list(plot = T, verbosity = 3))


### still need to make table and build them as dataframe as doing this for simple sbm
get_datatable.BipartiteSBM <- function(model,sbmMat, type = c('raw','ordered','predicted')){
  if(type[[1]] == 'raw'){
    return(sbmMat$matrix)
  }else{
    clustering <- setNames(mySbmModel$memberships, c("row", "col"))
    oRow <- order(clustering$row, decreasing = T)
    oCol <- order(clustering$col, decreasing = F)
    uRow <- cumsum(table(clustering$row)) + 0.5
    uCol <- cumsum(table(clustering$col)) + 0.5
    if(type[[1]] == 'ordered'){
      return(mySbmModel$networkData[oRow, oCol])
    }else{
      return(mySbmModel$connectParam$mean[clustering$row, clustering$col][oRow, oCol])
    }
  }
}








