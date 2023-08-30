# create 19 breaks and 20 rgb color values ranging from white to red
brks <- quantile(df, probs = seq(.05, .95, .05), na.rm = TRUE)
clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
  {paste0("rgb(255,", ., ",", ., ")")}
datatable(df) %>% formatStyle(names(df), backgroundColor = styleInterval(brks, clrs))

get_datatable <- function(model,matrix, type = c('raw','ordered','predicted')){
  UseMethod("get_datatable",model)
}

get_datatable.default <- function(model,matrix, type = c('raw','ordered','predicted')){
  return(NULL)
}

### still need to make table and build them as dataframe
get_datatable.BipartiteSBM <- function(model,matrix, type = c('raw','ordered','predicted')){
  if(type[[1]] == 'raw'){
    return(shinySbm:::as.data.frame.sbmMatrix(matrix))
  }else{
    clustering <- setNames(model$memberships, c("row", "col"))
    oRow <- order(clustering$row, decreasing = TRUE)
    oCol <- order(clustering$col)
    uRow <- cumsum(table(clustering$row)) + 0.5
    uCol <- cumsum(table(clustering$col)) + 0.5
    if(type[[1]] == 'ordered'){
      oMat <- model$networkData[oRow, oCol] %>%
        `colnames<-`(matrix$nodes_names$col[oCol]) %>%
        `rownames<-`(matrix$nodes_names$row[oRow]) %>%
        as.data.frame()
      return(oMat)
    }else{
      pMat <- model$connectParam$mean[clustering$row, clustering$col][oRow, oCol] %>%
        `colnames<-`(matrix$nodes_names$col[oCol]) %>%
        `rownames<-`(matrix$nodes_names$row[oRow]) %>%
        as.data.frame()
      return(pMat)
    }
  }
}


get_datatable.SimpleSBM <- function(model,matrix, type = c('raw','ordered','predicted')){
  if(type[[1]] == 'raw'){
    return(shinySbm:::as.data.frame.sbmMatrix(matrix))
  }else{
    clustering <- list(row = model$memberships, col = model$memberships)
    if (ordered) {
      oRow <- order(clustering$row)
      uRow <- cumsum(table(clustering$row)) + 0.5
      uCol <- uRow[-length(uRow)]
      uRow <- uRow[length(uRow)] - uRow[-length(uRow)] + 0.5
      nb_rows <- dim(model$networkData)[1]
      if(type[[1]] == 'ordered'){
        oMat <- model$networkData[oRow, oRow][nb_rows:1, ] %>%
          `colnames<-`(matrix$nodes_names[oRow]) %>%
          `rownames<-`(matrix$nodes_names[oRow][nb_rows:1]) %>%
          as.data.frame()
        return(oMat)
      }else{
        pMat <-  model$connectParam$mean[clustering$row, clustering$col][oRow, oRow][nb_rows:1, ] %>%
          `colnames<-`(matrix$nodes_names[oRow]) %>%
          `rownames<-`(matrix$nodes_names[oRow][nb_rows:1]) %>%
          as.data.frame()
        return(pMat)
      }
    }
  }
}



library(shinySbm)

binetmat <- fungusTreeNetwork$fungus_tree %>%
  `colnames<-`(fungusTreeNetwork$tree_names) %>%
  `rownames<-`(fungusTreeNetwork$fungus_names)

bisbmmat <- binetmat %>%
  as.data.frame() %>%
  shinySbm:::buildSbmMatrix()

bisbm <- estimateBipartiteSBM(netMat = bisbmmat$matrix, model = 'bernoulli',
                              estimOptions = list(plot = T, verbosity = 3))




uninetmat <- fungusTreeNetwork$tree_tree %>%
  `colnames<-`(fungusTreeNetwork$tree_names) %>%
  `rownames<-`(fungusTreeNetwork$tree_names)

unisbmmat <- uninetmat %>%
  as.data.frame() %>%
  shinySbm:::buildSbmMatrix()

unisbm <- estimateBipartiteSBM(netMat = unisbmmat$matrix, model = 'poisson',
                               estimOptions = list(plot = T, verbosity = 3))



get_datatable(bisbm,bisbmmat, type = c('raw','ordered','predicted')[1]) %>% as_big_table()

get_datatable(unisbm,unisbmmat, type = c('raw','ordered','predicted')[2]) %>% View



as_big_table <- function(data){
  DT::datatable(
    as.data.frame(data),
    extensions = c("FixedColumns", "FixedHeader", "KeyTable"),
    option = list(
      fixedHeader = TRUE,
      fixedColumns = list(leftColumns = 1),
      scrollX = T,
      scrollY = T,
      keys = TRUE,
      paging = F
    )
  ) %>%
    DT::formatStyle(c(1:ncol(data)), border = "1px solid #ddd")
}





