library(sbm)
library(ggplot2)
library(dplyr)
library(reshape2)

plot_sbm <- function(fit,ordered = F, transpose = NULL, labels = list(row = 'row',col = 'col'),
                     col_pred = "red",col_value = scales::muted("black",l=0)){
    UseMethod(".plot_sbm", fit)
}


.plot_sbm.BipartiteSBM_fit <- function(fit, ordered = F, transpose = F, labels = list(row = 'row',col = 'col'),
                     col_pred = "red",col_value = scales::muted("black",l=0)) {
  clustering <- setNames(fit$memberships, c("row", "col"))
  if(ordered){
    oRow <- order(clustering$row, decreasing = !transpose)
    oCol <- order(clustering$col, decreasing = transpose)
    uRow <- cumsum(table(clustering$row)) + 0.5
    uCol <- cumsum(table(clustering$col)) + 0.5
    tCol <- if(transpose){uRow}else{uCol}
    tCol <- tCol[-length(tCol)]
    tRow <- if(transpose){uCol}else{uRow}
    tRow <- tRow[length(tRow)] - tRow[-length(tRow)] + 0.5
    mat_exp <- fit$connectParam$mean[clustering$row,clustering$col][oRow,oCol]
    mat_pure <- fit$networkData[oRow,oCol]
  }else{
    tRow <- NULL
    tCol <- NULL
    mat_exp <- fit$connectParam$mean[clustering$row,clustering$col]
    mat_pure <- fit$networkData
  }

  plot_net <- reshape2::melt(mat_exp) %>%
    mutate(base_value  = reshape2::melt(mat_pure)$value)

  if(transpose) {names(plot_net)[c(1,2)] <- c('Var2','Var1')}

  plt <- ggplot(data = plot_net, aes(x = Var2, y = Var1, fill = base_value, alpha = base_value)) +
    geom_tile(aes(x = Var2, y = Var1, alpha = value), fill = col_pred, size = 0, show.legend = FALSE) +
    geom_tile(show.legend = FALSE) +
    geom_hline(yintercept = tRow,
               col = col_pred, size = .3) +
    geom_vline(xintercept = tCol,
               col = col_pred, size = .3) +
    scale_fill_gradient(low = scales::muted("white"), high = col_value,
                        guide = "colourbar") +
    ylab(if(transpose){labels$col}else{labels$row}) + xlab(if(transpose){labels$row}else{labels$col}) +
    scale_alpha(range = c(0,1)) +
    scale_x_discrete(breaks = "")+
    scale_y_discrete(breaks = "",guide = guide_axis(angle = 0)) +
    coord_equal(expand = FALSE) +
    theme_bw(base_size = 20, base_rect_size = 1, base_line_size  = 1) +
    theme(axis.ticks =  element_blank())
  plot(plt)
}


.plot_sbm.SimpleSBM_fit <- function(fit, ordered = F, transpose = NULL, labels = list(row = 'row',col = 'col'),
                                      col_pred = "red",col_value = scales::muted("black",l=0)) {

}

data_bi <- fungusTreeNetwork$fungus_tree
data_uni <- fungusTreeNetwork$tree_tree
data_uni2 <- (fungusTreeNetwork$tree_tree<1)*1
my_sbm_bi <- estimateBipartiteSBM(data_bi)
my_sbm_uni <- estimateSimpleSBM(data_uni, model = 'poisson')
my_sbm_uni2 <- estimateSimpleSBM(data_uni2)

fit <- my_sbm_uni
ord <- T
transpose <- T
labels <- list(col = 'tree',row = 'fungus')


clustering <- list(row = fit$memberships,col = fit$memberships)
if(ordered){
  oRow <- order(clustering$row)
  oCol <- order(clustering$col)
  uRow <- cumsum(table(clustering$row)) + 0.5
  uCol <- cumsum(table(clustering$col)) + 0.5
  uCol <- uCol[-length(uCol)]
  uRow <- uRow[-length(uRow)]
  mat_exp <- fit$connectParam$mean[clustering$row,clustering$col][oRow,oCol]
  mat_pure <- fit$networkData[oRow,oCol]
}else{
  tRow <- NULL
  tCol <- NULL
  mat_exp <- fit$connectParam$mean[clustering$row,clustering$col]
  mat_pure <- fit$networkData
}

plot_net <- reshape2::melt(mat_exp) %>%
  mutate(base_value  = reshape2::melt(mat_pure)$value)


ggplot(data = plot_net, aes(x = Var2, y = Var1, fill = base_value, alpha = base_value)) +
  geom_tile(aes(x = Var2, y = Var1, alpha = value), fill = 'red', size = 0, show.legend = FALSE) +
  geom_tile(show.legend = FALSE) +
  geom_hline(yintercept = uRow,
             col = 'red', size = .3) +
  geom_vline(xintercept = uCol,
             col = 'red', size = .3) +
  scale_fill_gradient(low = scales::muted("white"), high = 'black',
                      guide = "colourbar") +
  ylab("") + xlab("") +
  scale_alpha(range = c(0,1)) +
  scale_x_discrete(breaks = "")+
  scale_y_discrete(breaks = "",guide = guide_axis(angle = 0)) +
  coord_equal(expand = FALSE) +
  theme_bw(base_size = 20, base_rect_size = 1, base_line_size  = 1) +
  theme(axis.ticks =  element_blank())



plot_sbm(my_sbm_bi, ordered = T, transpose = T,col_pred = 'deeppink3',col_value = 'darkblue')


plot_sbm(my_sbm_uni, ordered = T, transpose = F,col_pred = 'deeppink3',col_value = 'darkblue')
