library(sbm)
library(ggplot2)
library(dplyr)



data <- fungusTreeNetwork$fungus_tree

my_sbm <- estimateBipartiteSBM(data)

my_sbm$dimLabels <- c(row = "fungus", col = 'tree')
my_sbm$dimLabels

clustering <- setNames(my_sbm$memberships, c('row', 'col'))
mat <- my_sbm$networkData

clustering <- `names<-`(rev(clustering),c('row', 'col'))
mat <- t(my_sbm$networkData)

plotMyMatrix(mat,
           my_sbm$dimLabels,
           clustering)

plotMyMatrix(my_sbm$expectation,
           my_sbm$dimLabels,
           clustering)

plot(my_sbm)
