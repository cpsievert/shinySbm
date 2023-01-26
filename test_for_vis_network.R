
rm(list  =ls())
source(file = 'R/fct_specific_plots.R')


data_uni <- sbm::fungusTreeNetwork$tree_tree
my_sbm_uni <- sbm::estimateSimpleSBM(data_uni,model = 'poisson')
data_bi <- sbm::fungusTreeNetwork$fungus_tree
my_sbm_bi <- sbm::estimateBipartiteSBM(data_bi,model = 'bernoulli')

res_for_vis <- build_node_edge(my_sbm_uni)
netPlot(res_for_vis$nodes, res_for_vis$edges, res_for_vis$type)

res_for_vis <- build_node_edge(my_sbm_uni, oriented = T)
netPlot(res_for_vis$nodes, res_for_vis$edges, res_for_vis$type)

res_for_vis <- build_node_edge(my_sbm_bi)
netPlot(res_for_vis$nodes, res_for_vis$edges, res_for_vis$type)

my_sbm_uni$blockProp
my_sbm_uni$connectParam$mean
my_sbm_uni$entropy
my_sbm_uni$expectation
my_sbm_uni$memberships

my_sbm_uni$memberships
my_sbm_uni$probMemberships
my_sbm_bi$memberships
my_sbm_bi$probMemberships



list_group <- sapply(1:my_sbm_uni$nbBlocks,function(i){
  indices <- my_sbm_uni$indMemberships[,i] == 1
  sbm::fungusTreeNetwork$tree_names[indices]
  })

list_group <- lapply(c('row','col'),function(margin){
  names <- sapply(1:as.list(my_sbm_bi$nbBlocks)[[margin]],function(i){
    indices <- my_sbm_bi$indMemberships[[margin]][,i] == 1
    if(margin == 'col'){
      name <- sbm::fungusTreeNetwork$tree_names[indices]
    }else{
      name <- sbm::fungusTreeNetwork$fungus_names[indices]
    }
    return(name)
  })
})
list_group

my_sbm_bi$indMemberships
