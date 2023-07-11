### --------- Packages ---------------------------------------------------------
require(sbm)
require(magrittr)
require(shinySbm)

### --------- Renv -------------------------------------------------------------


### --------- Nodes Names ------------------------------------------------------

fungus_names <- as.character(sbm::fungusTreeNetwork$fungus_names)
tree_names <- as.character(sbm::fungusTreeNetwork$tree_names)

### --------- Applying SBM -----------------------------------------------------

sbm_tree <- sbm::estimateSimpleSBM(sbm::fungusTreeNetwork$tree_tree,
  model = "poisson",
  dimLabels = c("Tree")
)

sbm_fungus <- sbm::estimateBipartiteSBM(sbm::fungusTreeNetwork$fungus_tree,
  model = "bernoulli",
  dimLabels = c(
    row = "Fungus",
    col = "Tree"
  )
)

### --------- Building Graphs (edges list) -------------------------------------

get_graph_tree <- function(x = sbm::fungusTreeNetwork$tree_tree,
                           labels = "tree",
                           node_names = as.character(sbm::fungusTreeNetwork$tree_names)) {
  nodes <- data.frame(
    id = 1:length(node_names),
    label = node_names
  )

  # Edges table
  edges <- data.frame(
    # edges start from
    from = sapply(nodes$id, function(i) { # for each block nb
      rep(i, each = length(node_names) - i + 1) # connection with the ones it is not yet connected
    }) %>% unlist(),
    # edges end to
    to = sapply(nodes$id, function(i) { # for each block nb
      i:length(node_names) # connection with the ones it is not yet connected
    }) %>% unlist()
  ) %>%
    dplyr::mutate(value = apply(., 1, function(i) {
      x[i[1], i[2]] # get connection values
    }) %>% unlist())

  ## Add labels
  nodes$id <- paste0(labels, "_", nodes$id)
  edges$from <- paste0(labels, "_", edges$from)
  edges$to <- paste0(labels, "_", edges$to)

  return(list(nodes = nodes, edges = edges, type = "unipartite"))
}

get_graph_fungus <- function(x = sbm::fungusTreeNetwork$fungus_tree,
                             labels = c(row = "fungi", col = "tree"),
                             node_names = list(
                               row = as.character(sbm::fungusTreeNetwork$fungus_names),
                               col = as.character(sbm::fungusTreeNetwork$tree_names)
                             )) {
  margins <- c(nrow, ncol)
  nodes <- purrr::map_dfr(c(1, 2), function(i) {
    data.frame(
      id = paste0(labels[[i]], "_", 1:margins[[i]](x)),
      type = labels[[i]],
      label = node_names[[i]]
    )
  })
  edges <- data.frame(
    from = rep(nodes$id[nodes$type == "fungi"], ncol(x)),
    to = rep(nodes$id[nodes$type == "tree"], each = nrow(x)),
    value = as.vector(x)
  )
  return(list(nodes = nodes, edges = edges, type = "bipartite"))
}

## Check if same
tree_tree_net <- get_graph_tree()
identical(tree_tree_net, shinySbm::FungusTreeNetwork$networks$tree_tree)
fungus_tree_net <- get_graph_fungus()
identical(fungus_tree_net, shinySbm::FungusTreeNetwork$networks$fungus_tree)


### --------- Gathering Objects ------------------------------------------------

FungusTreeNetwork <- list(
  networks = list(
    fungus_names = fungus_names,
    tree_names = tree_names,
    tree_tree = tree_tree_net,
    fungus_tree = fungus_tree_net
  ),
  sbmResults = list(
    tree_tree = sbm_tree,
    fungus_tree = sbm_fungus
  )
)

save(FungusTreeNetwork,file = "data/FungusTreeNetwork.rda")
