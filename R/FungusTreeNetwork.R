#' FungusTreeNetwork
#'
#' @description
#' fungus-tree interaction network
#'
#' This data set provides information about $154$ fungi sampled on $51$ tree species.
#' Composed of nodes and edges lists build based on `sbm` data package.
#'
#' @usage FungusTreeNetwork
#'
#' @format
#' A list of the following entries :
#'
#' \describe{
#'   \item{tree_tree}{
#'   \enumerate{
#'      \item{nodes : data.frame describing nodes of tree_tree network}
#'      \item{edges : data.frame describing edges of tree_tree network}
#'      \item{type : this network is "unipartite"}
#'    }
#'   }
#'   \item{fungus_tree}{
#'   \enumerate{
#'      \item{nodes : data.frame describing nodes of fungus_tree network}
#'      \item{edges : data.frame describing edges of fungus_tree network}
#'      \item{type : this network is "bipartite"}
#'    }
#'   }
#' }
#' @source Vacher, Corinne, Dominique Piou, and Marie-Laure Desprez-Loustau. "Architecture of an antagonistic tree/fungus network: the asymmetric influence of past evolutionary history." PloS one 3.3 (2008): e1740.
"FungusTreeNetwork"
