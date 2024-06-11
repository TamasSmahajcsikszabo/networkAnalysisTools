#' Gets network descriptives to an igraph graph
#' Returns string of descriptives
#' @export
get_network_descriptives <- function(graph,
                                     order = "Order=",
                                     size = "Size=",
                                     clustering = "Clustering Coeffiecient=",
                                     diameter = "Diameter=",
                                     mean_distance = "Mean distance=") {
  o <- paste0(order, igraph::vcount(graph))
  s <- paste0(size, igraph::ecount(graph))
  t <- paste0(clustering, round(igraph::transitivity(graph), 3))
  d <- paste0(diameter, round(igraph::diameter(graph ,3)))
  md <- paste0(mean_distance, round(igraph::mean_distance(graph), 3))
  return(o,"; ", s, "; ", t, "; ", d, "; ", md)
}
