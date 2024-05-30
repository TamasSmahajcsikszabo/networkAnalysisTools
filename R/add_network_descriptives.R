require("igraph")

#' Add network descriptives to an igraph graph
#' Returns string of descriptives
#' @export
add_network_descriptives <- function(graph,
                                     order = "Order=",
                                     size = "Size=",
                                     clustering = "Clustering Coeffiecient=",
                                     diameter = "Diameter=",
                                     mean_distance = "Mean distance=") {
  o <- paste0(order, vcount(graph))
  s <- paste0(size, ecount(graph))
  t <- paste0(clustering, round(transitivity(graph), 3))
  d <- paste0(diameter, round(diameter(graph ,3)))
  md <- paste0(mean_distance, round(mean_distance(graph), 3))
  return(o,"; ", s, "; ", t, "; ", d, "; ", md)
}
