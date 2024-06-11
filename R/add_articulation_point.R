#' Adds articulation point to an igraph graph
#' Returns a graph
#' @export
add_articulation_point <- function(graph) {
  igraph::V(graph)[igraph::articulation_points(graph)]$articulation_point <- "*"
  igraph::V(graph)$articulation_point[is.na(igraph::V(graph)$articulation_point)] <- ""
  return(graph)
}
