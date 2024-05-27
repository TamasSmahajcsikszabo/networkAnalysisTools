require("igraph")

#' Adds articulation point to an igraph graph
#' Returns a graph
add_articulation_point <- function(graph) {
  V(graph)[articulation_points(graph)]$articulation_point <- "*"
  V(graph)$articulation_point[is.na(V(graph)$articulation_point)] <- ""
  return(graph)
}
