library(igraph)
library(tibble)

#' Helper function to get the degree distribution of the graph
#' @param graph  - an igraph graph
#' @returns a tibble
#' @export
degree_distribution_summary <- function(graph) {
    tibble(
        "Degree" = 0:max(degree(graph), na.rm = TRUE),
        "Degree dist." = degree_distribution(graph, mode = "all")
    )
}
