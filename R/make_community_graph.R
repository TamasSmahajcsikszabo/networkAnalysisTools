library(igraph)


#' Makes qgraph graph with communities 
#' @param graph An igraph graph
#' @param covMatrix Covariance matrix of the underlying parameters
#' @param labels string vector of node labels
#' @returns qraph graph
#' @export

make_community_graph <- function(graph, covMatrix, labels) {
    igraph_converted <- as.igraph(graph, attributes = TRUE)
    group_estimation <- cluster_spinglass(
        igraph_converted,
        weights = NULL,
        vertex = NULL,
        spins = 25,
        parupdate = FALSE,
        start.temp = 1,
        stop.temp = 0.01,
        cool.fact = 0.99,
        update.rule = c("config", "random", "simple"),
        gamma = 0.5,
        implementation = c("orig", "neg"),
        gamma.minus = 1
    )
    grouping_order <- data.frame(id = group_estimation$membership) %>%
        left_join(global_colors) %>%
        select(color) %>%
        unlist()
    labels <- get_item_names(covMatrix, labels)
    qgraph(covMatrix, graph = "glasso", tuning = 0.5, layout = "spring", sampleSize = 888, theme = "TeamFortress", details = TRUE, threshold = FALSE, color = grouping_order, labels = labels)
}

