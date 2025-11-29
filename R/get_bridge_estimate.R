#' Gets bridge influence statistics
#' @param network igraph graph
#' @param dec - integer - decimal count
#' @param seed random seed
#' @param methid string name of method (spinglass, fast_greedy, walktrap, optimal)
#' @param weights boolen - if TRUE extracts the weight of the network and uses it in community detection
#' @returns character vector
#' @export
get_bridge_estimate <- function(network, dec = 2, seed = 1234, method = "fast_greedy", weights = TRUE) {
    if (!is.null(seed)) {
        set.seed(seed)
    }
    if (weights) {
        W <- E(network)$weight
    } else {
        W <- NULL
    }
    if (method == "spinglass") {
        group_estimation <- cluster_spinglass(
            network,
            weights = W,
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
    } else if (method == "fast_greedy") {
        group_estimation <- cluster_fast_greedy(
            network,
            weights = W
        )
    } else if (method == "walktrap") {
        group_estimation <- cluster_walktrap(network,
            weights = W
        )
    } else if (method == "optimal") {
        group_estimation <- cluster_optimal(network,
            weights = W
        )
    }
    if (length(unique(group_estimation$membership)) > 1) {
        bridge_estimate <- networktools::bridge(network, communities = group_estimation$membership)
    } else {
        bridge_estimate <- networktools::bridge(network)
    }

    itemrownames <- names(bridge_estimate[1][[1]])

    # standardize scores into a tibble
    output_tibble <- tibble(.rows = length(bridge_estimate[[1]]))

    for (i in seq(1, 5)) {
        target_vector <- tibble(round(standardize(bridge_estimate[[i]]), dec))
        colnames(target_vector) <- names(bridge_estimate)[i]
        output_tibble <- bind_cols(output_tibble, target_vector)
    }
    # add communities
    output_tibble <- bind_cols(output_tibble, bridge_estimate[6])
    output_tibble <- output_tibble[, names(bridge_estimate)]
    output_tibble <- output_tibble %>%
        mutate(Community = as_factor(communities)) %>%
        dplyr::select(-communities)
    output_tibble
}
