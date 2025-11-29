library(igraph)
library(tibble)
library(dplyr)

#' Function to compute various communities by various community detection methods
#' @param graph  - an igraph graph
#' @param i - integer - number of runs
#' @param path string - destination file to write results
#' @returns a tibble
#' @export
simulate_communities <- function(graph, i = 1000, path = "simulate.RDS") {
    results <- tibble()
    totalN <- 4 * 2 * i

    for (method in c("optimal", "spinglass", "walktrap", "fast_greedy")) {
        for (w in c(TRUE, FALSE)) {
              for (iteration in 1:i) {
                  estimate <- get_bridge_estimate(graph, seed = NULL, method = method, weights = w)
                  estimate["Item"] <- V(graph)$name
                  estimate <- estimate[c("Item", "Community")]
                  estimate["Iteration"] <- iteration
                  estimate["Method"] <- method
                  estimate["Weights"] <- w
                  results <- bind_rows(results, estimate)
                  progress <- paste0("\r", round(((nrow(results) / vcount(graph)) / totalN) * 100), "%")
                  cat(progress)
              }
          }
    }
    saveRDS(results, path)
}
