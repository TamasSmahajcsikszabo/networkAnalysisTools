library(igraph)
library(tibble)
library(dplyr)

#' Computes the True Positive Rates from the result of simulate_communities()
#' @param simulatedCommunities - result of simulate_communities()
#' @param graph - an igraph graph
 #' @returns a tibble of TPR statistics
#' @export

get_TPR <- function(simulatedCommunities, graph) {
    true_table <- tibble(
        Item = V(graph)$name,
        `Community (True)` = V(graph)$subscale_enum
    )
    original_communities <- list()
    for (community in unique(true_table$`Community (True)`)) {
        original_community <- list(true_table[true_table$`Community (True)` == community, ]$Item)
        original_communities[community] <- original_community
    }
    simulatedCommunities <- simulatedCommunities %>%
        group_by(Item, Method, Weights, Community) %>%
        summarise(N = n()) %>%
        ungroup()

    best_found_communities <- list()
    for (method in c("optimal", "spinglass", "walktrap", "fast_greedy")) {
        for (weight in c(TRUE, FALSE)) {
            slice <- simulatedCommunities[simulatedCommunities$Method == method & simulatedCommunities$Weights == weight, ]
            for (community in as.numeric(unique(slice$Community))) {
                found_community <- list(unique(slice[slice$Community == community, ]$Item))

                label <- paste0(method, ", ", weight)
                names(found_community) <- label
                best_found_communities <- append(best_found_communities, found_community)
            }
        }
    }

    results <- tibble()
    subscales <- data.frame(item = V(graph)$name, subscale = V(graph)$subscale)

    for (original_community in original_communities) {
        for (i in seq_along(best_found_communities)) {
            match <- length(intersect(original_community, unlist(best_found_communities[i])))
            label <- strsplit(names(best_found_communities)[i], ", ")[[1]]
            method <- label[1]
            weight <- label[2]
            subscale <- suppressMessages(unique(data.frame(item = original_community) %>% left_join(subscales) %>% dplyr::select(subscale))[[1]])
            if (match > 0) {
                result <- tibble(
                    Method = method,
                    Weights = weight,
                    Subscale = subscale,
                    `# of True community membership` = match,
                    `TPR` = match / length(unlist(original_community)),
                    `Original Subscale` = paste0(original_community, collapse = ", "),
                    `Found Community` = paste0(unlist(best_found_communities[i]), collapse = ", ")
                )
                results <- bind_rows(results, result)
            }
        }
    }
    results <- unique(results)
    results
    results %>%
        group_by(Method, `Original Subscale`) %>%
        filter(TPR == max(TPR))
}
