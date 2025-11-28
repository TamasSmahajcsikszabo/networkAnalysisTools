library(tibble)
library(networktools)
library(ggplot2)
library(stringr)
library(dplyr)

#' Create Plot of Graph with bridge influence metric
#' @param network igraph graph
#' @param toolname string name of node, 
#' @returns plot of graph enriched with bridge influnce
#' @export
make_bridge_plot <- function(network, toolname) {
    igraph_converted <- as.igraph(network, attributes = TRUE)
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

    if (length(unique(group_estimation$membership)) > 1) {
        bridge_estimate <- networktools::bridge(network, communities = group_estimation$membership)
    } else {
        bridge_estimate <- networktools::bridge(network)
    }

    # standardize scores into a tibble
    output_tibble <- tibble(.rows = length(bridge_estimate[[1]]))

    for (i in seq(1, 5)) {
        target_vector <- tibble(standardize(bridge_estimate[[i]]))
        colnames(target_vector) <- names(bridge_estimate)[i]
        output_tibble <- bind_cols(output_tibble, target_vector)
    }
    # add communities
    output_tibble <- bind_cols(output_tibble, bridge_estimate[6])
    output_tibble <- output_tibble[, names(bridge_estimate)]
    output_tibble <- output_tibble %>%
        mutate(item = names(bridge_estimate[[1]]))
    output_tibble <- output_tibble %>%
        mutate(Community = as_factor(communities))

    # reshape
    reshaped_output <- output_tibble %>%
        gather(1:5, key = measure, value = value)

    for (r in seq(1, nrow(reshaped_output))) {
        reshaped_output[r, "var"] <- find_tool(reshaped_output[r, "item"][[1]], data_labels)
        reshaped_output[r, "longitem"] <- paste0(reshaped_output[r, "var"], ": ", reshaped_output[r, "item"])
    }

    names(bridge_estimate[[1]]) <- unique(reshaped_output$longitem)



    # create plot

    reshaped_output %>%
        ggplot(aes(longitem, value)) +
        geom_path(aes(group = measure), color = "cornflowerblue") +
        geom_point(size = 4, alpha = 1 / 2) +
        coord_flip() +
        scale_color_manual(values = global_colors$color) +
        theme_light() +
        scale_x_discrete(limits = rev(levels(as_factor(names(bridge_estimate[[1]]))))) +
        facet_wrap(~measure, ncol = 5) +
        theme(
            legend.position = "bottom"
        ) +
        labs(
            x = "Item",
            y = "Z-scores",
            title = paste0("Bridge Expected Influence Estimates for ", toolname)
        ) +
        theme(
            plot.title = element_text(size = 11)
        )
}

