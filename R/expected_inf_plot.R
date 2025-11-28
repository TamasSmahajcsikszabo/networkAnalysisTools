library(tibble)
library(networktools)
library(ggplot2)
library(stringr)

#' Create Plot of Graph with expected influence metric
#' @param network igraph graph
#' @param toolname string name of node, 
#' @returns plot of graph enriched with expected influnce
#' @export
expected_inf_plot <- function(network, toolname) {
    influence_df <- networktools::expectedInf(network, step = c("both"), directed = FALSE)
    step1 <- unlist(influence_df["step1"])
    step2 <- unlist(influence_df["step2"])

    # z-scores
    step1_mean <- mean(step1, na.rm = TRUE)
    step1_sd <- sd(step1, na.rm = TRUE)
    step1 <- (step1 - step1_mean) / step1_sd
    step2_mean <- mean(step2, na.rm = TRUE)
    step2_sd <- sd(step2, na.rm = TRUE)
    step2 <- (step2 - step2_mean) / step2_sd

    varnames <- names(step1)
    varnames <- stringr::str_replace(varnames, "step1.", "")
    for (i in seq_along(varnames)) {
        tool <- find_tool(varnames[i], data_labels)
        varnames[i] <- paste0(tool, ": ", varnames[i])
    }
    influence_tibble <- tibble(
        OneStep = step1,
        TwoStep = step2,
        item = factor(varnames, levels = varnames)
    ) %>%
        gather(1:2, key = Method, value = inf)

    influence_tibble %>%
        ggplot(aes(item, inf, group = Method, color = Method)) +
        geom_point(shape = 0, size = 2) +
        geom_path() +
        coord_flip() +
        scale_color_manual(values = global_colors$color[1:2]) +
        theme_light() +
        scale_x_discrete(limits = rev(levels(influence_tibble$item))) +
        labs(
            title = paste0("One- and Two-Step Expected Influence estimates for ", toolname),
            x = "Items",
            y = "Expected Influence (z-scores)"
        ) +
        theme(
            plot.title = element_text(size = 11)
        )
}
