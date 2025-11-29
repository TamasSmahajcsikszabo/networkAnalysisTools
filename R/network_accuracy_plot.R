#' Helper function to make a network accuracy plot

network_accuracy_plot <- function(accuracy, tool = "", multiple = FALSE, save_data = FALSE) {
    sample_data <- accuracy$sampleTable %>%
        filter(nchar(id) > 7) %>%
        arrange(desc(value))

    sample_data <- sample_data %>%
        left_join(data_labels, by = c("node1" = "var")) %>%
        left_join(data_labels, by = c("node2" = "var")) %>%
        rename(
            new_node_1 = labels.x,
            new_node_2 = labels.y
        ) %>%
        filter(!is.na(new_node_2)) %>%
        mutate(
            tool1 = str_sub(node1, 1, str_locate(node1, "_")[, 1] - 1),
            tool2 = str_sub(node2, 1, str_locate(node2, "_")[, 1] - 1)
        ) %>%
        mutate(id = if_else(tool1 == tool2,
            paste0(tool1, ": ", new_node_1, " - ", new_node_2),
            paste0(tool1, ": ", new_node_1, " - ", tool2, ": ", new_node_2)
        ))

    bootstrap_data_aggr <- accuracy$bootTable %>%
        left_join(data_labels, by = c("node1" = "var")) %>%
        left_join(data_labels, by = c("node2" = "var")) %>%
        rename(
            new_node_1 = labels.x,
            new_node_2 = labels.y
        ) %>%
        filter(!is.na(new_node_2)) %>%
        mutate(
            tool1 = str_sub(node1, 1, str_locate(node1, "_")[, 1] - 1),
            tool2 = str_sub(node2, 1, str_locate(node2, "_")[, 1] - 1)
        ) %>%
        mutate(id = if_else(tool1 == tool2,
            paste0(tool1, ": ", new_node_1, " - ", new_node_2),
            paste0(tool1, ": ", new_node_1, " - ", tool2, ": ", new_node_2)
        )) %>%
        filter(nchar(id) > 7) %>%
        arrange(desc(value)) %>%
        group_by(id) %>%
        summarize(
            m = mean(value, na.rm = TRUE),
            lower = m - 1.96 * sd(value, na.rm = TRUE),
            upper = m + 1.96 * sd(value, na.rm = TRUE)
        )



    bootstrap_data <- accuracy$bootTable %>%
        filter(nchar(id) > 7) %>%
        arrange(desc(value))

    if (save_data) {
        saveRDS(sample_data, "../output/sample_data.RDS")
        saveRDS(bootstrap_data, "../output/bootstrap_data.RDS")
        saveRDS(bootstrap_data_aggr, "../output/bootstrap_data_aggr.RDS")
    }

    ggplot() +
        geom_vline(aes(xintercept = 0), linetype = "dotted", size = 0.75) +
        geom_point(data = bootstrap_data_aggr, aes(m, reorder(id, m)), shape = 22, fill = "cornflowerblue", size = 2) +
        geom_errorbar(data = bootstrap_data_aggr, aes(m, id, xmin = lower, xmax = upper), color = "cornflowerblue") +
        geom_point(data = sample_data, aes(value, reorder(id, value)), shape = 21, fill = "coral", size = 2) +
        theme_light() +
        labs(
            title = paste0("Edge Weight Accuracy Estimates with Bootstrap for ", tool),
            x = "Edge Weight",
            y = "Edge",
            caption = "DOT - the sample edge weight, SQUARE and confidence interval - the bootstrap estimates for edge weights (mean and 95% CI)"
        ) +
        scale_x_continuous(breaks = seq(-0.2, 0.8, 0.05))
}
