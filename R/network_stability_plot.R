#' Visualizes the result of network stability estimation
#' @param stability_estimate - result of network_stability_estimator()
#' @returns Plot
#' @export
network_stability_plot <- function(stability_estimate) {
    stability_estimate %>%
        mutate(b = as.numeric(b)) %>%
        group_by(sample_rate, measure) %>%
        mutate(mean_r = mean(r, na.rm = TRUE)) %>%
        filter(!is.na(r)) %>%
        filter(!is.na(sample_rate)) %>%
        ggplot(aes(reorder(percent(sample_rate), desc(sample_rate)), r, group = measure)) +
        geom_point(aes(y = mean_r, color = measure), size = 3) +
        geom_line(aes(y = mean_r, color = measure), size = 1) +
        geom_jitter(aes(color = measure), alpha = 1 / 5, size = 0.5) +
        geom_hline(aes(yintercept = 0)) +
        ylim(c(-1, 1)) +
        theme_light() +
        scale_color_manual(values = c("coral4", "coral", "cornflowerblue", "darkturquoise")) +
        scale_fill_manual(values = c("coral4", "coral", "cornflowerblue", "darkturquoise")) +
        labs(
            title = "Stability estimates of centrality measures with bootstrap",
            x = "Portion of original sample",
            y = "Average correlation with the original sample",
            fill = "Centrality measure",
            color = "Centrality measure",
            caption = "Large points are average correlation estimates, jittered points are actual bootstrap estimates"
        ) +
        # scale_x_continuous(breaks = seq(1.0, 0.0, -0.1)) +
        scale_y_continuous(breaks = seq(-1.0, 1.0, 0.1))
}
