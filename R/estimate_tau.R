#' Estimates Kendall's tau
#' @param x numeric vector or list
#' @param y numeric vector
#' @returns tau
#' @export
estimate_tau <- function(x, y = NULL) {
    if (is.list(x)) {
        input <- x
        x <- input[[1]]
        y <- input[[2]]
    }

    concordants <- rep(0, length(x))
    discordants <- rep(0, length(x))
    total <- c(0)
    max_op <- length(x) * (length(y) - 1)
    for (index in seq(1, length(x))) {
        for (pair_index in seq(1, length(x))[!seq(1, length(x)) == index]) {
            first <- list(x[index], y[index])
            second <- list(x[pair_index], y[pair_index])
            if ((first[[1]] > second[[1]] && first[[2]] > second[[2]]) ||
                (first[[1]] < second[[1]] && first[[2]] < second[[2]])) {
                concordants[[index]] <- 1
            } else {
                discordants[[index]] <- -1
            }
            total <- total + 1
            cat(paste0("\r", "Progress: ", round(total / max_op, 2) * 100, "%"))
        }
    }
    tau <- (sum(concordants) + sum(discordants)) / total
    if (is.nan(tau) && (length(x) == 1 || length(y) == 1)) {
        warning("Estimation not possible; input values are scalars")
    } else {
        cat(paste0("\n", "Tau estimate = ", tau))
    }
}
