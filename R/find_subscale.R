#' Article-specific helper function to match original category of a subscale item to predicted
#' @param original string vector of true membership names
#' @param predicted string vector of predicted membership of names
#' @returns list of output stats
#' @export
find_subscale <- function(original, predicted) {
    original <- to_vector(original)
    predicted <- to_vector(predicted)
    common <- intersect(original, predicted)
    match_rate <- length(common) / length(original)
    if (identical(common, original)) {
        output <- list(
            "match" = TRUE, "FP" = predicted[!predicted %in% common],
            "TP" = common, "original" = original, "predicted" = predicted, "match_rate" = match_rate
        )
    } else {
        output <- list(
            "match" = FALSE, "FP" = predicted[!predicted %in% common],
            "TP" = common, "original" = original, "predicted" = predicted, "match_rate" = match_rate
        )
    }

    output
}
