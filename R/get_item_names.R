#' Gets row names 
#' @param covMatrix data frame
#' @param labels vector of string names
#' @param to_replace optional string vector to be replaced
#' @returns character vector
#' @export
get_item_names <- function(covMatrix, labels, to_replace = c("PCA.")) {
    new_labels <- c()
    original_names <- unlist(lapply(rownames(covMatrix), function(x) {
        #TODO: to_replace map
        stringr::str_replace(x, to_replace, "")
    }))
    for (i in seq_along(original_names)) {
        if (stringr::str_detect(original_names[i], "\\.")) {
            split_labels <- unlist(strsplit(original_names[i], "\\."))
            lookup_names <- paste0(unlist(lapply(split_labels, function(x) {
                labels[names(labels) == x]
            })), collapse = " + ")
            split_labels <- paste0(split_labels, collapse = " + ")
            new_labels[i] <- paste0(split_labels, ": \n", lookup_names)
        } else {
            new_labels[i] <- paste0(original_names[i], ": \n", labels[names(labels) == original_names[i]])
        }
    }
    new_labels
}
