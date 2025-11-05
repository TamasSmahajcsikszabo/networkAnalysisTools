#' Gets the probability from logit
#' @param logit logit value
#' @export
get_p <- function(logit){
  exp(logit)/(1+exp(logit))
}