#' Transforms name vector to uniquie tibble
#' @param namevector string vector of names
#' @export
get_itemnames <- function(namevector){
    gridN <- tibble(expand.grid(namevector, namevector))
    gridN <- gridN |> filter(!Var1==Var2)
    gridN
}
