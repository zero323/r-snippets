#' Given atomic vector x find all indices i where x[i] == x[i + 1]
#' 
#' @param x atomic vector
#' @return vector of indices
#' @examples 
#' neighboreq(rep(0, 10))
#' neighboreq(1:10)
#'
neighboreq <- function(x) {
    stopifnot(is.atomic(x))
    which(c(NA, x) == c(x, NA)) - 1
}