#' Generate random sequence of elements choosen with replacement from given vetcor
#'
#' @param n single natural number
#' @param x numeric vector of length k with unique elements
#' @param p vector of probabilities
#' @return vector of length n and type == type(x) 
#' 
gendiscrete <- function(n, x, p) {
    stopifnot(is.numeric(n), length(n) == 1)
    stopifnot(length(unique(x)) == length(x))
    stopifnot(is.numeric(p), all(is.finite(p)))
    stopifnot(length(p) == length(x))
    
    if(sum(p) != 1) {
        warning("sum(p) != 1")
        p / sum(p)
    }
    
    cp <- cumsum(p)
    sapply(runif(n), function(y) x[max(which(y < cp))]) 
}
