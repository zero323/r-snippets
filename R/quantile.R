#' Compute quantiles corresponding to the given probabilities
#'
#' @param x numeric vector
#' @param p numeric vector of probabilites
#' @return  quantiles of x
#' @examples
#' quantile(rnorm(100), 0.5)
#' quantile(1:100, c(0.25, 0.5, 0.75))
quantile <- function(x, p) {
    stopifnot(is.numeric(x), is.finite(x))
    stopifnot(is.numeric(p), all(p >= 0 && p <= 1))
    xs <- sort(x)
    h <- (length(x) - 1) * p + 1
    q <- xs[floor(h)] + (h - floor(h)) * (xs[floor(h) + 1] - x[floor(h)])
    # If p == 1 then xs[floor(h) + 1] gives NA hence we have to remove NAs
    q[is.na(q)] <- xs[length(xs)]
    q
}
