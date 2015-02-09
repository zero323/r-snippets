#' Calculate k-Winsorized mean of vector x.
#'
#' @param x numeric vector of length n
#' @param k non-negative integer <= (n - 1) / 2
#' @return k-Winsorized mean of the x
#' @examples
#' winsor(1:1000, 3)
#' winsor(rnorm(1000), 25)
#' winsor(runif(1000), 100)
#' @export
#'
winsor <- function(x, k) {
    stopifnot(is.numeric(x), is.finite(x))
    stopifnot(k < 0 || k <= (length(x) - 1) / 2)
    xs <- sort(x)[(k + 1):(length(x) - k)]
    mean(c(
        rep(xs[1], k), xs, rep(xs[length(xs)], k)
    ))
}