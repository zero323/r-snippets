#' Calculate Wilcoxon signed rank test
#'
#' @param x numeric vector
#' @param y numeric vector
#' @return Wilcoxon signed rank test statistic
#' @examples
#' wilcoxteststat(runif(10) + 1, rnorm(10))
#' wilcoxteststat(rnorm(100), rnorm(100))
#' wilcoxteststat(1:10, 1:10)
wilcoxteststat <- function(x, y) {
    stopifnot(length(x) == length(y))
    stopifnot(is.numeric(x), is.numeric(y))
    stopifnot(!anyNA(x), !anyNA(y))
    mask <- x != y
    # Compute vector of differences between x and y
    d <- y[mask] - x[mask]
    abs(sum(sign(d) * rank(abs(d))))
}