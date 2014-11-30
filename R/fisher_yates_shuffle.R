#' Fisher–Yates shuffle
#' @param x atomic vector
#' @return shuffled x
#' @export
fisher_yates_shuffle <- function(x) {
    if (length(x) != 0) {
        for(i in 1:(length(x) - 1)) {
            j <- round(runif(1, i, length(x)))
            x[c(i, j)] <- x[c(j, i)]
        }
    }
    x
}