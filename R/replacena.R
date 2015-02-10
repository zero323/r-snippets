#' Replace missing values in numeric vector 
#' with arithmetic mean of its flanking elements
#' or throws an error if it is impossible.
#' 
#' @param x numeric vector
#' @return numeric vector
#' @examples
#' replacena(c(1, NA, 2))
#' replacena(c(0, NA, 0, NA, 0))
#'
replacena <- function(x) {
    # If the first or the last element are missing 
    # there is nothing to do
    stopifnot(is.numeric(x))
    stopifnot(!(is.na(x[1] || is.na(x[length(x)]))))
    nas <- which(is.na(x))
    x[nas] <- (x[nas - 1] + x[nas + 1]) / 2
    # Fail if any NA left 
    stopifnot(!anyNA(x))
    x
}