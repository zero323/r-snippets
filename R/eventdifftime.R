#' Given data frame with at least 3 columns (h, m, s)
#' representing hours, minutes, and seconds respectively
#' of a single day compute diffrences in seconds between
#' ordered events
#'
#' @param x data frame with n rows
#' @return numeric vector of length n - 1
#'
eventdifftime <- function(x) {
    stopifnot(is.data.frame(x))
    stopifnot(all(c("h", "m", "s") %in% colnames(x)))
    
    stopifnot(is.numeric(x$h), is.finite(x$h))
    stopifnot(is.numeric(x$m), is.finite(x$m))
    stopifnot(is.numeric(x$s), is.finite(x$s))
              
    stopifnot(x$h >= 0, x$h <= 23)
    stopifnot(x$m >= 0, x$m <= 59)
    stopifnot(x$s >= 0, x$s <= 59)
    
    secs <- sort(apply(
        x[, c('h', 'm', 's')], 1, function(x) sum(x * 60 ** c(2, 1, 0))
    ))
    secs[-1] - secs[-length(secs)]
}
