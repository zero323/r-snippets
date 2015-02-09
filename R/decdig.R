#' Create vector with n most significant decimal digits of the given number
#'
#' @param x a single numeric value
#' @param n a single natural number, n <= 16
#' @return integer vector with n most significant decimal digits of x
#' @examples
#' decdig(pi, 3)
#' decdig(exp(1), 5)
#' decdig(1e-2, 3)
#' @export
#' 
decdig <- function(x, n) {
    stopifnot(is.numeric(x))
    stopifnot(is.numeric(n), n <= 16, n > 0)
    ax <- abs(x)
    
    ceil.log10.ax <- ceiling(log10(ax))
    
    # Check if x is a power of 10
    is.power.of.10 <- as.integer(10 ** ceil.log10.ax == ax)
    
    # Compute how many decimal places we have to shift x to the left to get
    # n-digit integer 
    shift.factor <-  10 ** (n - ceil.log10.ax - is.power.of.10)
    
    # Shift and compute individual digits
    xi <- ((ax * shift.factor) %/% 10**((n - 1):0)) %% 10
    
    # Correct for possible x == 0
    xi[is.na(xi)] <- 0
    xi
}
