#' Determine a local minimum of a continuous real function f
#'
#' @param f function
#' @param a numeric 
#' @param b numeric > a
#' @param eps numeric (default 1e-16)
#' @param maxiter  maximum number of iterations
#' @return  list containing approximation of local minimum,
#'   value at local minimum and some control data
#' @examples
#' golden_ratio(sin, -pi, pi)
#' golden_ratio(sign, -1, 1, eps = 1e-3)
#' golden_ratio(function(x) -1, -100, 100, maxiter = 10000)
#' 
golden_ratio <- function(f, a, b, eps = 1e-16, maxiter = 100) {
    stopifnot(
        is.numeric(a) & is.finite(a),
        is.numeric(b) & is.finite(b),
        b > a,
        is.numeric(eps) & is.finite(eps) & eps > 0,
        is.numeric(maxiter) & is.finite(maxiter) & maxiter > 0
    )
    
    phi <- (sqrt(5) - 1) / 2
    convergence <- 1
    
    for(i in 1:maxiter) {
        # Update xl, xp
        xl <- b - phi * (b - a)
        xp <- a + phi * (b - a)
        if(f(xl) > f(xp)) {
            a <- xl
        } else {
            b <- xp
        }
        
        # Check if method converged
        if(abs(b - a) < eps) {
            convergence <- 0
            break
        }
    }
    
    if(convergence == 1) warning("Method didn’t converge")
    
    list(
        par = (a + b) / 2,
        value = f((a + b) / 2),
        counts = i,
        convergence = convergence,
        message = NULL
    )
}
