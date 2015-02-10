#' Convert matrix from wide form to long
#' 
#' @param x matrix
#' @param names vector of names
#' @return matrix in a long format
#' @examples
#' unwind(WorldPhones, c("count", "where", "when"))
#' unwind(
#'    matrix(c(1, 0, 0, 1), nrow=2, dimnames = list(c("x", "y"), c("x", "y"))), 
#'    c("value", "c1", "c2")
#' )
#'
unwind <- function(x, names) {
    stopifnot(is.matrix(x), !is.null(colnames(x)), !is.null(rownames(x)))
    stopifnot(length(names) == 3)
    setNames(
        data.frame(
            as.vector(x),
            rep(colnames(x), each=nrow(x)),
            rep(rownames(x), ncol(x))
        ),
        names
    )
}