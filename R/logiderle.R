#' Retrun logical vector w of length n where
#' w_{l} == TRUE if there exists such p that l %in% i[p]:j[p]
#'
#' @param i an integer vector of lenght m
#' @param j an integer vector of lenght m
#' @param n natural number
#' @return logical vector of length n
#' @examples
#' logiderle(c(1L, 4L), c(1L, 6L), 7L)
#' logiderle(c(1L), c(2L), 2L)
#' 
logiderle <- function(i, j, n) {

    all(i[2:length(i)] > j[1:(length(j) - 1)])  
    stopifnot(
        is.integer(i),
        is.integer(j),
        is.integer(n) & n >= 0,
        all(i <= j),
        all(1 <= j),
        all(j <= n),
        length(i) == 1 |  all(i[2:length(i)] > j[1:(length(j) - 1)])
    )
    sapply(1:n, function(l) any(i <= l & l <= j))
}