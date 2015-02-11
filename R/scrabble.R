#' Return n most valuable words from dictionary
#'
#' @param x named integer vector with values of individual letters
#' @param n number of words to return
#' @param dict legal words
#' @param y available letters
#' @return character vector of length <= n
#' @examples
#' scrabble(x = c(a = 1, b = 1, f = 1, o = 1, r = 1), 1, c("foo", "bar"))
#' scrabble(
#'    x = c(a = 1, b = 1, f = 1, o = 1, r = 1), 1,
#'    c("foo", "bar"),
#'    y = c(a = 1, b = 1, f = 0, o = 1, r = 1)
#' )
#' 
scrabble <- function(x, n, dict, y = NULL) {
    stopifnot(is.numeric(x), all(is.finite(x)), !is.null(names(x)))
    stopifnot(is.numeric(n), is.finite(n))
    stopifnot(is.character(dict), length(dict) > 0, !any(is.na(dict)))
    stopifnot(is.null(y) || all(names(x) %in% names(y)))
    
    get_score <- function(word) {
        # Split into individual characters and count
        chars <-sapply(unlist(strsplit(word, "")), length)
        
        # If there are missing characters in x or y
        stopifnot(all(names(chars) %in% names(x)))
        stopifnot(is.null(y) || all(names(chars) %in% names(y))) 
        
        # Compute scores
        if (is.null(y) || all(chars <= y[names(chars)])) {
            sum(chars * x[names(chars)])
        } else {
            -1
        }
    }

    scores <- sapply(dict, get_score)
    
    # Filter
    feasible <- which(scores >= 0)
    dict[feasible][order(scores[feasible], decreasing = TRUE)][1:min(length(feasible), n)]
}