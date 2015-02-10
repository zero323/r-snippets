#' Return at most k random numeric values taken from file
#'
#' @param k integer
#' @param file input file
#' @return numeric vector of length min(k, length(readLines(file)))
#' @examples
#' con <- textConnection(c("1", "0"))
#' comb(10, con)
#' close(con)
#'
comb <- function(k, file="") {
    
    while(TRUE) {
        v <- scan(file, what=double(), n=1, quiet=TRUE)
            if(v <= 0) break
    }
}