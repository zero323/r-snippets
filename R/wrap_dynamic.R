#' Omplements a minimum raggedness word wrap algorithm
#'
#' @param text
#' @param h
#' @return
#'
wrap_dynamic <- function(text, h = 76) {
    tokens <- unlist(strsplit(text, "\\s+"))
    ntokens <- length(tokens)
    
    stopifnot(all(sapply(tokens, nchar) <= h))
    
    lengths <- as.vector(sapply(tokens, nchar))
    
    
    costs <- c(0, rep(Inf, ntokens - 1))
    current <- c(lengths[1], rep(Inf, ntokens - 1))
    trace <- matrix(FALSE, nrow = ntokens, ncol = ntokens)
    
    get_br_cost <- function(costs, current, j, ntokens) {
        # If we are in the first line Inf
        # otherwise cost from the previous + penalty
        cost <- h - current[-ntokens]
        c(Inf, costs[-ntokens] + ifelse(cost >= 0, cost, Inf) ** 2)
    }
    
    get_extend_cost <- function(costs, current, j, ntokens){
        # If we can add to the current line then cost doesn't change
        # otherwise Inf
        ifelse(h - current - 1 - lengths[j] < 0 | is.infinite(current), Inf, costs)
    }
    j <- 2
    for (j in 2:ntokens) {
        br_cost <- get_br_cost(costs, current, j, ntokens)
        extend_cost <- get_extend_cost(costs, current, j, ntokens)
        mask <- br_cost < extend_cost

        current <- ifelse(
            mask,
            lengths[j],
            ifelse(is.finite(extend_cost), current + 1 + lengths[j], Inf)
        )
        
        costs <- ifelse(mask, br_cost, ifelse(is.finite(current), costs, Inf))
        trace[, j] <- mask
        cbind(costs, current, trace[, j])
        j <- j + 1
    }
    
   
    i <- which.min(costs + (h - current) ** 2)
    
    result <- c("")
    
    for (j in ntokens:1){
        result[1] <- paste(tokens[j], result[1])
        if(trace[i, j]) {
            result <- c("", result)
            i <- i - 1
        }
    }
    
    unlist(lapply(result, function(x) gsub("\\s+$", "", x)))
}
