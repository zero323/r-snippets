#' For given adjacency matrix check if any two not directly connected vertices
#' have common following node
#'
#' @param graph binary adjacency matrix 
#' @return single logical value
#' @examples
#' inquisition(matrix(ifelse(runif(100) > 0.75, 1, 0), nrow = 10))
#' 
inquisition <- function(graph) {
    stopifnot(
        is.matrix(graph),
        diff(dim(graph)) == 0,
        all(graph %in% c(0, 1))
    )
    nvertices <- dim(graph)[1]
    
    for(i in 1:max(1, (nvertices - 1))) {
        for (j in min(nvertices, i + 1):nvertices) {
            if(graph[i, j] == 0 && graph[j, i] == 0 && any(graph[i, ] == 1 & graph[j, ] == 1)) {
                return(TRUE)
            }
        }
    }
    
    FALSE
}
