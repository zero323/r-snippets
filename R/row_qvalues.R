#' Helper to compute rowwise qvalues for 
#'
#' @param x numeric matrix. See genefilter::rowttests for details
#' @param fac group factor. See genefilter::rowttests for details
#' @return qvalue object. See qvalue::qvalue for details
#'
row_qvalues <- function(x, fac) {
    genefilter::rowttests(x=x, fac=fac) %>%
    magrittr::extract2('p.value') %>%
    qvalue::qvalue() 
}
