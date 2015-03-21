#' Read file in a binary mode and re-encode
#' Adapted from Marek Gagolewski, Advanced Data Analysis Software Development with R
#'
#' @param fname path to the input file
#' @param from input encoding
#' @param to output encoding
#' @param newlines character, regular expression
#' @return character
#'
read_and_reencode <- function(fname, from='utf-8', to='', newlines='\\r\\n|\\r|\\n') {
    readBin(fname, what = 'raw', size = 1, n = file.info(fname)$size) %>% 
        stringi::stri_encode(from=from, to=to) %>%
        stringi::stri_split_regex(pattern=newlines) %>%
        unlist()
}
