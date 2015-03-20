#' Ngrams tokenizer
#' 
#' @param n integer
#' @param skip_word_none boolean see: ?stri_split_boundaries
#' @param skip_word_number boolean see: ?stri_split_boundaries
#' @return n-gram tokenizer function
#' @examples
#' trigram_tokenizer <- ngram_tokenizer(3)
#' trigram_tokenizer(as.character(citation()))
#' 
ngram_tokenizer <- function(n = 1L, skip_word_none = TRUE, skip_word_number = FALSE) {
    stopifnot(is.numeric(n), is.finite(n), n > 0)
    
    #' To avoid :: calls
    stri_split_boundaries <- stringi::stri_split_boundaries
    stri_join <- stringi::stri_join
    
    options <- stringi::stri_opts_brkiter(
        type="word", skip_word_none = skip_word_none, skip_word_number = skip_word_number
    )
    
    #' Tokenizer
    #' 
    #' @param x character
    #' @return character vector with n-grams
    function(x) {
        stopifnot(is.character(x))
    
        # Split into word tokens
        tokens <- unlist(stri_split_boundaries(x, opts_brkiter=options))
        len <- length(tokens)
    
        if(all(is.na(tokens)) || len < n) {
            # If we didn't detect any words or number of tokens is less than n return empty vector
            character(0)
        } else {
            sapply(
                1:max(1, len - n + 1),
                function(i) stri_join(tokens[i:min(len, i + n - 1)], collapse = " ")
            )
        }
    }
}