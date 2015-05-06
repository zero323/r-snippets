#' Remove comments from a source code using tags list
#' It should be done with a proper lexer but who cares ;)
#'
#' @param text character vector with input lines
#' @param tags 2-column character matrix with comment strings. Use NA to indicate "\\n" as closing tag
#' @return text without comments and empty lines. More or less.
uncomment <- function(text, tags) {
    multiline <- "(.|[\r\n])*?"
    inline <- "[^\n]*"
    
    combined <- paste(text, collapse = "\n")
    Hmisc::escapeRegex(tags)
    
    pattern <- paste(apply(
        escaped_tags,
        1,
        function(x) {
            if(!is.na(x[2])) {
                paste("(", x[1], multiline, x[2], ")", sep = "")
            } else {
                paste("(", x[1], inline, ")", sep = "")
            }
        }
    ), collapse = "|")
    Filter(
        function(x) nchar(gsub("\\s+", "", x)) != 0,
        unlist(strsplit(gsub(pattern, "", combined), "\n"))
    )
}
