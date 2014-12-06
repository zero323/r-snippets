#' Replace email addresses formed according to W3C recommendation
#'
#' @param x character vector
#' @param replacement character 
#' @return character vector
#' @export

replace_emails <- function(x, replacement = "__EMAIL__") {
 
    # http://www.w3.org/TR/html5/forms.html#valid-e-mail-address
    pattern <- paste(c(
        "[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+",
        "@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])",
        "?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*"
    ), collapse = "", sep = "")
    
    unlist(lapply(
        x,
        function(token) gsub(pattern, replacement, token,  perl = TRUE)
    ))
}