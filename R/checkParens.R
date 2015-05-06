#' Determine if  parentheses in a given text are properly nested.
#'
#' @param text character vector
#' @param parens matrix containing opening and closing parens
#' @return index of first invalid bracket or 0
#' @examples
#' checkParens1("(a (( b ) ( c )) ( d ) e )")
#' checkParens2("(* (+ `(1 2 3)) 4)")
#'
checkParens1 <- function(text, parens = matrix(c("(", "{", "[", ")", "}", "]"), ncol=2)) {
    stopifnot(is.character(text), is.character(parens))
    stopifnot(is.matrix(parens), ncol(parens) == 2)

    opening <- parens[, 1]
    closing <- parens[, 2]
    
    # Split input into individual characters
    characters <- unlist(strsplit(text, ""))
    
    # Rather inefective but it is cleaner than
    # handling full stack implementation
    stack <- character(0)
    mismatch <- 0
    
    if (length(characters) != 0) {
        
        for (i in min(1, length(characters)):length(characters)) {
            char <- characters[i]
            # If char is a closing bracket
            if (char %in% closing) {
                # If we there is a mismatch between brackets
                if (length(stack) == 0 || which(stack[1] == opening) != which(char == closing)) {
                    mismatch <- i
                    break
                } else {
                    # Closing bracket matches previous opening
                    stack <- stack[-1]
                }
            # If we have opening bracket we push on stack
            } else if (char %in% opening) {
                stack <- c(char, stack)
            }
        }
    
        # Check if stack is empty 
        if (length(stack) != 0 && mismatch == 0) {
            mismatch <- length(characters)
        }
    }
    
    mismatch
}


#' Determine if  parentheses in a given text are properly nested.
#'
#' @param text character vector
#' @param parens matrix containing opening and closing parens
#' @return index of first invalid bracket or 0
#' @examples
#' checkParens1("(a (( b ) ( c )) ( d ) e )")
#' checkParens2("(* (+ `(1 2 3)) 4)")
#'
checkParens2 <- function(text, parens = matrix(c("(", "{", "[", ")", "}", "]"), ncol=2)) {
    stopifnot(is.character(text), is.character(parens))
    stopifnot(is.matrix(parens), ncol(parens) == 2)
    
    opening <- parens[, 1]
    closing <- parens[, 2]
    characters <- unlist(strsplit(text, ""))    
    
    loop <- function(stack, i, characters) {
        if(length(characters) == 0 && length(stack) == 0) {
            0
        } else if(length(characters) == 0) {
            i - 1
        } else if (characters[1] %in% closing) {
            if (length(stack) == 0 || which(stack[1] == opening) != which(characters[1] == closing)) {
                i
            } else {
                loop(stack[-1], i + 1, characters[-1])
            }
        } else if (characters[1] %in% parens) {
            loop(c(characters[1], stack), i + 1, characters[-1])
        } else {
            loop(stack, i + 1, characters[-1])
        }
    }
    
    loop(character(0), 1, characters)
}
