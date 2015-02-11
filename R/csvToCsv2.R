#' Substitute field separators "," for ";"
#' and decimal separators „.” for „,”
#'
#' @param infname character, input path
#' @param outfname  character, output path.
#'
csvToCsv2 <- function(infname, outfname) {
    # We could test if correct, if we have access and so on
    # but if it any of these is not true we will fail anyway
    # and expecetion can be catched somwhere in the calling code
    stopifnot(is.character(infname), length(infname) == 1, nchar(infname) > 0)
    stopifnot(is.character(outfname), length(outfname) == 1, nchar(outfname) > 0)
    
    translate <- function(line) {
        
        # State switch
        # TRUE - we are inside character field
        # FALSE - we are not inside character field
        # NA - '""' encountered 
        char_field <- FALSE
        chars <- unlist(strsplit(line, ""))
        
        for (i in 1:length(chars)) {
            char <- chars[i]
        
            char_field <- if (identical(char, '"')) {
                # We enter char field
                if (is.na(char_field) | !char_field) {
                    TRUE
                    
                # Either the end of the charfield
                # or '""'
                } else {
                    NA
                }
            } else {
                # We've seen '""' and next character
                # is not '"' so we are outside
                # charfield
                if(is.na(char_field))  {
                    FALSE
                } else {
                    char_field
                }
            }
            
            # Set char depending on state
            chars[i] <- if(identical(char_field, FALSE) && char %in% c(",", ".")) {
                if (identical(char, ",")) {
                    ";"
                } else { ","}
            } else {
                char
            }
        }
        
        # Join characters and return
        paste(chars, sep = "", collapse = "")
    }
    
    writeLines(unlist(lapply(readLines(infname), translate)), outfname)   
}
