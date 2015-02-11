
uncomment <- function(text, tags) {
    multiline <- "(.|[\r\n])*?"
    inline <- "[^\n]*"
    
    combined <- paste(text, collapse = "\n")
    escaped_tags <- if (require(Hmisc)){
        Hmisc::escapeRegex(tags)
    } else {
        gsub("\\*", "\\\\*", tags)
    }
    
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

## ---- Examples ----

# tests:

testthat::expect_equal(
    uncomment(
        text = c("#This file is empty"),
        tags = matrix(c("#", NA), ncol=2)
    ),
    character(0)
)

testthat::expect_equal(
    uncomment(
        text = c(
            '#inlcude <iostream>',
            '/* This is a',
            'Hello world',
            'function */',
            'int main( ) {',
            '    printf( "Hello World" );',
            '}'
        ), tags = matrix(c("/*", "//", "*/", NA), ncol=2)),
        c(
            '#inlcude <iostream>',
            'int main( ) {',
            '    printf( "Hello World" );',
            '}'
        )
)

testthat::expect_equal(
    uncomment(
        text = c(
            "/* comment */",
            "foo();",
            "/* comment */"
        ), tags = matrix(c("/*", "//", "*/", NA), ncol=2)),
        c("foo();")
)

testthat::expect_equal(
    uncomment(
        text = c(
            " // /* single line comment",
            "foo();",
            "// */ single line comment"
        ), tags = matrix(c("/*", "//", "*/", NA), ncol=2)),
        c("foo();")
)

testthat::expect_equal(
    uncomment(
        text = c(
            " /* comment",
            "foo();",
            "*/  not longer a comment */"
        ), tags = matrix(c("/*", "//", "*/", NA), ncol=2)),
        c( "   not longer a comment */")
)


# Absurd ;)
testthat::expect_equal(
    uncomment(
        text = c(
            "string foo = '// comment'"
        ), tags = matrix(c("/*", "//", "*/", NA), ncol=2)),
        c( "string foo = '")
)