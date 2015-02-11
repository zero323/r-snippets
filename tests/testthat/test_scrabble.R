testthat::expect_equal(
    scrabble(c(a = 1, b = 0), 1, c("a", "b")),
    c("a")
)

testthat::expect_equal(
    scrabble(c(a = 1, b = 0), 2, c("a", "b")),
    c("a", "b")
)

testthat::expect_equal(
    scrabble(c(a = 1, b = 2, c = 3, d = 4), 1, c("aaaaa", "bb", "c", "d")),
    c("aaaaa")
)

testthat::expect_equal(
    scrabble(c(a = 1, b = 2, c = 3, d = 99), 1, c("aaaaa", "bb", "c", "d")),
    c("d")
)

testthat::expect_equal(
    scrabble(
        c(a = 1, b = 2, c = 3, d = 99), 1,
        c("aaaaa", "bb", "c", "d"),
        y = c(a = 100, b = 100, c = 100, d = 0)),
    c("aaaaa")
)

testthat::expect_equal(
    {   
        x <- structure(
            c(1, 5, 3, 2, 6, 2, 1, 5, 5, 3, 3, 1, 3, 2, 2, 3, 2, 1, 7, 1, 5, 2, 1, 1, 5, 2, 3, 1, 2, 1, 9, 5),
            .Names = c(
                "a", "ą", "b", "c", "ć", "d", "e", "ę", "f", "g", "h", "i", "j",
                "k", "l", "ł", "m", "n", "ń", "o", "ó", "p", "r", "s", "ś", 
                "t", "u", "w", "y", "z", "ź", "ż"
        ))
        
        y <- structure(
            c(2, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
            .Names = c(
                "a", "ą", "b", "c", "ć", "d", "e", "ę", "f", "g", "h", "i", "j",
                "k", "l", "ł", "m", "n", "ń", "o", "ó", "p", "r", "s", "ś", 
                "t", "u", "w", "y", "z", "ź", "ż"
        ))
        
        scrabble(x = x, 3, dict = c("aba", "wąż", "baz"), y = y)
    },
    c("aba")
)

testthat::expect_equal(
    {   
        x <- structure(
            c(1, 5, 3, 2, 6, 2, 1, 5, 5, 3, 3, 1, 3, 2, 2, 3, 2, 1, 7, 1, 5, 2, 1, 1, 5, 2, 3, 1, 2, 1, 9, 5),
            .Names = c(
                "a", "ą", "b", "c", "ć", "d", "e", "ę", "f", "g", "h", "i", "j",
                "k", "l", "ł", "m", "n", "ń", "o", "ó", "p", "r", "s", "ś", 
                "t", "u", "w", "y", "z", "ź", "ż"
        ))
        scrabble(x = x, 2, c("anaconda", "banan", "wężowisko"))
    },
    c("wężowisko", "anaconda")
)