testthat::expect_equal(
    colnames(unwind(WorldPhones, c("count", "where", "when"))),
    c("count", "where", "when")
)

testthat::expect_equal(
    unwind(WorldPhones, c("count", "where", "when"))[2, 1],
    60423
)

testthat::expect_equal(
    as.character(unwind(WorldPhones, c("count", "where", "when"))[2, 2]),
    "N.Amer"
)

testthat::expect_equal(
    dim(unwind(matrix(c(1), dimnames = list("A", "B")), c("x", "y", "z"))),
    c(1, 3)
)

testthat::expect_equal(
    {
        m <- matrix(1:6, nrow = 3, dimnames = list(letters[1:3], letters[5:6]))
        unwind(m, c("val", "col", "row"))
    },
    data.frame(val=1:6, col = rep(letters[5:6], each = 3), row=rep(letters[1:3], 2)) 
)

testthat::expect_equal(
    dim(
        unwind(
            matrix(
                1:110,
                nrow =11,
                dimnames = list(as.character(1:11), as.character(1:10))
            ),
            c("x", "y", "z")
        )
    ),
    c(110, 3)
)

testthat::expect_equal(
    unwind(
        matrix(1:110, nrow =11, dimnames = list(as.character(1:11), as.character(1:10))),
        c("x", "y", "z")
    )[110, 1],
    110
)

testthat::expect_equal(
    unwind(
        matrix(1:110, nrow =11, dimnames = list(as.character(1:11), as.character(1:10))),
        c("x", "y", "z")
    )[1, 1],
    1
)


testthat::expect_equal(
    colnames(
        unwind(
            matrix(1:4, nrow =2, dimnames = list(c("a", "b"), c("a", "b"))),
            c("x", "y", "z")
        )
    ),
    c("x", "y", "z")
)

testthat::expect_equivalent(
    sapply(
        unwind(
            matrix(1:4, nrow =2, dimnames = list(c("a", "b"), c("a", "b"))),
            c("x", "y", "z")
        ),
        class
    ),
    c("integer",  "factor",  "factor")
)

testthat::expect_equal(
    unwind(
        matrix(1:110, nrow =11, dimnames = list(as.character(1:11), as.character(1:10))),
        c("x", "y", "z")
    )[110, 1],
    110
)

testthat::expect_error(
    unwind(iris, c("a", "b", "c"))
)

testthat::expect_error(
    unwind(WorldPhones, c("count", "where"))
)