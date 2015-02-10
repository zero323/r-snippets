testthat::expect_false(
    inquisition(
        matrix(c(
            1, 0,
            0, 1),
            nrow = 2, byrow = TRUE
        )
    )
)

testthat::expect_true(
    inquisition(
        matrix(c(
            0, 0, 0,
            1, 0, 0,
            1, 0, 0),
            nrow = 3, byrow = TRUE
        )
    )
)

testthat::expect_false(
    inquisition(
        matrix(c(
            0, 0, 0, 1,
            1, 0, 0, 1,
            0, 0, 0, 0,
            0, 0, 0, 0),
            nrow = 4, byrow = TRUE
        )
    )
)

testthat::expect_false(
    inquisition(
        matrix(c(
            1, 0, 1, 0, 0,
            0, 1, 0, 0, 0,
            0, 0, 0, 1, 0,
            0, 0, 0, 0, 0,
            0, 0, 0, 0, 1),
            nrow = 5, byrow = TRUE
        )
    ) 
)

testthat::expect_true(
    inquisition(
        matrix(c(
            0, 0, 0, 0, 1,
            1, 0, 0, 0, 0,
            0, 0, 0, 0, 1,
            0, 0, 0, 0, 1,
            1, 0, 0, 0, 0),
            nrow = 5, byrow = TRUE
        )
    ) 
)

testthat::expect_true(
    inquisition(
        matrix(c(
            0, 0, 0,
            1, 0, 0,
            1, 0, 0),
            nrow = 3, byrow = TRUE
        )
    ) 
)

testthat::expect_false(
    inquisition(
        matrix(c(
            0, 0, 0,
            0, 0, 1,
            1, 0, 0),
            nrow = 3, byrow = TRUE
        )
    ) 
)

testthat::expect_false(
    inquisition(matrix(c(0)))
)

testthat::expect_false(
    inquisition(matrix(c(1)))
)

testthat::expect_error(
    inquisition(matrix(1, 0))
)

testthat::expect_error(
    inquisition(matrix(2, 2))
)

testthat::expect_error(
    inquisition(data.frame(x=c(0, 1), y=c(1, 0)))
)