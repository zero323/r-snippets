testthat::expect_error(wilcoxteststat(1:3, 1:5))

testthat::expect_error(wilcoxteststat(NA, 1))
testthat::expect_error(wilcoxteststat(letters[1:10], 1:10))
testthat::expect_equal(wilcoxteststat(0, 0), 0)
testthat::expect_equal(wilcoxteststat(x=c(-3), y=c(0)), 1)
testthat::expect_equal(wilcoxteststat(c(0, 0, 1), c(0, 0, 0)), 1)
testthat::expect_equal(
    wilcoxteststat(
        c(110, 122, 125, 120, 140, 124, 123, 137, 135, 145),
        c(125, 115, 130, 140, 140, 115, 140, 125, 140, 135)
    ),
    9
)