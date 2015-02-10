library(testthat)

#testthat::expect_equal(shortestpath(matrix(0), 0, 0), 0)

testthat::test_that("", {
    m <- matrix(c(0, 0, 1, 0), ncol=2)
    testthat::expect_equal(shortestpath(m, 0, 1), 1)
    testthat::expect_true(is.na(shortestpath(m, 1, 0)))
})



testthat::test_that("", {
    m <- matrix(c(
        0, 1, 1, 0,
        1, 0, 1, 0,
        0, 1, 0, 1,
        0, 0, 1, 0), ncol=4, byrow = TRUE)
    
    testthat::expect_equal(shortestpath(m, 0, 3), 2)
    testthat::expect_equal(shortestpath(m, 3, 0), 3)
    testthat::expect_equal(shortestpath(m, 1, 0), 1)
})


testthat::test_that("", {
    m <- structure(c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0), .Dim = c(5L, 5L))
    testthat::expect_true(is.na(shortestpath(m, 0, 4)))
})
