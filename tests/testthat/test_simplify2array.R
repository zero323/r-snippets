
testthat::expect_error(simplify2array(list(1:3, 1:10)))
testthat::expect_error(simplify2array(list(1, matrix(1))))
testthat::expect_equal(simplify2array(list()), numeric(0))
testthat::expect_equal(simplify2array(list(1:10)), matrix(1:10))
testthat::expect_equal(simplify2array(list(1, 2, 3)), 1:3)
testthat::expect_equal(
    simplify2array(list(1:10, 10:1, -1:-10)),
    matrix(c(1:10, 10:1, -1:-10), ncol = 3)
)