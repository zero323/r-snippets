testthat::expect_error(winsor(rep(Inf, 10), 1))
testthat::expect_error(winsor(letters, 3))
testthat::expect_error(winsor(1:3, 99))
testthat::expect_error(winsor(1:3, -1))

testthat::expect_equal(winsor(1:10, 3), 5.5)
testthat::expect_equal(winsor(10:1, 3), 5.5)
testthat::expect_equal(winsor(1:10, 0), 5.5)

testthat::expect_equal(winsor(rep(1, 9), 2), 1)
testthat::expect_equal(winsor(c(-1 * 5:1, 0, 5:1), 4), 0)
testthat::expect_equal(winsor(c(-1, 1, -1), 1), -1)

set.seed(323)
n <- round(runif(1, 5, 100))
x <- rnorm(n)
k <- round(runif(1, 1, (n - 1) / 2))
testthat::expect_equal(winsor(x, k), winsor(sample(x, n), k))