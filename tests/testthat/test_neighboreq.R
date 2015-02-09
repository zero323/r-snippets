testthat::expect_equal(
    neighboreq(c(1, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0)),
    c(1, 4, 5, 7, 9)
)

testthat::expect_equal(neighboreq(1:10), numeric(0))
testthat::expect_equal(neighboreq(10:1), numeric(0))
testthat::expect_equal(neighboreq(rep(1:2, 10)), numeric(0))
testthat::expect_equal(neighboreq(rep(c(1, 2), each=2, times=3)), seq(1, 12, 2))
testthat::expect_equal(neighboreq(1), numeric(0))
testthat::expect_equal(neighboreq(numeric(0)), numeric(0))
testthat::expect_equal(neighboreq(c(0, 0)), 1)
testthat::expect_equal(neighboreq(rep(NA, 10)), numeric(0))
testthat::expect_equal(neighboreq(c(1:3, NA, NA, 3:1)), numeric(0))
testthat::expect_equal(neighboreq(c(1, 2, 2, NA, NA, 3, 3)), c(2, 6))
testthat::expect_equal(neighboreq(c(1, NA)), numeric(0))
testthat::expect_equal(neighboreq(c(Inf, Inf)), c(1))