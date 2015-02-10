
testthat::expect_equal(mode(1), 1)
testthat::expect_equal(mode(numeric(0)), NA_integer_)
testthat::expect_equal(mode(c(1, NA, NA)), 1)
testthat::expect_equal(mode(c(NA, 1, NA)), 1)
testthat::expect_equal(mode(c(NA, NA, 1)), 1)
testthat::expect_equal(mode(c(1, 2, 2, 3, 3, 3, 4, 4, 4, 4)), 4)
testthat::expect_true(mode(c(1, 2)) %in% c(1, 2))