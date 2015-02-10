testthat::expect_equal(
    logiderle(c(1L, 4L), c(1L, 6L), 7L),
    c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE)
)

testthat::expect_equal(
    logiderle(c(1L, 3L), c(2L, 5L), 5L),
    c(TRUE, TRUE, TRUE, TRUE, TRUE)
)

testthat::expect_equal(
    logiderle(c(1L, 4L), c(2L, 5L), 5L),
    c(TRUE, TRUE, FALSE, TRUE, TRUE)
)

testthat::expect_equal(
    logiderle(c(1L), c(1L), 1L),
    c(TRUE)
)

testthat::expect_equal(
    logiderle(c(1L), c(2L), 2L),
    c(TRUE, TRUE)
)

testthat::expect_equal(
    logiderle(c(3L), c(4L), 4L),
    c(FALSE, FALSE, TRUE, TRUE)
)

testthat::expect_equal(
    logiderle(c(1L, 5L), c(2L, 6L), 7L),
    c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE)
)

testthat::expect_equal(
    logiderle(c(1L, 2L, 3L), c(1L, 2L, 3L), 5L),
    c(TRUE, TRUE, TRUE, FALSE, FALSE)
)

testthat::expect_error(
    logiderle(c(1L, 1L), c(1L, 1L), 5L)
)

testthat::expect_error(
    logiderle(c(1L, 6L), c(1L, 7L), 5L)
)

testthat::expect_error(
    logiderle(c(1L, 6L), c(1L, 7L), 5L)
)

testthat::expect_error(
    logiderle(c(2L), c(1L), 2L)
)