testthat::expect_equal(
    gendiscrete(4, "A", 1),
    rep("A", 4)
)

testthat::expect_equal(gendiscrete(5, letters[1], 1), rep("a", 5))

testthat::expect_equal(
    {
        runif <- function(n, ...) rep(0.75, n)
        gendiscrete(2, letters[1:2], c(0.5, 0.5))
    },
    rep("b", 2)
)

testthat::expect_false(
    1 %in% {
        runif <- function(n, ...) stats::runif(n, 0.5, 1)
        gendiscrete(2, 1:10, rep(10, 10))
    }
)

testthat::expect_equal(
    length(gendiscrete(10, 1:10, rep(1, 10) / 10)),
    10
)

testthat::expect_true(
    min(gendiscrete(10, -10:10, runif(21))) >= -10
)

testthat::expect_true(
    max(gendiscrete(10, -10:10, runif(21))) <= 10
)

testthat::expect_error(
    gendiscrete(1, 1, 1:1000)
)

testthat::expect_error(
    gendiscrete('A', 1:10, rep(1, 10) / 10)
)

testthat::expect_warning(
    gendiscrete(10, runif(10), 1:10)
)

testthat::expect_error(
    gendiscrete(1, 1, NA)
)