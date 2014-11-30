library(testthat)

testthat::expect_equal(
    fisher_yates_shuffle(logical(0)),
    logical(0)
)

testthat::expect_equal(
    fisher_yates_shuffle("A"),
    c("A")
)

testthat::expect_equal(
    length(intersect(
        fisher_yates_shuffle(1:100),
        1:100
    )),
    100
)