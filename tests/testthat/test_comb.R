testthat::expect_equal(
    comb(5, textConnection(c(as.character(rep(1, 50)), "0"))),
    rep(1, 5)
)

testthat::expect_equal(
    {
        runif <- function(n, i, j) i
        comb(5, textConnection(c(as.character(1:100), "0")))
    },
    1:5
)

testthat::expect_equal(
    length(comb(200, textConnection(c(as.character(1:100), "0")))),
    100
)

set.seed(323)
for (i in 1:10) {
    testthat::expect_true(
        chisq.test(comb(100, textConnection(c(as.character(rep(1:10, 1000)), "0"))))$p.value > 0.05,
    )
}
