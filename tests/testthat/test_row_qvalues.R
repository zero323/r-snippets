library(testthat)

testthat::test_that('Test row_qvalues', {
    set.seed(323)
    expressionData <- matrix(runif(1000), 100)
    fac <- factor(rep(c(1, 2), each=5))

    testthat::expect_identical(
        row_qvalues(expressionData, fac) %>% magrittr::inset2('call', NULL),
        qvalue::qvalue(genefilter::rowttests(expressionData, fac)$p.value) %>% magrittr::inset2('call', NULL)
    )
})
