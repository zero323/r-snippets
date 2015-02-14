testthat::test_that('Test plot_prcomp', {
    plot_prcomp(prcomp(iris[, -5]), iris[, 5])
})