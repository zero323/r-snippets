testthat::expect_equal(
    golden_ratio(sin, -3, 0, eps = 1e-10, maxiter = 100)[c('convergence', 'value', 'par')],
    list(convergence = 0, value = -1, par = -pi / 2)
)

testthat::expect_true(
    is.null(golden_ratio(sin, -3, 0, eps = 1e-10, maxiter = 100)$message)
)

testthat::expect_equal(
    golden_ratio(abs, -3, 3, eps = 1e-16, maxiter = 10000)$value,
    0
)

testthat::expect_equal(
    golden_ratio(abs, -3, 3, eps = 1e-16, maxiter = 10000)[c('convergence')],
    list(convergence = 0)
)

testthat::expect_warning(
    golden_ratio(function(x) x, -100, 0, eps = 1e-16, maxiter = 100)
)

testthat::expect_equal(
    golden_ratio(function(x) x, -100, 0, eps = 1e-16, maxiter = 100),
    list(par = -100, value = -100, counts = 100, convergence = 1, message = NULL)
)

testthat::expect_equal(
    golden_ratio(function(x) 1, -1, 1, eps = 1e-16, maxiter = 100)$value,
    1
)

testthat::expect_equal(
    golden_ratio(function(x) x, 0, 1, eps = 1e-16, maxiter = 100)$value,
    0
)