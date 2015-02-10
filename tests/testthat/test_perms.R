library(testthat)

testthat::expect_error(perms(-1))
testthat::expect_true(is.matrix(perms(1)))
testthat::expect_equal(dim(perms(1)), c(1, 1))
testthat::expect_equal(perms(2), matrix(c(1, 2, 2, 1), ncol = 2))
testthat::expect_equal(perms(3)[, 1], c(1, 1, 2, 2, 3, 3))
testthat::expect_true({
    p <- perms(5)   
    identical(dim(unique(p)), dim(p))
})

testthat::expect_true({
    p <- perms(7)   
    all(p %in% seq(7))
})

testthat::expect_true({
    p <- perms(8)   
    identical(dim(unique(p)), dim(p))
})

testthat::expect_true({
    p <- perms(8)   
    all(p %in% seq(8))
})