testthat::test_that('Test read_and_reencode', {
    fname <- tempfile()
    writeLines(text=stringi::stri_rand_lipsum(5), con=fname)
    #' Written lines + newline at the end of the file
    testthat::expect_equal(length(read_and_reencode(fname)), 6)
})