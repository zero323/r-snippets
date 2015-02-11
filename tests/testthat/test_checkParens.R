test_checkParens <- function(checkParens) {
    testthat::expect_equal(checkParens(""), 0)
    testthat::expect_equal(checkParens("]"), 1)
    testthat::expect_equal(checkParens("("), 1)
    testthat::expect_equal(checkParens("(a (( b ) ( c )) ( d ) e )"), 0)
    testthat::expect_equal(checkParens("z <- x[(y>0)]; sum(x)"), 0)
    testthat::expect_equal(checkParens("{}{}"), 0)
    testthat::expect_equal(checkParens("{()[]}"), 0)
    testthat::expect_equal(checkParens("([)]"), 3)
    testthat::expect_equal(checkParens("{()[)]"), 5)
    testthat::expect_equal(checkParens("((((("), 5)
    testthat::expect_equal(checkParens("(foo + bar) == 1 + 3"), 0)
    testthat::expect_equal(checkParens("()()()()(((((())))))()()()()()()()()"), 0)
}

test_checkParens(checkParens1)
test_checkParens(checkParens2)