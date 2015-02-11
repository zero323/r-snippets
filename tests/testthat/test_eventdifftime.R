eventdifftime <- function(x) {
    stopifnot(is.data.frame(x))
    stopifnot(all(c("h", "m", "s") %in% colnames(x)))
    
    stopifnot(is.numeric(x$h), is.finite(x$h))
    stopifnot(is.numeric(x$m), is.finite(x$m))
    stopifnot(is.numeric(x$s), is.finite(x$s))
              
    stopifnot(x$h >= 0, x$h <= 23)
    stopifnot(x$m >= 0, x$m <= 59)
    stopifnot(x$s >= 0, x$s <= 59)
    
    secs <- sort(apply(
        x[, c('h', 'm', 's')], 1, function(x) sum(x * 60 ** c(2, 1, 0))
    ))
    secs[-1] - secs[-length(secs)]
}

## ---- Examples ----

# tests:
testthat::expect_equal(
    eventdifftime(data.frame(h = numeric(0), m = numeric(0), s = numeric(0))),
    numeric(0)
)

testthat::expect_equal(
    eventdifftime(
        data.frame(h = rep(0, 3), m = rep(0, 3), s=rep(0, 3))
    ),
    c(0, 0)
)

testthat::expect_equal(
    eventdifftime(
        data.frame(h = c(0, 23), m = c(0, 59), s = c(0, 59))
    ),
    c(86400 - 1)
)

testthat::expect_equal(
    eventdifftime(data.frame(h = c(13, 14, 13), m = c(1, 3, 2), s = c(1, 4, 2))),
    c(61, 3662)
)

testthat::expect_error(
    eventdifftime(data.frame(h = letters,  m = letters, s = letters))    
)

testthat::expect_error(
    eventdifftime(data.frame(h = c(0, 25),  m = c(0, 0), s = c(0, 0)))    
)
