#include <Rcpp.h>
using namespace Rcpp;

NumericVector randperm(const NumericVector x);

// [[Rcpp::export]]
NumericVector sample2(const NumericVector x, int k) {
    if (k > x.size() | k < 1) {
        stop("!(0 < k < x.size())");
    }
    return randperm(x)[seq(0, k - 1)];
}