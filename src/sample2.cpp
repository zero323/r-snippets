#include <Rcpp.h>
using namespace Rcpp;

NumericVector randperm(const NumericVector x);


//' Generate a random, wihtout replacement,
//' subvector of length k of a given numeric vector x
//' 
//' @param x numeric vector
//' @param k number of elements
//' @return random subvector of x
//' @examples
//' sample2(1:100, 5)
//' sample2(runif(5, 0, 100), 3)
//' sample2(iris$Sepal.Length, 20)
//' 
// [[Rcpp::export]]
NumericVector sample2(const NumericVector x, int k) {
    if (k > x.size() | k < 1) {
        stop("!(0 < k < x.size())");
    }
    return randperm(x)[seq(0, k - 1)];
}