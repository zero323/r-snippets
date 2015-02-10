#include <Rcpp.h>
using namespace Rcpp;


//' Remove all missing values from a given numeric vector.
//' 
//' @param x numeric vector
//' @return numeric vector
//'
// [[Rcpp::export]]
NumericVector naomit(const NumericVector x) {
    return x[!is_na(x)];
}