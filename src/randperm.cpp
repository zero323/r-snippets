#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector randperm(const NumericVector x) {
    NumericVector y = Rcpp::clone(x);
    
    RNGScope scope;
    int j;
    int temp;
    for (int i = 0; i < y.size(); i++) {
        j = R::runif(i, y.size());
        temp = y[j];
        y[j] = y[i];
        y[i] = temp;
    }
    return y;
}