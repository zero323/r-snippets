#include <Rcpp.h>
using namespace Rcpp;


//' Impute all missing values found in a given nondecreasingly ordered numeric vector x.
//'
//' @param x numeric vector
//' @return copy of x with imputed NAs
//' @examples
//' NAimput(c(NA, NA, 4, 5, NA))
//' NAimput(c(1, NA, 2, NA, NA, 3))
//'
// [[Rcpp::export]]
NumericVector NAimput(const NumericVector x) {
    
    if(all(is_na(x))) stop("Cannot imput from NAs");
    NumericVector y = Rcpp::clone(x);
    
    
    int i = 0;
    
    IntegerVector indices = seq(0, y.size() - 1);
    IntegerVector notNa = indices[!is_na(y)];
    int firstNotNa = min(notNa);
    int lastNotNa = max(notNa);
    
    for (int i = 0; i < firstNotNa; i++) {
        y[i] = y[firstNotNa];
    }
    
    for (int i = lastNotNa; i < y.size(); i++) {
        y[i] = y[lastNotNa];
    }
    
    int lastFinite = firstNotNa;
    
    for (int i = firstNotNa; i <= lastNotNa; i++) {
        if (!NumericVector::is_na(y[i])) {
            if (y[i] < y[lastFinite]) stop("x should be ordered non-decreasingly");
            
            if (lastFinite != i - 1) {
                for(int j = 0; j < i - lastFinite; j++) {
                    y[lastFinite + j] = y[lastFinite] + j * (y[i] - y[lastFinite]) / (i - lastFinite);
                }
            }
            lastFinite = i;
        }
    }
    
    return y;
}