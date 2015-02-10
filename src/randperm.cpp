#include <Rcpp.h>
using namespace Rcpp;

//' Generate a random permutation of a given numeric vector.
//' 
//' @param x numeric vector
//' @return permuted x
//' @examples
//' randperm(1:10)
//' letters[randperm(1:length(letters))]
//' randperm(iris$Species)
//' 
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