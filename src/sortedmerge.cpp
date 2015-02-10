#include <Rcpp.h>
using namespace Rcpp;

//' Merge two sorted (nondecreasingly or nonincreasingly) vectors
//' 
//' @param first numeric vector
//' @param second numeric vector
//' @return numeric vector
//' @examples
//' sortedmerge(c(1, 3, 5), c(2, 4, 6))
//' sortedmerge(3:1, 3:1)
//' 
// [[Rcpp::export]]
NumericVector sortedmerge(const NumericVector first, const NumericVector second) {
    if(is_true(any(is_na(first)))) stop("First contains NAs");
    if(is_true(any(is_na(second)))) stop("Second contains NAs");
    
    if(first.size() == 0) stop("First is empty");
    if(second.size() == 0) stop("Second is empty");
    
    NumericVector result(first.size() + second.size());
    
    bool dec = is_true(all(diff(first - min(first)) <= 0)) & is_true(all(diff(second - min(second)) <= 0));
    bool inc = is_true(all(diff(first - min(first)) >= 0)) & is_true(all(diff(second - min(second)) >= 0));
    
    if (!dec & !inc) {
        stop("Both vectors should be sorted in the same way.");
    }   
    
    for (int i = 0, j = 0, k = 0; k < result.size(); k++) {
        if (i < first.size() & j < second.size()) {
            if (first[i] < second[j] & dec | first[i] > second[j] & inc) {
                result[k] = second[j];
                j += 1;
            } else {
                result[k] = first[i];
                i += 1;
            }
        }
        else if(i < first.size()) {
            result[k] = first[i];
            i += 1;
        } else {
            result[k] = second[j];
            j += 1;
        }
    }
    
    return result;
}