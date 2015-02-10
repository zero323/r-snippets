#include <Rcpp.h>
using namespace Rcpp;

//' Generate all permutations
//'
//' @param n integer
//' @return matrix of permutations
//' 
// [[Rcpp::export]]
IntegerMatrix perms(int n) {
    if (n <= 0) {
        stop("n has to be >= 1");
    }
    IntegerVector vec = seq_len(n);
    int nrows = std::accumulate(vec.begin(), vec.end(), 1, std::multiplies<int>());
    IntegerMatrix result(nrows, n);
    
    
    // Note: given column wise order of values
    // it is not practical approach but 
    // for any large n creating matrix
    // is not feasible anyway

    for(int i = 0; i < nrows; i++) {
        if (i > 0) {
            std::next_permutation(vec.begin(), vec.end());
        }
        
        for(int j = 0; j < n; j++) {
            result(i, j) = vec[j];
        }
    }
    
    return result;
}
