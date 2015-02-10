#include <Rcpp.h>
using namespace Rcpp;


//' Take list of integer vectors and convert to array
//'
//' @param x List
//' @return array
//' 
// [[Rcpp::export]]
NumericVector simplify2array(List x) {
    if(x.size() == 0) {
        return NumericVector(0);
    }
    
    NumericVector xi = x[0];
    int size_of_x0 = xi.size();
    NumericVector result(size_of_x0 * x.size());
    
    for(int i = 0; i < x.size(); i++) {
        // "list of numeric vectors on input"
        // Rcpp seems to treat matrices as vectors
        if(!Rf_isVectorAtomic(x[i]) | Rf_isMatrix(x[i])) {
            stop("All elements of x should be atomic vectors");
        }
        
        NumericVector xi = x[i];
        
        if(xi.size() != size_of_x0) stop("Wrong size");
        for(int j = 0; j < size_of_x0; j++) {
            result[i * size_of_x0 + j] = xi[j];
        }
    }
    
    if (size_of_x0 > 1) {
        result.attr("dim") = NumericVector::create(size_of_x0, x.size());
    }
    
    return result;
}
