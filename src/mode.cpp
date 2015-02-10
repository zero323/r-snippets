#include <Rcpp.h>
// [[Rcpp::plugins("cpp11")]]
#include <map>

using namespace Rcpp;

//' Determine the most frequently occurring value in an integer vector
//'
//' @param x integer vector
//' @return integer mode of x
//' 
// [[Rcpp::export]]
int mode(IntegerVector x) {
    std::map<int,int> counts;
    int mode = NA_INTEGER;
    int modeCount = -1;
    
    for (int i = 0; i < x.size(); i++){
        if (!IntegerVector::is_na(x[i])) {
            if(counts.count(x[i]) > 0) {
                counts[x[i]] += 1;
            } else {
                counts[x[i]] = 1;
            }
        }
    }
    
    for(auto pair: counts) {
        if (pair.second > modeCount) {
            modeCount = pair.second;
            mode = pair.first;
        }
    }
    
    return mode ;
}
