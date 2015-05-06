#include <Rcpp.h>
using namespace Rcpp;


//' Compute the length of the longest common subsequence of two numeric vectors.
//'
//' @param first numeric vector
//' @param second numeric vector
//' @param trace bool
//' @return length of the lcs 
//' @examples
//' lcs(charToRaw("GAGAGTAGATAG"), charToRaw("ATA"))
//' lcs(charToRaw("FOOBAR"), charToRaw("BARFOO"))
//'
// [[Rcpp::export]]
int lcs(NumericVector first, NumericVector second, bool trace = false) {
    if (first.size() == 0 | second.size() == 0) return 0;
    
    IntegerVector lengths(first.size() + 1, 0);
    
    for (int j = 1;  j <= second.size(); j++) {
        IntegerVector temp(first.size() + 1, 0);
        
        for (int i = 1; i <= first.size(); i++) {
            if (first[i - 1] == second[j - 1]) {
                temp[i] = lengths[i - 1] + 1;
            }
            
            if (temp[i] < lengths[i]) {
                temp[i] = lengths[i];
            }
            
            if (temp[i] < lengths[i - 1]) {
                temp[i] = lengths[i - 1];
            }
        }
        lengths = temp;
        
    }
    return max(lengths);
}
