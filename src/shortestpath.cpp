#include <Rcpp.h>
#include <queue>  
#include <set>

//' Given binary adjacency matrix return shortest path between vertices
//' 
//' @param G integer matrix
//' @param x integer source
//' @param y integer sink
//' @return shortest path integer
//' 
// [[Rcpp::export]]
int shortestpath(Rcpp::IntegerMatrix G, int x, int y) {
    if (G.nrow() != G.ncol()) Rcpp::stop("G should be square");
    if (x < 0 | x > G.nrow()) Rcpp::stop("x out of range");
    if (y < 0 | y > G.nrow()) Rcpp::stop("y out of range");
    
    // We are already in the sink
    if (x == y) return 0;
    
    // Init set of visited na queue
    std::set<int> visited;
    visited.insert(x);
    std::queue<std::pair<int, int> > queue;
    queue.push(std::pair<int, int>(x, 0));
    
    while(!queue.empty()) {
        // Take first from queue
        std::pair<int, int> current = queue.front();
        queue.pop();
        
        // Check if sink
        if(current.first == y) return current.second;
        
        // Add all neighbours to queue
        for (int j = 0; j < G.ncol(); j++) {
            if (G(current.first, j) == 1 & visited.count(j) == 0) {
                queue.push(std::pair<int, int>(j, current.second + 1));
                visited.insert(j);
            }
        }
        
    }
    
    // We couldn't reach the sink node
    return R_PosInf;
}
