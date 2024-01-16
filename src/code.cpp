#include <Rcpp.h>
using namespace Rcpp;

// Define the vectorized between function
// x: NumericVector
// left: double
// right: double
// Returns a LogicalVector
LogicalVector between_vectorized(NumericVector x, double left, double right) {
  int n = x.size();
  LogicalVector result(n);

  for (int i = 0; i < n; i++) {
    result[i] = x[i] >= left && x[i] <= right;
  }

  return result;
}

// [[Rcpp::export]]
LogicalVector between_cpp(NumericVector x, double left, double right) {
  return between_vectorized(x, left, right);
}
