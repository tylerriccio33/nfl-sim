#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List slice_cpp(DataFrame df, LogicalVector logical_vector) {
  int n = df.nrows();
  LogicalVector subset_logical(n);

  for (int i = 0; i < n; ++i) {
    subset_logical[i] = logical_vector[i];
  }

  DataFrame filtered_df = df[subset_logical, _];
  return List::create(filtered_df["A"], filtered_df["B"]);
}

