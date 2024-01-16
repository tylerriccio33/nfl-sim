#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
DataFrame copy_cols_cpp(DataFrame df1, DataFrame df2, CharacterVector cols) {
  int numCols = cols.size();
  
  for (int i = 0; i < numCols; i++) {
    String colName = cols[i];
    df1[colName] = df2[colName];
  }
  
  return df1;
}
