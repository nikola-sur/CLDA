#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector predict_CLDA_cpp(List mod, NumericMatrix x) {
  // Collect inputs ---
  NumericVector beta = mod["beta"];
  NumericMatrix Sigma_w = mod["Sigma_w"];
  NumericVector xbar_0 = mod["xbar_0"];
  NumericVector xbar_1 = mod["xbar_1"];
  int n = x.nrow();
  
  
  // Make predictions
  NumericVector preds(n);
  
  for (int i = 0; i < n; i++) {
    preds[i] = 0.0;
  }
  
  
  return preds;
}

