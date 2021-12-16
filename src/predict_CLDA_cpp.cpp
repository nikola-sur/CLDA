#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector predict_CLDA_cpp(List mod, NumericMatrix x) {
  // Collect inputs ---
  int n = x.nrow();
  int p = x.ncol();
  NumericVector beta = mod["beta"];
  NumericVector xbar_0 = mod["xbar_0"];
  NumericVector xbar_1 = mod["xbar_1"];
  NumericMatrix Sigma_w = mod["Sigma_w"];
  NumericMatrix x_old = Rcpp::clone(x);
  // arma::mat x = x_old;
  
  // Make predictions
  NumericVector preds(n);
  NumericVector obs(p);
  NumericVector diffs_0(p);
  NumericVector diffs_1(p);
  
  for (int i = 0; i < n; i++) {
    for (int j = 0; j < p; j++) { // Retrieve the i-th row
      obs[j] = x(i, j);
    }
    
    diffs_0 = obs - xbar_0;
    diffs_1 = obs - xbar_1;
    
    preds[i] = 0.0;
  }
  
  
  return preds;
}

