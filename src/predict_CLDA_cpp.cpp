#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector predict_CLDA_cpp(List mod, arma::mat x) {
  // Collect inputs ---
  int n = x.n_rows;
  int p = x.n_cols;
  arma::vec beta = mod["beta"];
  arma::vec xbar_0 = mod["xbar_0"];
  arma::vec xbar_1 = mod["xbar_1"];
  arma::mat Sigma_w = mod["Sigma_w"];

    
  // Make predictions
  NumericVector preds(n);
  arma::vec obs(p);
  arma::vec diffs_0(p);
  arma::vec diffs_1(p);
  
  for (int i = 0; i < n; i++) {
    for (int j = 0; j < p; j++) { // Retrieve the i-th row
      obs(j) = x(i, j);
    }
    
    diffs_0 = obs - xbar_0;
    diffs_1 = obs - xbar_1;
    
    preds[i] = 0.0;
  }
  
  
  return preds;
}

