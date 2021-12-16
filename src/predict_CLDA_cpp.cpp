#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector predict_CLDA_cpp(List mod, arma::mat x) {
  // Collect inputs ---
  int n = mod["n"];
  int n_0 = mod["n_0"];
  int n_1 = mod["n_1"];
  int n_test = x.n_rows;
  int p = x.n_cols;
  arma::vec beta = mod["beta"];
  arma::vec xbar_0 = mod["xbar_0"];
  arma::vec xbar_1 = mod["xbar_1"];
  arma::mat Sigma_w = mod["Sigma_w"];

    
  // Make predictions
  double beta_obs_0;
  double beta_obs_1;
  double inv_val;
  double score_0;
  double score_1;
  IntegerVector preds(n_test);
  arma::vec obs(p);
  arma::vec diffs_0(p);
  arma::vec diffs_1(p);
  arma::rowvec beta_Sigma(p);

  //   
  // for (int i = 0; i < n_test; i++) {
  //   for (int j = 0; j < p; j++) { // Retrieve the i-th row
  //     obs(j) = x(i, j);
  //   }
  //   
  //   diffs_0 = obs - xbar_0;
  //   diffs_1 = obs - xbar_1;
  //   beta_Sigma = beta.t() * Sigma_w;
  //   inv_val = 1.0/as_scalar(beta_Sigma * beta);
    
  //   beta_obs_0 = arma::dot(beta.t(), diffs_0);
  //   beta_obs_1 = arma::dot(beta.t(), diffs_1);
  //   
  //   score_0 = beta_obs_0 * inv_val * beta_obs_0 - 2.0 * log(n_0/n);
  //   score_1 = beta_obs_1 * inv_val * beta_obs_1 - 2.0 * log(n_1/n);
  //   
  //   if (score_0 < score_1) {
  //     preds[i] = 0;
  //   } else {
  //     preds[i] = 1;
  //   }
  // }
  
  
  return preds;
}

