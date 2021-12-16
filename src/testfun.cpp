
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
int testfun(int num)
{
  int tot = 0;
  for (int i=1; i <= num; i++) {
    tot += i;
  }
  
  return tot;
}