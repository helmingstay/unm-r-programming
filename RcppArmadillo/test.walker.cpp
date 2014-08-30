// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadilloExtensions/sample.h>

using namespace Rcpp ;

// [[Rcpp::export]]
CharacterVector csample_char( CharacterVector x, 
                              int size,
                              bool replace, 
                              NumericVector prob = NumericVector::create()
                              ) {
  RNGScope scope ;
  CharacterVector ret = RcppArmadillo::sample(x, size, replace, prob);
  return ret ;
}

// [[Rcpp::export]]
NumericVector csample_num( 
    NumericVector x,
    int size,
    bool replace,
    NumericVector prob = NumericVector::create()
) {
  RNGScope scope;
  NumericVector ret = RcppArmadillo::sample(x, size, replace, prob);
  return ret;
}
