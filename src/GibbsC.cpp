#include <Rcpp.h>
using namespace Rcpp;

//' @title A Gibbs sampler using Rcpp
//' @description A Gibbs sampler using Rcpp
//' @param x0 float, first component of the start point
//' @param y0 float, second component of the start point
//' @param n int, the number of samples to be generated
//' @return a matrix of shape n*2, 
//' @examples
//' \dontrun{
//' GibbsC(0,0,1000)
//' }
//' @export
// [[Rcpp::export]]
NumericMatrix GibbsC(double x0, double y0, int n) {
  NumericMatrix mat(n, 2);
  double x = x0;
  double y = y0;
  double sig = sqrt(0.19);
  for(int i = 0; i < n; i++) {
    x = rnorm(1, 0.9*y, sig)[0];
    y = rnorm(1, 0.9*x, sig)[0];
    mat(i, 0) = x;
    mat(i, 1) = y;
  }
  return(mat);
}