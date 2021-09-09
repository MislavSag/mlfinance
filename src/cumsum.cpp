#include <Rcpp.h>
#include <vector> 
using namespace Rcpp;

//'   Returns a vector whose elements are the cumulative sums of the inputted
//'   numeric vector. The argument 'threshold' controls the max/min 
//'   ceiling/floor of the running sum before it is reset; i.e., if the 
//'   threshold is set to (+/-) Inf, one would, essentially, compute the base R 
//'   'cumsum', albeit much slower
//' 
//' @param x          A numeric vector
//' @param threshold  A numeric scalar specifying the cumulative 
//'                   threshold(reset)
//'
//' @return           A named numeric vector whose names correspond to the 
//'                   group of summed values before the specified threshold
// [[Rcpp::export]]
Rcpp::NumericVector cumsum_reset(Rcpp::NumericVector x, double threshold){

  int group = 0; 
  double runsum = 0;
  Rcpp::NumericVector value;
  std::vector<int> groups;

  for(int i = 0; i < x.size(); i++){
    runsum += x[i];
    if(runsum > threshold){
      runsum = x[i];
      group += 1;
    }
    groups.push_back(group);
    value.push_back(runsum);
  }
  value.attr("names") = groups;
  return value;
}


