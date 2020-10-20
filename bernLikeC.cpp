#include <Rcpp.h>
using namespace Rcpp;

//[[Rcpp::export]]
NumericMatrix bernLikeC(NumericMatrix x, NumericMatrix params) {
  int nrow = x.nrow(), ncol = x.ncol();
  int k = params.nrow();
  NumericMatrix out(nrow, k);
  
  for(int i = 0; i < nrow; i++){
    for (int j = 0; j < k; j++){
	  double total = 0;
	  for (int m = 0; m < ncol; m++){
	    if(x(i,m) == 0){
		  total += log(1.0-params(j,m));
		} else{
		  total += log(params(j,m));
		}out(i,j) = total;
	}
	}
	}
	
	return out;
}