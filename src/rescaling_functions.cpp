// [[Rcpp::depends(RcppGSL)]]
#include <Rcpp.h>
#include <RcppGSL.h>

using namespace Rcpp;

// -------------------
// Rescaling functions
// -------------------

//' This function rescales a vector of numerical values into the unit interval
//' [0,1] using a C++ implementation for efficiency.
//'
//' @name minmax_scaler
//' @param x a numeric vector, preferably of feature values computed by other functions in sawlog
//' @return x a numeric vector, rescaled into the [0,1] unit interval
//' @author Trent Henderson
//' @export
//' @examples
//' x <- 1 + 0.5 * 1:1000 + arima.sim(list(ma = 0.5), n = 1000)
//' outs <- minmax_scaler(x)
//'
// [[Rcpp::export]]
NumericVector minmax_scaler(NumericVector x) {
  
  int n = x.size();
  double old_min = 0.0;
  double old_max = 0.0;
  double new_min = 0.0;
  double new_max = 1.0;
  NumericVector x_new(n);
  
  // Calculate min
  
  for (int i = 0;  i < n; ++i){
    if (x[i] < old_min){
      old_min = x[i];
    }
  }
  
  // Calculate max
  
  for (int i = 0; i < n; ++i){
    if (x[i] > old_max){
      old_max = x[i];
    }
  }
  
  // Rescale into [0,1] range
  
  for (int i = 0;  i < n; ++i){
    x_new[i] = ((new_max-new_min)/(old_max-old_min))*(x[i]-old_max)+new_max;
  }
  return x_new;
}

//' This function rescales a vector of numerical values into z-scores using a C++
//' implementation for efficiency.
//'
//' @name zscore_scaler
//' @param x a numeric vector, preferably of feature values computed by other functions in sawlog
//' @return x a numeric vector, rescaled into z-score range
//' @author Trent Henderson
//' @export
//' @examples
//' x <- 1 + 0.5 * 1:1000 + arima.sim(list(ma = 0.5), n = 1000)
//' outs <- zscore_scaler(x)
//'
// [[Rcpp::export]]
NumericVector zscore_scaler(NumericVector x) {
  
  int n = x.size();
  double sum = 0.0;
  double mean = 0.0;
  double var = 0.0;
  double sd = 0.0;
  NumericVector x_new(n);
  
  // Calculate sum
  
  for(int i = 0; i < n; ++i){
    sum += x[i];
  }
  
  // Calculate mean
  
  mean = sum/n;
  
  // Calculate variance
  
  for(int i = 0; i < n; ++i){
    var += (x[i]-mean)*(x[i]-mean);
  }
  
  var /= (n-1);
  
  // Calculate standard deviation
  
  sd = sqrt(var);
  
  // Final z-score calculation
  
  for(int i = 0; i < n; ++i){
    x_new[i] = (x[i]-mean)/sd;
  }
  
  return x_new;
}

//' This function rescales a vector of numerical values with a Sigmoidal transformation
//' using a C++ implementation for efficiency.
//'
//' @name sigmoid_scaler
//' @param x a numeric vector, preferably of feature values computed by other functions in sawlog
//' @return x a numeric vector, rescaled into Sigmoidal range
//' @author Trent Henderson
//' @export
//' @examples
//' x <- 1 + 0.5 * 1:1000 + arima.sim(list(ma = 0.5), n = 1000)
//' outs <- sigmoid_scaler(x)
//'
// [[Rcpp::export]]
NumericVector sigmoid_scaler(NumericVector x) {
  
  int n = x.size();
  double sum = 0.0;
  double mean = 0.0;
  double var = 0.0;
  double sd = 0.0;
  NumericVector x_new(n);
  
  // Calculate sum
  
  for(int i = 0; i < n; ++i){
    sum += x[i];
  }
  
  // Calculate mean
  
  mean = sum/n;
  
  // Calculate variance
  
  for(int i = 0; i < n; ++i){
    var += (x[i]-mean)*(x[i]-mean);
  }
  
  var /= (n-1);
  
  // Calculate standard deviation
  
  sd = sqrt(var);
  
  // Perform Sigmoidal transformation
  
  for(int i = 0; i < n; ++i){
    x_new[i] = 1/(1+exp(-((x[i]-mean/sd))));
  }
  
  return x_new;
}

//' This function rescales a vector of numerical values with an outlier-robust
//' Sigmoidal transformation using a C++ implementation for efficiency.
//'
//' @name robustsigmoid_scaler
//' @param x a numeric vector, preferably of feature values computed by other functions in sawlog
//' @return x a numeric vector, rescaled into Sigmoidal range
//' @author Trent Henderson
//' @references B.D. Fulcher and N.S. Jones. hctsa: A computational framework for automated time-series phenotyping using massive feature extraction. Cell Systems 5, 527 (2017).
//' @references B.D. Fulcher, M.A. Little, N.S. Jones Highly comparative time-series analysis: the empirical structure of time series and their methods. J. Roy. Soc. Interface 10, 83 (2013).
//' @export
//' @examples
//' x <- 1 + 0.5 * 1:1000 + arima.sim(list(ma = 0.5), n = 1000)
//' outs <- robustsigmoid_scaler(x)
//'
// [[Rcpp::export]]
NumericVector robustsigmoid_scaler(NumericVector x) {
  
  int n = x.size();
  double median = 0.0;
  int perc_25 = 0;
  int perc_75 = 0;
  double Q1 = 0.0;
  double Q3 = 0.0;
  double iqr = 0.0;
  NumericVector x_new(n);
  
  // Calculate median
  
  if(n % 2 != 0){
    median = x[n/2];
  }
  else{
    median = (x[n/2] + x[(n/2)-1])/2;
  }
  
  // Q1 and Q3
  
  perc_25 = (n-1) * 25 / 100.0;
  Q1 = x[perc_25];
  
  perc_75 = (n-1) * 75 / 100.0;
  Q1 = x[perc_75];
  
  // Calculate interquartile range
  
  iqr = Q3-Q1;
  
  // Perform Sigmoidal transformation
  
  for(int i = 0; i < n; ++i){
    x_new[i] = 1/(1+exp(-((x[i]-median)/(iqr/1.35))));
  }
  
  return x_new;
}

//' This function rescales a vector of numerical values by subtracting the mean using a C++
//' implementation for efficiency.
//'
//' @name mean_scaler
//' @param x a numeric vector, preferably of feature values computed by other functions in sawlog
//' @return x a numeric vector, rescaled into x-mean range
//' @author Trent Henderson
//' @export
//' @examples
//' x <- 1 + 0.5 * 1:1000 + arima.sim(list(ma = 0.5), n = 1000)
//' outs <- mean_scaler(x)
//'
// [[Rcpp::export]]
NumericVector mean_scaler(NumericVector x) {
  
  int n = x.size();
  double sum = 0.0;
  double mean = 0.0;
  NumericVector x_new(n);
  
  // Calculate sum
  
  for(int i = 0; i < n; ++i){
    sum += x[i];
  }
  
  // Calculate mean
  
  mean = sum/n;
  
  // Final scaling
  
  for(int i = 0; i < n; ++i){
    x_new[i] = x[i]-mean;
  }
  
  return x_new;
}
