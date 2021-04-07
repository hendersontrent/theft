#include <Rcpp.h>

using namespace Rcpp;

// -------
// Helpers
// -------

// Min of a vector

NumericVector minC(NumericVector x, bool narm = true){
  int n = x.size();
  LogicalVector index = is_na(x);
  NumericVector omin(1);
  bool na_check = false;
  bool na_check_all = true;

  for(int i = 0; i<n; i++){
    if (index[i]) na_check = true;
  }

  for(int i = 0; i<n; i++){
    if (!index[i]) na_check_all = false;
  }

  if (narm) {
    for(int i = n-1; i >= 0; i--){
      if (!index[i]) omin[0] = x[i];
    }

    for(int i = 1; i < n; i++) {
      if (!index[i]) omin[0] = std::min(x[i], omin[0]);
    }

    if (na_check_all) {
      omin[0] = NA_REAL;
    }
  } else if (na_check) {
    omin[0] = NA_REAL;
  } else {
    omin[0] = x[0];
    for(int i = 1; i < n; i++){
      omin = std::min(x[i], omin[0]);
    }
  }

  return omin;
}

// Max of a vector

NumericVector maxC(NumericVector x, bool narm = true){
  int n = x.size();
  LogicalVector index = is_na(x);
  NumericVector omax(1);
  bool na_check = false;
  bool na_check_all = true;

  for(int i = 0; i<n; i++){
    if (index[i]) na_check = true;
  }

  for(int i = 0; i<n; i++){
    if (!index[i]) na_check_all = false;
  }

  if (narm) {
    for(int i = n-1; i >= 0; i--){
      if (!index[i]) omax[0] = x[i];
    }

    for(int i = 1; i < n; i++) {
      if (!index[i]) omax[0] = std::max(x[i], omax[0]);
    }

    if (na_check_all) {
      omax[0] = NA_REAL;
    }
  } else if (na_check) {
    omax[0] = NA_REAL;
  } else {
    omax[0] = x[0];
    for(int i = 1; i < n; i++){
      omax = std::max(x[i], omax[0]);
    }
  }

  return omax;
}

// Mean of a vector

NumericVector meanC(NumericVector x, bool narm = true){
  int n = x.size();
  LogicalVector index = is_na(x);
  bool na_check = false;
  bool na_check_all = true;
  int n_corrected = 0;
  NumericVector out(1);

  for(int i = 0; i<n; i++){
    if (index[i]) na_check = true;
  }

  for(int i = 0; i<n; i++){
    if (!index[i]) na_check_all = false;
  }

  for(int i = 0; i<n; i++){
    if (!index[i]) n_corrected++;
  }

  /* narm = T */
  if (narm){
    if (na_check_all) {
      out[0] = NA_REAL;
    }  else {
      out[0] = 0;
      for(int i = 0; i < n; ++i) {
        if (!index[i]) out[0] += x[i] / n_corrected;
      }
    }
  }

  /* narm = F */
  if (!narm){
    if (na_check) {
      out[0] = NA_REAL;
    } else {
      for(int i = 0; i < n; ++i) {
        out[0] += x[i] / n;
      }
    }
  }

  return out;
}


// --------------------
// Rescaling functions
// --------------------

//' This function rescales a vector of numerical values into the unit interval
//' [0,1] using a C++ implementation for efficiency.
//'
//' @param x a numeric vector, preferably of feature values computed by other catchEmAll package functions
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
  double new_min = 0.0;
  double new_max = 1.0;
  NumericVector x_new(n);

  // Calculate min and max

  NumericVector old_min = minC(x);
  NumericVector old_max = maxC(x);
  double old_min_vec = old_min[0];
  double old_max_vec = old_max[0];

  // Rescale into [0,1] range

  for (int i = 0;  i < n; ++i){
    if(x[i] == NA_REAL){
      x_new[i] = NA_REAL;
    } else{
      x_new[i] = ((new_max-new_min)/(old_max_vec-old_min_vec))*(x[i]-old_max_vec)+new_max;
    }
  }
  return x_new;
}

//' This function rescales a vector of numerical values into z-scores using a C++
//' implementation for efficiency.
//'
//' @param x a numeric vector, preferably of feature values computed by other catchEmAll package functions
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
  double var = 0.0;
  double sd = 0.0;
  NumericVector x_new(n);

  // Calculate mean

  NumericVector mu = meanC(x);
  double the_mean = mu[0];

  // Calculate variance

  for(int i = 0; i < n; ++i){
    if(x[i] == NA_REAL){
      continue;
    } else{
      var += (x[i]-the_mean)*(x[i]-the_mean);
    }
  }

  var /= (n-1);

  // Calculate standard deviation

  sd = sqrt(var);

  // Final z-score calculation

  for(int i = 0; i < n; ++i){
    if(x[i] == NA_REAL){
      x_new[i] = NA_REAL;
    } else{
      x_new[i] = (x[i]-the_mean)/sd;
    }
  }

  return x_new;
}

//' This function rescales a vector of numerical values with a Sigmoidal transformation
//' using a C++ implementation for efficiency.
//'
//' @param x a numeric vector, preferably of feature values computed by other catchEmAll package functions
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
  double var = 0.0;
  double sd = 0.0;
  NumericVector x_new(n);

  // Calculate mean

  NumericVector mu = meanC(x);
  double the_mean = mu[0];

  // Calculate variance

  for(int i = 0; i < n; ++i){
    if(x[i] == NA_REAL){
      continue;
    } else{
      var += (x[i]-the_mean)*(x[i]-the_mean);
    }
  }

  var /= (n-1);

  // Calculate standard deviation

  sd = sqrt(var);

  // Perform Sigmoidal transformation

  for(int i = 0; i < n; ++i){
    if(x[i] == NA_REAL){
      x_new[i] = NA_REAL;
    } else{
      x_new[i] = 1/(1+exp(-((x[i]-the_mean/sd))));
    }
  }

  // Rescale into unit interval

  x_new = minmax_scaler(x_new);

  return x_new;
}

//' This function rescales a vector of numerical values with an outlier-robust
//' Sigmoidal transformation using a C++ implementation for efficiency.
//'
//' @param x a numeric vector, preferably of feature values computed by other catchEmAll package functions
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
  NumericVector x1 = wrap(na_omit(x));
  int n1 = x1.size();
  double median = 0.0;
  int perc_25 = 0;
  int perc_75 = 0;
  double Q1 = 0.0;
  double Q3 = 0.0;
  double iqr = 0.0;
  NumericVector x_new(n);

  // Calculate median

  std::sort(x1.begin(), x1.end());

  if(n % 2 != 0.0){
    median = x1[n1/2];
  } else{
    median = (x1[n1/2] + x1[n/2-1])/2;
  }

  // Q1 and Q3

  perc_25 = (n1-1) * 25 / 100.0;
  Q1 = x1[perc_25];

  perc_75 = (n1-1) * 75 / 100.0;
  Q1 = x1[perc_75];

  // Calculate interquartile range

  iqr = Q3-Q1;

  // Perform Sigmoidal transformation

  for(int i = 0; i < n; ++i){
    if(x[i] == NA_REAL){
      x_new[i] = NA_REAL;
    } else{
      x_new[i] = 1/(1+exp(-((x[i]-median)/(iqr/1.35))));
    }
  }

  // Rescale into unit interval

  x_new = minmax_scaler(x_new);

  return x_new;
}

//' This function rescales a vector of numerical values by subtracting the mean using a C++
//' implementation for efficiency.
//'
//' @param x a numeric vector, preferably of feature values computed by other catchEmAll package functions
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
  NumericVector x_new(n);

  // Calculate mean

  NumericVector mu = meanC(x);
  double the_mean = mu[0];

  // Final scaling

  for(int i = 0; i < n; ++i){
    if(x[i] == NA_REAL){
      x_new[i] = NA_REAL;
    } else{
      x_new[i] = x[i]-the_mean;
    }
  }

  return x_new;
}
