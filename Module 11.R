setwd("/Users/user/Desktop/R PRO 11")
getwd()
tukey.outlier = function(x) {
  Q1 = quantile(x, 0.25, na.rm = TRUE)
  Q3 = quantile(x, 0.75, na.rm = TRUE)
  IQR = Q3 - Q1
  lower = Q1 - 1.5 * IQR
  upper = Q3 + 1.5 * IQR
  return(x < lower | x > upper)
}

tukey_multiple = function(x) {
  outliers = array(TRUE, dim = dim(x))
  for (j in 1:ncol(x)) {
    outliers[, j] = outliers[, j] && tukey.outlier(x[, j])
  }
  outlier.vec = vector("logical", length = nrow(x))
  for (i in 1:nrow(x)) {
    outlier.vec[i] = all(outliers[i, ])
  }
  return(outlier.vec)
}

set.seed(123)
test_mat = matrix(rnorm(50), nrow = 10)
tukey_multiple(test_mat)

corrected_tukey = function(x) {
  outliers = array(TRUE, dim = dim(x))
  for (j in 1:ncol(x)) {
    outliers[, j] = outliers[, j] & tukey.outlier(x[, j])
  }
  outlier.vec = vector("logical", length = nrow(x))
  for (i in 1:nrow(x)) {
    outlier.vec[i] = all(outliers[i, ])
  }
  return(outlier.vec)
}

corrected_tukey(test_mat)

