#' ---
#' title: "Functions for Sample Size Estimation"
#' author: "MS Patterson, matthewpatterson@usda.gov; Paolo Arevalo"
#' date: "`r format(Sys.time(), '%B %d, %Y')`"
#' output: github_document
#' ---
#'

#' ### Required packages
#+ Packages
library(tidyverse)

#' ### Functions related to sample size determination
#' The following sample size calculation functions are derived from the work of
#' _Foody, G. M. (2009). Sample size determination for image classification
#' accuracy assessment and comparison. International Journal of Remote Sensing,
#' 30(20), 5273-5291._ https://doi.org/10.1080/01431160903130937 
#'
#' Each of the three sample size caclulation functions relates to a particular
#' approach. `genSample1()` is an implementation of the typical sample size
#' calcuation, using only the target accuracy (p0), the half-width of the
#' Confidence Interval (h), and the tolerance for Type I error (alpha).
#'
#' `genSample2()` is used when it is desired to be able to reliably test for a
#' result being a certain distance from the target accuracy. It requires the
#' minimum detectable difference (min_diff), and also that tolerance for Type II
#' errors be specified (beta).
#'
#' `genSample3()` is used when a confidence interval is of more interest than
#' testing against a target accuracy. See eq. 22-25. This function requires the
#' specification of the target or expected accuracy (p0), alpha and beta, and
#' the difference in accuracy that can be detected with a power of 1 - beta (d).
#'
#' The function `contCorrect()` performs a continuity correction on sample size
#' estimates. A continuity correction may be necessary when using equations that
#' assume a continious distribution (samples are discrete), which results in a 
#' slight underestimate of the necessary sample size. It is most appropriate to 
#' apply to the estimate of sample size produced by `genSample2()`.
#' 
#' The function `powerEst()` can be used to estimate the power of a sample size,
#' given the minimum difference to be detected (min_diff), the expected accuracy (p0), 
#' and alpha.

#+ SampleSizeCode
genSample1 <- function(p0 = 0.85, h = 0.01, alpha = 0.05){
  # convert the input probalities into z scores
  z_alpha <- qnorm((1 - alpha/2))
  # calculate n_prime sample size
  n <- z_alpha^2 * (p0 * (1 - p0)) / (h^2)
  
  return(round(n))
}

genSample2 <- function(p0 = 0.85, min_diff = 0.01, alpha = 0.05, beta = 0.2){
  # convert the input probalities into z scores
  z_alpha <- qnorm((1 - alpha))
  z_beta <-  qnorm((1 - beta))
  
  # calculate the actual n prime estimate 
  p1 <- p0 - min_diff
  noom <- (z_alpha * sqrt(p0 * (1 - p0))) + (z_beta * sqrt(p1 * (1 - p1)))
  n <- (noom / min_diff)^2
  
  return(round(n))
}


genSample3 <-  function(p0 = 0.85, d = 0.1, alpha = 0.05, beta = 0.2){
  z_alpha <- qnorm((1 - alpha/2))
  z_beta <-  qnorm((1 - beta))
  n <- (2 * p0 * (1 - p0) * (z_alpha + z_beta)^2) / d^2
  
  return(round(n))
}

contCorrect <- function(n_prime, min_diff = 0.01, p0 = 0.85){
  n <- (n_prime / 4) * (1 + sqrt(1 + 2 / (n_prime * min_diff)))^2
  
  return(round(n))
}

powerEst <- function(n, min_diff, p0, alpha=0.05){
  p1 <- p0 - min_diff
  z_alpha <- qnorm((1 - alpha) + 0.025)
  noom <- sqrt(n) * min_diff - (1 / (2 * sqrt(n))) - z_alpha * sqrt(p0 * min_diff)
  denoom <- sqrt(p1 * (1 - p1))
  pow <- pnorm(noom/denoom)
  
  return(pow)
}

#' ### Functions related to sample size optimization

#' Function to obtain a confusion matrix based on two vectors of equal length
#' Output matrix is square and has labels resulting from the union of the
#' unique values of the two vectors. First vector is rows, second is columns.
#' 
#' **v1** First vector of integer values
#' 
#' **v2** Second vector of integer values
#' 
#' **code_levels** List of integers with all the factors to be used as levels
#' 
#' returns **ctab** Crosstabulation of the two vectors

calc_ct <- function(v1, v2, code_levels){
  if(missing(code_levels)){
    l1 <-  unique(v1)  
    l2 <-  unique(v2)
    code_levels <-  sort(union(l1,l2))
    f1 <-  factor(v1, levels=code_levels)
    f2 <-  factor(v2, levels=code_levels)
    ct <-  table(f1, f2)
  } else { 
    f1 <-  factor(v1, levels=code_levels)
    f2 <-  factor(v2, levels=code_levels)
    ctab <-  table(f1, f2)
  }
  
  return(ctab)
}


#' Function to calculate optimal sample size for an expected confusion matrix
#' Taken from the Excel spreadsheet provided in: 
#' _Wagner, J. E., & Stehman, S. V. (2015). Optimizing sample size allocation to
#' strata for estimating area and map accuracy. Remote Sensing of Environment, 
#' 168, 126–133._ https://doi.org/10.1016/j.rse.2015.06.027
#' Test data taken from the 4x4 spreadsheet
#' 
#' **cm**: Confusion matrix as a dataframe, in proportions. Must be square. 
#' Classes in rows and cols must be in the same order.
#' 
#' **i**: Column/row number to be used as the target for the estimation
#' 
#' **n**: Total sample size
#' 
#' Returns the optimized sample size for the class of interest, and associated
#' standard errors. 

optimal_sample_alloc <-  function(cm, i, n){
  if(1/sum(cm) < 0.99){stop("Confusion matrix does not add to 1")}
  
  # Calc variances and sort them according to position of
  # class of interest (i.e. 'i')
  csum <- sum(cm[,i])
  var_user <- (cm[i,i] * (sum(cm[i,-i]))) / (sum(cm[i,]))^2
  
  var_prod_class <- (sum(cm[-i,i])^2 * cm[i,i] * (sum(cm[i, -i]))) / csum^4
  var_prod_others <- (cm[i,i]^2 * cm[-i,i] * rowSums(cm[-i,-i])) / csum^4
  
  var_prod <-  vector(length = length(cm))
  var_prod[i] <- var_prod_class
  var_prod[-i] <- var_prod_others
  
  var_parea <- cm[,i] * (rowSums(cm[, -i]))
  
  # Calculate K's and min K =! 0
  k <- vector(length = length(cm))
  k[-i] <- var_prod[-i] + var_parea[-i]
  k[i] <- var_user + var_prod[i] + var_parea[i]
  mink <- min(k[k>0])
  
  # Calc theoretical smallest sample size
  nhat <- (k / mink)^0.5
  
  # Calc optimal sample size
  nh <- round((nhat / sum(nhat)) * n)
  
  # Calc standard errors
  if (nh[i] == 0){
    se_user = 0
  } else {
    se_user <- sqrt(var_user / nh[i])
  }
  
  se_prod <- sum(var_prod[nh > 0] / nh[nh > 0])^0.5
  se_parea <- sum(var_parea[nh > 0] / nh[nh > 0])^0.5
  
  out <- list(sample_alloc=nh, 
             se_user=se_user,
             se_prod=se_prod,
             se_parea=se_parea)
  
  return(out)
}

#' ### Example Optimization
#' 
#' Tests data below is drawn from the spreadsheet distributed in association
#' with the Wagner & Stehman paper. 

# Test data for the previous function. 
testcm <-  data.frame(c(0.01400, 0.00100, 0.00200, 0.00400), 
                    c(0.00000,0.00900, 0.00000, 0.00200),
                    c(0.00300, 0.00250, 0.28800, 0.02500),
                    c(0.00300, 0.00250, 0.03000, 0.61400))
colnames(testcm) <-  c("1", "2", "3", "4")

# Run the example
optimal_sample_alloc(testcm, i = 1, n = 1000)