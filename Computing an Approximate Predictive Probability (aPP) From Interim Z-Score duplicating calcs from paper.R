
#-------------------------------------------------------------------------------
# Helper Functions
#-------------------------------------------------------------------------------

#' Inverses of the approximation
pp_to_z = function(pp, n, N, alpha=0.025){
  r = n/N
  qnorm(pp)*sqrt(1-r) + qnorm(1-alpha)*sqrt(r)
}


pp_to_p = function(pp, n, N, alpha=0.025){
  z = pp_to_z(pp, n, N, alpha)
  z_to_p(z)
}

pp_to_prob = function(prob, n, N, thr=0.975){
  1 - pp_to_p(prob, n, N, alpha=1-thr)
}

#' Convert p-value or posterior probability to z-score
p_to_z = function(p) qnorm(1-p)

#' Convert z-score to p-value
z_to_p = function(z) pnorm(-z)


z_to_prob = function(z) 1 - z_to_p(z)

 

#' Create an approximate predictive probability from an interim Z-score
#' @param z: z-score 
#' @param n: information in data used to compute the z-score
#' @param N: information when the data are complete.
#' @param alpha: the level of the final analysis (one-sided)
#' @return predictive probability scalar in [0, 1]
z_to_pp = function(z, n, N, alpha=0.025){
  stopifnot(n>0)
  stopifnot(N>=n)
  stopifnot(alpha>=0 & alpha<=1)
  r = n/N
  pnorm((z - qnorm(1-alpha)*sqrt(r))/sqrt(1-r))
}

#' Create an approximate predictive probability from a p-value
#' @param p: one-sided p-value from a frequentest analysis. 
#' @param n: information used to compute the z-score
#' @param N: information when the data are complete.
#' @param alpha: the level of the final analysis (one-sided). 
#'               for Bayes analysis use alpha=1-thr.
#' @return predictive probability scalar in [0, 1]
p_to_pp = function(p, n, N, alpha=0.025) {
  z = p_to_z(p)
  z_to_pp(z, n, N, alpha)
}




ptreat <- 0.264
pcont <- 0.244
N <- 498 

n <- 432
pvalue <- 0.3535
p_to_pp(p=pvalue, n=n, N=1400)

n <- 515
pvalue <- 0.2058
p_to_pp(p=pvalue, n=n, N=1400)

n <- 621
pvalue <- 0.3372
p_to_pp(p=pvalue, n=n, N=1400)

n <- 715
pvalue <- 0.4994
p_to_pp(p=pvalue, n=n, N=1400)

















