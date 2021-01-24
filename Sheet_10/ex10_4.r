bin_conf <- function(n, p, alpha){
  # confidence interval for binomial r.v. (from Ex8.1)
  
  low = qbinom(alpha/2, n, p) + 1
  up = qbinom(alpha/2, n, p, lower.tail=FALSE)
  return(c(low, up))
}

wald_test <- function(Ns, M, alpha, lam_0){
  # Estimate error of type I for different N
  
  q = qchisq(1 - alpha, 1)
  sizes = c()
  for(n in Ns){
    size = 0
    for(m in 1:M){
      # Wald test
      data = rpois(n, lam_0)
      lam = mean(data)
      W = n * (lam - lam_0)^2 / lam
      if(W >= q){
        # rejection
        size = size + 1/M
      }
    }
    sizes = c(sizes, size)
  }
  
  plot(Ns, 100 * sizes, type="l", xlab="N", ylab="Error of Type I [ in %]", col="dodgerblue",
       main=sprintf("Wald test (alpha = %d %%)", 100 * alpha))
  # M * size(n) ~ Bin(M, pI(n)), because we have M independent tests which are rejected with p=pI(n).
  # pI(n) -> alpha as n -> inf, so we can construct a confidence interval for
  # the estimator size(n) when pI(n) is close to alpha.
  conf = bin_conf(M, alpha, 0.05)
  abline(h=100 * alpha, col="darkorange")
  abline(h=100 * conf[1] / M, col="darkorange", lty=3)
  abline(h=100 * conf[2] / M, col="darkorange", lty=3)
  legend("topright", col=c("dodgerblue", "darkorange"), lty=c(1,3),
         legend=c("estimate for the size of the test",
                  "95% conf. interval for esimator \n when size of the test is alpha"))
  # We see that for N >= 50 the estimated size of the test is close to alpha.
  # Looking at the confidence interval, we can see that for N >= 50 the estimator behaves like
  # we would expect under the assumption that the test has size alpha.
  
}
 
set.seed(10) 
wald_test(seq(5, 200, 5), 10000, 0.05, 1)