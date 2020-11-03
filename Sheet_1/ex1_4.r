# CDF and pseudoinverse for i)
cdf_f <- function(x){
  y <- x 
  y[x < 0] = 1/2*exp(x[x < 0])
  y[0 <= x & x < 1] = 1/2
  y[1 < x] = 1 - 1/2*exp(1-x[1 < x])
  return(y)
}

f_inv <- function(x){
  y <- x
  mask = x <= 1/2
  y[mask] = log(2*x[mask])
  y[!mask] = 1 - log(2-2*x[!mask])
  return(y)
}

# CDF for ii)
cdf_exp <- function(x){
  y <- x
  y[x < 0] = 0
  y[x >= 0] = 1-exp(-2*x[x >= 0])
  return(y)
}

# Generation
gen_f <- function(N){
  U = runif(N)
  return(f_inv(U))
}

gen_exp <- function(N){
  return(rexp(N, 2))
}

for(i in 1:2){
  gen = list(gen_f, gen_exp)[[i]]
  cdf = list(cdf_f, cdf_exp)[[i]]
  par(mfrow=c(2,2))
  for(N in c(500,1000)){
    X = gen(N)
    x = seq(-10,10, length.out=1000)
    # Plot empirical cumulative distribution function against true CDF
    plot(ecdf(X), col="dodgerblue", ylab="CDF", verticals=TRUE, do.points=FALSE,
         main=sprintf("True CDF vs ECDF of Sample (N = %d)", N))
    legend("bottomright", legend=c("ECDF", "True CDF"),
           col=c("dodgerblue", "darkorange"), lwd=2)
    lines(x, cdf(x), col="darkorange")
    # Plot histogram
    hist(X, breaks=14,
         main=c(sprintf("Sample Mean: %.2f | Sample Variance: %.2f",
                        mean(X), var(X))))
  }
  par(oma=c(0,0,2,0))
  title(c("i)", "ii)")[[i]] , outer=TRUE)
}