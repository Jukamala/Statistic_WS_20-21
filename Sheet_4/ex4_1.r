# Generate N discrete uniform r.v. + MOM/MLE estimates
exercise4.1c <- function(N, L){
    X = ceiling(runif(N, 0, L))
    return(c(2 * mean(X) + 1, max(X), X))
}

Ns = seq(10, 150, 10)
for(L in c(10, 100, 1000)){
  moms = c()
  mles = c()
  for(N in Ns){
    res = exercise4.1c(N, L)
    moms = c(moms, res[1])
    mles = c(mles, res[2])
  }
  vals = c(moms, mles)
  plot(Ns, moms, type="l", col="dodgerblue", lwd=2, ylim=c(min(vals),max(vals)),
       main=sprintf("Estimated L vs true value, L=%d", L), ylab="", xlab="N")
  lines(Ns, mles, col="darkorange", lwd=2)
  abline(h=L, col="red")
  legend("topright", c("MOM", "MLE", "Ground Truth"),
         col=c("dodgerblue", "darkorange", "red"), lwd=2)
}
