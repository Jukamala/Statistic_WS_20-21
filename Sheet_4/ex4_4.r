# Generate N normal r.v. + Bayes/MLE estimates
simu_normal <- function(N, M){
    mles = c()
    mas = c()
    mbs = c()
    for(i in 1:M){
        X = rnorm(N, 0, 1)
        mle = mean(X)
        mles = c(mles, mle)
        mas = c(mas, 2*N/(2*N+1)*mle)
        mbs = c(mbs, 2*N/(2*N+1)*mle + 1/(2*N+1))
    }
    return(matrix(c(mas, mbs, mles), 3, M, byrow=TRUE))
}

Ns = seq(10, 100, 10)
ma_means = c()
mb_means = c()
mle_means = c()
for(N in Ns){
    est = simu_normal(N, 500)
    # Mean of sampled estimates
    ma_means = c(ma_means, mean(est[1,]))
    mb_means = c(mb_means, mean(est[2,]))
    mle_means = c(mle_means, mean(est[3,]))
}
vals = c(ma_means, mb_means, mle_means)
plot(Ns, mle_means, type="l", col="dodgerblue", lwd=2, ylim=c(min(vals),max(vals)),
     main="Average estimates for M=500 samples", ylab="", xlab="N")
lines(Ns, ma_means, col="darkorange", lwd=2)
lines(Ns, mb_means, col="green", lwd=2)
legend("topright", c("MLE", "N(0,2) prior", "N(1,2) prior"),
       col=c("dodgerblue", "darkorange", "green"), lwd=2)

for(N in c(10, 50, 100)){
    est = simu_normal(N, 500)
    plot(density(est[1,]), type="l", col="dodgerblue", lwd=2, ylab="", xlab="",
         main=sprintf("Estimates densities, sampled M=500 times with N=%d values each", N))
    lines(density(est[2,]), col="darkorange", lwd=2)
    lines(density(est[3,]), col="green", lwd=2)
    legend("topright", c("MLE", "N(0,2) prior", "N(1,2) prior"),
           col=c("dodgerblue", "darkorange", "green"), lwd=2)
}
