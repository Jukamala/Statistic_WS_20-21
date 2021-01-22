analyze_data <- function(data){

  # Exploratory analysis shows that data might be a mixture of two distributions,
  # one with values in [0,10] and the other one which looks gaussian with mean ~25.
  # Because we excpect small values the latter one is the contamination.
  par(mfrow=c(2,2))
  plot(ecdf(data), col="dodgerblue", pch=20)
  hist(data)
  hist(data[data < 10])
  hist(data[data >= 10])
  
  # Only uncorupted data
  data = data[data < 10]
  n = length(data)
  
  # MLEs for lognormal and exp distributions
  log_mean = mean(log(data))
  log_sd = sd(log(data))
  exp_lam = 1 / mean(data)
  
  # Samples of the distributions (we used the quantiles to get a better, representative sample)
  # set.seed(10)
  # log_data = exp(rnorm(n, mean=log_mean, sd=log_sd))
  # exp_data = rexp(n, rate=exp_lam)
  levels = seq(0, 1, length.out=n+2)[1:n+1]
  log_q = exp(qnorm(levels, mean=log_mean, sd=log_sd))
  exp_q = qexp(levels, rate=exp_lam)
  
  # Visual analysis shows that the lognormal data fits a bit better to the data,
  # although both seem to be a good fit
  par(mfrow=c(2,2))
  # QQ-Plots
  qqplot(data, exp_q, col="dodgerblue", pch=20, main="Exponential data", ylab="QQ-Plot")
  abline(a=0, b=1, col="grey")
  qqplot(data, log_q, col="dodgerblue", pch=20, main="Lognormal data", ylab="")
  abline(a=0, b=1, col="grey")
  # (overlapping) histograms
  bins = seq(0, ceiling(max(c(data, log_q, exp_q))), 0.5)
  hist(exp_q, col=rgb(1,0,0,0.5), breaks=bins, main="", xlab="", ylab="Overlapping Histrogram")
  hist(data, col=rgb(0,0,1,0.5), breaks=bins, add=TRUE)
  legend("topright", legend=c("Exponential", "data"),
         col=c(rgb(1,0,0), rgb(0,0,1)), pch=c(15,15), cex=c(1,1))
  hist(log_q, col=rgb(1,0,0,0.5), breaks=bins, main="", xlab="", ylab="")
  hist(data, col=rgb(0,0,1,0.5), breaks=bins, add=TRUE)
  legend("topright", legend=c("Lognormal", "data"),
         col=c(rgb(1,0,0), rgb(0,0,1)), pch=c(15,15), cex=c(1,1))
  
  # chi-square goodness-of-fit test
  ind = c(0)
  Zs = c()
  p_logs = c()
  p_exps = c()
  Z = 0
  p_log = 0
  p_exp = 0
  for(i in seq(0, 9.5, 0.5)){
    # Extend interval
    p_log = p_log + pnorm(log(i+1), mean=log_mean, sd=log_sd) - pnorm(log(i), mean=log_mean, sd=log_sd)
    p_exp = p_exp + pexp(i+1, rate=exp_lam) - pexp(i, rate=exp_lam)
    Z = Z + sum(data >= i & data < i + 1)
    # Rule of thumb: np > 5
    if(n * min(p_exp, p_log) > 5 || i == 9.5){
      ind = c(ind, i + 0.5)
      Zs = c(Zs, Z)
      p_logs = c(p_logs, n * p_log)
      p_exps = c(p_exps, n * p_exp)
      Z = 0
      p_log = 0
      p_exp = 0
    }
  }
  
  # Output results
  cat("interval  |")
  for(i in 2:length(ind)){
    cat(sprintf(" [%.1f, %.1f]", ind[i-1], ind[i]))
  }
  cat("\n")
  caption = c("Z        ", "np (logn)", "np (exp) ")
  vals = list(Zs, p_logs, p_exps)
  for(i in 1:3){
    cat(caption[i], "| ")
    for(v in vals[i]){
      cat(sprintf("%10.2f", v))
    }
    cat("\n")
  }
  cat("\n")
  
  # test statistic
  chi_log = 0
  chi_exp = 0
  N = length(Zs)
  for(i in 1:N){
    chi_log = chi_log + (Zs[i] - p_logs[i])^2 / p_logs[i]
    chi_exp = chi_exp + (Zs[i] - p_exps[i])^2 / p_exps[i]
  }
  
  test <- function(label, chi, q){
    qu = qchisq(0.95, N-q-1)
    p = pchisq(chi, N-q-1)
    cat(sprintf("%s chi = %5.2f, quantile = %5.2f, p-value = %4.2f\n", label, chi, qu, p))
    if(chi < qu){
      cat("No evidence for unfitness of distribution.\n")
    }
    else{
      cat("Null rejected, evidence sugests distribution does not match.\n")
    }
  }
  
  test("logn:", chi_log, 2)
  test("exp: ", chi_exp, 1)
  
    
}

data = read.csv("Sheet_10/measurement.csv")$data
analyze_data(data)