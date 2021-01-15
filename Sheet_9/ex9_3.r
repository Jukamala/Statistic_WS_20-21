std_test <- function(std1, std2, n1, n2, alpha){
  F = std1^2 / std2^2
  q = qf(1 - alpha/2, n1-1, n2-1)
  
  cat(sprintf("F = %.2f, quantile = %.2f\n", F, q))
  if(F < q){
    cat("Evidence suggests different variances.")
  }
  else{
    cat("No evidence for different variances.")
  }
  
}

std_test(6.73, 9.47, 61, 61, 0.1)