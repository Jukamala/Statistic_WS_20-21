indep_test <- function(data, alpha){
  rs = rowSums(data)
  cs = colSums(data)
  N = sum(data)
  #  Contingency tables
  cat(sprintf("%3d %3d | %4d \n%3d %3d | %4d \n--------|---- \n%3d %3d | %4d\n\n",
              data[1, 1], data[1,2], rs[1], data[2, 1], data[2,2], rs[2], cs[1], cs[2], N))
  # test statistic
  chi = 0
  for(i in 1:length(rs)){
    for(j in 1:length(cs)){
      ex = rs[i]*cs[j]/N
      chi = chi + (data[i, j] - ex)^2 / ex
    }
  }
  q = qchisq(1-alpha, 1)
  cat(sprintf("chi = %.2f, quant = %.2f\n", chi, q))
  if(chi < q){
    cat("No evidendce for dependence.")
  }
  else{
    cat("Independence rejected, evidence suggests dependence.")
  }
  
}

indep_test(matrix(c(366, 178, 429, 255), nrow=2, byrow=TRUE), 0.05)
indep_test(matrix(c(452, 92, 520, 164), nrow=2, byrow=TRUE), 0.05)