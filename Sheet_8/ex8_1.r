bin_q <- function(n, beta, alpha){
  # P(#{X_k <= Q_beta} < low) >= alpha/2
  low = qbinom(alpha/2, n, beta) + 1
  # P(#{X_k <= Q_beta} > up) >= alpha/2
  up = qbinom(alpha/2, n, beta, lower.tail=FALSE)
  # P(low <= #{X_k <= Q_beta} <= up) >= 1 - alpha
  level = pbinom(up, n, beta) - pbinom(low-1, n, beta)
  cat(sprintf("Confidence interval for %.2f-Quantile:\n", beta))
  cat(sprintf("[%d, %d] - level: %.2f %%", low, up, 100 * level))
}

bin_q(200, 0.5, 0.05)