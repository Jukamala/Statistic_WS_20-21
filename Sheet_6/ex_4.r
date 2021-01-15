p0 = 0.03
n = 500
Y = 21
gamma = 0.95

p_hat = Y/n
delta = qnorm((1+gamma)/2) * sqrt(p_hat*(1-p_hat)/n)
c1 = p_hat - delta
c2 = p_hat + delta

cat(sprintf("%.2f-confidence interval of p: [%.2f%%, %.2f%%]\n",
            gamma, 100 * c1, 100 * c2))
cat(sprintf("This does %scontain the initial p0 = %.2f%%\n",
    c("not ", "")[1 + (c1 <= p0 && c2 >= p0)], 100 * p0))