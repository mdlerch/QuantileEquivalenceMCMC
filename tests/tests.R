library(QuantileEquivalenceMCMC)
set.seed(149)
y <- cbind(rnorm(1000), rnorm(1000), rnorm(1000))
qed(y, prob = 0.95)

QuantileEquivalenceMCMC:::
