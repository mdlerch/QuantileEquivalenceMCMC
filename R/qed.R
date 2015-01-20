qed <- function(X, prob, epsilon = 0.005, alpha = 0.05)
{
    nchains <- ncol(X)
    result <- numeric(nchains)
    # find the overall quantile
    quant <- quantile(X, prob)
    for (i in 1:nchains)
    {
        # find the chain probability of less than quant
        # this is our uncentered Xbar
        p <- sum(X[ , i] < quant) / nrow(X)
        # use prob for standard deviation
        s <- sqrt(prob * (1 - prob))
        # standardized and centralized Xbar
        # p ~ N(prob, sqrt(prob * (1 - prob) / n))
        # Xbar ~ N(0, 1 / sqrt(n))
        Xbar <- (p - prob) / s
        # epsilon in the transformed space
        epsilon.test <- epsilon / s
        # do test on each chain
        result[i] <- onePeq(Xbar, 1, nrow(X), epsilon.test, alpha)
    }
    return(prod(result))
}
