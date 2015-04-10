qed <- function(X, prob, epsilon = 0.01, alpha = 0.05)
{
    nchains <- ncol(X)
    n <- nrow(X)
    result <- numeric(nchains)
    # find the overall quantile
    quant <- quantile(X, prob)
    for (i in 1:nchains)
    {
        # X ~ bernoulli(p)
        # phat ~ N(p, sqrt(p (1 - p) / n))
        phat <- sum(X[ , i] < quant) / nrow(X)
        # use prob for standard deviation
        # this is sd of X_i
        s <- sqrt(prob * (1 - prob) / n)
        # standardized and centralized Z
        z <- 1 / s * (phat - prob)
        # move epsilon from phat scale to Xbar scale
        epsilon.tilde <- 1 / s * epsilon
        # do test on each chain
        result[i] <- onePZeq(z, epsilon.tilde, alpha)
    }
    return(prod(result))
}
