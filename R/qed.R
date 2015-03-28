qed <- function(X, prob, epsilon = 0.005, alpha = 0.05)
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
        s <- sqrt(prob * (1 - prob))
        # standardized and centralized Xbar
        # phat ~ N(p, sqrt(prob * (1 - prob) / n))
        # Xbar = sqrt(n/(pq)) phat
        # Xbar ~ N(sqrt(n/(pq))p, 1)
        Xbar <- phat * sqrt(n) / s
        # center Xbar about null value
        Xbar <- Xbar - sqrt(n) / s * prob
        # move epsilon from phat scale to Xbar scale
        epsilon.test <- epsilon * sqrt(n) / s
        # do test on each chain
        result[i] <- onePZeq(Xbar, epsilon.test, alpha)
    }
    return(prod(result))
}
