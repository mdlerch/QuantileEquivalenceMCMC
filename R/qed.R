qed <- function(X, prob = NULL, quant = NULL, epsilon = 0.01, alpha = 0.05)
{
    # check input
    if (is.null(prob) & is.null(quant))
    {
        stop("Must provide one of prob or quant")
    }
    else if (!is.null(prob) & !is.null(quant))
    {
        stop("Provide only one of prob or quant")
    }
    if (alpha >= 1 | alpha <= 0)
    {
        stop("alpha must be between 0 and 1")
    }
    if (!is.null(prob) & (prob >= 1 | prob <= 0))
    {
        stop("prob must be between 0 and 1")
    }


    # _quantile_ or _probability_ problem?
    if (is.null(prob))
    {
        prob <- sum(X < quant) / length(X)
    }

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
        result[i] <- eq_test_one_param(z, epsilon.tilde, alpha)
    }
    return(as.integer(prod(result)))
}
