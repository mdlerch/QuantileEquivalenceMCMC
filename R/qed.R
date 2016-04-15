qed <- function(chains, prob, quant, epsilon = 0.01, alpha = 0.05)
{
    # check input
    if (missing(prob) & missing(quant))
    {
        stop("Must provide one of prob or quant")
    }
    else if (!missing(prob) & !missing(quant))
    {
        stop("Provide only one of prob or quant")
    }
    if (alpha >= 1 | alpha <= 0)
    {
        stop("alpha must be between 0 and 1")
    }
    if (missing(quant))
    {
        if (prob >= 1 | prob <= 0)
        {
            stop("prob must be between 0 and 1")
        }
    }

    # TODO: check if chains are matrix or convertible to mcmc.list
    if (is.matrix(chains))
    {
        out <- qedtest(chains, prob, quant, epsilon, alpha)
    } else {
        stop("mcmc.list compatibility is on the way")
        # chains <- as.mcmc.list(chains)
        # x <- lapply(x, as.matrix)
        # S2 <- array(sapply(x, var, simplify=TRUE), dim=c(Nvar,Nvar,Nchain))
        # W <- apply(S2, c(1,2), mean)
        # xbar <- matrix(sapply(x, apply, 2, mean, simplify=TRUE), nrow=Nvar,
        #              ncol=Nchain)
        # B <- Niter * var(t(xbar))
    }

    out
}

qedtest <- function(chains, prob, quant, epsilon = 0.01, alpha = 0.05)
{
    # _quantile_ or _probability_ problem?
    if (missing(prob))
    {
        prob <- sum(chains < quant) / length(chains)
    }

    nchains <- ncol(chains)
    if (nchains < 2) stop("Need at least two chains")
    n <- nrow(chains)
    result <- numeric(nchains)
    # find the overall quantile
    quant <- quantile(chains, prob)
    for (i in 1:nchains)
    {
        # chains ~ bernoulli(p)
        # phat ~ N(p, sqrt(p (1 - p) / n))
        phat <- sum(chains[ , i] < quant) / nrow(chains)
        # use prob for standard deviation
        # this is sd of chains_i
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
