#' Compute the quantile equivalence diagnostic
#'
#' @description
#' Perform the quantile equivalence diagnostic for a single chain
#'
#' @param X matrix of chains
#' @param quant quantile of interest
#' @param prob null probability of observation below quantile quant
#' @param epsilon margin of error on quantile
#' @param alpha significance level
#'
#' @details
#' Argument X is a matrix where each column is a chain
#'
#' @return returned
#' @export
#'
qed <- function(X, quant, prob, epsilon = 0.01, alpha = 0.05)
{
    nchains <- ncol(X)
    result <- numeric(nchains)
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
        epsilon <- epsilon / s
        # do test on each chain
        result[i] <- onePeq(Xbar, 1, nrow(X), epsilon, alpha)
    }
    return(prod(result))
}
