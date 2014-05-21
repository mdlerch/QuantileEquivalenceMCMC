#' Compute the quantile equivalence diagnostic
#'
#' @description
#' Perform the quantile equivalence diagnostic for a single chain
#'
#' @param X matrix of chains
#' @param q quantile of interest
#' @param quant percentile of quantile
#' @param epsilon margin of error on quantile
#' @param alpha significance level
#'
#' @details
#' Argument X is a matrix where each column is a chain
#'
#' @return returned
#' @export
#'
qed <- function(X, q, quant, epsilon = 0.01, alpha = 0.05)
{
    nchains <- ncol(X)
    result <- numeric(nchains)
    for (i in 1:nchains)
    {
        # find the chain percentile
        p <- sum(X > q) / nrow(X)
        # use q for standard deviation
        s <- sqrt(q * (1 - q))
        result[i] <- onePeq(p - q, s, nrow(X), epsilon, alpha)
    }
    return(prod(result))
}
