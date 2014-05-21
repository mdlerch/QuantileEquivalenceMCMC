#' Compute the quantile equivalence diagnostic
#'
#' @description
#' Perform the quantile equivalence diagnostic for a single chain
#'
#' @param X chain
#' @param q quantile of interest
#' @param quant percentile of quantile
#' @param epsilon margin of error on quantile
#' @param alpha significance level
#'
#' @details
#' work in progress
#'
#' @return returned
#' @export
#'
qed <- function(X, q, quant, epsilon = 0.01, alpha = 0.05)
{
    # find the chain percentile
    p <- sum(X > q) / length(X)
    # use q for standard deviation
    s <- sqrt(q * (1 - q))
    onePeq(p - q, s, length(X), epsilon, alpha)
}
