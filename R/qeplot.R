#' Plot quantiles from each chain
#'
#' @description
#' Plot quantiles from each chain
#'
#' @param chains A matrix of MCMC's.  Each chain is a column.
#' @param p A probability for a quantile or a vector of probabilities.
#'
#' @details
#' Plots specified quantiles from each chain so that the researcher can
#' assess convergence
#'
#' @return Nothing is returned
#' @export
#'
qeplot <- function(chains, prob)
{
    quant <- quantile(chains, prob)
    p <- matrix(NA, ncol = ncol(chains), nrow = length(prob))
    for (i in 1:length(prob))
    {
        p[i, ] <- apply(chains, 2, function(x) sum(x < quant[i]) / length(x))
    }

    q <- matrix(NA, ncol = ncol(chains), nrow = length(prob))
    for (i in 1:length(prob))
    {
        q[i, ] <- apply(chains, 2, function(x) quantile(x, prob[i]))
    }
    return(q)
}
