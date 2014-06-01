#' Plot quantiles from each chain
#'
#' @description
#' Plot quantiles from each chain
#'
#' @param chains A matrix of MCMC's.  Each chain is a column.
#' @param p A probability for a quantile or a vector of probabilities.
#' @param ... Arguments to plot
#'
#' @details
#' Plots specified quantiles from each chain so that the researcher can
#' assess convergence
#'
#' @export
#' @return
#' A list.
#'
qeplot <- function(chains, prob, ...)
{
    # get probabilities associated with overall quantile
    quant <- quantile(chains, prob)
    p <- matrix(NA, ncol = ncol(chains), nrow = length(prob))
    for (i in 1:length(prob))
    {
        p[i, ] <- apply(chains, 2, function(x) sum(x < quant[i]) / length(x))
    }

    # get quantiles associated with specified probability
    q <- matrix(NA, ncol = ncol(chains), nrow = length(prob))
    for (i in 1:length(prob))
    {
        q[i, ] <- apply(chains, 2, function(x) quantile(x, prob[i]))
    }

    # using length of a single chain for N.  should it be nchains times this?
    # actually I do like this with N.  We might also consider putting bars on
    # each chain's point estimate.  This will get messy quickly, so just
    # putting one set on the "middle" point makes things cleaner and we can
    # think that if the bar includes a chain's point, then bars around a chain
    # would include the "middle" point
    se <- sqrt(prob * (1 - prob) / nrow(chains))

    plot(p, q, type = "n", ...)

    points(prob, quant, pch = 0)
    arrows(prob + 2 * se, quant, prob - 2 * se, quant, angle = 90, code = 3)
    for (i in 1:length(prob))
    {
        for (j in 1:ncol(chains))
        {
            lines(c(p[i, j], prob[i]), c(quant[i], q[i, j]), col = j)
            points(p[i, j], quant[i], col = j, pch = 19)
            points(prob[i], q[i, j], col = j, pch = 19)
        }
    }

    return(list(prob = prob, quant = quant, p = p, q = q))
}
