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
#' @return
#' A list.
#' @export
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
    se <- sqrt(prob * (1 - prob) / nrow(chains))

    plot(p ~ q, type = "n", ...)

    points(prob ~ quant, pch = 0)
    arrows(quant, prob + 2 * se, quant, prob - 2 * se, angle = 90, code = 3)
    for (i in 1:length(prob))
    {
        for (j in 1:ncol(chains))
        {
            lines(c(quant[i], q[i, j]), c(p[i, j], prob[i]), col = j)
            points(quant[i], p[i, j], col = j, pch = 19)
            # arrows(quant[i], p[i, j] + 2 * se, quant[i], p[i, j] - 2 * se, angle = 90, col = j, code = 3)
            points(q[i, j], prob[i], col = j, pch = 19)
        }
    }

    return(list(prob = prob, quant = quant, p = p, q = q))
}
