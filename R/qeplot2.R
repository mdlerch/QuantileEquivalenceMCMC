#' Plot error bars for each chain
#'
#' @description
#' Plot error bars for each chain
#'
#' @param chains chains
#' @param prob a single probability
#' @param ... arguments to plot
#'
#' @details
#' Plot error bars for each chain
#'
#' @export
#' @return Nothing is returned
#'
qeplot2 <- function(chains, prob, ...)
{
    # get probabilities associated with overall quantile
    quant <- quantile(chains, prob)
    for (i in 1:length(prob))
    {
        p <- apply(chains, 2, function(x) sum(x < quant[i]) / length(x))
    }

    # using length of a single chain for N.
    se <- sqrt(prob * (1 - prob) / nrow(chains))

    plot(rep(1:ncol(chains), 2), c(p + 2 * se, p - 2 * se), type = "n", 
         xlab = "Quantile", ylab = "Probability", ...)

    for (i in 1:ncol(chains))
    {
        points(i, p[i], col = i)
        arrows(i, p[i] + 2 * se, i, p[i] - 2 * se, angle = 90, code = 3, col = i)
    }
    lines(c(0, ncol(chains) + 1), rep(prob, 2), lty = 2)
}
