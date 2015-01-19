qeplot <- function(chains, prob, quant,
                   xlab = NULL, ylab = NULL, main = NULL)
{
    if (missing(prob) & missing(quant))
    {
        stop("Must provide at least one of 'prob' or 'quant'")
    }

    if (!missing(prob) & !missing(quant))
    {
        warning("Provide only one of 'prob' or 'quant'\nUsing 'prob'...")
    }

    # Using naming scheme in manuscript

    if (!missing(prob))
    {
        p_hat <- prob
        C_hat <- quantile(chains, p_hat)
    }
    else
    {
        C_hat <- quant
        p_hat <- sum(chains < C_hat) / length(c(chains))
    }

    # get probabilities associated with overall quantile
    p_hat_i <- apply(chains, 2, function(x) sum(x < C_hat) / length(x))

    # get quantiles associated with overall probability
    C_hat_i <- apply(chains, 2, function(x) quantile(x, p_hat))

    # set up plot
    plot(p_hat, C_hat, type = "n", xlab = xlab, ylab = ylab, ...)

    # plot overall point
    points(p_hat, C_hat, pch = 0)

    for (i in 1:ncol(chains))
    {
        # plot horizontal band
        points(p_hat_i[i], C_hat, col = i)
        # plot vertical band
        points(p_hat, C_hat_i[i], col = i)
        # connection line
        lines(c(p_hat_i[i], p_hat), c(C_hat, C_hat_i[i]), col = j)
    }

    # invisibly return all values necessary to create this plot
    invisible(list(p_hat = p_hat, C_hat = C_hat,
                   p_hat_i = p_hat_i, C_hat_i = C_hat_i))
}
