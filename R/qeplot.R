qeplot <- function(chains, prob, quant, pars = NULL, epsilon = 0.015, plot = TRUE, ...)
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

    chains <- extract_chains(chains, pars)

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

    # get error bars
    # delta <- epsilon - qnorm(.975) * sqrt(p_hat * (1- p_hat) / nrow(chains))
    # if (delta < 0)
    # {
    #     delta <- 0
    # }

    # set up plot
    if (plot)
    {
        plot(p_hat_i, C_hat_i, type = "n",
             xlab = "Probability", ylab = "Quantile", ...)

        # plot overall point
        points(p_hat, C_hat, pch = 19)

        # plot crossing lines
        abline(v = p_hat, lty = 1, lwd = 8, col = "#d0d0d0")
        abline(h = C_hat, lty = 1, lwd = 8, col = "#d0d0d0")

        # # plot error bars
        # if (bars)
        # {
        #     arrows(p_hat, C_hat, p_hat + delta, C_hat, angle = 90)
        #     arrows(p_hat, C_hat, p_hat - delta, C_hat, angle = 90)
        # }

        for (i in 1:ncol(chains))
        {
            # plot horizontal band
            points(p_hat_i[i], C_hat, col = i, pch = 19)
            # plot vertical band
            points(p_hat, C_hat_i[i], col = i, pch = 19)
            # connection line
            lines(c(p_hat_i[i], p_hat), c(C_hat, C_hat_i[i]), col = i)
        }
    }

    # invisibly return all values necessary to create this plot
    invisible(list(p_hat = p_hat, C_hat = C_hat,
                   p_hat_i = p_hat_i, C_hat_i = C_hat_i))
}
