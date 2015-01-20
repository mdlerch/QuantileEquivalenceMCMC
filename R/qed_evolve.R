plot_qed_evolve <- function(chains, prob, step = 100)
{
    nchains <- ncol(chains)
    nsamps <- nrow(chains)
    C_hat <- array(NA, dim = c(floor(nsamps / step) - 1, nchains))

    i <- step
    j <- 0
    while (i < nsamps)
    {
        j <- j + 1
        C_hat[j, ] <- apply(chains[1:i, ], 2, function(x) {quantile(x, prob)})
        i <- i + step
    }

    invisible(list(quantiles = C_hat,
                   samples = (1:(floor(nsamps / step) - 1)) * step))
}
