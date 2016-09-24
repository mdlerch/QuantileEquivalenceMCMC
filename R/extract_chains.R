# TODO: should I test whether there are multiple chains here?
# TODO: mcmc.list extract single parameter
extract_chains <- function(chains, pars = NULL)
{
    # already matrix
    if (is.matrix(chains))
    {
        return(chains)
    }

    # stan
    if (class(chains) == "stanfit")
    {
        chains <- extract(chains, pars = pars, permuted = FALSE)
        if (length(dim(chains)) == 3 & dim(chains)[3] == 1)
        {
            chains <- chains[ , , 1]
        }
        if (length(dim(chains)) > 2)
        {
            stop("Could not extract single parameter")
        }
        return(chains)
    }

    # mcmc.list
    if (class(chains) == "mcmc.list") {
        chains <- lapply(chains, as.matrix)
        stop("Don't know how to extract single param yet...")
    }


    stop("Unrecognized class of chains.  Try converting to matrix")
}

