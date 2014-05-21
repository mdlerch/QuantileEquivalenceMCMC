#' Two one-sided test of equivalence for summary stats
#'
#' @description
#' My description
#'
#' @param mu description
#' @param se description
#' @param n description
#' @param delta description
#' @param alpha description
#' @param H0 description
#'
#' @details
#' This is a
#'
#' @return 1 for reject, 0 for FTR
#'
tost <- function(mu, sd, n, epsilon, alpha = 0.05, null = 0)
{
    mu <- (mu - null) / sd
    eps_twid <- sqrt(n) * epsilon
    C_alpha <- sqrt(qchisq(alpha, 1, ncp = eps_twid))

    return(abs(mu) < 1 / sqrt(n) * C_alpha)

}
