#' One parameter test of equivalence
#'
#' @description
#' Perform a one parameter test of equivalence for a single sample based on the
#' sample statistics
#'
#' @param mu mean of the sample
#' @param sd true standard deviation
#' @param n sample size
#' @param epsilon margin of error
#' @param alpha significance level
#' @param null null hypothesis value
#'
#' @details
#' Consider X_i ~ (theta, sigma) with sigma known.  We test the hypothesis H:
#' abs(theta - null) >= epsilon against K: abs(theta - null) < epsilon.  From
#' Wellek 2010 4.1, the UMP test is to compare abs(mu) to n^(-1/2) * C where C
#' is the alphath quantile from a chisquare distribution with non centrality
#' parameter sqrt(n)*epsilon and one degree of freedom.
#'
#' Wellek shows this for X_i ~ N(theta, sigma), but I think it should follow as
#' long as Xbar ~ N(theta, sigma).
#'
#' @references
#' Wellek 2010 Testing Statistical Hypotheses of Equivalence and Noninferiority
#'
#'
#' @return 1 for reject, 0 for FTR
#' @export
#'
onePZeq <- function(z, epsilon, alpha = 0.05)
{
    # rejection region boundary
    C_alpha <- sqrt(qchisq(alpha, 1, ncp = epsilon^2))

    # return result of test
    return(abs(z) < C_alpha)
}
