\name{qeplot}
\alias{qeplot}
\title{Plot Quantiles From Each Chain}
\usage{
qeplot(chains, prob, quant, epsilon = 0.015, bars = TRUE, plot = TRUE, ...)
}
\arguments{
\item{chains}{The MCMC samples.  See \code{help(qed)} for more}
\item{prob}{numeric value in (0, 1).  The posterior probability of interest.}
\item{quant}{numeric value.  The posterior quantile of interest.}
\item{epsilon}{numeric value.  The tolerance level for the QED.}
\item{plot}{boolean.  Whether or not to create the plot.}
\item{...}{additional arguments to \code{plot}.}
}
\value{
A list.  A list is invisibly returned which has the contents: \code{p_hat}
which is the overall probability value, \code{C_hat} which is the overall
quantile value, \code{p_hat_i} which is a vector of the observed chain
probabilities for \code{C_hat}, and \code{C_hat_i} which is a vector of the
observed chain quantiles for \code{p_hat}.
}
\description{
    The accompanying plot to the qed MCMC convergence diagnostic.
}
\details{
    The function may be used to generate the qeplot or to return the values
    that go into the calculation of the qeplot, which are returned invisibly.
}
\references{Lerch and Higgs.  Equivalence testing for MCMC convergence.  To
    appear.}
\author{Michael Lerch \email{mdlerch@gmail.com}}
\seealso{\code{\link{qed}}}

