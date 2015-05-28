\name{qed}
\alias{qed}
\title{Compute the Quantile Equivalence Diagnostic}
\description{
    Compute the Quantile Equivalence Diagnostic
}
\usage{
qed(X, prob = NULL, quant = NULL, epsilon = 0.01, alpha = 0.05)
}
\arguments{
    \item{X}{a numeric matrix.  matrix of chains for a scalar parameter.  Chains are given by
        columns of X, each of the same lenth, \code{nrow(X)}}
    \item{prob}{numeric value in (0, 1).  The posterior probability of interest.}
    \item{quant}{numeric value.  The posterior quantile of interest.}
    \item{epsilon}{numeric value.  The tolerance level for the QED.}
    \item{alpha}{numeric value in (0, 1).  The significance level for the QED.}
}
\details{
    Extension of description
}
\value{An integer.  1 indicates convergence according to the QED.  0 indicates
    continued sampling is required.}
\references{Lerch and Higgs.  Equivalence testing for MCMC convergence.  To
    appear.}
\author{Michael Lerch \email{mdlerch@gmail.com}}
\seealso{\code{\link{qeplot}}}
