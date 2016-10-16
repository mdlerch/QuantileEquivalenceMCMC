\name{qed}
\alias{qed}
\title{Compute the Quantile Equivalence Diagnostic}
\description{
    Compute the quantile equivalence diagnostic
}
\usage{
qed(chains, prob, quant, epsilon = 0.01, alpha = 0.05, pars = NULL)
}
\arguments{
    \item{chains}{The MCMC samples.  See details for more}
    \item{prob}{numeric value in (0, 1).  The posterior probability of interest.}
    \item{quant}{numeric value.  The posterior quantile of interest.}
    \item{epsilon}{numeric value.  The tolerance level for the QED.}
    \item{alpha}{numeric value in (0, 1).  The significance level for the QED.}
    \item{pars}{character.  The name of the parameter to be used for the
        calculation of the QED.}
}
\details{
    Precisely one of \code{prob} and \code{quant} arguments is to be provided.

    The \code{chains} argument can accommodate multiple types.  The input may
    be a matrix for a single scalar parameter where the columns indicate the
    multiple chains and the rows indicate the samples in each chain.  Or, the
    input may be a \code{stan.fit} type object.  In this case, the parameter of
    interest must be provided with the \code{pars} argument.  Or, the input may
    be a list of \code{mcarray} objects, which might be generated from
    \code{rjags}.  Again, the parameter of interested is to be provided with
    \code{pars}.  Or, the input may be an \code{mcmc.list} object, which might
    be generated from \code{coda}.  Again, the parameter of iterest is to be
    provided with \code{pars}.

}
\value{An integer.  1 indicates convergence according to the QED.  0 indicates
    continued sampling is required.}
\references{Lerch and Higgs.  Equivalence testing for MCMC convergence.  To
    appear.}
\author{Michael Lerch \email{mdlerch@gmail.com}}
\seealso{\code{\link{qeplot}}}
