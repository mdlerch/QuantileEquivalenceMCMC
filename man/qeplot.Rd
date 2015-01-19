\name{qeplot}
\alias{qeplot}
\title{Plot quantiles from each chain}
\usage{
qeplot(chains, prob, errorBars = T, ...)
}
\arguments{
\item{chains}{A matrix of MCMC's.  Each chain is a column.}

\item{p}{A posterior probability}

\item{errorBars}{Show wald error bars on overall estimate.  Default T.}

\item{...}{Arguments to plot}
}
\value{
A list.
}
\description{
Plot quantiles from each chain
}
\details{
Plots specified quantiles from each chain so that the researcher can
assess convergence
}

