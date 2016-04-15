\name{qeplot}
\alias{qeplot}
\title{Plot quantiles from each chain}
\usage{
qeplot(chains, prob, quant, epsilon = 0.015, bars = TRUE, plot = TRUE, ...)
}
\arguments{
\item{chains}{A matrix of MCMC's.  Each chain is a column.}
\item{prob}{A posterior probability}
\item{quant}{A posterior quantile}
\item{epsilon}{QED tolerance level}
\item{bars}{Whether or not to plot approximate convergence range}
\item{plot}{Whether or not to create the plot}
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

