\name{qed}
\alias{qed}
\title{Compute the quantile equivalence diagnostic}
\usage{
qed(X, prob, epsilon = 0.005, alpha = 0.05)
}
\arguments{
\item{X}{matrix of chains}

\item{prob}{posterior probability of interest}

\item{epsilon}{margin of error on probability}

\item{alpha}{significance level}
}
\value{
returned
}
\description{
Perform the quantile equivalence diagnostic for a matrix of chains
}
\details{
Argument X is a matrix where each column is a chain
}

