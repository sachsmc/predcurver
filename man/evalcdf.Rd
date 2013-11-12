\name{evalcdf}
\alias{evalcdf}
\title{Evaluates and empirical cdf at a value}
\usage{
  evalcdf(cdf, u)
}
\arguments{
  \item{cdf}{The cdf to be evaluated. The first column is
  the sorted vector x, the second column is the probability
  that X is less than x.}

  \item{u}{A vector of values at which the cdf will be
  evaluated.}
}
\value{
  output A numeric vector between 0 and 1
}
\description{
  Finds the closted observed value less than u and returns
  the probability.
}
\keyword{distribution}
\keyword{function}

