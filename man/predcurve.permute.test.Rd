\name{predcurve.permute.test}
\alias{predcurve.permute.test}
\title{Permutation test for the difference in summary measures}
\usage{
  predcurve.permute.test(R1, R2, vl = 0.5, vh = 0.5,
    pl = 0.5, ph = 0.5, D = 2000, multicore = FALSE)
}
\arguments{
  \item{R}{An object of class "predcurve"}

  \item{vl}{The lower percentile threshold}

  \item{vh}{The higher percentile threshold}

  \item{pl}{The lower percentile threshold}

  \item{ph}{The higher percentile threshold}

  \item{D}{number of permutations to run}

  \item{multicore}{Logical, use multicore (Unix-only)}
}
\value{
  output Distribution of the summary statistic under the
  null, along with two-sided p-values
}
\description{
  Permutation test for either the partial total gain,
  partial proportion of explained variation, or both.
}
\keyword{accuracy}
\keyword{curve}
\keyword{permutation}
\keyword{prediction}
\keyword{predictiveness}
\keyword{test}

