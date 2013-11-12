\name{ptg.predcurve}
\alias{ptg.predcurve}
\title{Computes the partial total gain of the predictiveness curve}
\usage{
  ptg.predcurve(R, vl = 0.5, vh = 0.5, ...)
}
\arguments{
  \item{R}{An object of class "predcurve"}

  \item{vl}{The lower risk threshold}

  \item{vh}{The higher risk threshold}
}
\value{
  output An estimate of the partial total gain
}
\description{
  Computes the total gain over a restricted range of risk
  percentile. Setting vl = vh = .5 will give the
  unrestricted total gain.
}
\keyword{accuracy}
\keyword{curve}
\keyword{prediction}
\keyword{predictiveness}

