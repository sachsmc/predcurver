\name{ppev}
\alias{ppev}
\title{Computes the partial proportion of explained variation of the predictiveness curve}
\usage{
  ppev(R, pl = 0.5, ph = 0.5)
}
\arguments{
  \item{R}{An object of class "predcurve"}

  \item{pl}{The lower percentile threshold}

  \item{ph}{The higher percentile threshold}
}
\value{
  output An estimate of the partial proportion of explained
  variation
}
\description{
  Computes the proportion of explained variation over a
  restricted range of risk percentile. Setting vl = vh = .5
  will give the unrestricted total gain.
}
\keyword{accuracy}
\keyword{curve}
\keyword{prediction}
\keyword{predictiveness}

