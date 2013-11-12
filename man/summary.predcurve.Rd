\name{summary.predcurve}
\alias{summary.predcurve}
\title{Summarizes the predictiveness curve}
\usage{
  summary.predcurve(R, vl = NULL, vh = NULL, pl = NULL,
    ph = NULL, ...)
}
\arguments{
  \item{R}{An object of class "predcurve"}

  \item{vl}{Lower percentile threshold}

  \item{vh}{Upper percentile threshold}

  \item{pl}{Lower risk threshold}

  \item{ph}{Upper risk threshold}
}
\value{
  output NULL
}
\description{
  Thresholds are optional, but only provide v or p, not
  both.
}
\keyword{accuracy}
\keyword{curve}
\keyword{prediction}
\keyword{predictiveness}

