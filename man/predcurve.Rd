\name{predcurve}
\alias{predcurve}
\title{Computes the predictiveness curve from a vector of estimated risks}
\usage{
  predcurve(risk, ...)
}
\arguments{
  \item{risk}{A vector of risk values which must be in (0,
  1)}
}
\value{
  output A object of class "predcurve", which is a data
  frame that contains two columns: the risk percentiles,
  and the risk values
}
\description{
  The predictiveness curve is defined as the distribution
  of the risk, as a function of the risk percentiles.
}
\keyword{accuracy}
\keyword{curve}
\keyword{prediction}
\keyword{predictiveness}

