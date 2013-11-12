#' Computes the predictiveness curve from a vector of estimated risks
#' 
#' The predictiveness curve is defined as the distribution of the risk, as a function of the risk percentiles. 
#'
#' @param risk A vector of risk values which must be in (0, 1)
#'
#' @return output A object of class "predcurve", which is a data frame that contains two columns: the risk percentiles, and the risk values
#'
#' @keywords prediction accuracy predictiveness curve
#'
#' @export
#' 
predcurve.default <- function(risk, ...){ 

  pd <- as.data.frame(cbind(1:length(risk)/length(risk), risk[order(risk)]))
  colnames(pd) <- c("percentile", "risk")
  class(pd) <- c("predcurve", "data.frame")
  return(pd)

}
