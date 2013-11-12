#' Computes the partial total gain of the predictiveness curve
#' 
#' Computes the total gain over a restricted range of risk percentile. Setting vl = vh = .5 will give the unrestricted total gain. 
#'
#' @param R An object of class "predcurve"
#' @param vl The lower risk threshold
#' @param vh The higher risk threshold
#'
#' @return output An estimate of the partial total gain
#'
#' @keywords prediction accuracy predictiveness curve
#'
#' @export
#' 
ptg <- function(R, vl = .5, vh = .5, ...){
  
  theta <- mean(R[,2])
  return(
    (integrate(function(t) abs(evalcdf(R, t) - theta), 0, vl, ...)$value + 
            integrate(function(t) abs(evalcdf(R, t) - theta), vh, 1, ...)$value)/((vl*theta + (1-vh)*(1-theta)))
    )
 
}