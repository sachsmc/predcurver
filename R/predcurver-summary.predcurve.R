#' Summarizes the predictiveness curve
#' 
#' Thresholds are optional, but only provide v or p, not both.
#'
#' @param R An object of class "predcurve"
#' @param vl Lower percentile threshold
#' @param vh Upper percentile threshold
#' @param pl Lower risk threshold
#' @param ph Upper risk threshold
#' 
#'
#' @return output NULL
#'
#' @keywords prediction accuracy predictiveness curve
#'
#' @export
#' 
summary.predcurve <- function(R, vl = NULL, vh = NULL, pl = NULL, ph = NULL, ...){
  theta <- mean(R[,2])
  if(!is.null(vl) & is.null(pl)) pl <- evalcdf(R[,2:1], vl)
  if(!is.null(vh) & is.null(ph)) ph <- evalcdf(R[,2:1], vh)
  if(is.null(vl) & !is.null(pl)) vl <- evalcdf(R, pl)
  if(is.null(vh) & !is.null(ph)) vh <- evalcdf(R, ph)
  if(is.null(vh) & is.null(vl) & is.null(ph) & is.null(pl)){
    vh <- vl <- ph <- pl <- .5
  }
  
  ptg.est <- ptg(R, vl, vh)
  ppev.est <- ppev(R, pl, ph)
  
  print(R)
  cat("\n thresholds: v = (", vl, ", ", vh, ") \n ")
  cat("            p = (", pl, ", ", ph, ") \n ")
  cat("pTG = ", round(ptg.est, 2), "\n")
  cat("pPEV = ", round(ppev.est, 2), "\n")
  
  return(list(ptg = ptg.est, ppev = ppev.est))
  
}