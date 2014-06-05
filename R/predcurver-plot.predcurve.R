#' Plots the predictiveness curve
#' 
#'
#' @param R An object of class "predcurve"
#' @param ... other arguments passed to plot
#' 
#'
#' @return output NULL
#'
#' @keywords prediction accuracy predictiveness curve
#'
#' @export
#' 
plot.predcurve<- function(R, vl = NULL, vh = NULL, ...){
  theta <- mean(R[,2])
  plot(R$risk ~ R$percentile, type = 's', ...)
  abline(h = theta, lty = 2)
  
  if(!is.null(vl)){
    pl <- evalcdf(R[,2:1], vl)
    seq.l <- 1:max((1:length(R$percentile))[R$percentile < pl])
    gons.y <- c(rep(theta, length(seq.l)), R[seq.l[length(seq.l):1],2])
    gons.x <- c(R[seq.l, 1], R[seq.l[length(seq.l):1],1])
    polygon(gons.x, gons.y, border = NA, col = "grey")
  }
  if(!is.null(vh)){
    ph <- evalcdf(R[,2:1], vh)
    seq.l <- min((1:length(R$percentile))[R$percentile > ph]):length(R$percentile)
    gons.y <- c(rep(theta, length(seq.l)), R[seq.l[length(seq.l):1],2])
    gons.x <- c(R[seq.l, 1], R[seq.l[length(seq.l):1],1])
    polygon(gons.x, gons.y, border = NA, col = "grey")
    
  }
  
  
}