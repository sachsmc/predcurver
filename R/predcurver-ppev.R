#' Computes the partial proportion of explained variation of the predictiveness curve
#' 
#' Computes the proportion of explained variation over a restricted range of risk percentile. Setting vl = vh = .5 will give the unrestricted total gain. 
#'
#' @param R An object of class "predcurve"
#' @param pl The lower percentile threshold
#' @param ph The higher percentile threshold
#'
#' @return output An estimate of the partial proportion of explained variation
#'
#' @keywords prediction accuracy predictiveness curve
#'
#' @export
#' 
ppev <-  function(R, pl=.5, ph=.5){
  theta <- mean(R[,2])
  R.inv <- R[,2:1]
  mesh1 <- c(0,R.inv[R.inv[,1] <= pl,1])
  mesh2 <- c(R.inv[R.inv[,1] >= ph,1],1)
  vvv1 <- mesh1[-length(mesh1)] + (mesh1[2:length(mesh1)] - mesh1[1:(length(mesh1)-1)])/2
  vvv2 <- mesh2[-length(mesh2)] + (mesh2[2:length(mesh2)] - mesh2[1:(length(mesh2)-1)])/2
  true.ppev <- (sum((vvv1 - theta)^2*(evalcdf(R.inv, mesh1[2:length(mesh1)]) - 
                                        evalcdf(R.inv, mesh1[1:(length(mesh1)-1)]))) 
                + sum((vvv2 - theta)^2*(evalcdf(R.inv,mesh2[2:length(mesh2)]) - 
                                          evalcdf(R.inv,mesh2[1:(length(mesh2)-1)]))))/(theta*(1-theta))
  return(true.ppev)
  
}
