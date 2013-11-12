#' Prints the predictiveness curve
#' 
#'
#' @param R An object of class "predcurve"
#' 
#'
#' @return output NULL
#'
#' @keywords prediction accuracy predictiveness curve
#'
#' @export
#' 
print.predcurve <- function(R, ...){
  
  print.data.frame(R[seq(1, dim(R)[1], length.out = 10),], digits = 2)
  cat("-------------------------\n")
  cat(paste("        Avg. risk:", round(mean(R[,2]), 2)))
  cat("\n No. Observations: ")
  cat(dim(R)[1])
  
}