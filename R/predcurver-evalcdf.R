#' Evaluates and empirical cdf at a value
#' 
#' Finds the closted observed value less than u and returns the probability. 
#'
#' @param cdf The cdf to be evaluated. The first column is the sorted vector x, the second column is the probability that X is less than x. 
#' @param u A vector of values at which the cdf will be evaluated. 
#'
#' @return output A numeric vector between 0 and 1
#'
#' @keywords distribution function
#'
#' @export
#' 
evalcdf <- function(cdf, u) {  
  k <- length(u)
  dex <- sapply(u, function(x) x <= cdf[,1])
  temp <- colSums(dex) == 0
  out <- suppressWarnings(tryCatch(apply(dex, 2, function(x) min(cdf[x,2])), error = function(w) NA))
  out[temp] <- 1
  return(out)
} 