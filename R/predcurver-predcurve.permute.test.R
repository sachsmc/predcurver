#' Permutation test for the difference in summary measures
#' 
#' Permutation test for either the partial total gain, partial proportion of explained variation, or both.
#'
#' @param R An object of class "predcurve"
#' @param vl The lower percentile threshold
#' @param vh The higher percentile threshold
#' @param pl The lower percentile threshold
#' @param ph The higher percentile threshold
#' @param D number of permutations to run
#' @param multicore Logical, use multicore (Unix-only)
#'
#' @return output Distribution of the summary statistic under the null, along with two-sided p-values
#'
#' @keywords prediction accuracy predictiveness curve permutation test
#'
#' @export
#' 
predcurve.permute.test <- function(R1, R2, vl = .5, vh = .5, pl = .5, ph = .5, 
                                   D = 2000, multicore = FALSE){
  theta <- mean(R1[,2])
  
  dptg.est <- ptg(R1, vl, vh) - ptg(R2, vl, vh)
  dppev.est <- ppev(R1, pl, ph) - ppev(R2, pl, ph)
  
  swapOne <- function(R1, R2, vl, vh, pl, ph){
    nnn <- dim(R1)[1]
    swap <- rbinom(nnn, 1, p = .5)
    
    R1.swap <- R1
    R2.swap <- R2
    R1.swap[swap==1,2] <- R2[swap==1,2]
    R2.swap[swap==1,2] <- R1[swap==1,2]
    
    return(list(dptg = ptg(R1.swap, vl, vh) - ptg(R2.swap, vl, vh),
    dppev = ppev(R1.swap, pl, ph) - ppev(R2.swap, pl, ph)))
    
  }
   
  if(multicore){
    
    res <- mclapply(1:D, function(i) unlist(swapOne(R1, R2, vl, vh, pl, ph)))
    
  } else{
    
    res <- lapply(1:D, function(i) unlist(swapOne(R1, R2, vl, vh, pl, ph)))
        
  }
   res2 <- as.data.frame(matrix(unlist(res), ncol = 2, byrow = T))
   colnames(res2) <- c("dpTG", "dpPEV")
   
p.dptg <- ifelse(dptg.est < 0, sum(res2$dpTG < dptg.est)+1, sum(res2$dpTG > dptg.est)+1)/(D+1)
p.dppev <-ifelse(dppev.est < 0, sum(res2$dpPEV < dppev.est)+1, sum(res2$dpPEV > dppev.est)+1)/(D+1)

  return(list(p.dpTG = p.dptg, p.dpPEV = p.dppev))
  
}