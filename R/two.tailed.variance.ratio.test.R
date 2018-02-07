##' Two tailed variance ratio test
##' 
##' The two tailed variance ratio test is intended for testing for difference between the variances of two groups.
##' Refer to Biostatistical Analysis, by Jerrold H. Zar, fifth edition, section 8.5, page 151 
##' 
##' @title Two tailed variance ratio test
##' 
##' @param group_1 a numeric vector
##' @param group_2 a numeric vector
##' @param alpha the desired significance cut off
##'  
##' 
##' @author Daniel Kick (\email{drk8b9@@mail.missouri.edu})
##' 
##' @references Zar, J.H. 2010. \emph{Biostatistical Analysis}. 5th Edition. Pearson
##'   Prentice-Hall. Upper Saddle River, NJ. ISBN-10: 0131008463. ISBN-13:
##'   978013100846.
##'   \url{http://www.pearsonhighered.com/educator/product/Biostatistical-Analysis/9780131008465.page}
##' 
##' @export


#M <- as.data.frame(read.csv("S:/Data_Daniel/170926_coherent_composite/src/zar5_variance_ratio_test/example8.7.csv"))
#alpha <- 0.05
#group_1 <- M[,1]
#group_2 <- M[,2]

#Two tailed variance ratio test for eqality of two variances
two_tailed_variance_ratio_test <- function(group_1, group_2, alpha){
  group_1 <- as.data.frame(group_1)
  group_2 <- as.data.frame(group_2)
  group_1 <- group_1[!is.na(group_1)]
  group_2 <- group_2[!is.na(group_2)]
  n_1 <- length(group_1)
  deg_f_1 <- n_1 -1
  variance_1 <- (sd(group_1)^2)
  
  n_2 <- length(group_2)
  deg_f_2 <- n_2 -1
  variance_2 <- (sd(group_2)^2)
  
  F_stat <- max((variance_1 / variance_2), (variance_2 / variance_1))
  
  if ( (variance_1 / variance_2) > (variance_2 / variance_1) ){
    numer_df <- deg_f_1
    denom_df <- deg_f_2
  } else {
    numer_df <- deg_f_2
    denom_df <- deg_f_1
  }
  
  # for reference see http://wiki.socr.umich.edu/index.php/SMHS_ProbabilityDistributions#Generating_Probability_Tables
  right_tail_p <- alpha/2
  crit_val <- qf(right_tail_p, numer_df, denom_df, lower.tail = FALSE)
  
  if (F_stat < crit_val){
    #print("insufficent evidence to reject H0")
    pooled_variance <- (deg_f_1*variance_1 + deg_f_2*variance_2) / (deg_f_1 + deg_f_2)
    #print(as.character(pooled_variance))
    return(0)
  } else {
    #print("Sufficient evidence to reject H0")
    #print(paste("variance of group 1:", as.character(variance_1)))
    #print(paste("variance of group 2:", as.character(variance_2)))
    return(1)
  }
}



