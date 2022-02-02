Z_Test_UnpairedProportionEquivalence <- function(g1, g2, Delta, alpha = 0.05, ContCorr = TRUE){
  #Assume g1 and g2 are vectors containing the binomial data for group 1 and group 2 respectively.
  #Delta: Equivalence threshold of +/- 5%.
  # You will want to carefully think about and select your own
  # value for Delta before you conduct your test.
  # alpha: significance level
  # ContCorr: PErform continuity correction (TRUE/FALSE)
  
  ### Test for difference:
  n1 <- length(g1) #sample size group 1
  n2 <- length(g2) #sample size group 2
  p1 <- sum(g1, na.rm = TRUE)/n1 #p1 hat
  p2 <- sum(g2, na.rm = TRUE)/n2 #p2 hat
  n <- n1 + n2 #overall sample size
  p <- sum(g1,g2, na.rm = TRUE)/n #p hat
  cHA <- 1/(2*min(n1,n2))
  
  # without continuity correction
  z <- (p1 - p2)/sqrt(p*(1-p)*(1/n1 + 1/n2)) #test statistic
  pval <- 1 - pnorm(abs(z)) #p-value reject H0 if it is <= alpha/2 (two-tailed)
  
  # with continuity correction
  zHA <- (abs(p1 - p2) - cHA)/sqrt((p1*(1-p1)/(n1-1)) + 
                                     (p2*(1-p2)/(n2-1))) #with continuity correction
  pvalHA <- 1 - pnorm(abs(zHA)) #p-value reject H0 if it is <= alpha/2 (two-tailed)
  
  if(ContCorr==TRUE){
    if (pvalHA<=alpha/2) {
      reject <- TRUE 
    } else{
      reject <- FALSE 
    }
  }else if(ContCorr==FALSE){
    if (pval<=alpha/2) {
      reject <- TRUE 
    } else{
      reject <- FALSE 
    }
  }
  
  ### Test for equivalence:
  n1 <- length(g1) #sample size group 1
  n2 <- length(g2) #sample size group 2
  p1 <- sum(g1, na.rm = TRUE)/n1 #p1 hat
  p2 <- sum(g2, na.rm = TRUE)/n2 #p2 hat
  n <- n1 + n2 #overall sample size
  p <- sum(g1, g2, na.rm = TRUE)/n #p hat
  cHAeq <- sign(p1-p2)* (1/(2*min(n1, n2)))
  
  # without continuity correction
  z1 <- (Delta - (p1 - p2))/sqrt(p*(1-p)*(1/n1 + 1/n2)) 
  #test statistic for H01
  z2 <- ((p1 - p2) + Delta)/sqrt(p*(1-p)*(1/n1 + 1/n2)) 
  #test statistic for H02
  pval1 <- 1 - pnorm(z1) #p-value (upper tail) reject H0, if it is <= alpha (one tail)
  pval2 <- 1 - pnorm(z2) #p-value (upper tail) reject H0, if it is <= alpha (one tail)
  
  # with continuity correction
  zHA1 <- (Delta - abs(p1 - p2) + 
             cHAeq)/sqrt((p1*(1-p1)/(n1-1)) + (p2*(1-p2)/(n2-1))) 
  #with continuity correction
  zHA2 <- (abs(p1 - p2) + Delta - cHAeq)/sqrt((p1*(1- 
                                                     p1)/(n1-1)) + (p2*(1-p2)/(n2-1))) 
  #with continuity correction
  pvalHA1 <- 1 - pnorm(zHA1) #p-value (upper tail) reject H0, if it is <= alpha (one tail)
  pvalHA2 <- 1 - pnorm(zHA2) #p-value (upper tail) reject H0, if it is <= alpha (one tail)
  
  if(ContCorr==TRUE){
    if (pvalHA1<=alpha) {
      reject1 <- TRUE 
    } else{
      reject1 <- FALSE 
    }
    if (pvalHA2<=alpha) {
      reject2 <- TRUE 
    } else{
      reject2 <- FALSE 
    }
  }else if(ContCorr==FALSE){
    if (pval1<=alpha) {
      reject1 <- TRUE 
    } else{
      reject1 <- FALSE 
    }
    if (pval2<=alpha) {
      reject2 <- TRUE 
    } else{
      reject2 <- FALSE 
    }
  }
  
  ## Return
  Conclusion <- ifelse(reject==TRUE & (reject1==TRUE & reject2==TRUE), 
                       "Conclusion: Trivial difference between proportions (i.e. yes there is a difference, but it's too small for you to care about because it is smaller than Delta)",
                       ifelse(reject==TRUE & (reject1==FALSE | reject2==FALSE),
                              "Conclusion: Relevant difference between proportions (i.e. larger than Delta)",
                              ifelse(reject==FALSE & (reject1==TRUE & reject2==TRUE),
                                     "Conclusion: Equivalence of proportions",
                                     ifelse(reject==FALSE & (reject1==FALSE | reject2==FALSE),
                                            "Conclusion: indeterminate (i.e. underpowered tests).", 
                                            "Conclusion: NA")
                              )
                       ) 
  )
  return(
    list(Delta = Delta,
         Alpha = alpha, 
         Conclusion = Conclusion, 
         ZTest_for_difference = ifelse(ContCorr==TRUE, pvalHA, pval), 
         ZTest_for_equivalence_1 = ifelse(ContCorr==TRUE, pvalHA1, pval1), 
         ZTest_for_equivalence_2 = ifelse(ContCorr==TRUE, pvalHA2, pval2))
  )
}
