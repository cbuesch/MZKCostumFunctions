#' @export
KalanMeierEquivalenceTest <- function(delta, alpha, t0, Equiv_NonInf, data, grp, plot){
  # delta:        non-inferiority/equivalence margin
  # alpha:          significance level
  # t0:             time point of interest
  # Equiv_NonInf:	  "NonInf" for non-inferiority, "Equivalence" for equivalence test
  # data:           df containing time and status for each individual
  # grp:            group indicator (must be a factor with exactly two levels)
  # plot:           if TRUE, a plot of the two Kaplan Meier curves will be given

  ## Needed Packages:
  require(tidyverse)
  require(survival)
  require(survminer)
  ## Extracting grp into "Gruppe"
  df <- data %>% rename(Gruppe = !!grp)

  ## Confidence interval for Survival difference between two groups at specific
  ## time point
  # Survfit objects
  km_fit1 <- survfit(Surv(time, status) ~ 1, type = "kaplan-meier",
                    data = df %>% filter(Gruppe == levels(Gruppe)[1]))
  km_fit2 <- survfit(Surv(time, status) ~ 1, type = "kaplan-meier",
                     data = df %>% filter(Gruppe == levels(Gruppe)[2]))
  # Variance of both groups at t0
  var1 <- summary(km_fit1, times = t0)[7]$std.err^2
  var2 <- summary(km_fit2, times = t0)[7]$std.err^2
  # Survival estimates at t0
  t1 <- summary(km_fit1, times = t0)[6]$surv - summary(km_fit2, times = t0)[6]$surv
  # Upper and lower CI at t0
  conf_int_l <- t1 - qnorm(1 - alpha) * sqrt(var1 + var2)
  conf_int_u <- t1 + qnorm(1 - alpha) * sqrt(var1 + var2)
  # Plot
  if (plot == TRUE) {
    fit_plot <- survfit(Surv(time, status) ~ Gruppe,
                        data = df)
    # Basic survival curves
    print(ggsurvplot(fit_plot, data = df, conf.int = TRUE))
  }
  conf_int <- list(diff = t1, lower.bound = conf_int_l, upper.bound = conf_int_u)

  ## Decision:
  if (Equiv_NonInf == "NonInf") {
    if (conf_int$upper.bound <= delta) {
      decision = "reject H0 (non-inferiority): Upper CI $<=$ delta"
    }
    else {
      decision = "don't reject H0: Upper CI $>$ delta"
    }
  }else if (Equiv_NonInf == "Equivalence") {
    if (-delta <= conf_int$lower.bound & conf_int$upper.bound <=
        delta) {
      decision = "Equivalent: Upper CI $<=$ delta AND Lower CI $<=$ -delta"
    }else {
      decision = "Not equivalent: Upper CI $>$ delta OR Lower CI $>$ -delta"
    }
  }else {
    return("Equiv_NonInf must be either  is not valid, choose 'NonInf' or 'Equivalence'")
  }
  return(list(conf_int = conf_int, decision = decision))
}




KalanMeierEquivalenceTest_MultipleTimePoints <- function(delta, alpha, tStart, tEnd, Equiv_NonInf, data, grp, plot){
  # delta:        non-inferiority/equivalence margin
  # alpha:          significance level
  # tStart:         time point of interest to start from
  # tEnd:           time point of interest to end
  # Equiv_NonInf:	  "NonInf" for non-inferiority, "Equivalence" for equivalence test
  # data:           df containing time and status for each individual
  # grp:            group indicator (must be a factor with exactly two levels)
  # plot:           if TRUE, a plot of the two Kaplan Meier curves will be given

  ## Performing Tests for all time points between tStart and tEnd:
  Equiv_NonInf_Tests <- list(
    t0       = seq(from = tStart, to = tEnd, by = 1),
    TestsResults = rep(list(NA), times = length(seq(from = tStart, to = tEnd, by = 1)))
  )
  for(i in tStart:tEnd){
    Equiv_NonInf_Tests$TestsResults[[i]] <-
      KalanMeierEquivalenceTest(delta = delta, alpha = alpha, t0 = i,
                                Equiv_NonInf = Equiv_NonInf, data = data,
                                grp = grp, plot = FALSE)
  }

  ## Print overall result
  # if(sum(
  #   sapply(Equiv_NonInf_Tests$TestsResults,
  #          function(x){x$decision == "Not equivalent: Upper CI $>$ delta OR Lower CI $>$ -delta"})
  # ) > 0){
  #   cat("not rejected --> not equivalent")
  # }else{
  #   cat("rejected --> equivalent")
  # }
  #
  # if(sum(
  #   sapply(Equiv_NonInf_Tests$TestsResults,
  #          function(x){x$decision == "don't reject H0: Upper CI $>$ delta"})
  # ) > 0){
  #   cat("not rejected --> not non-inferiority")
  # }else{
  #   cat("rejected --> non-inferiority")
  # }

  # Plot
  if (plot == TRUE) {
    fit_plot <- survfit(Surv(time, status) ~ Gruppe,
                        data = data %>% rename(Gruppe = !!grp))
    # Basic survival curves
    print(ggsurvplot(fit_plot, data = data %>% rename(Gruppe = !!grp), conf.int = TRUE))
  }

  # Results
  return(
    data.frame(
      t0 = Equiv_NonInf_Tests$t0,
      PE_Diff       = sapply(Equiv_NonInf_Tests$TestsResults, function(x) x$conf_int$diff),
      CI_Diff_lower = sapply(Equiv_NonInf_Tests$TestsResults, function(x) x$conf_int$lower.bound),
      CI_Diff_upper = sapply(Equiv_NonInf_Tests$TestsResults, function(x) x$conf_int$upper.bound),
      Decision      = sapply(Equiv_NonInf_Tests$TestsResults, function(x) x$decision))
  )
}
