#' @export
extract_coxme_table <- function (mod){
  # Extracting summary table of proportional cox regression with random effect
  # using package coxme.

  beta <- mod$coefficients
  confint_beta <- confint(mod)
  nvar <- length(beta)
  nfrail <- nrow(mod$var) - nvar
  se <- sqrt(diag(mod$var)[nfrail + 1:nvar])
  z<- round(beta/se, 2)
  p<- signif(1 - pchisq((beta/se)^2, 1), 2)

  fixed_table=data.frame(
    coefficient = beta,
    se_coef = se,
    HR = exp(beta),
    CI_lower = exp(confint_beta)[,1],
    CI_upper = exp(confint_beta)[,2],
    testStatistik = z,
    pvalue = p)
  colnames(fixed_table) <- c("coefficient", "se (coeffeicient)", "HR", "Lower HR-95%-CI",
                             "Upper HR-95%-CI", "test statistik z", "p-value")
  return(list(fixed = fixed_table, random = mod$vcoef))
}
