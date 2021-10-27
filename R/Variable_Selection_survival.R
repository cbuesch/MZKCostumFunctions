Variable_Selection_survival <- function(df., time, status){
  # df.: Data set
  # time: time variable for right censored survival endpoint, must be included in df.
  # status: The status indicator (0=alive, 1=dead) for right censored survival survival endpoint, must be included in df.

  # to add: baum modelle

  predictor.variablennames <- names(df. %>% dplyr::select(-c(!!time, !!status)))
  result.matrix <- matrix(NA, nrow = dim(df.)[2]-2, ncol = 8,
                          dimnames = list(Variables = predictor.variablennames,
                                          Method    = c("StepSelect AIC: Both (full model)",
                                                        "StepSelect AIC: Both (null model)",
                                                        "StepSelect AIC: Backward",
                                                        "StepSelect AIC: Forward",
                                                        "LassoCV (lambda.min), partial-likelihood",
                                                        "LassoCV (lambda.min), Harrel's concordance measure",
                                                        "LassoCV (lambda.1se), partial-likelihood",
                                                        "LassoCV (lambda.1se), Harrel's concordance measure")))


  ### AIC: Stepwise selection
  fit.AIC.full <- coxph(formula = as.formula(paste0("Surv(", time, ", ", status, ") ~ 1 +.")), data = df.)
  fit.AIC.null <- coxph(formula = as.formula(paste0("Surv(", time, ", ", status, ") ~ 1")), data = df.)
  # Both starting with full model
  fit.AIC.bothFull <- stepAIC(object = fit.AIC.full,
                              scope =list(upper = fit.AIC.full,
                                          lower = fit.AIC.null),
                              direction = "both", trace = FALSE)
  result.matrix[,1] <- ifelse(predictor.variablennames %in% names(fit.AIC.bothFull$xlevels), 1, 0)

  # Both starting with null model
  fit.AIC.bothNull <- stepAIC(object = fit.AIC.null,
                              scope =list(upper = fit.AIC.full,
                                          lower = fit.AIC.null),
                              direction = "both", trace = FALSE)
  result.matrix[,2] <- ifelse(predictor.variablennames %in% names(fit.AIC.bothNull$xlevels), 1, 0)

  # Backward starting with full model
  fit.AIC.backward <- stepAIC(object = fit.AIC.full,
                              scope =list(upper = fit.AIC.full,
                                          lower = fit.AIC.null),
                              direction = "backward", trace = FALSE)
  result.matrix[,3] <- ifelse(predictor.variablennames %in% names(fit.AIC.backward$xlevels), 1, 0)

  # Forward starting with null model
  fit.AIC.forward <- stepAIC(object = fit.AIC.null,
                             scope =list(upper = fit.AIC.full,
                                         lower = fit.AIC.null),
                             direction = "forward", trace = FALSE)
  result.matrix[,4] <- ifelse(predictor.variablennames %in% names(fit.AIC.forward$xlevels), 1, 0)



  ### LASSO (ridge macht kein Sinn, da dort keine Variablen komplett entfernt werden)
  x <- model.matrix(~ ., df. %>% dplyr::select(-c(!!time, !!status)))[,-1]
  y <- df_model_mortalitaet %>%
    mutate(
      time = ifelse(time==0, 0.01, time) # setting survival times of 0 to 0.01 so that glmnet runs
    ) %>% dplyr::select(c(!!time, !!status)) %>% as.matrix()


  ## cross validation:
  set.seed(123456)
  cvfit.Lasso.default <- cv.glmnet(x, y, family = "cox", alpha = 1, type.measure = "default") # partial-likelihood for the Cox model

  set.seed(123456)
  cvfit.Lasso.C   <- cv.glmnet(x, y, family = "cox", alpha = 1, type.measure = "C")  # Harrel's concordance measure

  ## Variables left in final model
  #  partial-likelihood
  result.matrix[,5] <- as.numeric(coef(cvfit.Lasso.default, s = "lambda.min")!=0)
  result.matrix[,7] <- as.numeric(coef(cvfit.Lasso.default, s = "lambda.1se")!=0)

  # Harrel's concordance measure
  result.matrix[,6] <- as.numeric(coef(cvfit.Lasso.C, s = "lambda.min")!=0)
  result.matrix[,8] <- as.numeric(coef(cvfit.Lasso.C, s = "lambda.1se")!=0)



  ### Return
  result.matrix.final <- ifelse(result.matrix[order(rowSums(result.matrix)),]==0, "nein", "ja")
  return(result.matrix.final)
}
