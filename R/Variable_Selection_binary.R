#' @export
Variable_Selection_binary <- function(df., outcome){
  # df.: Data set
  # outcome: outcome of interest (factor variable), must be included in df.

  # to add: baum modelle



  # needed function to extract variables selected in variable selection method
  Selected_Variables <- function(predictor.variablennames, var_selected){
    h <- rep(NA, length(predictor.variablennames))
    #var_selected <- coef(cvfit.Lasso.default, s = "lambda.min")@Dimnames[[1]][which(as.numeric(coef(cvfit.Lasso.default, s = "lambda.min")!=0)==1)][-1]

    for(j in 1:length(predictor.variablennames)){
      for (k in 1:length(var_selected)) {
        var <- var_selected[k]
        for(i in 0:nchar(var)){
          if(str_sub(var, start = 0, end = nchar(var)-i) == predictor.variablennames[j]){
            h[j] <- 1
            break
          }
        }
      }

      if(is.na(h[j])){ h[j]=0 }
    }
    return(h)
  }



  # Variables used for prediction
  predictor.variablennames <- names(df. %>% dplyr::select(-c(!!outcome)))
  # Result matrix
  result.matrix <- matrix(NA, nrow = dim(df.)[2]-1, ncol = 12,
                          dimnames = list(Variables = predictor.variablennames,
                                          Method    = c("StepSelect AIC: Both (full model)",
                                                        "StepSelect AIC: Both (null model)",
                                                        "StepSelect AIC: Backward",
                                                        "StepSelect AIC: Forward",
                                                        "LassoCV (lambda.min), deviance",
                                                        "LassoCV (lambda.min), misclassification error",
                                                        "LassoCV (lambda.min), MSE",
                                                        "LassoCV (lambda.min), MAE",
                                                        "LassoCV (lambda.1se), deviance",
                                                        "LassoCV (lambda.1se), misclassification error",
                                                        "LassoCV (lambda.1se), MSE",
                                                        "LassoCV (lambda.1se), MAE")))


  ### AIC: Stepwise selection
  fit.AIC.full <- glm(formula = as.formula(paste0(outcome, "~1+.")), data = df., family = binomial)
  fit.AIC.null <- glm(formula = as.formula(paste0(outcome, "~1")),   data = df., family = binomial)
  # Both starting with full model
  fit.AIC.bothFull <- stepAIC(object = fit.AIC.full,
                              scope =list(upper = fit.AIC.full,
                                          lower = fit.AIC.null),
                              direction = "both", trace = FALSE)
  result.matrix[,1] <- ifelse(predictor.variablennames %in% names(fit.AIC.bothFull$model)[-1], 1, 0)

  # Both starting with null model
  fit.AIC.bothNull <- stepAIC(object = fit.AIC.null,
                              scope =list(upper = fit.AIC.full,
                                          lower = fit.AIC.null),
                              direction = "both", trace = FALSE)
  result.matrix[,2] <- ifelse(predictor.variablennames %in% names(fit.AIC.bothNull$model)[-1], 1, 0)

  # Backward starting with full model
  fit.AIC.backward <- stepAIC(object = fit.AIC.full,
                              scope =list(upper = fit.AIC.full,
                                          lower = fit.AIC.null),
                              direction = "backward", trace = FALSE)
  result.matrix[,3] <- ifelse(predictor.variablennames %in% names(fit.AIC.backward$model)[-1], 1, 0)

  # Forward starting with null model
  fit.AIC.forward <- stepAIC(object = fit.AIC.null,
                             scope =list(upper = fit.AIC.full,
                                         lower = fit.AIC.null),
                             direction = "forward", trace = FALSE)
  result.matrix[,4] <- ifelse(predictor.variablennames %in% names(fit.AIC.forward$model)[-1], 1, 0)



  ### LASSO (ridge macht kein Sinn, da dort keine Variablen komplett entfernt werden)
  x <- model.matrix(~ ., df. %>% dplyr::select(-c(!!outcome)))[,-1]
  y <- df. %>% dplyr::select(!!outcome) %>% as.matrix()

  fit.Lasso <- glmnet(x, y, family = "binomial", alpha = 1) #alpha: 1-->lasso, 0-->ridge
  ## cross validation:
  set.seed(123456)
  cvfit.Lasso.default <- cv.glmnet(x, y, family = "binomial", alpha = 1, type.measure = "default") # deviance

  set.seed(123456)
  cvfit.Lasso.class   <- cv.glmnet(x, y, family = "binomial", alpha = 1, type.measure = "class")  # misclassification error

  set.seed(123456)
  cvfit.Lasso.mse     <- cv.glmnet(x, y, family = "binomial", alpha = 1, type.measure = "mse")  # Mean-Squared Error

  set.seed(123456)
  cvfit.Lasso.mae     <- cv.glmnet(x, y, family = "binomial", alpha = 1, type.measure = "mae")  # Mean Absolute Error

  ## Variables left in final model
  # deviance
  result.matrix[,5] <- Selected_Variables(predictor.variablennames = predictor.variablennames,
                                          var_selected = coef(cvfit.Lasso.default, s = "lambda.min")@Dimnames[[1]][which(as.numeric(coef(cvfit.Lasso.default, s = "lambda.min")!=0)==1)][-1])

  result.matrix[,9] <- Selected_Variables(predictor.variablennames = predictor.variablennames,
                                          var_selected = coef(cvfit.Lasso.default, s = "lambda.1se")@Dimnames[[1]][which(as.numeric(coef(cvfit.Lasso.default, s = "lambda.1se")!=0)==1)][-1])

  # misclassification error
  result.matrix[,6]  <- Selected_Variables(predictor.variablennames = predictor.variablennames,
                                           var_selected = coef(cvfit.Lasso.class, s = "lambda.min")@Dimnames[[1]][which(as.numeric(coef(cvfit.Lasso.class, s = "lambda.min")!=0)==1)][-1])
  result.matrix[,10] <- Selected_Variables(predictor.variablennames = predictor.variablennames,
                                           var_selected = coef(cvfit.Lasso.class, s = "lambda.1se")@Dimnames[[1]][which(as.numeric(coef(cvfit.Lasso.class, s = "lambda.1se")!=0)==1)][-1])

  # Mean-Squared Error
  result.matrix[,7]  <- Selected_Variables(predictor.variablennames = predictor.variablennames,
                                           var_selected = coef(cvfit.Lasso.mse, s = "lambda.min")@Dimnames[[1]][which(as.numeric(coef(cvfit.Lasso.mse, s = "lambda.min")!=0)==1)][-1])
  result.matrix[,11] <- Selected_Variables(predictor.variablennames = predictor.variablennames,
                                           var_selected = coef(cvfit.Lasso.mse, s = "lambda.1se")@Dimnames[[1]][which(as.numeric(coef(cvfit.Lasso.mse, s = "lambda.1se")!=0)==1)][-1])

  # Mean Absolute Error
  result.matrix[,8]  <- Selected_Variables(predictor.variablennames = predictor.variablennames,
                                           var_selected = coef(cvfit.Lasso.mae, s = "lambda.min")@Dimnames[[1]][which(as.numeric(coef(cvfit.Lasso.mae, s = "lambda.min")!=0)==1)][-1])
  result.matrix[,12] <- Selected_Variables(predictor.variablennames = predictor.variablennames,
                                           var_selected = coef(cvfit.Lasso.mae, s = "lambda.1se")@Dimnames[[1]][which(as.numeric(coef(cvfit.Lasso.mae, s = "lambda.1se")!=0)==1)][-1])


  ### Return
  result.matrix.final <- ifelse(result.matrix[order(rowSums(result.matrix)),]==0, "nein", "ja")
  return(result.matrix.final)
}
