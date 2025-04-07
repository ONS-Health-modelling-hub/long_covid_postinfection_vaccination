fit.mod <- function(outcome, exposures, covariates, dataset) {

  mod_call <- paste(c(
    "glm(",
    outcome,
    " ~ ",
    paste(c(exposures, covariates), collapse=" + "),
    ", data=dataset",
    ", family=binomial)"
  ), collapse="")

mod <- eval(parse(text=mod_call))
vcov <- vcovCL(mod, cluster=~participant_id)
coeff <- coeftest(mod, vcov=vcov)
vif <- vif(mod)
aic <- AIC(mod)
bic <- BIC(mod)
auc <- as.numeric(auc(roc(response=mod$y,
                          predictor=mod$fitted.values,
                          levels=0:1, direction="<")))


return(list(mod=mod, vcov=vcov, coeff=coeff, vif=vif, aic=aic, bic=bic, auc=auc))

}
