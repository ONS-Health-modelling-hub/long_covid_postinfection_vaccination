predict.robust <- function(x, vcov, newdata) {
  
  if(missing(newdata)) {newdata <- x$model}
  
  tt <- delete.response(terms(x))
  m.mat <- model.matrix(tt, data=newdata)
  m.coef <- x$coef
  
  fit <- predict(x, newdata=newdata)
  se.fit <- sqrt(diag(m.mat %*% vcov %*% t(m.mat)))
  t.crit <- qt(0.025, df.residual(x), lower.tail=FALSE)
  
  prob <- 1 / (1 + exp(-fit))
  prob_lcl <- 1 / (1 + exp(-(fit - t.crit*se.fit)))
  prob_ucl <- 1 / (1 + exp(-(fit + t.crit*se.fit)))
  
  return(list(fit=fit, se_fit=se.fit,
              prob=prob, prob_lcl=prob_lcl, prob_ucl=prob_ucl))
  
}
