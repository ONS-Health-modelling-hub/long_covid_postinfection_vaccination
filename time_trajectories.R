time.trajectories <- function(out_dir, covs) {
  
  ###################################### TIME TRAJECTORIES ######################################
  exposures_slope <- NULL
  
  ### filter out visits after first dose
  dat <- dat[dat$visit_date<dat$covid_vaccine_date1,]
  
  ### set up outcome list
  mod2_outcome_list <- c("lc", "lc_lim")
  
  ### set up covariate specifications
  mod2_covs_list <- list(covs[-1],                                 # none
                         c(covs[-1], "weeks"),                     # linear time trend
                         c(covs[-1], "poly(weeks, 2)"),            # second degree polynomial
                         c(covs[-1], "poly(weeks, 3)"),            # third degree polynomial
                         # c(covs[-1], paste("bs(weeks, knots=c(",   # spline with knots at Q1-3
                         #                   knots_time_qtls[1], ",",
                         #                   knots_time_qtls[2], ",",
                         #                   knots_time_qtls[3], "), degree=3)", sep=""))
                         c(covs[-1], paste("bs(weeks, df=6, degree=3)", sep="")),
                         c(covs[-1], paste("ns(weeks, df=2, Boundary.knots=quantile(weeks, c(.10, .90)))", sep=""))) # natural spline with knots at Q1-3

  ### loop over outcomes and specifications
  mod2_test_out <- as.list(NULL)
  mod2_test_list <- as.list(NULL)
  
  for(i in 1:length(mod2_outcome_list)) {

    for(j in 1:length(mod2_covs_list)) {

      ### fit model
      mod2_test <- fit.mod(outcome = mod2_outcome_list[i],
                           covariates = mod2_covs_list[[j]],
                       #    exposures = c("flag_dose1", "flag_dose2"),
                            exposures = exposures_slope,
                           dataset = dat)

      ### collect outputs across specifications within each outcome
      if(length(grep("weeks", rownames(mod2_test$coeff))) > 0) {

        coeff_name <- mod2_covs_list[[j]][grep("weeks", mod2_covs_list[[j]])]
        coeff_pvalue <- paste(round(mod2_test$coeff[grep("weeks", rownames(mod2_test$coeff)),4], 4),
                              collapse="; ")
      } else {
        coeff_name <- NA
        coeff_pvalue <- NA
      }
      
      mod2_test_out[[j]] <- c(mod2_outcome_list[i],
                              coeff_name,
                              coeff_pvalue,
                              mod2_test$aic,
                              mod2_test$bic,
                              mod2_test$auc)

    }

    mod2_test_list[[i]] <- t(data.frame(mod2_test_out))
    
  }
  
  ### collect outputs across outcomes
  mod2_test_df <- mod2_test_list[[1]]
  if(length(mod2_outcome_list)>1) {
    for(i in 2:length(mod2_outcome_list)) {mod2_test_df <- rbind(mod2_test_df, mod2_test_list[[i]])}
  }
  
  mod2_test_df <- as.data.frame(mod2_test_df)

  
  rownames(mod2_test_df) <- NULL
  colnames(mod2_test_df) <- c("outcome", "time_var", "time_pvalues", "aic", "bic", "auroc")
  
  write.csv(mod2_test_df,
            file=paste(out_dir, "\\test_time_trajectories_mod2.csv", sep=""),
            row.names=FALSE)
  
}
