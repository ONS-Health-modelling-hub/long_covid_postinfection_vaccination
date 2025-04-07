modelling <- function(out_dir,
                      covs,
                      min_n_before_dose1 = 0, min_n_before_dose2 = 0,
                      min_n_after_dose1 = 0, min_n_after_dose2 = 0,
                      min_time_dose1 = 0, min_time_dose2 = 0,
                      not_infected_wave_1 = 0, truncate_wave_1 = 0,
                      not_first_visit_pos_swab = 0,
                      unvaccinated = 0, vaccinated_after_study = 0) {

  ######################################### DATA PREP #########################################

  ### apply filters
  dat <- dat[is.na(dat$n_before_dose1) | dat$n_before_dose1 >= min_n_before_dose1,]
  dat <- dat[is.na(dat$n_before_dose2) | dat$n_before_dose2 >= min_n_before_dose2,]
  dat <- dat[is.na(dat$n_after_dose1) | dat$n_after_dose1 >= min_n_after_dose1,]
  dat <- dat[is.na(dat$n_after_dose2) | dat$n_after_dose2 >= min_n_after_dose2,]
  dat <- dat[is.na(dat$time_dose1) | (dat$time_dose1 < 0 | dat$time_dose1 >= min_time_dose1),]
  dat <- dat[is.na(dat$time_dose2) | (dat$time_dose2 < 0 | dat$time_dose2 >= min_time_dose2),]
  
  
  if (not_infected_wave_1 == 1){
    dat <- dat[dat$infection_date >= as.numeric(as.Date("2020-09-23")),]
  }
  
  if (truncate_wave_1 == 1){
    dat$infection_date <- ifelse(dat$infection_date < as.numeric(as.Date("2020-09-23")),
                                 as.numeric(as.Date("2020-09-23")), dat$infection_date)
  }
  
  if (not_first_visit_pos_swab == 1){
    dat <- dat[dat$flag_first_visit_pos_swab == 0,]
  }
  
  if (unvaccinated == 1){
    dat <- dat[dat$vacc_after_study==0,]
  }
  
  if (vaccinated_after_study == 1){
    dat <- dat[dat$vaccinated==1,]
  }
  dat_pers <- dat[dat$visit_date==dat$last_visit_date,]
  print(nrow(dat_pers))  
  ################################## MODEL 1: CHANGE IN LEVEL ##################################
  # 
  # ### define exposures
  # exposures_level <- c("flag_dose1", "flag_dose2")

  # ### any LC
  # mod1_any <- fit.mod(outcome = "lc",
  #                     covariates = covs,
  #                     exposures = exposures_level,
  #                     dataset = dat)
  
  # ### activity-limiting LC
  # mod1_lim <- fit.mod(outcome = "lc_lim",
  #                     covariates = covs,
  #                     exposures = exposures_level,
  #                     dataset = dat)

  # write.csv(mod1_any$coeff, file=paste(out_dir, "\\coeffs_mod1_any.csv", sep=""))
  # write.csv(mod1_lim$coeff, file=paste(out_dir, "\\coeffs_mod1_lim.csv", sep=""))
  
  ################################## MODEL 2: CHANGE IN SLOPE ##################################
  
  ### add time since vaccination to exposure list
  exposures_slope <- c("flag_dose1",
                       "flag_dose2",
                       "I(weeks_dose1*as.numeric(as.character(flag_dose1)))",
                       "I(weeks_dose2*as.numeric(as.character(flag_dose2)))")
  
  ### any LC
  mod2_any <- fit.mod(outcome = "lc",
                      covariates = covs,
                      exposures = exposures_slope,
                      dataset = dat)
  
  ### activity-limiting LC
  mod2_lim <- fit.mod(outcome = "lc_lim",
                      covariates = covs,
                      exposures = exposures_slope,
                      dataset = dat)
  
  
  write.csv(mod2_any$coeff, file=paste(out_dir, "\\coeffs_mod2_any.csv", sep=""))
  write.csv(mod2_lim$coeff, file=paste(out_dir, "\\coeffs_mod2_lim.csv", sep=""))
  
  write.csv(mod2_any$vcov, file=paste(out_dir, "\\vcov_mod2_any.csv", sep=""))
  write.csv(mod2_lim$vcov, file=paste(out_dir, "\\vcov_mod2_lim.csv", sep=""))
  
  ################################### COLLATE MODEL METRICS ###################################
  
  # ### AIC and AUROC statistics
  # model_metrics <- data.frame(
  #   outcome = c("any lc", "any lc", "limiting lc", "limiting lc"),
  #   model = c("levels", "levels + slopes", "levels", "levels + slopes"),
  #   aic = c(mod1_any$aic, mod2_any$aic, mod1_lim$aic, mod2_lim$aic),
  #   bic = c(mod1_any$bic, mod2_any$bic, mod1_lim$bic, mod2_lim$bic),
  #   auroc = c(mod1_any$auc, mod2_any$auc, mod1_lim$auc, mod2_lim$auc)
  # )
  
  ### AIC and AUROC statistics
  model_metrics <- data.frame(
    outcome = c("any lc", "limiting lc"),
    aic = c(mod2_any$aic, mod2_lim$aic),
    bic = c(mod2_any$bic, mod2_lim$bic),
    auroc = c(mod2_any$auc, mod2_lim$auc)
  )
  
  write.csv(model_metrics, file=paste(out_dir, "\\model_metrics.csv", sep=""), row.names=FALSE)
  
  # ### VIFs
  # all_vars <- c(rownames(mod1_any$vif),
  #               rownames(mod2_any$vif),
  #               rownames(mod1_lim$vif),
  #               rownames(mod2_lim$vif))
  
  ### VIFs
  all_vars <- c(rownames(mod2_any$vif),
                rownames(mod2_lim$vif))
  
  all_vars <- data.frame(characteristic=all_vars[!duplicated(all_vars)])
  
  # mod1_any_vif <- data.frame(characteristic=rownames(mod1_any$vif), mod1_any=mod1_any$vif[,3])
  # mod1_lim_vif <- data.frame(characteristic=rownames(mod1_lim$vif), mod1_lim=mod1_lim$vif[,3])
  
  mod2_any_vif <- data.frame(characteristic=rownames(mod2_any$vif), mod2_any=mod2_any$vif[,3])
  mod2_lim_vif <- data.frame(characteristic=rownames(mod2_lim$vif), mod2_lim=mod2_lim$vif[,3])
  
#   model_vifs <- sqldf("
#   select a.*, b.mod1_any, c.mod2_any, d.mod1_lim, e.mod2_lim
#   from all_vars as a
#   left join mod1_any_vif as b on a.characteristic=b.characteristic
#   left join mod2_any_vif as c on a.characteristic=c.characteristic
#   left join mod1_lim_vif as d on a.characteristic=d.characteristic
#   left join mod2_lim_vif as e on a.characteristic=e.characteristic
# ")
  
  model_vifs <- sqldf("
  select a.*, c.mod2_any, e.mod2_lim
  from all_vars as a
  left join mod2_any_vif as c on a.characteristic=c.characteristic
  left join mod2_lim_vif as e on a.characteristic=e.characteristic
")
  
  write.csv(model_vifs, file=paste(out_dir, "\\model_vifs.csv", sep=""), row.names=FALSE)
  
  ################################### TIME TRAJECTORY PLOTS ###################################
  
  ### set up prediction matrix for time trajectory plots
  ### spanning week 13 to week 48, with dose 1 in week 24 and dose 2 in week 36
  ### for covariates, use sample mean age and modal levels of categorical variables
  pred_df <- data.frame(
    weeks = (85:336)/7,
    flag_dose1 = factor(c(rep(0,84), rep(1,168))),
    weeks_dose1 = ((-83):168)/7,
    flag_dose2 = factor(c(rep(0,168), rep(1,84))),
    weeks_dose2 = ((-167):84)/7,
    calendar_time_infection = 250,
    age_at_visit = 50,
    female = factor("1", levels=c("0","1")),
    non_white = factor("0", levels=c("0","1")),
    imd_quintile = factor("5", levels=as.character(1:5)),
    gor9d = factor("7", levels=as.character(1:12)),
    health_conditions = factor("0", levels=c("0","1")),
    hscw_pf = factor("0", levels=c("0","1")),
    covid_admitted = factor("0", levels=c("0","1"))
#    infected_wave_1 = factor("0", levels=c("0","1"))
  )
  
  ### generate predicted probabilities for model 1 (levels)
  # pred_df_mod1 <- data.frame(
  #   model = "Changes in level",
  #   outcome = c(rep("Any severity",nrow(pred_df)),
  #               rep("Activity limiting",nrow(pred_df))),
  #   weeks = rep(pred_df$weeks,2),
  #   rbind(data.frame(predict.robust(mod1_any$mod, mod1_any$vcov, pred_df)),
  #         data.frame(predict.robust(mod1_lim$mod, mod1_lim$vcov, pred_df)))
  # )
  
  ### generate predicted probabilities for model 2 (levels + slopes)
  pred_df_mod2 <- data.frame(
    model = "Changes in level and slope",
    outcome = c(rep("Any severity",nrow(pred_df)),
                rep("Activity limiting",nrow(pred_df))),
    weeks = rep(pred_df$weeks,2),
    rbind(data.frame(predict.robust(mod2_any$mod, mod2_any$vcov, pred_df)),
          data.frame(predict.robust(mod2_lim$mod, mod2_lim$vcov, pred_df)))
  )
  pred_df_mod <- pred_df_mod2
  ### stack predicted probabilities
#  pred_df_mod <- rbind(pred_df_mod1, pred_df_mod2)
  pred_df_mod$outcome <- relevel(pred_df_mod$outcome, ref="Any severity")
  
  write.csv(pred_df_mod, paste0(out_dir, "\\pred_plot_data.csv"), row.names=FALSE)
  ### plot predicted probabilities
  pred_plot <- ggplot(pred_df_mod, aes(x=weeks, y=prob, colour=outcome)) +
    geom_line() +
    geom_ribbon(aes(ymin=prob_lcl, ymax=prob_ucl, colour=NULL, fill=outcome), alpha=0.1) +
    geom_vline(xintercept=c(24,36), linetype="dashed", colour="grey50", size=0.5) +
#    facet_wrap(~model, nrow=1) +
    scale_x_continuous(limits=c(12,48), breaks=c(12,24,36,48), expand=c(0,0)) +
    #scale_y_continuous(limits=c(0,0.20), expand=c(0,0)) +
    scale_colour_manual(values=c("blue", "orange")) +
    scale_fill_manual(values=c("blue", "orange")) +
    ylab("Probability") +
    xlab("Weeks since positive test") +
    theme(
      axis.title=element_text(size=13, colour="black", face="plain"),
      axis.text=element_text(size=12, colour="black", face="plain"),
      axis.line=element_blank(),
      axis.ticks.x=element_line(size=0.5, colour="black"),
      axis.ticks.y=element_line(size=0.5, colour="black"),
      panel.border=element_rect(size=0.5, colour="black", fill=NA),
      panel.background=element_blank(),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      panel.spacing=unit(1.5, "lines"),
      plot.margin=margin(0.2,1,0.2,0.5, unit="lines"),
      strip.background=element_blank(),
      strip.text=element_text(size=13, colour="black", face="bold"),
      legend.title=element_blank(),
      legend.position="bottom",
      legend.direction="horizontal",
      legend.justification="center",
      legend.text=element_text(size=12, colour="black", face="plain")
    )
  
  ggsave(plot=pred_plot,
         filename=paste(out_dir, "\\pred_probs.jpg", sep=""),
         width=20,
         height=12,
         units="cm")
  
}
