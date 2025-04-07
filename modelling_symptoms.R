modelling.symptoms <- function(out_dir,
                               covs) {
  
  
  ################################## MODEL 2: CHANGE IN SLOPE ##################################
  
  ### add time since vaccination to exposure list
  exposures_slope <- c("flag_dose1",
                       "flag_dose2",
                       "I(weeks_dose1*as.numeric(as.character(flag_dose1)))",
                       "I(weeks_dose2*as.numeric(as.character(flag_dose2)))")
  
  symptoms <- c("lc_fever", "lc_weakness_tiredness", "lc_diarrhoea", "lc_loss_of_smell",
                "lc_shortness_of_breath", "lc_vertigo_dizziness", "lc_trouble_sleeping", "lc_headache",
                "lc_nausea_vomiting", "lc_loss_of_appetite", "lc_sore_throat", "lc_chest_pain", "lc_worry_anxiety",
                "lc_memory_loss_confusion", "lc_muscle_ache", "lc_abdominal_pain", "lc_loss_of_taste", "lc_cough",
                "lc_palpitations", "lc_low_mood_not_enjoying", "lc_concentrate", "lc_over_5_symptoms", "lc_over_3_symptoms")
  
  # df <- data.frame(symptoms = symptoms)
  # for (i in 1:length(symptoms)){
  #   num_reports_list[[i]] <- sum(dat[[symptoms[i]]])
  # }
  # df$num_reports <- num_reports_list
  
  num_reports_list <- as.list(NULL)
  plot_dataframe <- as.list(NULL)

  for (i in 1:length(symptoms)){
    
    symptom <- symptoms[i]
    print(symptom)
    ### number of reports of log covid with symptom
    num_reports_list[[i]] <- sum(dat[[symptom]])
    
    ### any LC with symptom
    mod2 <- fit.mod(outcome = symptom,
                    covariates = covs,
                    exposures = exposures_slope,
                    dataset = dat)
    
#    write.csv(mod2$coeff, file=paste(out_dir, "\\coeffs_mod2_", symptom, ".csv", sep=""))
#    write.csv(mod2$vcov, file=paste(out_dir, "\\vcov_mod2_", symptom, ".csv", sep=""))
    
    ################################### COLLATE MODEL METRICS ###################################
    
    
    ### AIC and AUROC statistics
    model_metrics <- data.frame(
      outcome = symptom,
      aic = mod2$aic,
      bic = mod2$bic,
      auroc = mod2$auc
    )
    
#    write.csv(model_metrics, file=paste(out_dir, "\\model_metrics_", symptom, ".csv", sep=""), row.names=FALSE)
    
    ### VIFs
    all_vars <- rownames(mod2$vif)
    
    all_vars <- data.frame(characteristic=all_vars[!duplicated(all_vars)])
    
    mod2_vif <- data.frame(characteristic=rownames(mod2$vif), mod2=mod2$vif[,3])
    
    
    model_vifs <- sqldf("
  select a.*, c.mod2
  from all_vars as a
  left join mod2_vif as c on a.characteristic=c.characteristic
")
    
#    write.csv(model_vifs, file=paste(out_dir, "\\model_vifs_", symptom, ".csv", sep=""), row.names=FALSE)
    
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
    )
    
    
    
    ### generate predicted probabilities for model 2 (levels + slopes)
    pred_df_mod2 <- data.frame(
      model = "Changes in level and slope",
      outcome = c(rep(paste0("Long COVID with ", symptom), nrow(pred_df))),
      weeks = rep(pred_df$weeks),
      rbind(data.frame(predict.robust(mod2$mod, mod2$vcov, pred_df))))
    
    pred_df_mod <- pred_df_mod2
    plot_dataframe[[i]] <- pred_df_mod
    # pred_df_mod$outcome <- relevel(pred_df_mod$outcome, ref="Any severity")
    
    ### plot predicted probabilities
    pred_plot <- ggplot(pred_df_mod, aes(x=weeks, y=prob)) +
      geom_line() +
      geom_ribbon(aes(ymin=prob_lcl, ymax=prob_ucl, colour=NULL), alpha=0.1) +
      geom_vline(xintercept=c(24,36), linetype="dashed", colour="grey50", size=0.5) +
      #    facet_wrap(~model, nrow=1) +
      scale_x_continuous(limits=c(12,48), breaks=c(12,24,36,48), expand=c(0,0)) +
      #scale_y_continuous(limits=c(0,0.20), expand=c(0,0)) +
      # scale_colour_manual(values=c("blue", "orange")) +
      # scale_fill_manual(values=c("blue", "orange")) +
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
    
#    ggsave(plot=pred_plot,
#           filename=paste(out_dir, "\\pred_probs_", symptom, ".jpg", sep=""),
#           width=20,
#           height=12,
#           units="cm")
    
  }
  
  # find top 10 symptoms
  df <- data.frame(cbind(symptoms=symptoms[1:21], num_reports_list=num_reports_list[1:21]))
  df$number <- 1:nrow(df)
  df <- as.data.frame(lapply(df, unlist))
  
  ordered_df <- df[order(-df$num_reports_list),]$number[1:10]
  
  top_10_data <- rbind(data.frame(plot_dataframe[ordered_df[1]]), data.frame(plot_dataframe[ordered_df[2]]), 
                       data.frame(plot_dataframe[ordered_df[3]]), data.frame(plot_dataframe[ordered_df[4]]), 
                       data.frame(plot_dataframe[ordered_df[5]]), data.frame(plot_dataframe[ordered_df[6]]), 
                       data.frame(plot_dataframe[ordered_df[7]]), data.frame(plot_dataframe[ordered_df[8]]), 
                       data.frame(plot_dataframe[ordered_df[9]]), data.frame(plot_dataframe[ordered_df[10]])) 
  
  num_sympt_data <- rbind(data.frame(plot_dataframe[22]), data.frame(plot_dataframe[23]))
  
  ### order symptoms according to probability at 12 weeks
  week12 <- top_10_data[!duplicated(top_10_data$outcome),]
  week12 <- week12[order(week12$prob, decreasing=TRUE),]
  top_10_data$outcome <- factor(top_10_data$outcome, levels=week12$outcome)
  
  levels(top_10_data$outcome) <- c("Weakness and tiredness",
                                   "Loss of smell",
                                   "Shortness of breath", 
                                   "Loss of taste",
                                   "Headache",
                                   "Trouble sleeping",
                                   "Difficulty concentrating",
                                   "Muscle ache", 
                                   "Worry or anxiety",
                                   "Memory loss or confusion"
                                   )
  
  num_sympt_data$outcome <- factor(num_sympt_data$outcome, 
                                   levels=c("Long COVID with lc_over_5_symptoms",
                                   "Long COVID with lc_over_3_symptoms"))
  
  levels(num_sympt_data$outcome) <- c("Over 5 symptoms", "Over 3 symptoms")

  
  all_data <- rbind(top_10_data, num_sympt_data)
  
#  write.csv(top_10_data, paste0(out_dir, "\\pred_plot_symptoms_data.csv"), row.names=FALSE)
  write.csv(all_data, paste0(out_dir, "\\pred_plot_symptoms_data_12.csv"), row.names=FALSE)
  
  ### plot predicted probabilities
  pred_plot <- ggplot(top_10_data, aes(x=weeks, y=prob, color=outcome)) +
    geom_line() +
    #  geom_ribbon(aes(ymin=prob_lcl, ymax=prob_ucl, colour=NULL, fill=outcome), alpha=0.1) +
    geom_vline(xintercept=c(24,36), linetype="dashed", colour="grey50", size=0.5) +
    #  facet_wrap(~outcome, nrow=1) +
    scale_x_continuous(limits=c(12,48), breaks=c(12,24,36,48), expand=c(0,0)) +
    #scale_y_continuous(limits=c(0,0.20), expand=c(0,0)) +
    #  scale_colour_manual(values=c("blue", "orange")) +
    #  scale_fill_manual(values=c("blue", "orange")) +
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
      legend.position="right",
      legend.direction="vertical",
      legend.justification="center",
      legend.text=element_text(size=12, colour="black", face="plain")
    )
  
  
  ggsave(plot=pred_plot,
         filename=paste(out_dir, "\\pred_probs_top_10_symptoms.jpg", sep=""),
         width=20,
         height=12,
         units="cm")
  
  ################################### COMBINE RESULTS ###################################
  
  sympts <- c(
    "weakness_tiredness",
    "shortness_of_breath",
    "loss_of_smell",
    "muscle_ache",
    "concentrate",
    "trouble_sleeping",
    "headache",
    "loss_of_taste",
    "memory_loss_confusion",
    "worry_anxiety"
  )
  
  level1_coeff <- NULL
  level2_coeff <- NULL
  slope1_coeff <- NULL
  slope2_coeff <- NULL
  
  level1_p <- NULL
  level2_p <- NULL
  slope1_p <- NULL
  slope2_p <- NULL
  
  level1_est <- NULL
  level2_est <- NULL
  slope1_est <- NULL
  slope2_est <- NULL
  
  for(i in 1:length(sympts)) {
    
    dat_out <- read.csv(paste(out_dir, "\\coeffs_mod2_lc_", sympts[i], ".csv", sep=""))
    
    level1_coeff[i] <- dat_out[2,2]
    level2_coeff[i] <- dat_out[3,2]
    slope1_coeff[i] <- dat_out[4,2]
    slope2_coeff[i] <- dat_out[5,2]
    
    level1_p[i] <- dat_out[2,5]
    level2_p[i] <- dat_out[3,5]
    slope1_p[i] <- dat_out[4,5]
    slope2_p[i] <- dat_out[5,5]
    
    level1_est[i] <- (exp(dat_out[2,2]) - 1) * 100
    level2_est[i] <- (exp(dat_out[3,2]) - 1) * 100
    slope1_est[i] <- (exp(dat_out[6,2] + dat_out[4,2]) - 1) * 100
    slope2_est[i] <- (exp(dat_out[6,2] + dat_out[4,2] + dat_out[5,2]) - 1) * 100
    
  }
  
  level1_df <- data.frame(effect="level", dose=1, symptom=sympts, coeff=level1_coeff, pvalue=level1_p, est_pct=level1_est)
  level1_df <- level1_df[order(level1_df$est_pct),]
  
  level2_df <- data.frame(effect="level", dose=2, symptom=sympts, coeff=level2_coeff, pvalue=level2_p, est_pct=level2_est)
  level2_df <- level2_df[order(level2_df$est_pct),]
  
  slope1_df <- data.frame(effect="slope", dose=1, symptom=sympts, coeff=slope1_coeff, pvalue=slope1_p, est_pct=slope1_est)
  slope1_df <- slope1_df[order(slope1_df$est_pct),]
  
  slope2_df <- data.frame(effect="slope", dose=2, symptom=sympts, coeff=slope2_coeff, pvalue=slope2_p, est_pct=slope2_est)
  slope2_df <- slope2_df[order(slope2_df$est_pct),]
  
  out_df <- rbind(level1_df, level2_df, slope1_df, slope2_df)
  write.csv(out_df, file=paste(out_dir, "\\combined_results.csv", sep=""), row.names=FALSE)
  
}
