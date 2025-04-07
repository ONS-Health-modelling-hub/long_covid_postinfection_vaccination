het.effects.vaccine <- function(out_dir, covs) {

    modifier = "vaccine_vector"

  #################################### HETEROGENEOUS EFFECTS ####################################
      
    ### interact dose 1 and 2 flags and slopes with selected effect modifier
    exposures_het <- paste(c("flag_dose1",
                               "flag_dose2",
                               "I(weeks_dose1*as.numeric(as.character(flag_dose1)))",
                               "I(weeks_dose2*as.numeric(as.character(flag_dose2)))"), modifier, sep="*")

    ### fit model
    mod_het_any <- fit.mod(outcome = "lc",
                         covariates = covs,
                         exposures = exposures_het,
                         dataset = dat)
    
    ### fit model
    mod_het_lim <- fit.mod(outcome = "lc_lim",
                           covariates = covs,
                           exposures = exposures_het,
                           dataset = dat)
    
    write.csv(mod_het_any$coeff, file=paste(out_dir, "\\coeffs_mod2_het_any.csv", sep=""))
    write.csv(mod_het_lim$coeff, file=paste(out_dir, "\\coeffs_mod2_het_lim.csv", sep=""))
    
    write.csv(mod_het_any$vcov, file=paste(out_dir, "\\vcov_mod2_het_any.csv", sep=""))
    write.csv(mod_het_lim$vcov, file=paste(out_dir, "\\vcov_mod2_het_lim.csv", sep=""))

    ### set up prediction matrix for time trajectory plots
    ### spanning week 13 to week 48, with dose 1 in week 24 and dose 2 in week 36
    ### for covariates, use sample mean age and modal levels of categorical variables
    pred_df_0 <- data.frame(
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
      covid_admitted = factor("0", levels=c("0","1")),
      vaccine_vector = factor("0", levels=c("0","1"))
    )

    ### set up prediction matrix for time trajectory plots
    ### spanning week 13 to week 48, with dose 1 in week 24 and dose 2 in week 36
    ### for covariates, use sample mean age and modal levels of categorical variables
    pred_df_1 <- data.frame(
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
      covid_admitted = factor("0", levels=c("0","1")),
      vaccine_vector = factor("1", levels=c("0","1"))
    )

        ### generate predicted probabilities for model 2 (levels + slopes)
    pred_df_mod2_0 <- data.frame(
      outcome = c(rep("Any severity",nrow(pred_df_0)),
                  rep("Activity limiting",nrow(pred_df_0))),
      outcome_vacc_type = c(rep("Any severity 0",nrow(pred_df_0)),
                  rep("Activity limiting 0",nrow(pred_df_0))),
      vaccine_type = c(rep("mRNA vaccinated",nrow(pred_df_0)*2)),
      weeks = rep(pred_df_0$weeks,2),
      rbind(data.frame(predict.robust(mod_het_any$mod, mod_het_any$vcov, pred_df_0)),
            data.frame(predict.robust(mod_het_lim$mod, mod_het_lim$vcov, pred_df_0))))

    
    pred_df_mod2_1 <- data.frame(
      outcome = c(rep("Any severity",nrow(pred_df_1)),
                  rep("Activity limiting",nrow(pred_df_1))),
      outcome_vacc_type = c(rep("Any severity 1",nrow(pred_df_1)),
                            rep("Activity limiting 1",nrow(pred_df_1))),
      vaccine_type = c(rep("Vector vaccinated",nrow(pred_df_1)*2)),
                       weeks = rep(pred_df_1$weeks,2),
                       rbind(data.frame(predict.robust(mod_het_any$mod, mod_het_any$vcov, pred_df_1)),
                             data.frame(predict.robust(mod_het_lim$mod, mod_het_lim$vcov, pred_df_1)))
      )
 
    pred_df_mod <- rbind(pred_df_mod2_0, pred_df_mod2_1)
    pred_df_mod$vaccine_type <- as.factor(pred_df_mod$vaccine_type)
    pred_df_mod$outcome_vacc_type <- as.factor(pred_df_mod$outcome_vacc_type)

    ### stack predicted probabilities
    #  pred_df_mod <- rbind(pred_df_mod1, pred_df_mod2)
    pred_df_mod$outcome <- relevel(pred_df_mod$outcome, ref="Any severity")

    write.csv(pred_df_mod, paste0(out_dir, "\\pred_plot_vaccination_data.csv"), row.names=FALSE)
    
    ### plot predicted probabilities
    pred_plot <- ggplot(pred_df_mod, aes(x=weeks, y=prob, colour=vaccine_type)) +
      geom_line(aes(linetype=outcome)) +
      geom_ribbon(aes(ymin=prob_lcl, ymax=prob_ucl, colour=NULL, fill=outcome_vacc_type), alpha=0.1) +
      geom_vline(xintercept=c(24,36), linetype="dashed", colour="grey50", size=0.5) +
      scale_x_continuous(limits=c(12,48), breaks=c(12,24,36,48), expand=c(0,0)) +
      #scale_y_continuous(limits=c(0,0.20), expand=c(0,0)) +
      scale_colour_manual(values=c("blue", "red")) +
      scale_fill_manual(values=c("blue", "blue", "red", "red"), guide=FALSE) +
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
           filename=paste(out_dir, "\\pred_probs_het_effect.jpg", sep=""),
           width=20,
           height=12,
           units="cm")
    
}