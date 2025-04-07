descriptive.stats <- function(out_dir, n_plots, seed_for_plots, get_hh_icc, covs_for_hh_icc) {
  
  ####################################### FOLLOW-UP TIME STATS #######################################

  
  ### person-level dataset: snapshot at last follow-up visit
  dat_pers <- dat[dat$visit_date==dat$last_visit_date,]
  
  ### calculate follow-up time stats
  time_since_infection <- c(as.numeric(summary(dat_pers$time)), sd(dat_pers$time))
  time_since_dose1 <- c(as.numeric(summary(dat_pers$time_dose1)), sd(dat_pers$time_dose1))
  time_since_dose2 <- c(as.numeric(summary(dat_pers$time_dose2)), sd(dat_pers$time_dose2))
  time_since_first_lc_response <- c(as.numeric(summary(dat_pers$visit_date - dat_pers$first_lc_response_date)),
                                    sd(dat_pers$visit_date - dat_pers$first_lc_response_date))

  fu_time_stats <- data.frame(time_since_infection, time_since_dose1,
                              time_since_dose2, time_since_first_lc_response)
  
  rownames(fu_time_stats) <- c("min", "q1", "median", "mean", "q3", "max", "sd")

  write.csv(fu_time_stats, file=paste(out_dir, "\\fu_time_stats.csv", sep=""))
  
  ############################### COUNTS OF VACCINATED DURING FOLLOW-UP ##############################
  
  ### age-by-health cross-tab of counts (all participants, received dose 1, received dose 2)
  vaccinated_during_fu_xtab <- sqldf("
  select
    a.*, b.n_dose1, c.n_dose2,
    round(b.n_dose1/a.n_total*100, 2) as pct_dose1,
    round(c.n_dose2/a.n_total*100, 2) as pct_dose2
  from(
    select age10, health_conditions, count(*) as n_total
    from dat_pers
    group by age10, health_conditions
  ) as a
  left join(
    select age10, health_conditions, sum(during_fu_dose1) as n_dose1
    from dat_pers
    group by age10, health_conditions
  ) as b
  on a.age10=b.age10 and a.health_conditions=b.health_conditions
  left join(
    select age10, health_conditions, sum(during_fu_dose2) as n_dose2
    from dat_pers
    group by age10, health_conditions
  ) as c
  on a.age10=c.age10 and a.health_conditions=c.health_conditions
  ")
  
  ### write out to working directory
  write.csv(vaccinated_during_fu_xtab,
            file=paste(out_dir, "\\vaccinated_during_fu_xtab.csv", sep=""),
            row.names=FALSE)
  
  ### age-by-health cross-tab of counts (all participants, received dose 1, received dose 2)
  vaccinated_during_fu_xtab_just_age <- sqldf("
  select
    a.*, b.n_dose1, c.n_dose2,
    round(b.n_dose1/a.n_total*100, 2) as pct_dose1,
    round(c.n_dose2/a.n_total*100, 2) as pct_dose2
  from(
    select age10, count(*) as n_total
    from dat_pers
    group by age10
  ) as a
  left join(
    select age10, sum(during_fu_dose1) as n_dose1
    from dat_pers
    group by age10
  ) as b
  on a.age10=b.age10 
  left join(
    select age10, sum(during_fu_dose2) as n_dose2
    from dat_pers
    group by age10
  ) as c
  on a.age10=c.age10
  ")
  
  ### write out to working directory
  write.csv(vaccinated_during_fu_xtab_just_age,
            file=paste(out_dir, "\\vaccinated_during_fu_xtab_just_age.csv", sep=""),
            row.names=FALSE)
  
  ################################## POST-VACCINE MEASUREMENT STATS ##################################
  
  ### calculate distributional stats
  n_before_dose1 <- c(as.numeric(summary(dat_pers$n_before_dose1)), sd(dat_pers$n_before_dose1))
  n_before_dose2 <- c(as.numeric(summary(dat_pers$n_before_dose2)), sd(dat_pers$n_before_dose2))
  n_after_dose1 <- c(as.numeric(summary(dat_pers$n_after_dose1)), sd(dat_pers$n_after_dose1))
  n_after_dose2 <- c(as.numeric(summary(dat_pers$n_after_dose2)), sd(dat_pers$n_after_dose2))
  
  n_measurement_stats <- data.frame(n_before_dose1, n_before_dose2,
                                    n_after_dose1, n_after_dose2)
  
  rownames(n_measurement_stats) <- c("min", "q1", "median", "mean", "q3", "max", "sd")
  
  write.csv(n_measurement_stats, file=paste(out_dir, "\\n_measurement_stats.csv", sep=""))
  
  ### calclulate frequency distributions
  n_measurement_freq <- data.frame(n_measurements = 0:max(dat_pers[,c("n_before_dose1",
                                                                      "n_before_dose2",
                                                                      "n_after_dose1",
                                                                      "n_after_dose2")]))

  n_measurement_freq <- sqldf("

  select *
  from n_measurement_freq as a
  
  left join(
    select n_before_dose1, count(*) as before_dose1
    from dat_pers
    group by n_before_dose1
  ) as b
  on a.n_measurements = b.n_before_dose1
  
  left join(
    select n_before_dose2, count(*) as before_dose2
    from dat_pers
    group by n_before_dose2
  ) as c
  on a.n_measurements = c.n_before_dose2
  
  left join(
    select n_after_dose1, count(*) as after_dose1
    from dat_pers
    group by n_after_dose1
  ) as d
  on a.n_measurements = d.n_after_dose1
  
  left join(
    select n_after_dose2, count(*) as after_dose2
    from dat_pers
    group by n_after_dose2
  ) as e
  on a.n_measurements = e.n_after_dose2
  
")
  
  n_measurement_freq$n_before_dose1 <- NULL
  n_measurement_freq$n_before_dose2 <- NULL
  n_measurement_freq$n_after_dose1 <- NULL
  n_measurement_freq$n_after_dose2 <- NULL

  n_measurement_freq[is.na(n_measurement_freq)] <- 0
  
  write.csv(n_measurement_freq,
            file=paste(out_dir, "\\n_measurement_freq.csv", sep=""),
            row.names=FALSE)
  
  ############################ DEMOGRAPHIC CHARACTERISTICS AT LAST VISIT ############################
  
  ### compare covariate distributions between vaccine types - continuous variables
  cov_dist_cont <- cov.dist.cont(
    vars = c("age_at_visit", "time", "time_dose1", "time_dose2"),
    dataset = dat_pers,
    exposure = "vaccine_vector"
  )
  
  write.csv(cov_dist_cont, file=paste(out_dir, "\\cov_dist_cont.csv", sep=""), row.names=FALSE)
  
  ### compare covariate distributions between vaccine types - categorical variables
  cov_dist_cat <- cov.dist.cat(
    vars = c("age10", "sex", "gor9d", "ethnicityg", "non_white", "imd_quintile", 
             "health_conditions", "health_status", "healthsocial_worker", "hscw_pf",
             "covid_admitted", "covid_nhs_contact"),
    dataset = dat_pers,
    exposure = "vaccine_vector"
  )
  
  write.csv(cov_dist_cat, file=paste(out_dir, "\\cov_dist_cat.csv", sep=""), row.names=FALSE)
  
  ################################ LC BEFORE VS. AFTER VACCINATION ################################
  
  ### cross-tabulate LC counts before and after first/second dose vaccination
  write.csv(before.after.stats(dat)$xtab,
            file=paste(out_dir, "\\before_after_xtab_all.csv", sep=""),
            row.names=FALSE)
  
  write.csv(before.after.stats(dat[dat$vaccine_vector=="1",])$xtab,
            file=paste(out_dir, "\\before_after_xtab_vector.csv", sep=""),
            row.names=FALSE)
  
  write.csv(before.after.stats(dat[dat$vaccine_vector=="0",])$xtab,
            file=paste(out_dir, "\\before_after_xtab_mrna.csv", sep=""),
            row.names=FALSE)
  
  ### calculate distribution of time to/since first/second dose vaccination
  write.csv(before.after.stats(dat)$time,
            file=paste(out_dir, "\\before_after_time_all.csv", sep=""),
            row.names=FALSE)
  
  write.csv(before.after.stats(dat[dat$vaccine_vector=="1",])$time,
            file=paste(out_dir, "\\before_after_time_vector.csv", sep=""),
            row.names=FALSE)
  
  write.csv(before.after.stats(dat[dat$vaccine_vector=="0",])$time,
            file=paste(out_dir, "\\before_after_time_mrna.csv", sep=""),
            row.names=FALSE)
  
  ### plot measurements for randomly selected participants with at least 2 observations
  ### before and after first-dose vaccination, and who had LC before vaccination
  set.seed(seed_for_plots)
  out_dir_plots = paste(out_dir, "\\Individual participant plots", sep="")
  unlink(out_dir_plots, recursive=TRUE)
  dir.create(out_dir_plots)
  
  part_samp <- dat[dat$n_before_dose1>=2 &
                     dat$n_after_dose1>=2 &
                     ave(dat$lc * dat$flag_not_dose1, dat$participant_id, FUN=max)==1,]
  part_samp <- part_samp$participant_id[!duplicated(part_samp$participant_id)]
  part_samp <- sample(part_samp, size=n_plots, replace=FALSE)
  
  for(i in 1:length(part_samp)) {
    
    dat_samp <- dat[dat$participant_id==part_samp[i],]
    
    jpeg(filename=paste(out_dir_plots, "\\before_after", i, ".jpg", sep=""))
    layout(rbind(1,2), heights=c(7,1))
    plot(x=dat_samp$time_dose1,
         y=dat_samp$lc + 0.05,
         xlab="Days since first dose", ylab="",
         cex.lab=1.3, cex.axis=1.3,
         ylim=c(-0.2,1.2), yaxt="n",
         pch=16, cex=1.5, col="blue")
    points(x=dat_samp$time_dose1,
           y=dat_samp$lc_lim - 0.05,
           pch=16, cex=1.5, col="orange")
    axis(side=2, at=0:1, labels=c("No LC", "LC"), las=1, cex.axis=1.3)
    abline(v=0, lty=2)
    par(mar=c(0,0,0,0))
    plot.new()
    legend("center",
           c("Any severity", "Activity limiting"),
           pch=16,
           cex=1.5,
           col=c("blue", "orange"),
           bty="n",
           horiz=TRUE)
    dev.off()
    
  }
  
  ############################### TRAJECTORY IN TIME SINCE INFECTION ###############################
  
  ### define ventile groups
  dat$time20 <- cut(dat$time, breaks=quantile(dat$time, probs=seq(0,1,0.05)),
                    include.lowest=TRUE)
  print(quantile(dat$time, probs=seq(0,1,0.05)))
  ventile_labels <- quantile(dat$time, probs=seq(0,1,0.05))[-1]
  
  ### plot smoothed logit of LC (any severity) over time
  p_any <- aggregate(dat$lc, by=list(dat$time20), FUN=sum)$x /
    aggregate(dat$lc, by=list(dat$time20), FUN=length)$x
  
  logit_any <- log(p_any / (1 - p_any))
  
  loess_any <- loess(logit_any ~ I(1:20))
  
  jpeg(filename=paste(out_dir, "\\time_lc_any.jpg", sep=""))
  plot(x=1:20, y=logit_any, xlab="Ventile group", ylab="Logit",
       pch=16, cex=1.5, cex.lab=1.3, cex.axis=1.3,)
  lines(predict(loess_any), col="red", lwd=1.5)
  dev.off()
  
  ### plot smoothed logit of LC (any severity) over time for hospitalised
  dat_hosp <- dat[dat$covid_admitted == 1,]
  p_any <- aggregate(dat_hosp$lc, by=list(dat_hosp$time20), FUN=sum)$x /
    aggregate(dat_hosp$lc, by=list(dat_hosp$time20), FUN=length)$x
  
  logit_any <- log(p_any / (1 - p_any))
  
  loess_any <- loess(logit_any ~ I(1:20))
  
  jpeg(filename=paste(out_dir, "\\time_lc_any_hospitalised.jpg", sep=""))
  plot(x=1:20, y=logit_any, xlab="Ventile group", ylab="Logit",
       pch=16, cex=1.5, cex.lab=1.3, cex.axis=1.3,)
  lines(predict(loess_any), col="red", lwd=1.5)
  dev.off()
  
  ### plot smoothed logit of LC (any severity) over time for non hospitalised
  dat_hosp <- dat[dat$covid_admitted == 0,]
  p_any <- aggregate(dat_hosp$lc, by=list(dat_hosp$time20), FUN=sum)$x /
    aggregate(dat_hosp$lc, by=list(dat_hosp$time20), FUN=length)$x
  
  logit_any <- log(p_any / (1 - p_any))
  
  loess_any <- loess(logit_any ~ I(1:20))
  
  jpeg(filename=paste(out_dir, "\\time_lc_any_non_hospitalised.jpg", sep=""))
  plot(x=1:20, y=logit_any, xlab="Ventile group", ylab="Logit",
       pch=16, cex=1.5, cex.lab=1.3, cex.axis=1.3,)
  lines(predict(loess_any), col="red", lwd=1.5)
  dev.off()
  
  ### plot smoothed logit of LC (any severity) over time for wave 1 infected people
  dat_infect <- dat[dat$infected_wave_1 == 1,]
  p_any <- aggregate(dat_infect$lc, by=list(dat_infect$time20), FUN=sum)$x /
    aggregate(dat_infect$lc, by=list(dat_infect$time20), FUN=length)$x
  
  logit_any <- log(p_any / (1 - p_any))
  
#  loess_any <- loess(logit_any ~ I(1:length(logit_any)))
  
  jpeg(filename=paste(out_dir, "\\time_lc_any_wave_1.jpg", sep=""))
  plot(x=1:length(logit_any), y=logit_any, xlab="Ventile group", ylab="Logit",
       pch=16, cex=1.5, cex.lab=1.3, cex.axis=1.3,)
#  lines(predict(loess_any), col="red", lwd=1.5)
  dev.off()

  ### plot smoothed logit of LC (any severity) over time for post wave 1 infected people
  dat_infect <- dat[dat$infected_wave_1 == 0,]
  p_any <- aggregate(dat_infect$lc, by=list(dat_infect$time20), FUN=sum)$x /
    aggregate(dat_infect$lc, by=list(dat_infect$time20), FUN=length)$x

  logit_any <- log(p_any / (1 - p_any))

#  loess_any <- loess(logit_any ~ I(1:20))

  jpeg(filename=paste(out_dir, "\\time_lc_any_not_wave_1.jpg", sep=""))
  plot(x=1:length(logit_any), y=logit_any, xlab="Ventile group", ylab="Logit",
       pch=16, cex=1.5, cex.lab=1.3, cex.axis=1.3,)
#  lines(predict(loess_any), col="red", lwd=1.5)
  dev.off()
  
  ### plot smoothed logit of LC (activity limiting) over time
  p_lim <- aggregate(dat$lc_lim, by=list(dat$time20), FUN=sum)$x /
    aggregate(dat$lc_lim, by=list(dat$time20), FUN=length)$x
  
  logit_lim <- log(p_lim / (1 - p_lim))
  
  loess_lim <- loess(logit_lim ~ I(1:20))
  
  jpeg(filename=paste(out_dir, "\\time_lc_lim.jpg", sep=""))
  plot(x=1:20, y=logit_lim, xlab="Ventile group", ylab="Logit",
       pch=16, cex=1.5, cex.lab=1.3, cex.axis=1.3,)
  lines(predict(loess_lim), col="red", lwd=1.5)
  dev.off()
  
  ### log odds admitted to hospital
  dat$covid_admitted_numeric <- as.numeric(as.character(dat$covid_admitted))

  ### plot smoothed logit of being hispitalised over time
  p_any <- aggregate(dat$covid_admitted_numeric, by=list(dat$time20), FUN=sum)$x /
    aggregate(dat$covid_admitted_numeric, by=list(dat$time20), FUN=length)$x

  #  logit_any <- log(p_any / (1 - p_any))

  loess_any <- loess(p_any ~ I(1:20))

  jpeg(filename=paste(out_dir, "\\time_hospitalised.jpg", sep=""))
  plot(x=1:20, y=p_any, xlab="Ventile group", ylab="Proportion hospitalised",
       pch=16, cex=1.5, cex.lab=1.3, cex.axis=1.3,)
  lines(predict(loess_any), col="red", lwd=1.5)
  dev.off()

  ### log odds infected in wave 1
  dat$infected_wave_1 <- as.numeric(as.character(dat$infected_wave_1))

  ### plot smoothed logit of being infected in wave 1
  p_any <- aggregate(dat$infected_wave_1, by=list(dat$time20), FUN=sum)$x /
    aggregate(dat$infected_wave_1, by=list(dat$time20), FUN=length)$x

  #  logit_any <- log(p_any / (1 - p_any))

  loess_any <- loess(p_any ~ I(1:20))

  jpeg(filename=paste(out_dir, "\\time_infected_wave_1.jpg", sep=""))
  plot(x=1:20, y=p_any, xlab="Ventile group", ylab="Proportion infected in wave 1",
       pch=16, cex=1.5, cex.lab=1.3, cex.axis=1.3,)
  lines(predict(loess_any), col="red", lwd=1.5)
  dev.off()
  ################################# EXPLORE INTRA-HH CORRELATION #################################
  
  ### check no. people in HHs with at least one other person
  hh_n_people <- sqldf("
  select n_people, count(*) as freq
  from(
    select a.participant_id, b.n_people
    from dat_pers as a
    left join(
      select hh_id_fake, count(*) as n_people
      from dat_pers
      group by hh_id_fake
    ) as b
    on a.hh_id_fake=b.hh_id_fake
  )
  group by n_people
  ")
  
  write.csv(hh_n_people,
            file=paste(out_dir, "\\hh_n_people.csv", sep=""),
            row.names=FALSE)
  
  if(get_hh_icc==TRUE) {
    
    ### sort by HH ID and participant ID
    dat_pers <- dat_pers[with(dat_pers, order(hh_id_fake, participant_id)),]
    rownames(dat_pers) <- NULL
    
    ### estimate mixed-effects model, outcome = any LC
    mod_any <- glmer(as.formula(paste("lc ~ ",
                                      paste(c(covs_for_hh_icc, "(1 | hh_id_fake)"),
                                            collapse=" + "))),
                     family=binomial,
                     data=dat_pers)
    
    ### estimate mixed-effects model, outcome = activity-limiting LC
    mod_lim <- glmer(as.formula(paste("lc_lim ~ ",
                                      paste(c(covs_for_hh_icc, "(1 | hh_id_fake)"),
                                            collapse=" + "))),
                     family=binomial,
                     data=dat_pers)
    
    ### extract ICCs
    hh_icc <- as.data.frame(rbind(
      as.numeric(paste(icc(mod_any)[1:2])),
      as.numeric(paste(icc(mod_lim)[1:2]))
    ))
    
    colnames(hh_icc) <- c("icc_adjusted", "icc_conditional")
    rownames(hh_icc) <- c("lc_any", "lc_lim")
    
    write.csv(hh_icc, file=paste(out_dir, "\\hh_icc.csv", sep=""))
    
  }
  
}