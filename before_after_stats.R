before.after.stats <- function(df) {
  
  ### identify participants with at least 1 observation before and after vaccination
  dat_dose1 <- df[df$during_fu_dose1==1 & (df$last_before_dose1==1 | df$first_after_dose1),]
  dat_dose2 <- df[df$during_fu_dose2==1 & (df$last_before_dose2==1 | df$first_after_dose2),]
  
  ### convert datasets from long to wide format - LC status
  dat_lc_dose1 <- dcast(dat_dose1, participant_id~first_after_dose1, value.var="lc")
  dat_lclim_dose1 <- dcast(dat_dose1, participant_id~first_after_dose1, value.var="lc_lim")
  dat_lc_dose2 <- dcast(dat_dose2, participant_id~first_after_dose2, value.var="lc")
  dat_lclim_dose2 <- dcast(dat_dose2, participant_id~first_after_dose2, value.var="lc_lim")
  
  ### cross-tabulate LC before and after vaccination
  before_after_xtab <- data.frame(
    Dose = c(rep(1,8), rep(2,8)),
    Outcome = c(rep("lc",4), rep("lc_lim",4), rep("lc",4), rep("lc_lim",4)),
    rbind(
      as.data.frame(table(Before=dat_lc_dose1[["0"]], After=dat_lc_dose1[["1"]])),
      as.data.frame(table(Before=dat_lclim_dose1[["0"]], After=dat_lclim_dose1[["1"]])),
      as.data.frame(table(Before=dat_lc_dose2[["0"]], After=dat_lc_dose2[["1"]])),
      as.data.frame(table(Before=dat_lclim_dose2[["0"]], After=dat_lclim_dose2[["1"]]))
    )
  )
  
  ### convert datasets from long to wide format - time to/since vaccination
  dat_time_dose1 <- dcast(dat_dose1, participant_id~first_after_dose1, value.var="time_dose1")
  dat_time_dose2 <- dcast(dat_dose2, participant_id~first_after_dose2, value.var="time_dose2")
  
  ### distribution of time to/since vaccination
  before_after_time <- data.frame(
    statistic = c("min", "q1", "median", "mean", "q3", "max", "sd"),
    before_dose1 = c(as.numeric(summary(dat_time_dose1[["0"]])), sd(dat_time_dose1[["0"]])),
    after_dose1 = c(as.numeric(summary(dat_time_dose1[["1"]])), sd(dat_time_dose1[["1"]])),
    before_dose2 = c(as.numeric(summary(dat_time_dose2[["0"]])), sd(dat_time_dose2[["0"]])),
    after_dose2 = c(as.numeric(summary(dat_time_dose2[["1"]])), sd(dat_time_dose2[["1"]]))
  )
  
  return(list(xtab=before_after_xtab, time=before_after_time))
  
}
