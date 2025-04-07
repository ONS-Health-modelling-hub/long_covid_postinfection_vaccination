library(ggplot2)
library(RColorBrewer)

# save directory
out_dir <- "filepath"


###
#Symptoms plot
###

all_data <- read.csv("filepath\\pred_plot_symptoms_data_12.csv")
levels(all_data$outcome)



all_data$outcome <- factor(all_data$outcome, levels=c("Weakness and tiredness",
                                 "Loss of smell",
                                 "Shortness of breath", 
                                 "Loss of taste",
                                 "Headache",
                                 "Trouble sleeping",
                                 "Difficulty concentrating",
                                 "Muscle ache", 
                                 "Worry or anxiety",
                                 "Memory loss or confusion",
                              "Over 3 symptoms",
                              "Over 5 symptoms"))



### plot predicted probabilities
pred_plot <- ggplot(all_data, aes(x=weeks, y=prob)) +
  geom_line() +
  geom_ribbon(aes(ymin=prob_lcl, ymax=prob_ucl, colour=NULL), alpha=0.1) +
  geom_vline(xintercept=c(24,36), linetype="dashed", colour="grey50", size=0.5) +
  facet_wrap(~outcome, nrow=4, ncol=3, scales = "free_y") +
#  facet_wrap(~outcome, nrow=4, ncol=3) +
  scale_x_continuous(limits=c(12,48), breaks=c(12,24,36,48), expand=c(0,0)) +
  scale_y_continuous(expand=expansion(mult=c(0,0.1))) +
  #scale_y_continuous(limits=c(0,0.20), expand=c(0,0)) +
  #  scale_colour_manual(values=c("blue", "orange")) +
  #  scale_fill_manual(values=c("blue", "orange")) +
  ylab("Probability") +
  xlab("Weeks since positive test") +
  expand_limits(y=0) +
  theme(
    axis.title=element_text(size=8, colour="black", face="plain"),
    axis.text=element_text(size=8, colour="black", face="plain"),
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
    strip.text=element_text(size=8, colour="black", face="bold"),
    legend.title=element_blank(),
    legend.position="right",
    legend.direction="vertical",
    legend.justification="center",
    legend.text=element_text(size=8, colour="black", face="plain")
  )

pred_plot

ggsave(plot=pred_plot,
       filename=paste(out_dir, "\\pred_probs_symptoms_all_free_y.jpg", sep=""),
       width=16,
       height=20,
       units="cm")



###
#Predicted trajectory plot
###

trajectory_data <- read.csv("filepath\\pred_plot_data.csv")

trajectory_data$outcome <- factor(trajectory_data$outcome, levels=c("Any severity",
                                                                    "Activity limiting"))

### plot predicted probabilities
pred_plot <- ggplot(trajectory_data, aes(x=weeks, y=prob, colour=outcome)) +
  geom_line() +
  geom_ribbon(aes(ymin=prob_lcl, ymax=prob_ucl, colour=NULL, fill=outcome), alpha=0.1) +
  geom_vline(xintercept=c(24,36), linetype="dashed", colour="grey50", size=0.5) +
  #    facet_wrap(~model, nrow=1) +
  scale_x_continuous(limits=c(12,48), breaks=c(12,24,36,48), expand=c(0,0)) +
  scale_y_continuous(expand=expansion(mult=c(0,0.1))) +
#  scale_fill_grey() +
#  scale_colour_grey() +
  expand_limits(y=0) +
  scale_colour_brewer(palette="Blues") +
  #scale_y_continuous(limits=c(0,0.20), expand=c(0,0)) +
#  scale_colour_manual(values=c("blue", "orange")) +
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

pred_plot

ggsave(plot=pred_plot,
       filename=paste(out_dir, "\\pred_probs_colour.jpg", sep=""),
       width=20,
       height=12,
       units="cm")



###
#Vaccination data
###

vaccination_data <- read.csv("filepath\\pred_plot_vaccination_data.csv")

vaccination_data$outcome <- factor(vaccination_data$outcome, levels=c("Any severity",
                                                                    "Activity limiting"))


### plot predicted probabilities
pred_plot <- ggplot(vaccination_data, aes(x=weeks, y=prob, colour=vaccine_type)) +
  geom_line(aes(linetype=outcome)) +
  geom_ribbon(aes(ymin=prob_lcl, ymax=prob_ucl, colour=NULL, fill=outcome_vacc_type), alpha=0.1) +
  geom_vline(xintercept=c(24,36), linetype="dashed", size=0.5) +
  scale_x_continuous(limits=c(12,48), breaks=c(12,24,36,48), expand=c(0,0)) +
  scale_y_continuous(expand=expansion(mult=c(0,0.1))) +
  #scale_y_continuous(limits=c(0,0.20), expand=c(0,0)) +
#  scale_colour_manual(values=c("grey50", "grey20")) +
 # scale_fill_manual(values=c("grey50", "grey50", "grey20", "grey20"), guide=FALSE) +
  scale_colour_brewer(palette="Blues") +
  ylab("Probability") +
  xlab("Weeks since positive test") +
  expand_limits(y=0) +
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

pred_plot

ggsave(plot=pred_plot,
       filename=paste(out_dir, "\\pred_probs_het_effect_colour.jpg", sep=""),
       width=20,
       height=12,
       units="cm")




### sensitivity analyses

plot_trajectories <- function(data, ymax, save_name){
  data$outcome <- factor(trajectory_data$outcome, levels=c("Any severity",
                                                                      "Activity limiting"))
  pred_plot <- ggplot(data, aes(x=weeks, y=prob, colour=outcome)) +
    geom_line() +
    geom_ribbon(aes(ymin=prob_lcl, ymax=prob_ucl, colour=NULL, fill=outcome), alpha=0.1) +
    geom_vline(xintercept=c(24,36), linetype="dashed", colour="grey50", size=0.5) +
    #    facet_wrap(~model, nrow=1) +
    scale_x_continuous(limits=c(12,48), breaks=c(12,24,36,48), expand=c(0,0)) +
    scale_y_continuous(expand=expansion(mult=c(0,0.1))) +
#    scale_fill_grey() +
#    scale_colour_grey() +
    scale_colour_brewer(palette="Blues") +
    #scale_y_continuous(limits=c(0,0.20), expand=c(0,0)) +
    #  scale_colour_manual(values=c("blue", "orange")) +
    # scale_fill_manual(values=c("blue", "orange")) +
    ylab("Probability") +
    xlab("Weeks since positive test") +
    expand_limits(y=0) +
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
         filename=paste(out_dir, "\\", save_name, "_colour.jpg", sep=""),
         width=20,
         height=12,
         units="cm")
  
  return(pred_plot)
}

# 1 pre post dose 1
trajectory_data <- read.csv("filepath\\pred_plot_data.csv")
pred_plot <- plot_trajectories(trajectory_data, 0.15, "pred_probs_at_least_1_pre_post_dose1")
pred_plot

# 1 pre post dose 2
trajectory_data <- read.csv("filepath\\pred_plot_data.csv")
pred_plot <- plot_trajectories(trajectory_data, 0.15, "pred_probs_at_least_1_pre_post_dose2")
pred_plot

# 3 post dose 1
trajectory_data <- read.csv("filepath\\pred_plot_data.csv")
pred_plot <- plot_trajectories(trajectory_data, 0.13, "pred_probs_at_least_3_post_dose1")
pred_plot

# 3 post dose 2
trajectory_data <- read.csv("filepath\\pred_plot_data.csv")
pred_plot <- plot_trajectories(trajectory_data, 0.14, "pred_probs_at_least_3_post_dose2")
pred_plot

# 3 post dose 2
trajectory_data <- read.csv("filepath\\pred_plot_data.csv")
pred_plot <- plot_trajectories(trajectory_data, 0.13, "pred_probs_omit_1st_week_after_vacc")
pred_plot

# No wave 1
trajectory_data <- read.csv("filepath\\pred_plot_data.csv")
pred_plot <- plot_trajectories(trajectory_data, 0.13, "pred_probs_omit_infect_wave_1")
pred_plot

# Include unvaccinated
trajectory_data <- read.csv("filepath\\pred_plot_data.csv")
pred_plot <- plot_trajectories(trajectory_data, 0.13, "pred_probs_include_unvaccinated")
pred_plot




###
#Predicted trajectory plots for time from infeciton to first dose
###

trajectory_data <- read.csv("filepath\\pred_probs_any.csv")

trajectory_data$duration <- factor(trajectory_data$duration, levels=c("6 months",
                                                                      "9 months",
                                                                      "12 months",
                                                                      "15 months"))

### plot predicted probabilities
pred_plot <- ggplot(trajectory_data, aes(x=weeks, y=prob, colour=duration)) +
  geom_line() +
  geom_ribbon(aes(ymin=prob_lcl, ymax=prob_ucl, colour=NULL, fill=duration), alpha=0.1) +
  geom_vline(xintercept=c(24,36), linetype="dashed", colour="grey50", size=0.5) +
  #    facet_wrap(~model, nrow=1) +
  scale_x_continuous(limits=c(12,48), breaks=c(12,24,36,48), expand=c(0,0)) +
  scale_y_continuous(expand=expansion(mult=c(0,0.1))) +
#  scale_fill_grey() +
#  scale_colour_grey() +
  scale_colour_brewer(palette="Blues") +
  expand_limits(y=0) +
#  ylim(0, 0.125) +
  #scale_y_continuous(limits=c(0,0.20), expand=c(0,0)) +
  #  scale_colour_manual(values=c("blue", "orange")) +
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

pred_plot

ggsave(plot=pred_plot,
       filename=paste(out_dir, "\\pred_probs_time_infection_to_dose1_any.jpg", sep=""),
       width=20,
       height=12,
       units="cm")

trajectory_data <- read.csv("filepath\\pred_probs_lim.csv")

trajectory_data$duration <- factor(trajectory_data$duration, levels=c("6 months",
                                                                      "9 months",
                                                                      "12 months",
                                                                      "15 months"))

### plot predicted probabilities
pred_plot <- ggplot(trajectory_data, aes(x=weeks, y=prob, colour=duration)) +
  geom_line() +
  geom_ribbon(aes(ymin=prob_lcl, ymax=prob_ucl, colour=NULL, fill=duration), alpha=0.1) +
  geom_vline(xintercept=c(24,36), linetype="dashed", colour="grey50", size=0.5) +
  #    facet_wrap(~model, nrow=1) +
  scale_x_continuous(limits=c(12,48), breaks=c(12,24,36,48), expand=c(0,0)) +
  scale_y_continuous(expand=expansion(mult=c(0,0.1))) +
#  scale_fill_grey() +
#  scale_colour_grey() +
  scale_colour_brewer(palette="Blues") +
  expand_limits(y=0) +
  #  ylim(0, 0.125) +
  #scale_y_continuous(limits=c(0,0.20), expand=c(0,0)) +
  #  scale_colour_manual(values=c("blue", "orange")) +
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

pred_plot

ggsave(plot=pred_plot,
       filename=paste(out_dir, "\\pred_probs_time_infection_to_dose1_lim.jpg", sep=""),
       width=20,
       height=12,
       units="cm")

### density plot of interval between doses

load("filepath\\dataset.RData")
dat2 <- dat[!duplicated(dat$participant_id),]
dat2 <- dat2[!is.na(dat2$covid_vaccine_date2),]
dat2$dose_interval <- dat2$covid_vaccine_date2 - dat2$covid_vaccine_date1

int_chart <- ggplot(dat2, aes(x=dose_interval)) +
  geom_density(fill="grey", colour=NA) +
  xlab("Interval between vaccinations (days)") +
  ylab("Density") +
  scale_y_continuous(expand=expansion(mult=c(0,0.05))) +
  theme(
    axis.title.x=element_text(size=10, colour="black", face="plain"),
    axis.text.x=element_text(size=10, colour="black", face="plain"),
    axis.ticks.x=element_line(size=0.5, colour="black"),
    axis.line.x=element_blank(),
    axis.title.y=element_text(size=10, colour="black", face="plain"),
    axis.text.y=element_text(size=10, colour="black", face="plain"),
    axis.ticks.y=element_line(size=0.5, colour="black"),
    axis.line.y=element_blank(),
    panel.border=element_rect(size=0.5, colour="black", fill=NA),
    panel.background=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    panel.spacing=unit(1.5, "lines"),
    plot.margin=margin(0.2,1,0.2,0.5, unit="lines"),
    strip.background=element_blank(),
    strip.text=element_text(size=10, colour="black", face="bold"),
    legend.title=element_text(size=10, colour="black", face="plain"),
    legend.position="bottom",
    legend.direction="horizontal",
    legend.justification="center",
    legend.text=element_text(size=10, colour="black", face="plain")
  )

ggsave(plot=int_chart,
       filename=paste(out_dir, "\\dose_interval_chart.jpg", sep=""),
       width=14,
       height=12,
       units="cm")
