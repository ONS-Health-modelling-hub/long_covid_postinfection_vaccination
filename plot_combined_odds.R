plot.combined.odds <- function(out_dir, covs){

exposures_het <- paste(c("flag_dose1",
                         "flag_dose2",
                         "I(weeks_dose1*as.numeric(as.character(flag_dose1)))",
                         "I(weeks_dose2*as.numeric(as.character(flag_dose2)))"), "time_dose1_group", sep="*")

### fit model
mod_het <- fit.mod(outcome = "lc_lim",
                   covariates = covs,
                   exposures = exposures_het,
                   dataset = dat)

coef <- data.frame(mod_het$coeff[,])
cov <- data.frame(mod_het$vcov[,])

coefs_dose1 <- as.list(NULL)
dose1_coef0 <- sum(coef[c(8, 10),]$Estimate)
coefs_dose1[1] <- dose1_coef0

std_dev_dose1 <- as.list(NULL)
std_dev_dose1[1] <- sqrt(sum(cov[c(8,10),c(8,10)]))

indices_dose1 <- c(43:46)
for (i in 1:(length(indices_dose1))){
  print(coef[indices_dose1[i],]$Estimate)
  coefs_dose1[i+1] <- dose1_coef0 + coef[indices_dose1[i],]$Estimate
  std_dev_dose1[i+1] <- sqrt(sum(cov[c(8,10, indices_dose1[i]),c(8,10, indices_dose1[i])]))
}

dose1_data <- data.frame(num=5:1)
dose1_data$coefs <- coefs_dose1
dose1_data$std_dev <- std_dev_dose1
dose1_data <- as.data.frame(lapply(dose1_data, unlist))

dose1_data <- dose1_data %>% 
  mutate(coefs_odds = exp(coefs),
#         std_dev_odds = std_dev*coefs_odds,
         dose = "slope dose 1",
        lcl = exp(coefs - 1.96*std_dev),
         ucl = exp(coefs + 1.96*std_dev))

coefs_dose2 <- as.list(NULL)
dose2_coef0 <- sum(coef[c(8:10),]$Estimate)
coefs_dose2[1] <- dose2_coef0

std_dev_dose2 <- as.list(NULL)
std_dev_dose2[1] <- sqrt(sum(cov[c(8:10),c(8:10)]))

indices_dose2 <- c(47:50)
for (i in 1:(length(indices_dose2))){
  coefs_dose2[i+1] <- dose2_coef0 + coef[indices_dose2[i],]$Estimate
  std_dev_dose2[i+1] <- sqrt(sum(cov[c(8:10, indices_dose2[i]),c(8:10, indices_dose2[i])]))
}

dose2_data <- data.frame(num=5:1)
dose2_data$coefs <- coefs_dose2
dose2_data$std_dev <- std_dev_dose2
dose2_data <- as.data.frame(lapply(dose2_data, unlist))

dose2_data <- dose2_data %>% 
  mutate(num = num - 0.3,
         coefs_odds = exp(coefs),
#         std_dev_odds = std_dev*coefs_odds,
         dose = "slope dose 2",
          lcl = exp(coefs - 1.96*std_dev),
         ucl = exp(coefs + 1.96*std_dev))

all_data <- rbind(dose1_data, dose2_data)

labels <- c("<3 months", "3 to <6 months", "6 to <9 months", "9 to <12 months","12+ months")

all_data$num_factor <- as.factor(rep(labels,2))
all_data$num_factor <- fct_rev(all_data$num_factor)
all_data$num_factor <- factor(all_data$num_factor, 
                              levels=c("12+ months", "9 to <12 months", "6 to <9 months", "3 to <6 months", "<3 months"))


write.csv(all_data, paste0(out_dir, "\\combined_odds.csv"), row.names=FALSE)

odds_plot <- ggplot(all_data, aes(x=num_factor, y=(coefs_odds-1)*100, color=dose)) + 
  geom_point(position = position_dodge(width=0.3), size=2) + 
  geom_errorbar(aes(ymin = (lcl-1)*100, ymax=(ucl-1)*100), 
                position = position_dodge(width=0.3),
                width=0) +
  geom_hline(yintercept=0, linetype="dashed", colour="grey50", size=0.5) +
  ylab("Combined change in odds (%)") +
  xlab("Time from infection to dose 1") +
#  ylim(-6, 6) +
  scale_y_continuous(limits=c(-9,9)) +
#  scale_x_discrete(limits=rev(levels(num_factor)))+
  # position = position_dodge(width=0.9) +
#  scale_x_discrete(breaks = c(1:5), labels=labels) +
#  scale_y_continuous(breaks = c(1:5), labels = labels) +
  coord_flip() +
  theme(
    axis.title=element_text(size=13, colour="black", face="plain"),
    axis.text=element_text(size=12, colour="black", face="plain"),
#    axis.text.y = element_blank(),
    axis.line=element_blank(),
    axis.ticks.x=element_blank(),
    axis.ticks.y=element_blank(),
    panel.border=element_blank(),
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
    legend.text=element_text(size=12, colour="black", face="plain"))
 
odds_plot

ggsave(plot=odds_plot,
       filename=paste(out_dir, "\\combined_odds.jpg", sep=""),
       width=20,
       height=12,
       units="cm")
}
