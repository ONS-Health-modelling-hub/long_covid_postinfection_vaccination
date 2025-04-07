library(dplyr)
library(tidyverse)

filename <- "filepath\\cov_dist_cat"
data <- read.csv(paste0(filename, ".csv"))
           
data <- data %>%
    mutate(n_p_all = paste0(n_all, " (", round(p_all*100, digits=1), "%)"),
           n_p_0 = paste0(n0, " (", round(p0*100, digits=1), "%)"),
           n_p_1 = paste0(n1, " (", round(p1*100, digits=1), "%)"),
           std_diff_round = round(std_diff, digits=3))


write.csv(data, paste0(filename, "_for_presentation.csv"), row.names=FALSE)
