folder = "filepath"
filename <- "coeffs_mod2_lc_worry_anxiety"
file <- paste0(folder, filename, ".csv") 

data <- read.csv(file)

data <- data[(data$X == "flag_dose11" | data$X == "flag_dose21"),]

data$change_in_odds <- (exp(data$Estimate)-1)*100
data$lcl <- (exp(data$Estimate - 1.96*data$Std..Error)-1)*100
data$ucl <- (exp(data$Estimate + 1.96*data$Std..Error)-1)*100
data$model <- filename

write.csv(data, paste0(folder, filename, "_with_change_in_odds.csv"), row.names=TRUE)

data
  