out_dir = "filepath"

################################ ANY VACCINE TYPE ################################

main_results_path = "filepath"
symptoms_results_path = "filepath"

coeff_files = c(
  paste(main_results_path, "\\coeffs_mod2_any.csv", sep=""),
  paste(main_results_path, "\\coeffs_mod2_lim.csv", sep=""),
  paste(symptoms_results_path, "\\coeffs_mod2_lc_concentrate.csv", sep=""),
  paste(symptoms_results_path, "\\coeffs_mod2_lc_headache.csv", sep=""),
  paste(symptoms_results_path, "\\coeffs_mod2_lc_loss_of_smell.csv", sep=""),
  paste(symptoms_results_path, "\\coeffs_mod2_lc_loss_of_taste.csv", sep=""),
  paste(symptoms_results_path, "\\coeffs_mod2_lc_memory_loss_confusion.csv", sep=""),
  paste(symptoms_results_path, "\\coeffs_mod2_lc_muscle_ache.csv", sep=""),
  paste(symptoms_results_path, "\\coeffs_mod2_lc_shortness_of_breath.csv", sep=""),
  paste(symptoms_results_path, "\\coeffs_mod2_lc_trouble_sleeping.csv", sep=""),
  paste(symptoms_results_path, "\\coeffs_mod2_lc_weakness_tiredness.csv", sep=""),
  paste(symptoms_results_path, "\\coeffs_mod2_lc_worry_anxiety.csv", sep="")
)

outcomes = c(
  "Any LC",
  "Activity-limiting LC",
  "Difficulty concentrating",
  "Headache",
  "Loss of smell",
  "Loss of taste",
  "Memory loss or confusion",
  "Muscle ache",
  "Shortness of breath",
  "Trouble sleeping",
  "Weakness and tiredness",
  "Worry or anxiety"
)

out_list_any <- as.list(NULL)

for(i in 1:length(coeff_files)) {
  
  coeff <- read.csv(coeff_files[i])
  vcov <- read.csv(gsub("coeffs", "vcov", coeff_files[i]))
  
  coeff1 <- coeff[6,2]
  coeff2 <- coeff[6,2] + coeff[4,2]
  coeff3 <- coeff[6,2] + coeff[4,2] + coeff[5,2]
  
  est1 <- (exp(coeff1) - 1) * 100
  est2 <- (exp(coeff2) - 1) * 100
  est3 <- (exp(coeff3) - 1) * 100
  
  se1 <- coeff[6,3]
  se2 <- sqrt(coeff[6,3]^2 + coeff[4,3]^2 +
                2*vcov[6,5])
  se3 <- sqrt(coeff[6,3]^2 + coeff[4,3]^2 + coeff[5,3]^2 +
                2*vcov[6,5] + 2*vcov[6,6] + 2*vcov[4,6])
  
  lcl1 <- (exp(coeff1 - 1.96*se1) - 1) * 100
  lcl2 <- (exp(coeff2 - 1.96*se2) - 1) * 100
  lcl3 <- (exp(coeff3 - 1.96*se3) - 1) * 100
  
  ucl1 <- (exp(coeff1 + 1.96*se1) - 1) * 100
  ucl2 <- (exp(coeff2 + 1.96*se2) - 1) * 100
  ucl3 <- (exp(coeff3 + 1.96*se3) - 1) * 100
  
  p1 <- pnorm(abs(coeff1/se1), lower.tail=FALSE) * 2
  p2 <- pnorm(abs(coeff2/se2), lower.tail=FALSE) * 2
  p3 <- pnorm(abs(coeff3/se3), lower.tail=FALSE) * 2
  
  out_list_any[[i]] <- data.frame(
    Vaccine_type = "Any",
    Outcome = outcomes[i],
    Period = c("Before dose 1", "Between doses 1 and 2", "After dose 2"),
    Estimate = c(est1, est2, est3),
    LCL = c(lcl1, lcl2, lcl3),
    UCL = c(ucl1, ucl2, ucl3),
    Pvalue = c(p1, p2, p3)
  )
  
}

out_df_any <- out_list_any[[1]]
if(length(out_list_any)>1) {
  for(i in 2:length(out_list_any)) {out_df_any <- as.data.frame(rbind(out_df_any, out_list_any[[i]]))}
}

################################ mRNA VACCINES ################################

main_results_path = "filepath"

coeff_files = c(
  paste(main_results_path, "\\coeffs_mod2_any_het.csv", sep=""),
  paste(main_results_path, "\\coeffs_mod2_lim_het.csv", sep="")
)

outcomes = c(
  "Any LC",
  "Activity-limiting LC"
)

out_list_mrna <- as.list(NULL)

for(i in 1:length(coeff_files)) {
  
  coeff <- read.csv(coeff_files[i])
  vcov <- read.csv(gsub("coeffs", "vcov", coeff_files[i]))
  
  coeff1 <- coeff[7,2]
  coeff2 <- coeff[7,2] + coeff[5,2]
  coeff3 <- coeff[7,2] + coeff[5,2] + coeff[6,2]
  
  est1 <- (exp(coeff1) - 1) * 100
  est2 <- (exp(coeff2) - 1) * 100
  est3 <- (exp(coeff3) - 1) * 100
  
  se1 <- coeff[7,3]
  se2 <- sqrt(coeff[7,3]^2 + coeff[5,3]^2 +
                2*vcov[7,6])
  se3 <- sqrt(coeff[7,3]^2 + coeff[5,3]^2 + coeff[6,3]^2 +
                2*vcov[7,6] + 2*vcov[7,7] + 2*vcov[5,7])
  
  lcl1 <- (exp(coeff1 - 1.96*se1) - 1) * 100
  lcl2 <- (exp(coeff2 - 1.96*se2) - 1) * 100
  lcl3 <- (exp(coeff3 - 1.96*se3) - 1) * 100
  
  ucl1 <- (exp(coeff1 + 1.96*se1) - 1) * 100
  ucl2 <- (exp(coeff2 + 1.96*se2) - 1) * 100
  ucl3 <- (exp(coeff3 + 1.96*se3) - 1) * 100
  
  p1 <- pnorm(abs(coeff1/se1), lower.tail=FALSE) * 2
  p2 <- pnorm(abs(coeff2/se2), lower.tail=FALSE) * 2
  p3 <- pnorm(abs(coeff3/se3), lower.tail=FALSE) * 2
  
  out_list_mrna[[i]] <- data.frame(
    Vaccine_type = "mRNA",
    Outcome = outcomes[i],
    Period = c("Before dose 1", "Between doses 1 and 2", "After dose 2"),
    Estimate = c(est1, est2, est3),
    LCL = c(lcl1, lcl2, lcl3),
    UCL = c(ucl1, ucl2, ucl3),
    Pvalue = c(p1, p2, p3)
  )
  
}

out_df_mrna <- out_list_mrna[[1]]
if(length(out_list_mrna)>1) {
  for(i in 2:length(out_list_mrna)) {out_df_mrna <- as.data.frame(rbind(out_df_mrna, out_list_mrna[[i]]))}
}

################################ VECTOR VACCINES ################################

coeff_files = c(
  paste(main_results_path, "\\coeffs_mod2_any_het.csv", sep=""),
  paste(main_results_path, "\\coeffs_mod2_lim_het.csv", sep="")
)

outcomes = c(
  "Any LC",
  "Activity-limiting LC"
)

out_list_vector <- as.list(NULL)

for(i in 1:length(coeff_files)) {
  
  coeff <- read.csv(coeff_files[i])
  vcov <- read.csv(gsub("coeffs", "vcov", coeff_files[i]))
  
  coeff1 <- coeff[7,2]
  coeff2 <- coeff[7,2] + coeff[5,2] + coeff[34,2]
  coeff3 <- coeff[7,2] + coeff[5,2] + coeff[34,2] + coeff[6,2] + coeff[35,2]
  
  est1 <- (exp(coeff1) - 1) * 100
  est2 <- (exp(coeff2) - 1) * 100
  est3 <- (exp(coeff3) - 1) * 100
  
  se1 <- coeff[7,3]
  se2 <- sqrt(coeff[7,3]^2 + coeff[5,3]^2 + coeff[34,3]^2 +
                2*vcov[7,6] + 2*vcov[7,35] + 2*vcov[5,35])
  se3 <- sqrt(coeff[7,3]^2 + coeff[5,3]^2 + coeff[34,3]^2 + coeff[6,3]^2 + coeff[35,3]^2 +
                2*vcov[7,6] + 2*vcov[7,35] + 2*vcov[7,7] + 2*vcov[7,36] +
                2*vcov[5,35] + 2*vcov[5,7] + 2*vcov[5,36] +
                2*vcov[34,7] + 2*vcov[34,36] +
                2*vcov[6,36])
  
  lcl1 <- (exp(coeff1 - 1.96*se1) - 1) * 100
  lcl2 <- (exp(coeff2 - 1.96*se2) - 1) * 100
  lcl3 <- (exp(coeff3 - 1.96*se3) - 1) * 100
  
  ucl1 <- (exp(coeff1 + 1.96*se1) - 1) * 100
  ucl2 <- (exp(coeff2 + 1.96*se2) - 1) * 100
  ucl3 <- (exp(coeff3 + 1.96*se3) - 1) * 100
  
  p1 <- pnorm(abs(coeff1/se1), lower.tail=FALSE) * 2
  p2 <- pnorm(abs(coeff2/se2), lower.tail=FALSE) * 2
  p3 <- pnorm(abs(coeff3/se3), lower.tail=FALSE) * 2
  
  out_list_vector[[i]] <- data.frame(
    Vaccine_type = "Adenovirus vector",
    Outcome = outcomes[i],
    Period = c("Before dose 1", "Between doses 1 and 2", "After dose 2"),
    Estimate = c(est1, est2, est3),
    LCL = c(lcl1, lcl2, lcl3),
    UCL = c(ucl1, ucl2, ucl3),
    Pvalue = c(p1, p2, p3)
  )
  
}

out_df_vector <- out_list_vector[[1]]
if(length(out_list_vector)>1) {
  for(i in 2:length(out_list_vector)) {out_df_vector <- as.data.frame(rbind(out_df_vector, out_list_vector[[i]]))}
}

################################ COMBINE RESULTS ################################

out_df <- rbind(out_df_any, out_df_mrna, out_df_vector)
write.csv(out_df, file=paste(out_dir, "\\combined_slopes.csv", sep=""), row.names=FALSE)
