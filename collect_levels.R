out_dir = "filepath"

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
  
  coeff1 <- coeff[2,2]
  coeff2 <- coeff[4,2]
  
  est1 <- (exp(coeff1) - 1) * 100
  est2 <- (exp(coeff2) - 1) * 100
  
  se1 <- coeff[2,3]
  se2 <- coeff[4,3]
  
  lcl1 <- (exp(coeff1 - 1.96*se1) - 1) * 100
  lcl2 <- (exp(coeff2 - 1.96*se2) - 1) * 100
  
  ucl1 <- (exp(coeff1 + 1.96*se1) - 1) * 100
  ucl2 <- (exp(coeff2 + 1.96*se2) - 1) * 100
  
  p1 <- pnorm(abs(coeff1/se1), lower.tail=FALSE) * 2
  p2 <- pnorm(abs(coeff2/se2), lower.tail=FALSE) * 2
  
  out_list_mrna[[i]] <- data.frame(
    Vaccine_type = "mRNA",
    Outcome = outcomes[i],
    Dose = c("Dose 1", "Dose 2"),
    Estimate = c(est1, est2),
    LCL = c(lcl1, lcl2),
    UCL = c(ucl1, ucl2),
    Pvalue = c(p1, p2)
  )
  
}

out_df_mrna <- out_list_mrna[[1]]
if(length(out_list_mrna)>1) {
  for(i in 2:length(out_list_mrna)) {out_df_mrna <- as.data.frame(rbind(out_df_mrna, out_list_mrna[[i]]))}
}

################################ VECTOR VACCINES ################################

main_results_path = "filepath"

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
  
  coeff1 <- coeff[2,2] + coeff[32,2]
  coeff2 <- coeff[4,2] + coeff[33,2]
  
  est1 <- (exp(coeff1) - 1) * 100
  est2 <- (exp(coeff2) - 1) * 100
  
  se1 <- sqrt(coeff[2,3]^2 + coeff[32,3]^2 + 2*vcov[2,33])
  se2 <- sqrt(coeff[4,3]^2 + coeff[33,3]^2 + 2*vcov[4,34])
  
  lcl1 <- (exp(coeff1 - 1.96*se1) - 1) * 100
  lcl2 <- (exp(coeff2 - 1.96*se2) - 1) * 100
  
  ucl1 <- (exp(coeff1 + 1.96*se1) - 1) * 100
  ucl2 <- (exp(coeff2 + 1.96*se2) - 1) * 100
  
  p1 <- pnorm(abs(coeff1/se1), lower.tail=FALSE) * 2
  p2 <- pnorm(abs(coeff2/se2), lower.tail=FALSE) * 2
  
  out_list_vector[[i]] <- data.frame(
    Vaccine_type = "Adenovirus vector",
    Outcome = outcomes[i],
    Dose = c("Dose 1", "Dose 2"),
    Estimate = c(est1, est2),
    LCL = c(lcl1, lcl2),
    UCL = c(ucl1, ucl2),
    Pvalue = c(p1, p2)
  )
  
}

out_df_vector <- out_list_vector[[1]]
if(length(out_list_vector)>1) {
  for(i in 2:length(out_list_vector)) {out_df_vector <- as.data.frame(rbind(out_df_vector, out_list_vector[[i]]))}
}

################################ COMBINE RESULTS ################################

out_df <- rbind(out_df_mrna, out_df_vector)
write.csv(out_df, file=paste(out_dir, "\\combined_levels.csv", sep=""), row.names=FALSE)
