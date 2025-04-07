het.effects <- function(out_dir, covs, modifiers) {
  
  #################################### HETEROGENEOUS EFFECTS ####################################
  
  ### specify outcomes
  het_outcome_list <- c("lc", "lc_lim")
  
  ### loop over outcomes and modifiers
  het_list2 <- as.list(NULL)
  lr_list2 <- as.list(NULL)

  for(i in 1:length(het_outcome_list)) {
  print(het_outcome_list[i])
    het_list1 <- as.list(NULL)
    lr_list1 <- as.list(NULL)
    
    for(j in 1:length(modifiers)) {
    print(modifiers[j])
      
      exposures_labels <- c("dose 1", "dose 2", "time since dose 1", "time since dose 2")
      
      ### interact dose 1 and 2 flags and slopes with selected effect modifier
      exposures_het <- paste(c("flag_dose1",
                               "flag_dose2",
                               "I(weeks_dose1*as.numeric(as.character(flag_dose1)))",
                               "I(weeks_dose2*as.numeric(as.character(flag_dose2)))"), modifiers[j], sep="*")
      
      ### fit model
      mod_het <- fit.mod(outcome = het_outcome_list[i],
                         covariates = covs,
                         exposures = exposures_het,
                         dataset = dat)

      ### collect model outputs across effect modifiers within each outcome
      out_het <- rbind(mod_het$coeff[grep("flag_dose11:", rownames(mod_het$coeff)),,drop=FALSE],
                       mod_het$coeff[grep(":flag_dose21", rownames(mod_het$coeff)),,drop=FALSE],
                       mod_het$coeff[grep(":I\\(weeks_dose1 \\* as.numeric\\(as.character\\(flag_dose1\\)\\)\\)", rownames(mod_het$coeff)),,drop=FALSE],
                       mod_het$coeff[grep(":I\\(weeks_dose2 \\* as.numeric\\(as.character\\(flag_dose2\\)\\)\\)", rownames(mod_het$coeff)),,drop=FALSE])

            out_het <- data.frame(outcome=het_outcome_list[i],
                            dose=rep(exposures_labels, each=nlevels(dat[[modifiers[j]]])-1),
                            modifier=modifiers[j],
                            group=levels(dat[[modifiers[j]]])[-1],
                            out_het)

      rownames(out_het) <- NULL
      colnames(out_het) <- c("outcome", "dose", "modifier", "group",
                             "estimate", "se", "z", "pvalue")

      het_list1[[j]] <- out_het
      
      ### collect LR test inferences across effect modifiers within each outcome
      out_lr <- as.data.frame(Anova(mod_het$mod, vcov.=mod_het$vcov))
      out_lr <- rbind(out_lr[grep("flag_dose1:", rownames(out_lr)),,drop=FALSE],
                      out_lr[grep(":flag_dose2", rownames(out_lr)),,drop=FALSE],
                      out_lr[grep(":I\\(weeks_dose1 \\* as.numeric\\(as.character\\(flag_dose1\\)\\)\\)", rownames(out_lr)),,drop=FALSE],
                      out_lr[grep(":I\\(weeks_dose2 \\* as.numeric\\(as.character\\(flag_dose2\\)\\)\\)", rownames(out_lr)),,drop=FALSE])

      out_lr <- data.frame(outcome=het_outcome_list[i],
                           dose=exposures_labels,
                           modifier=modifiers[j],
                           out_lr)
      
      rownames(out_lr) <- NULL
      colnames(out_lr) <- c("outcome", "dose", "modifier",
                            "chi_square", "df", "pvalue")
      
      lr_list1[[j]] <- out_lr
      
    }
    
    ### collect model outputs across outcomes
    het_df1 <- het_list1[[1]]
    if(length(het_list1)>1) {
      for(j in 2:length(het_list1)) {het_df1 <- rbind(het_df1, het_list1[[j]])}
    }
    
    het_list2[[i]] <- het_df1
    
    ### collect LR test inferences across outcomes
    lr_df1 <- lr_list1[[1]]
    if(length(lr_list1)>1) {
      for(j in 2:length(lr_list1)) {lr_df1 <- rbind(lr_df1, lr_list1[[j]])}
    }
    
    lr_list2[[i]] <- lr_df1
    
  }
  
  het_df2 <- het_list2[[1]]
  if(length(het_list2)>1) {
    for(i in 2:length(het_list2)) {het_df2 <- rbind(het_df2, het_list2[[i]])}
  }
  
  lr_df2 <- lr_list2[[1]]
  if(length(lr_list2)>1) {
    for(i in 2:length(lr_list2)) {lr_df2 <- rbind(lr_df2, lr_list2[[i]])}
  }


  het_df2$holm_adjusted_pvalue <- c(p.adjust(het_df2$pvalue[het_df2$outcome == 'lc'], method='holm'),
                                    p.adjust(het_df2$pvalue[het_df2$outcome == 'lc_lim'], method='holm'))

  lr_df2$holm_adjusted_pvalue <- c(p.adjust(lr_df2$pvalue[lr_df2$outcome == 'lc'], method='holm'),
                                   p.adjust(lr_df2$pvalue[lr_df2$outcome == 'lc_lim'], method='holm'))
  
  write.csv(het_df2,
            file=paste(out_dir, "\\heterogeneous_effects.csv", sep=""),
            row.names=FALSE)
  
  write.csv(lr_df2,
            file=paste(out_dir, "\\heterogeneous_effects_lr.csv", sep=""),
            row.names=FALSE)
  
}
