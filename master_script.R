### load required packages
library(haven)
library(sqldf)
library(reshape2)
library(PropCIs)
library(lme4)
library(lmtest)
library(geepack)
library(sandwich)
library(splines)
library(car)
library(performance)
library(pROC)
library(ggplot2)
library(dplyr)
library(stats)
library(forcats)

### source all functions
fun_dir <- "filepath"
fun_list <- paste(fun_dir, list.files(fun_dir), sep="\\")
for(i in 1:length(fun_list)) {source(fun_list[i])}

################################# CREATE DATASETS FOR MODELLING #################################

data.creation(
  out_dir_any_pos_test = "filepath\\Any positive test",
  out_dir_any_pos_test_plus_unvacc = "filepath\\Any positive test plus unvaccinated",
  dataset_date = "20210906",
  cutoff_date = "2021-09-05"
)

#################### SET MASTER PARAMETERS, THEN RUN EACH SUBSEQUENT FUNCTION ####################

### for confirmed COVID-19 cases, with a positive test in or out of study, blood or swab
### use these parameters:

root = "filepath\\Any positive test"

modifiers = c("vaccine_vector", "age10", "female", "non_white",
              "imd_quintile", "health_conditions", "covid_admitted", "time_dose1_group")
covs = c("weeks",  # f(time since infection) must be first covariate in list
         "ns(calendar_time_infection, df=2, Boundary.knots=quantile(calendar_time_infection, c(.10, .90)))",
        "ns(age_at_visit, df=2, Boundary.knots=quantile(age_at_visit, c(.10, .90)))",
         "female",
         "non_white",
         "imd_quintile",
         "gor9d",
         "health_conditions",
         "hscw_pf",
         "covid_admitted")

### for any confirmed infection plus unvaccinated
### use these parameters:

root = "filepath\\Any positive test plus unvaccinated"

modifiers = c("vaccine_vector", "age10", "female", "non_white",
              "imd_quintile", "health_conditions", "covid_admitted", "time_dose1_group")
covs = c("weeks",  # f(time since infection) must be first covariate in list
         "ns(calendar_time_infection, df=2, Boundary.knots=quantile(calendar_time_infection, c(.10, .90)))",
         "ns(age_at_visit, df=2, Boundary.knots=quantile(age_at_visit, c(.10, .90)))",
         "female",
        "non_white",
        "imd_quintile",
        "gor9d",
        "health_conditions",
        "hscw_pf",
        "covid_admitted")

###################################### RUNNING FUNCTIONS ######################################

### load dataset
load(paste(root, "\\dataset.RData", sep=""))

ns(dat$calendar_time_infection, df=3, Boundary.knots=quantile(dat$calendar_time_infection, c(.10, .90)))


if (root == "filepath\\Any positive test"){
  
  ### run descriptive analyses
  descriptive.stats(
    out_dir = paste(root, "\\_Main analysis", sep=""),
    n_plots = 50,
    seed_for_plots = 210723,
    get_hh_icc = FALSE, ### warning: if set to TRUE, models take ages to run and don't converge!
    covs_for_hh_icc = covs
  )
  
  ### assess different specifications for time trajectory
  time.trajectories(
    out_dir = paste(root, "\\_Main analysis", sep=""),
    covs = covs
  )
  
  ### run models - main analysis
  modelling(
    out_dir = paste(root, "\\_Main analysis", sep=""),
    covs = covs
  )
  
  ### subgroup analysis: participants with at least 3 responses after dose 1
  modelling(
    out_dir = paste(root, "\\At least 3 post-dose1 responses", sep=""),
    covs = covs,
    min_n_after_dose1 = 3
  )
  
  ### subgroup analysis: participants with at least 3 responses after dose 2
  modelling(
    out_dir = paste(root, "\\At least 3 post-dose2 responses", sep=""),
    covs = covs,
    min_n_after_dose2 = 3
  )
  
  ### subgroup analysis: participants with at least 1 response before and after dose 1
  modelling(
    out_dir = paste(root, "\\At least 1 pre- and post-dose1 response", sep=""),
    covs = covs,
    min_n_before_dose1 = 1,
    min_n_after_dose1 = 1
  )
  
  ### subgroup analysis: participants with at least 1 response before and after dose 2
  modelling(
    out_dir = paste(root, "\\At least 1 pre- and post-dose2 response", sep=""),
    covs = covs,
    min_n_before_dose2 = 1,
    min_n_after_dose2 = 1
  )
  
  ### sensitivity analysis: model time since infection as cubic spline rather than linear
  modelling(
    out_dir = paste(root, "\\Spline time since infection", sep=""),
    covs = c(paste("bs(weeks, df=6, degree=3)", sep=""), covs[-1])
  )
  
  ### sensitivity analysis: model time since infection as natural cubic spline rather than linear
  modelling(
    out_dir = paste(root, "\\Natural spline time since infection", sep=""),
    covs = c(paste("ns(weeks, df=2, Boundary.knots=quantile(weeks, c(.10, .90)))", sep=""), covs[-1])
  )
  
  ### sensitivity analysis: omit responses within first week after vaccination
  modelling(
    out_dir = paste(root, "\\Omit responses in first week after vaccination", sep=""),
    covs = covs,
    min_time_dose1 = 8,
    min_time_dose2 = 8
  )
  
  ### sensitivity analysis: omit people where first visit is positive swab
  modelling(
    out_dir = paste(root, "\\Omit people where first visit is positive swab", sep=""),
    covs = covs,
    not_first_visit_pos_swab = 1,
  )
  
  ### sensitivity analysis: truncate people who were infected before 21 August 2020
  modelling(
    out_dir = paste(root, "\\Truncate people infected in wave 1", sep=""),
    covs = covs,
    truncate_wave_1 = 1
  )
  
  ### sensitivity analysis: omit people who were infected before 21 August 2020
  modelling(
    out_dir = paste(root, "\\Omit people infected in wave 1", sep=""),
    covs = covs,
    not_infected_wave_1 = 1
  )
  
   ### run model for each symptom
  modelling.symptoms(out_dir = paste(root, "\\Symptom analysis", sep=""),
                     covs = covs
  )
  
  ### test heterogenous effects (main analysis only)
  het.effects(
    out_dir = paste(root, "\\_Main analysis", sep=""),
    covs = covs,
    modifiers = modifiers
  )
  
  ### plot time trajectories by vaccine type
  het.effects.vaccine(
    out_dir = paste(root, "\\_Main analysis", sep=""),
    covs = covs
  )

  ### plot combined odds
  plot.combined.odds(out_dir = paste(root, "\\_Main analysis", sep=""),
                     covs = covs
  )
  
}


if (root == "filepath\\Any positive test plus unvaccinated"){
  
  ### run descriptive analyses for the unvaccinated populations
  descriptive.stats.unvacc(
    out_dir = paste(root, "\\_Main analysis", sep=""),
    n_plots = 50,
    seed_for_plots = 210723,
    get_hh_icc = FALSE, ### warning: if set to TRUE, models take ages to run and don't converge!
    covs_for_hh_icc = covs
  )
  
  ### run models - main analysis
  modelling(
    out_dir = paste(root, "\\_Main analysis", sep=""),
    covs = covs
  )
  
  ### subgroup analysis: include unvaccinated but not vaccinated after study (for study pop 4)
  modelling(
    out_dir = paste(root, "\\Include unvaccinated", sep=""),
    covs = covs,
    unvaccinated = 1
  )
  
  ### subgroup analysis: include vaccinated after study by not unvaccinated (for study pop 4)
  modelling(
    out_dir = paste(root, "\\Include vaccinated after study", sep=""),
    covs = covs,
    vaccinated_after_study = 1
  )
  
}
