data.creation <- function(out_dir_any_pos_test, 
                          out_dir_any_pos_test_plus_unvacc, 
                          dataset_date, 
                          cutoff_date) {

  ########################################### DATA PREP ###########################################
  
  ### read in visit-level data
  input_file <- paste0("filepath/data_participant_clean.dta")
  
  vars_of_interest <- c(
    "participant_id",
    "hh_id_fake",
    "cis20_samp",
    "visit_id",
    "visit_date",
    "visit_status",
    "long_covid_have_symptoms",
    "reduce_activities_long_covid",
    "long_covid_fever",
    "long_covid_weakness_tiredness",
    "long_covid_diarrhoea",
    "long_covid_loss_of_smell",
    "long_covid_shortness_of_breath",
    "long_covid_vertigo_dizziness",
    "long_covid_trouble_sleeping",
    "long_covid_headache",
    "long_covid_nausea_vomiting",
    "long_covid_loss_of_appetite",
    "long_covid_sore_throat",
    "long_covid_chest_pain",
    "long_covid_worry_anxiety",
    "long_covid_memory_loss_confusion",
    "long_covid_muscle_ache",
    "long_covid_abdominal_pain",
    "long_covid_loss_of_taste",
    "long_covid_cough",
    "long_covid_palpitations",
    "long_covid_low_mood_not_enjoying",
    "long_covid_difficult_concentrate",
    "age_at_visit",
    "sex",
    "ethnicityg",
    "country",
    "gor9d",
    "imd_samp",
    "health_conditions",
    "health_conditions_impact",
    "smoke_ever_regularly",
    "smoke_now_cigarettes",
    "smoke_now_cigar",
    "smoke_now_pipe",
    "smoke_now_vape",
    "smoke_now_nothing",
    "work_sector",
    "work_status",
    "work_status_v1",
    "work_status_v2",
    "work_healthcare",
    "work_healthcare_v1",
    "work_socialcare",
    "work_care_nursing_home",
    "work_direct_contact_patients_etc",
    "result_mk",
    "result_combined",
    "sympt_now_any",
    "sympt_now_fever",
    "sympt_now_muscle_ache_myalgia",
    "sympt_now_fatigue_weakness",
    "sympt_now_sore_throat",
    "sympt_now_cough",
    "sympt_now_shortness_of_breath",
    "sympt_now_headache",
    "sympt_now_nausea_vomiting",
    "sympt_now_abdominal_pain",
    "sympt_now_diarrhoea",
    "sympt_now_loss_of_taste",
    "sympt_now_loss_of_smell",
    "sympt_now_date",
    "covid_test_swab_result",
    "covid_test_swab_pos_first_date",
    "covid_test_swab_neg_last_date",
    "covid_test_blood",
    "covid_test_blood_result",
    "covid_test_blood_pos_first_date",
    "covid_test_blood_neg_last_date",
    "covid_think_havehad",
    "covid_date",
    "covid_nhs_contact",
    "covid_admitted",
    "sympt_covid_any",
    "sympt_covid_fever",
    "sympt_covid_muscle_ache_myalgia",
    "sympt_covid_fatigue_weakness",
    "sympt_covid_sore_throat",
    "sympt_covid_cough",
    "sympt_covid_shortness_of_breath",
    "sympt_covid_headache",
    "sympt_covid_nausea_vomiting",
    "sympt_covid_diarrhoea",
    "sympt_covid_loss_of_taste",
    "sympt_covid_loss_of_smell"
  )
  
  dat_full <- haven::read_dta(input_file, col_select=all_of(vars_of_interest))
  dat_full <- zap_labels(dat_full)
  
  ### drop visits after cut-off date
  dat_full$visit_date <- as.numeric(dat_full$visit_date)
  dat_full <- dat_full[dat_full$visit_date <= as.numeric(as.Date(cutoff_date)),]
  
  ### join vaccination data
  input_file <- paste0("filepath/data_participant_vaccination.dta")
  
  vacc_data <- haven::read_dta(input_file,
                               col_select=c("participant_id",
                                            "covid_vaccine_date1", "covid_vaccine_date2",
                                            "covid_vaccine_type1", "covid_vaccine_type2"))
  
  vacc_data <- zap_labels(vacc_data)
  
  dat_full <- merge(x = dat_full,
                    y = vacc_data,
                    by.x = "participant_id",
                    by.y = "participant_id",
                    all.x = TRUE,
                    all.y = FALSE)
  
  rm(vacc_data); gc()
  
  dat_full$covid_vaccine_date1 <- as.numeric(dat_full$covid_vaccine_date1)
  dat_full$covid_vaccine_date2 <- as.numeric(dat_full$covid_vaccine_date2)
  
  ################################### JOIN COVID STATUS VARIABLES ###################################
  
  ### Create first positive study blood test date
  dat_study_blood_pos1_date <- sqldf("
  select
    participant_id,
    min(visit_date) as study_blood_pos1_date
  from dat_full
  where result_combined=1
  group by participant_id
  ")
  
  ### merge first positive study blood test date
  dat_full <- merge(x = dat_full,
                    y = dat_study_blood_pos1_date,
                    by.x = "participant_id",
                    by.y = "participant_id",
                    all.x = TRUE,
                    all.y = FALSE)
  
  rm(dat_study_blood_pos1_date); gc()
  
  ### Change anyone who tested positive for anti-bodies
  ### after getting vaccinated to negative
  
  ### Study
  dat_full$result_combined <- ifelse(with(dat_full,
                                          !is.na(study_blood_pos1_date) &
                                            !is.na(covid_vaccine_date1) &
                                            study_blood_pos1_date >= covid_vaccine_date1),
                                     0, dat_full$result_combined)
  
  ### Drop the first positive study blood test date
  dat_full <- subset(dat_full, select = -c(study_blood_pos1_date))
  
  ### Non-study
  dat_full$covid_test_blood_pos_first_date <- as.numeric(dat_full$covid_test_blood_pos_first_date)
  dat_full$covid_test_blood_result <- ifelse(with(dat_full,
                                                  !is.na(covid_test_blood_pos_first_date) &
                                                    !is.na(covid_vaccine_date1) &
                                                    covid_test_blood_pos_first_date >= covid_vaccine_date1),
                                             0, dat_full$covid_test_blood_result)
  
  ### Change non-study blood first positive date to NA if
  ### the positive test took place after vaccination
  dat_full$covid_test_blood_pos_first_date <- ifelse(with(dat_full,
                                                          !is.na(covid_test_blood_pos_first_date) &
                                                            !is.na(covid_vaccine_date1) &
                                                            covid_test_blood_pos_first_date >= covid_vaccine_date1),
                                                     NA, dat_full$covid_test_blood_pos_first_date)
  
  ### ever tested positive by swab during the study?
  dat_full$swab_study <- ifelse(dat_full$result_mk==1, 1, 0)
  dat_full$swab_study[is.na(dat_full$swab_study)] <- 0
  
  ### ever tested positive on bloods during the study?
  dat_full$blood_study <- ifelse(dat_full$result_combined==1, 1, 0)
  dat_full$blood_study[is.na(dat_full$blood_study)] <- 0
  
  ### ever tested positive by swab outside of the study?
  dat_full$swab_nonstudy <- ifelse(dat_full$covid_test_swab_result==1, 1, 0)
  dat_full$swab_nonstudy[is.na(dat_full$swab_nonstudy)] <- 0
  
  ### ever tested positive on bloods outside of the study?
  dat_full$blood_nonstudy <- ifelse(dat_full$covid_test_blood_result==1, 1, 0)
  dat_full$blood_nonstudy[is.na(dat_full$blood_nonstudy)] <- 0
  
  ### think you've had COVID?
  dat_full$think_covid <- ifelse(dat_full$covid_think_havehad==1, 1, 0)
  dat_full$think_covid[is.na(dat_full$think_covid)] <- 0
  
  ### aggregate to person level
  dat_covid_status <- sqldf("
  select
    participant_id,
    max(swab_study) as swab_study_ever,
    max(blood_study) as blood_study_ever,
    max(swab_nonstudy) as swab_nonstudy_ever,
    max(blood_nonstudy) as blood_nonstudy_ever,
    max(think_covid) as think_covid_ever
  from dat_full
  group by participant_id
  ")
  
  ### derive COVID status variables
  dat_covid_status$covid_status <- ifelse(dat_covid_status$swab_study_ever==1 |
                                            dat_covid_status$blood_study_ever==1 |
                                            dat_covid_status$swab_nonstudy_ever==1 |
                                            dat_covid_status$blood_nonstudy_ever==1, "Test",
                                          ifelse(dat_covid_status$think_covid_ever==1, "Think", "None"))
  
  dat_covid_status$positive_test <- ifelse(dat_covid_status$covid_status=="Test", 1, 0)
  
  ### left-join variables onto dat_full
  dat_full <- merge(x=dat_full,
                    y=dat_covid_status,
                    by.x="participant_id",
                    by.y="participant_id",
                    all.x=TRUE,
                    all.y=FALSE)
  
  rm(dat_covid_status); gc()
  
  ### create flag for positive swab at first visit
  first_visit_result <- sqldf("
  select swab_study as flag_first_visit_pos_swab, 
  a.participant_id
  from dat_full as a
  left join(
    select
      participant_id,
      min(visit_date) as first_visit_date
    from dat_full
    group by participant_id
  ) as b
  on a.participant_id = b.participant_id
  where visit_date == first_visit_date
  group by a.participant_id
  ")
  
  # merge flag onto dataset
  dat_full <- merge(x=dat_full,
                    y=first_visit_result,
                    by.x="participant_id",
                    by.y="participant_id",
                    all.x=TRUE,
                    all.y=FALSE)
  
  rm(first_visit_result); gc()

  ################################# JOIN TIME-SINCE-COVID VARIABLES #################################
  
  ### coerce necessary variables to date format
  dat_full$covid_test_swab_pos_first_date <- as.numeric(dat_full$covid_test_swab_pos_first_date)
  dat_full$covid_test_blood_pos_first_date <- as.numeric(dat_full$covid_test_blood_pos_first_date)
  dat_full$covid_date <- as.numeric(dat_full$covid_date)
  
  ### COVID-19 arrived in the UK on 24 Jan 2020 - set any dates before this to NA
  dat_full$covid_test_swab_pos_first_date[dat_full$covid_test_swab_pos_first_date < as.numeric(as.Date("2020-01-24"))] <- NA
  dat_full$covid_test_blood_pos_first_date[dat_full$covid_test_blood_pos_first_date < as.numeric(as.Date("2020-01-24"))] <- NA
  dat_full$covid_date[dat_full$covid_date < as.numeric(as.Date("2020-01-24"))] <- NA
  
  ### join earliest positive swab test during the study
  swab_study_dates <- sqldf("
  select
    participant_id,
    min(visit_date) as swab_study_date
  from dat_full
  where swab_study=1
  group by participant_id
  ")
  
  dat_full <- merge(x=dat_full,
                    y=swab_study_dates,
                    by.x="participant_id",
                    by.y="participant_id",
                    all.x=TRUE,
                    all.y=FALSE)
  
  rm(swab_study_dates); gc()
  
  ### join earliest positive blood test during the study
  blood_study_dates <- sqldf("
  select
    participant_id,
    min(visit_date) as blood_study_date
  from dat_full
  where blood_study=1
  group by participant_id
")
  
  dat_full <- merge(x=dat_full,
                    y=blood_study_dates,
                    by.x="participant_id",
                    by.y="participant_id",
                    all.x=TRUE,
                    all.y=FALSE)
  
  rm(blood_study_dates); gc()
  
  ### join earliest positive swab and blood tests outside of the study,
  ### and date when first thought had COVID
  other_dates <- sqldf("
  select
    participant_id,
    min(covid_test_swab_pos_first_date) as swab_nonstudy_date,
    min(covid_test_blood_pos_first_date) as blood_nonstudy_date,
    min(covid_date) as think_covid_date
  from dat_full
  group by participant_id
")
  
  dat_full <- merge(x=dat_full,
                    y=other_dates,
                    by.x="participant_id",
                    by.y="participant_id",
                    all.x=TRUE,
                    all.y=FALSE)
  
  rm(other_dates); gc()
  
  ### define time-since-COVID variables
  dat_full$infection_date_all <- pmin(dat_full$swab_study_date, dat_full$blood_study_date,
                                      dat_full$swab_nonstudy_date, dat_full$blood_nonstudy_date,
                                      dat_full$think_covid_date, na.rm=TRUE)
  dat_full$infection_date_confirmed <- dat_full$swab_study_date
  
  dat_full$time_all <- dat_full$visit_date - dat_full$infection_date_all
  dat_full$time_confirmed <- dat_full$visit_date - dat_full$infection_date_confirmed
  
  ### sort dataset by participant ID, visit date, and visit ID
  dat_full <- dat_full[with(dat_full, order(participant_id, visit_date, -xtfrm(visit_id))),]
  
  ################################### DERIVE VACCINATION VARIABLES ###################################
  
  ### derive vaccine manufacturer (first dose)
  dat_full$vaccine_manufacturer <- ifelse(is.na(dat_full$covid_vaccine_type1), "None",
                                          ifelse(dat_full$covid_vaccine_type1==3, "Moderna",
                                                 ifelse(dat_full$covid_vaccine_type1==4, "Oxford/AZ",
                                                        ifelse(dat_full$covid_vaccine_type1==5, "Pfizer/BioNTech",
                                                               "Other/unknown"))))
  
  ### derive vaccine type (first dose)
  dat_full$vaccine_type <- ifelse(dat_full$vaccine_manufacturer=="Oxford/AZ", "Adenovirus vector",
                                  ifelse(dat_full$vaccine_manufacturer %in% c("Pfizer/BioNTech", "Moderna"), "mRNA",
                                         "None/other/unknown"))
  
  dat_full$vaccine_vector <- ifelse(dat_full$vaccine_type=="Adenovirus vector", 1, 0)
  
  ### flag if vaccinate manufacturer at second dose is different to at first dose
  dat_full$vaccine_diff_manufacturer <- 0
  dat_full$vaccine_diff_manufacturer[!is.na(dat_full$covid_vaccine_type1) &
                                       !is.na(dat_full$covid_vaccine_type2) &
                                       dat_full$covid_vaccine_type1!=dat_full$covid_vaccine_type2] <- 1
  
  ### flag if ever vaccinated (can be after last visit)
  dat_full$vaccinated <- ifelse(!is.na(dat_full$covid_vaccine_date1), 1, 0)

  ################################## APPLY STUDY INCLUSION CRITERIA ##################################
  
  for(cases in c("any positive test", "any positive test plus unvacc")) {
    
    dat <- dat_full

    if(cases=="all") {
      dat$infection_date <- dat$infection_date_all
      dat$time <- dat$time_all
      infection_clause <- "dat$covid_status!='None'"
      out_dir <- out_dir_all
    }
    
    if(cases=="confirmed") {
      dat$infection_date <- dat$infection_date_confirmed
      dat$time <- dat$time_confirmed
      infection_clause <- "dat$swab_study_ever==1"
      out_dir <- out_dir_confirmed
    }
    
    if(cases=="any positive test") {
      dat$infection_date <- dat$infection_date_all
      dat$time <- dat$time_all
      infection_clause <- "dat$positive_test==1"
      out_dir <- out_dir_any_pos_test
    }
    
    if(cases=="any positive test plus unvacc") {
      dat$infection_date <- dat$infection_date_all
      dat$time <- dat$time_all
      infection_clause <- "dat$positive_test==1"
      out_dir <- out_dir_any_pos_test_plus_unvacc
    }
    
    ### keep visits where participant responded to LC question
    dat <- dat[!is.na(dat$long_covid_have_symptoms) &
                 dat$long_covid_have_symptoms %in% c(0,1),]
    
    ### record sample size
    n1 <- sum(!duplicated(dat$participant_id))
    names(n1) <- "responded to LC question at least once"
    
    ### join date and age when first/last responded to LC question
    dat <- sqldf("
  select 
    a.*,
    b.first_lc_response_date,
    b.last_visit_date,
    b.first_lc_response_age
  from dat as a
  left join(
    select
      participant_id,
      min(visit_date) as first_lc_response_date,
      max(visit_date) as last_visit_date,
      min(age_at_visit) as first_lc_response_age
    from dat
    group by participant_id
  ) as b
  on a.participant_id = b.participant_id
  ")
    
    ### flag if first dose was after last visit
    dat$vacc_after_study <- ifelse(!is.na(dat$covid_vaccine_date1) & dat$covid_vaccine_date1 >= dat$last_visit_date, 1, 0)
    
    ### keep participants who were aged 18-69 when they responded to the LC question
    dat <- dat[(dat$first_lc_response_age>=18 & dat$first_lc_response_age < 70),]
    
    ### record sample size
    n2 <- sum(!duplicated(dat$participant_id))
    names(n2) <- "aged 18-69 at each reponse to LC question"
    
    ### keep participants with confirmed/suspected COVID-19 and non-missing infection date
    dat <- dat[eval(parse(text=infection_clause)),]
    dat <- dat[!is.na(dat$infection_date),]
    
    ### record sample size
    n3 <- sum(!duplicated(dat$participant_id))
    names(n3) <- "previously had COVID-19 with non-missing infection date"
    
    ### keep participants who had at least 12 weeks between time of infection and visit when
    ### last responded to the LC question and, for these participants, keep individual visits
    ### that took place from after 12 weeks of infection
    dat <- dat[dat$last_visit_date - dat$infection_date >= 84,]
    dat <- dat[dat$visit_date - dat$infection_date >= 84,]
    
    ### record sample size
    n4 <- sum(!duplicated(dat$participant_id))
    names(n4) <- "at least 12 weeks between infection and last responding to LC question"
    
    if(cases!="any positive test plus unvacc") {
      
      ### keep participants who received at least one dose of a COVID-19 vaccine (Oxford/AZ,
      ### Pfizer/BioNTech or Moderna) after the start of mass immunisation and before their last visit
      ### date during follow-up; for participants who received two doses, ensure the manufacturer was
      ### the same for both doses
      
      ### set vaccination info to NA for doses received after last visit when responded to LC question
      dat$vaccine_manufacturer[dat$covid_vaccine_date1 >= dat$last_visit_date] <- NA
      dat$vaccine_type[dat$covid_vaccine_date1 >= dat$last_visit_date] <- NA
      dat$covid_vaccine_date1[dat$covid_vaccine_date1 >= dat$last_visit_date] <- NA
      dat$covid_vaccine_date2[dat$covid_vaccine_date2 >= dat$last_visit_date] <- NA
      
      dat <- dat[!is.na(dat$covid_vaccine_date1) &
                 dat$covid_vaccine_date1 >= as.numeric(as.Date("2020-12-08")) &
                 dat$covid_vaccine_date1 < dat$last_visit_date &
                 dat$vaccine_manufacturer %in% c("Oxford/AZ", "Pfizer/BioNTech", "Moderna") &
                 dat$vaccine_diff_manufacturer==0,]
      
      ### record sample size
      n5 <- sum(!duplicated(dat$participant_id))
      names(n5) <- "ever vaccinated (at least first dose) with vaccine type of interest"
      
    } else if (cases=="any positive test plus unvacc") {
      
      ### this time also keep participants who were never vaccinated, or who received their first
      ### dose after their final visit
      
      dat <- dat[is.na(dat$covid_vaccine_date1) |
                   (!is.na(dat$covid_vaccine_date1) &
                      dat$covid_vaccine_date1 >= as.numeric(as.Date("2020-12-08")) &
                      dat$vaccine_manufacturer %in% c("Oxford/AZ", "Pfizer/BioNTech", "Moderna") &
                      dat$vaccine_diff_manufacturer==0),]
      
      ### record sample size
      n5 <- sum(!duplicated(dat$participant_id))
      names(n5) <- "if vaccinated, received vaccine type of interest"
      
    }
    
    if(cases!="any positive test plus unvacc") {
      
      ### keep participants who received their first dose after COVID-19 infection
      dat <- dat[dat$covid_vaccine_date1 > dat$infection_date,]
      
      ### record sample size
      n6 <- sum(!duplicated(dat$participant_id))
      names(n6) <- "received first dose after COVID-19 infection"
    
    } else if(cases=="any positive test plus unvacc") {
      
      ### keep participants who received their first dose after COVID-19 infection or were unvaccinated
      dat <- dat[((!is.na(dat$covid_vaccine_date1) & dat$covid_vaccine_date1 > dat$infection_date) | is.na(dat$covid_vaccine_date1)),]
   
      ### record sample size
      n6 <- sum(!duplicated(dat$participant_id))
      names(n6) <- "if vaccinated, received first dose after COVID-19 infection"
      
    }
  
    
    ### write out sample waterfall
    n_df <- as.data.frame(c(n1,n2,n3,n4,n5,n6))
    colnames(n_df) <- "freq"
    write.csv(n_df, file=paste(out_dir, "\\sample_waterfall.csv", sep=""))
    
    ################################## DERIVE OUTCOMES AND COVARIATES ##################################
    
    ### LC of any severity
    dat$lc <- dat$long_covid_have_symptoms
    
    ### LC resulting in activity limitation
    dat$lc_lim <- ifelse(dat$lc==1 &
                           !is.na(dat$reduce_activities_long_covid) &
                           dat$reduce_activities_long_covid %in% c(2,3), 1, 0)
    
    ### Create LC symptom variables 
    dat$lc_fever <- ifelse(dat$lc==1 & !is.na(dat$long_covid_fever) & dat$long_covid_fever==1, 1, 0)
    dat$lc_weakness_tiredness <- ifelse(dat$lc==1 & !is.na(dat$long_covid_weakness_tiredness) & dat$long_covid_weakness_tiredness==1, 1, 0)
    dat$lc_diarrhoea <- ifelse(dat$lc==1 & !is.na(dat$long_covid_diarrhoea) & dat$long_covid_diarrhoea==1, 1, 0)
    dat$lc_loss_of_smell <- ifelse(dat$lc==1 & !is.na(dat$long_covid_loss_of_smell) & dat$long_covid_loss_of_smell==1, 1, 0)
    dat$lc_shortness_of_breath <- ifelse(dat$lc==1 & !is.na(dat$long_covid_shortness_of_breath) & dat$long_covid_shortness_of_breath==1, 1, 0)
    dat$lc_vertigo_dizziness <- ifelse(dat$lc==1 & !is.na(dat$long_covid_vertigo_dizziness) & dat$long_covid_vertigo_dizziness==1, 1, 0)
    dat$lc_trouble_sleeping <- ifelse(dat$lc==1 & !is.na(dat$long_covid_trouble_sleeping) & dat$long_covid_trouble_sleeping==1, 1, 0)
    dat$lc_headache <- ifelse(dat$lc==1 & !is.na(dat$long_covid_headache) & dat$long_covid_headache==1, 1, 0)
    dat$lc_nausea_vomiting <- ifelse(dat$lc==1 & !is.na(dat$long_covid_nausea_vomiting) & dat$long_covid_nausea_vomiting==1, 1, 0)
    dat$lc_loss_of_appetite <- ifelse(dat$lc==1 & !is.na(dat$long_covid_loss_of_appetite) & dat$long_covid_loss_of_appetite==1, 1, 0)
    dat$lc_sore_throat <- ifelse(dat$lc==1 & !is.na(dat$long_covid_sore_throat) & dat$long_covid_sore_throat==1, 1, 0)
    dat$lc_chest_pain <- ifelse(dat$lc==1 & !is.na(dat$long_covid_chest_pain) & dat$long_covid_chest_pain==1, 1, 0)
    dat$lc_worry_anxiety <- ifelse(dat$lc==1 & !is.na(dat$long_covid_worry_anxiety) & dat$long_covid_worry_anxiety==1, 1, 0)
    dat$lc_memory_loss_confusion <- ifelse(dat$lc==1 & !is.na(dat$long_covid_memory_loss_confusion) & dat$long_covid_memory_loss_confusion==1, 1, 0)
    dat$lc_muscle_ache <- ifelse(dat$lc==1 & !is.na(dat$long_covid_muscle_ache) & dat$long_covid_muscle_ache==1, 1, 0)
    dat$lc_abdominal_pain <- ifelse(dat$lc==1 & !is.na(dat$long_covid_abdominal_pain) & dat$long_covid_abdominal_pain==1, 1, 0)
    dat$lc_loss_of_taste <- ifelse(dat$lc==1 & !is.na(dat$long_covid_loss_of_taste) & dat$long_covid_loss_of_taste==1, 1, 0)
    dat$lc_cough <- ifelse(dat$lc==1 & !is.na(dat$long_covid_cough) & dat$long_covid_cough==1, 1, 0)
    dat$lc_palpitations <- ifelse(dat$lc==1 & !is.na(dat$long_covid_palpitations) & dat$long_covid_palpitations==1, 1, 0)
    dat$lc_low_mood_not_enjoying <- ifelse(dat$lc==1 & !is.na(dat$long_covid_low_mood_not_enjoying) & dat$long_covid_low_mood_not_enjoying==1, 1, 0)
    dat$lc_concentrate <- ifelse(dat$lc==1 & !is.na(dat$long_covid_difficult_concentrate) & dat$long_covid_difficult_concentrate==1, 1, 0)
    
    ### create number of symptoms variable
    dat$num_symptoms <- dat$lc_fever + dat$lc_weakness_tiredness + dat$lc_diarrhoea + dat$lc_loss_of_smell +
    dat$lc_shortness_of_breath + dat$lc_vertigo_dizziness + dat$lc_trouble_sleeping + dat$lc_headache +
    dat$lc_nausea_vomiting + dat$lc_loss_of_appetite + dat$lc_sore_throat + dat$lc_chest_pain + dat$lc_worry_anxiety +
    dat$lc_memory_loss_confusion + dat$lc_muscle_ache + dat$lc_abdominal_pain + dat$lc_loss_of_taste + dat$lc_cough +
    dat$lc_palpitations + dat$lc_low_mood_not_enjoying + dat$lc_concentrate
    
    dat$lc_over_5_symptoms <- ifelse(dat$lc==1 & dat$num_symptoms>5, 1, 0)
    
    dat$lc_over_3_symptoms <- ifelse(dat$lc==1 & dat$num_symptoms>3, 1, 0)
    
    ### time since first and second doses
    dat$time_dose1 <- dat$visit_date - dat$covid_vaccine_date1
    dat$time_dose2 <- dat$visit_date - dat$covid_vaccine_date2
    
    ### for participants without a second dose, set last visit to time 0 and work backwards
    dat$time_dose2 <- ifelse(is.na(dat$time_dose2),
                             dat$visit_date - dat$last_visit_date,
                             dat$time_dose2)
    
    ### derive weeks from time (days) variables
    dat$weeks <- dat$time / 7
    dat$weeks_dose1 <- dat$time_dose1 / 7
    dat$weeks_dose2 <- dat$time_dose2 / 7
    
    ### flags for received first and second doses
    dat$flag_dose1 <- ifelse(dat$time_dose1>0, 1, 0)
    dat$flag_dose2 <- ifelse(dat$time_dose2>0, 1, 0)
    
    ### flags for not received first and second doses
    dat$flag_not_dose1 <- 1-dat$flag_dose1
    dat$flag_not_dose2 <- 1-dat$flag_dose2
    
    ### time from infection to first dose
    dat$time_infect_to_vacc1 <- dat$covid_vaccine_date1 - dat$infection_date
    
    time_dose1_breaks <- c(0, 91, 183, 274, 365, 
                           max(dat$time_infect_to_vacc1, na.rm=TRUE))
    dat$time_dose1_group <- cut(dat$time_infect_to_vacc1,
                                breaks = time_dose1_breaks,
                                labels=1:(length(time_dose1_breaks)-1),
                                include.lowest=TRUE)
    
    dat$time_infect_to_vacc2 <- dat$covid_vaccine_date2 - dat$infection_date
    time_dose2_breaks <- c(0, 91, 183, 274, 365, 
                           max(dat$time_infect_to_vacc2, na.rm=TRUE))
    dat$time_dose2_group <- ifelse(is.na(dat$time_infect_to_vacc2), "No dose 2", 
                                   cut(dat$time_infect_to_vacc2,
                                       breaks = time_dose2_breaks,
                                       labels=1:(length(time_dose2_breaks)-1),
                                       include.lowest=TRUE))
    
    ### number of visits before and after first and second doses
    dat$visits_dose1 <- ave(dat$flag_dose1, dat$participant_id, FUN=cumsum)
    dat$visits_dose2 <- ave(dat$flag_dose2, dat$participant_id, FUN=cumsum)
    
    dat$visits_not_dose1 <- ave(dat$flag_not_dose1, dat$participant_id, FUN=cumsum)
    dat$visits_not_dose2 <- ave(dat$flag_not_dose2, dat$participant_id, FUN=cumsum)
    
    ### derive calendar time
    dat$calendar_time_infection <- dat$infection_date - as.numeric(as.Date("2020-01-24"))
    
    dat <- sqldf("
  select a.*, b.n_before_dose1, b.n_before_dose2, b.n_after_dose1, b.n_after_dose2
  from dat as a
  left join(
    select
      participant_id,
      max(visits_not_dose1) as n_before_dose1,
      max(visits_not_dose2) as n_before_dose2,
      max(visits_dose1) as n_after_dose1,
      max(visits_dose2) as n_after_dose2
    from dat
    group by participant_id
  ) as b
  on a.participant_id = b.participant_id
  ")

    ### flag last visit before first and second doses
    dat <- sqldf("
  select a.*, b.time_before_dose1, c.time_before_dose2
  from dat as a
  left join(
    select participant_id, max(time_dose1) as time_before_dose1
    from dat
    where time_dose1<=0
    group by participant_id
  ) as b
  on a.participant_id = b.participant_id
  left join(
    select participant_id, max(time_dose2) as time_before_dose2
    from dat
    where time_dose2<=0
    group by participant_id
  ) as c
  on a.participant_id = c.participant_id
  ")
    
    dat$last_before_dose1 <- ifelse(!is.na(dat$time_before_dose1) &
                                      dat$time_dose1==dat$time_before_dose1, 1, 0)
    dat$last_before_dose2 <- ifelse(!is.na(dat$time_before_dose2) &
                                      dat$time_dose2==dat$time_before_dose2, 1, 0)
    
    ### flag first visit after first and second doses
    dat <- sqldf("
  select a.*, b.time_after_dose1, c.time_after_dose2
  from dat as a
  left join(
    select participant_id, min(time_dose1) as time_after_dose1
    from dat
    where time_dose1>0
    group by participant_id
  ) as b
  on a.participant_id = b.participant_id
  left join(
    select participant_id, min(time_dose2) as time_after_dose2
    from dat
    where time_dose2>0
    group by participant_id
  ) as c
  on a.participant_id = c.participant_id
  ")
    
    dat$first_after_dose1 <- ifelse(!is.na(dat$time_after_dose1) &
                                      dat$time_dose1==dat$time_after_dose1, 1, 0)
    dat$first_after_dose2 <- ifelse(!is.na(dat$time_after_dose2) &
                                      dat$time_dose2==dat$time_after_dose2, 1, 0)
    
    ### flag if participant has at least one pre and post first/second dose measurement
    dat$during_fu_dose1 <- ave(dat$last_before_dose1, dat$participant_id, FUN=max) *
      ave(dat$first_after_dose1, dat$participant_id, FUN=max)
    dat$during_fu_dose2 <- ave(dat$last_before_dose2, dat$participant_id, FUN=max) *
      ave(dat$first_after_dose2, dat$participant_id, FUN=max)
    
    ### flag if infected in wave 1
    dat$infected_wave_1 <- ifelse(dat$infection_date < as.numeric(as.Date("2020-09-23")),1, 0)
    
    ############################### DERIVE DEMOGRAPHIC CHARACTERISTICS ###############################


    ### define 10-year age-band variable (this used to be dat$age, which is not a column, but 
    # actually used dat$age_at_visit so just stating explicitly)
    age_breaks <- c(18, seq(29, 59, 10), 70)
    dat$age10 <- cut(dat$age_at_visit,
                     breaks=age_breaks,
                     labels=1:(length(age_breaks)-1),
                     include.lowest=TRUE)

    ### define male/female variable
    dat$female <- ifelse(dat$sex==2, 1, 0)
    
    ### define white/non-white variable
    dat$non_white <- ifelse(dat$ethnicityg==1, 0, 1)
    
    ### define health or socialcare worker variable
    dat$healthcare_worker <- ifelse(is.na(dat$work_healthcare) | dat$work_healthcare==0, 0, 1)
    dat$socialcare_worker <- ifelse(is.na(dat$work_socialcare) | dat$work_socialcare==0, 0, 1)
    dat$healthsocial_worker <- ifelse(dat$healthcare_worker==1 | dat$socialcare_worker==1, 1, 0)
    
    ### define patient-facing health or socialcare worker variable
    dat$hscw_pf <- ifelse(dat$healthsocial_worker==1 &
                            !is.na(dat$work_direct_contact_patients_etc) &
                            dat$work_direct_contact_patients_etc==1, 1, 0)
    
    ### define health status variable
    ### impute small number of NAs with 'no health condition' or 'no activity limitation'
    dat$health_conditions[is.na(dat$health_conditions)] <- 0
    dat$health_conditions_impact[is.na(dat$health_conditions_impact) &
                                   dat$health_conditions==1] <- 0
    
    dat$health_status <- -999
    dat$health_status[dat$health_conditions==0] <- 0
    dat$health_status[!is.na(dat$health_conditions_impact) &
                        dat$health_conditions_impact==0] <- 1
    dat$health_status[!is.na(dat$health_conditions_impact) &
                        dat$health_conditions_impact==1] <- 2
    dat$health_status[!is.na(dat$health_conditions_impact) &
                        dat$health_conditions_impact==2] <- 3
    
    ### define IMD quintile group
    dat$imd_quintile_eng <- ifelse(dat$imd_samp>(0*32844/5) &
                                     dat$imd_samp<=(1*32844/5), 1,
                                   ifelse(dat$imd_samp>(1*32844/5) &
                                            dat$imd_samp<=(2*32844/5), 2,
                                          ifelse(dat$imd_samp>(2*32844/5) &
                                                   dat$imd_samp<=(3*32844/5), 3,
                                                 ifelse(dat$imd_samp>(3*32844/5) &
                                                          dat$imd_samp<=(4*32844/5), 4,
                                                        ifelse(dat$imd_samp>(4*32844/5) &
                                                                 dat$imd_samp<=(5*32844/5), 5, -999)))))
    
    dat$imd_quintile_wal <- ifelse(dat$imd_samp>(0*1909/5) &
                                     dat$imd_samp<=(1*1909/5), 1,
                                   ifelse(dat$imd_samp>(1*1909/5) &
                                            dat$imd_samp<=(2*1909/5), 2,
                                          ifelse(dat$imd_samp>(2*1909/5) &
                                                   dat$imd_samp<=(3*1909/5), 3,
                                                 ifelse(dat$imd_samp>(3*1909/5) &
                                                          dat$imd_samp<=(4*1909/5), 4,
                                                        ifelse(dat$imd_samp>(4*1909/5) &
                                                                 dat$imd_samp<=(5*1909/5), 5, -999)))))
    
    dat$imd_quintile_sco <- ifelse(dat$imd_samp>(0*6976/5) &
                                     dat$imd_samp<=(1*6976/5), 1,
                                   ifelse(dat$imd_samp>(1*6976/5) &
                                            dat$imd_samp<=(2*6976/5), 2,
                                          ifelse(dat$imd_samp>(2*6976/5) &
                                                   dat$imd_samp<=(3*6976/5), 3,
                                                 ifelse(dat$imd_samp>(3*6976/5) &
                                                          dat$imd_samp<=(4*6976/5), 4,
                                                        ifelse(dat$imd_samp>(4*6976/5) &
                                                                 dat$imd_samp<=(5*6976/5), 5, -999)))))
    
    dat$imd_quintile_ni <- ifelse(dat$imd_samp>(0*890/5) &
                                    dat$imd_samp<=(1*890/5), 1,
                                  ifelse(dat$imd_samp>(1*890/5) &
                                           dat$imd_samp<=(2*890/5), 2,
                                         ifelse(dat$imd_samp>(2*890/5) &
                                                  dat$imd_samp<=(3*890/5), 3,
                                                ifelse(dat$imd_samp>(3*890/5) &
                                                         dat$imd_samp<=(4*890/5), 4,
                                                       ifelse(dat$imd_samp>(4*890/5) &
                                                                dat$imd_samp<=(5*890/5), 5, -999)))))
    
    dat$imd_quintile <- ifelse(dat$country==0, dat$imd_quintile_eng,
                               ifelse(dat$country==1, dat$imd_quintile_wal,
                                      ifelse(dat$country==2, dat$imd_quintile_ni,
                                             ifelse(dat$country==3, dat$imd_quintile_sco, -999))))
    
    ### check for missing IMD quintile group
    anyNA(dat$imd_quintile)
    
    ### people with unknown IMD quintile group - IMD rank is 13,293 but they live in Wales
    ### assuming their LSOA is actually in England and thus their IMD quintile group should be 3
    #dat$imd_quintile[dat$country==1 & dat$imd_samp==13293] <- 3
    
    ### define admission and healthcare contact variables
    ### impute NAs with 'not admitted' or 'no healthcare contact'
    dat$covid_admitted <- ifelse(is.na(dat$covid_admitted), 0, dat$covid_admitted)
    dat$covid_nhs_contact <- ifelse(is.na(dat$covid_nhs_contact), 0, dat$covid_nhs_contact)
    dat$covid_nhs_contact <- ifelse(dat$covid_nhs_contact==0 & dat$covid_admitted==1, 1,
                                    dat$covid_nhs_contact)
    
    ### set up factors
    dat$age10 <- as.factor(dat$age10)
    dat$sex <- as.factor(dat$sex)
    dat$ethnicityg <- as.factor(dat$ethnicityg)
    dat$country <- as.factor(dat$country)
    dat$gor9d <- factor(dat$gor9d, ordered=FALSE)
    dat$gor9d <- relevel(dat$gor9d, ref="7")
    dat$health_status <- as.factor(dat$health_status)
    dat$imd_quintile <- as.factor(dat$imd_quintile)
    dat$covid_vaccine_type1 <- as.factor(dat$covid_vaccine_type1)
    dat$covid_vaccine_type2 <- as.factor(dat$covid_vaccine_type2)
    dat$vaccine_manufacturer <- as.factor(dat$vaccine_manufacturer)
    dat$vaccine_manufacturer <- relevel(dat$vaccine_manufacturer, ref="Oxford/AZ")
    dat$vaccine_type <- as.factor(dat$vaccine_type)
    dat$vaccine_type <- relevel(dat$vaccine_type, ref="Adenovirus vector")
    
    ### convert binary flags to factors
    dat$flag_dose1 <- as.factor(dat$flag_dose1)
    dat$flag_dose2 <- as.factor(dat$flag_dose2)
    dat$vaccine_vector <- as.factor(dat$vaccine_vector)
    dat$female <- as.factor(dat$female)
    dat$non_white <- as.factor(dat$non_white)
    dat$health_conditions <- as.factor(dat$health_conditions)
    dat$hscw_pf <- as.factor(dat$hscw_pf)
    dat$covid_admitted <- as.factor(dat$covid_admitted)
    dat$vaccinated <- as.factor(dat$vaccinated)
    dat$infected_wave_1 <- as.factor(dat$infected_wave_1)
    dat$time_dose1_group <- as.factor(dat$time_dose1_group)
    dat$time_dose1_group <- relevel(dat$time_dose1_group, ref=1)
    dat$time_dose2_group <- as.factor(dat$time_dose2_group)
    dat$time_dose1_group <- relevel(dat$time_dose1_group, ref=1)
    dat$vacc_after_study <- as.factor(dat$vacc_after_study)
    
    ### write out dataset
   save(dat, file=paste(out_dir, "\\dataset.RData", sep=""))
    
  }

}
