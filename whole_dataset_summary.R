### load required packages
library(haven)

dataset_date = "20210906"
cutoff_date = "2021-09-05"
  
### read in visit-level data
input_file <- paste0("filepath/data_participant_clean.dta")

vars_of_interest <- c(
  "participant_id",
  "hh_id_fake",
  "visit_date",
  "consent"
)

dat_full <- haven::read_dta(input_file, col_select=all_of(vars_of_interest))
dat_full <- zap_labels(dat_full)

### drop visits after cut-off date
dat_full$visit_date <- as.numeric(dat_full$visit_date)
dat_full <- dat_full[dat_full$visit_date <= as.numeric(as.Date(cutoff_date)),]

### sort dataset by participant ID and visit date
dat_full <- dat_full[with(dat_full, order(participant_id, visit_date)),]

### number of unique participants
nrow(dat_full[!duplicated(dat_full$participant_id),])

### number of unique households
nrow(dat_full[!duplicated(dat_full$hh_id_fake),])

### take first visit per participant
dat <- dat_full[!duplicated(dat_full$participant_id),]

### keep participants with valid consent info
dat <- dat[dat$consent %in% c(1,5,16),]

### tabulate consent status
table(dat$consent, useNA="ifany")
round(table(dat$consent, useNA="ifany") / nrow(dat) * 100, 0)
