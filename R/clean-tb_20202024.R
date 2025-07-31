#### Clean TB 2020 onwards
library(tidyverse)

filedir <- ("data-raw/tb/20202024/")
source("R/helper.R")

#### 
# list.files(filedir)
## Notifications -- three slices of notification data: (1) by on-treatment (2) by registered and (3)
# by age/sex for only new and relapse
## Outcomes -- two slices of outcomes (by age/sex and by diagnosis/tbhistory), 
# in addition to 3 unique summary measures (coded as additinoal slices)

ntf_varnames <- read.csv("data-helpers/tb_20202024_notifications-varnames.csv")

ntf <- read.csv(paste0(filedir,"Notifications.csv")) %>%
  pivot_longer(cols = !starts_with("organisation"),
               names_to  = c("time", "name"),
               names_pattern = "(...............)(.*)",
               values_to = "n") %>%
  mutate(months = substr(time,1,10),
         year = substr(time,12,15),
         region = substr(organisationunitname,1,nchar(organisationunitname)-9)) %>%
  left_join(qs, by = "months") %>%
  left_join(ntf_varnames, by = "name") %>%
  transmute(  slice = case_when(tbhistory !="all" & treatment !="on treatment" ~ "notifications",
                                tbhistory !="all" ~ "notifications-on-treatment",
                                TRUE ~ "notifications-new-relapse_age_sex"),
              region, date  = zoo::as.yearqtr(paste0(year, " Q", quarter)),
            diagnosis, tbhistory, 
            sex, age,
            outcome = "notified",
            # n,
            n = ifelse(is.na(n), 0, n),
) 

oo_names <- read_csv("data-helpers/tb_20202024_outcomes-varnames.csv")

oo <- read.csv(paste0(filedir,"oUTCOMES_2020-Onwards.csv")) %>%
  mutate(across(everything(),as.character)) %>%
  pivot_longer(cols = !starts_with("organisation"),
               names_to = c("name", "time"),
               values_to = "n",
               names_pattern = "(.*)(\\..*\\.to.*)") %>%
  mutate(months = substr(time,2,11),
         year = substr(time,13,16),
         region = substr(organisationunitname,
                         1,
                         nchar(organisationunitname) - 9)
         ) %>%
  left_join(qs, by = "months") %>%
  left_join(oo_names, by = "name") %>%
  filter(hiv == "all") %>%
  filter(tbhistory != "community_DOT") %>%
  filter(!is.na(outcome))  %>%
  transmute(
            slice = case_when(
              outcome == "late_treatment_start" ~ "late_treatment_start",
              outcome == "died_all" ~ "died_all",
              outcome == "success_rate" ~ "success_rate",
              diagnosis == "all" ~ "outcomes-new-relapse_sex_age",
              diagnosis %in% c("unknown",
                               "eptb", "cd","bc") & outcome == "notified" ~ "notified_diagnosis_tbhistory_sex",
              diagnosis %in% c("unknown",
                               "eptb", "cd","bc") ~ "outcomes_diagnosis_tbhistory_sex"),
            region, date  = zoo::as.yearqtr(paste0(year, " Q", quarter)),
            diagnosis, tbhistory,
            sex, age,
            outcome, n = as.numeric(n)) 

##### BInd rows and write csv ####

ntf %>% bind_rows(oo) %>%
  write_csv("data-clean/tb_20202024.csv")
#### (1) outcomes by sex x diagnosis #####
oo %>%
  filter(slice == "diag-history") %>%
  transmute(region, date, outcome, sex, diagnosis, tbhistory, n = as.numeric(n)) %>%
  write_csv("data-clean/tb_20202024_outcomes-history-diagnosis.csv")

#### (2) outcomes by sex x age (new/relapse only)#####
oo %>% filter(slice == "age-sex") %>%
  transmute(region, date, outcome, sex, age, n = as.numeric(n)) %>%
  write_csv("data-clean/tb_20202024_outcomes-sex-age_new-relapse.csv")

#### (3) outcomes link####
oo %>%
  filter(slice %in% c("age-sex", "diag-history", "died_all")) %>%
  mutate(n = replace_na(as.numeric(n),0)) %>%
  group_by(region, date, outcome, slice) %>%
  summarize(has_missing = case_when(is.na(sum(n)) ~ TRUE,
                                    TRUE ~ FALSE),
            n = sum(n, na.rm = TRUE), .groups = 'drop') %>%
  mutate(outcome = case_when(outcome == "died_all"~ "died",
                             TRUE ~ outcome)) %>%
  write_csv("data-clean/tb_20202024_outcomes-link.csv")
  
