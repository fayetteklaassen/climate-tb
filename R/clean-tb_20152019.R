## Clean 2015-2019 data

##### load helpers #####
library(tidyverse)
library(readxl)
source("R/helper.R")
filedir <- ("data-raw/tb/20152019/")

#### Overview of contents ####
# list.files(filedir)
#### 7 files
## 1. annual_TB -- ignore
## 2. complted cured -- treatment outcomes (part 1)
## 3. notified details -- notifications by group
## 4. number died -- ignore
## 5. number died failure lftu == treatment outcomes (part 2)
## 6. proportion deaths --? ignore?
## 7. quarterly -- notifications

### 3. Notifications by age and sex ####
### Manual file splitting up the variable names
notif_age_sex_varnames <- read.csv("data-helpers/tb_20152019_notified-details-varnames.csv")

## Formatting the wide format to long format
notif_age_sex <- read.csv(paste0(filedir, "Notified details.csv")) %>% 
  pivot_longer(cols = starts_with("X"),
               names_to = c("name", "time"), 
               values_to = "n",
               names_pattern = "(.*)(\\..*\\.to.*)") %>%
  mutate(months = substr(time,2,11),
         year = as.numeric(substr(time,13,16)),
         region = substr(organisationunitname,1,nchar(organisationunitname)-9)) %>%
  left_join(qs, by = "months") %>%
  left_join(notif_age_sex_varnames, by = "name") %>%
  transmute(region,
            sex = case_when(sex == "f" ~ "female",
                            sex == "m" ~ "male",
                            !is.na("sex") ~ "all",
                            TRUE ~ NA),
            tbhistory, age, diagnosis,
            n,
            date  = zoo::as.yearqtr(paste0(year, " Q", quarter))) %>%
  ## Manually add the NAs for 2015Q1 ((and Q2)), and missing quarter in Karenga
  mutate(n = case_when(date == "2015 Q1" ~ NA,
                           date == "2015 Q2" & region != "Abim" ~ NA,
                           date == "2017 Q1" & region == "Karenga" ~ NA,
                           TRUE ~ replace_na(n, 0))) %>%
  transmute(slice = case_when(age == "all" ~ "notifications_diagnosis_tbhistory_sex",
                           TRUE ~ "notifications_sex_age"),
            region, date,
            diagnosis, tbhistory,
            sex, age,
            outcome = "notified", n) %>%
  filter(region %in% districts) 


### 7. Quarterly notifications ######
qtb <- read.csv(paste0(filedir,"Quarterly_TB 2015_2019.csv")) %>%
  pivot_longer(cols = starts_with("N"),
               names_to = c("name", "time"),
               values_to = "n",
               names_pattern = "(.*)(\\..*\\.to.*)") %>%
  mutate(months = substr(time,2,11),
         region = substr(organisationunitname, 1, nchar(organisationunitname)-9),
         year = as.numeric(substr(time,13,16))) %>%
  filter(region %in% districts) %>%
  left_join(qs, by = "months") %>%
  transmute(slice = "notifications",
            region, date = zoo::as.yearqtr(paste0(year, " Q", quarter)),
            diagnosis = "all", tbhistory = "all",
            sex = "all", age = "all",
            outcome = "notified", n) %>%
  ## Manually add the NAs for 2015Q1 ((and Q2)), and missing quarter in Karenga
  mutate(n = case_when(date == "2015 Q1" ~ NA,
                       date == "2015 Q2" & region != "Abim" ~ NA,
                       date == "2017 Q1" & region == "Karenga" ~ NA,
                       TRUE ~ replace_na(n, 0)))


#### 2. Treatment outcomes #####
cc_names <- read.csv("data-helpers/tb_20152019_cured-varnames.csv")

cc <- read.csv(paste0(filedir, "Complted Cured_.csv")) %>%
  pivot_longer(cols = starts_with("X"),
               names_to = c("name", "time"),
               values_to = "n",
               names_pattern = "(.*)(\\..*\\.to.*)") %>%
  mutate(months = substr(time,2,11),
         year = as.numeric(substr(time,13,16)),
         region = substr(organisationunitname,1,nchar(organisationunitname)-9)) %>%
  left_join(qs, by = "months") %>%
  left_join(cc_names, by = "name") %>%
  filter(hiv == "all") %>%
  transmute(slice = "outcomes_diagnosis_tbhistory",
            region, date = zoo::as.yearqtr(paste0(year, " Q", quarter)),
            diagnosis, tbhistory,
            sex, age,
            outcome, n) %>%
  ## Manually add the NAs for 2015Q1 ((and Q2)), and missing quarter in Karenga
  mutate(n = case_when(date == "2015 Q1" ~ NA,
                       date == "2015 Q2" & region != "Abim" ~ NA,
                       date == "2017 Q1" & region == "Karenga" ~ NA,
                       TRUE ~ replace_na(n, 0)))

#### 5. Treatment outcomes ####
dd_names <- read.csv("data-helpers/tb_20152019_died-varnames.csv")

dd <- read.csv(paste0(filedir, "Number Died_failure_LTFUP_.csv")) %>%
  pivot_longer(cols = starts_with("X"),
               names_to = c("name", "time"),
               values_to = "n",
               names_pattern = "(.*)(\\..*\\.to.*)") %>%
  mutate(months = substr(time,2,11),
         year = as.numeric(substr(time,13,16)),
         region = substr(organisationunitname,1,nchar(organisationunitname)-9)) %>%
  left_join(qs, by = "months") %>%
  left_join(dd_names, by = "name") %>%
  filter(hiv == "all") %>%
  transmute(slice = "outcomes_diagnosis_tbhistory",
            region, date = zoo::as.yearqtr(paste0(year, " Q", quarter)),
            diagnosis, tbhistory,
            sex, age,
            outcome, n) %>%
  ## Manually add the NAs for 2015Q1 ((and Q2)), and missing quarter in Karenga
  mutate(n = case_when(date == "2015 Q1" ~ NA,
                       date == "2015 Q2" & region != "Abim" ~ NA,
                       date == "2017 Q1" & region == "Karenga" ~ NA,
                       TRUE ~ replace_na(n, 0)))
  

notif_age_sex %>% bind_rows(qtb) %>%
  bind_rows(cc) %>% bind_rows(dd) %>%
  filter(region %in% districts) %>% 
write_csv("data-clean/tb_20152019.csv")

