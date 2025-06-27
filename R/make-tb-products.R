source("R/helper.R")
# ONly if changes are made 
# source("R/combine_clean-tb.R")

df <- read_csv("data-clean/tb_total.csv") %>%
         left_join(subregions, by = "region") %>%
         filter(diagnosis != "eptb") %>%
  mutate(date = zoo::as.yearqtr(date))


#### Make Clean Notifications Data  #####

df %>%
         filter(outcome == "notified") %>%
         group_by(slice,topdistrict,date,time) %>%
  ## sum over sex, tb history, diagnosis and age
         summarize(n = sum(n),.groups = 'drop') -> df_notif
  
df_notif %>% ggplot(
       aes(x = date, y = n, col = slice)) +
  geom_line() + geom_point() +
  facet_wrap(~topdistrict ) + 
  theme_bw()


df_notif %>% 
  filter(slice == "notifications" |
           slice == "notifications_diagnosis_tbhistory_sex" & time == "20072014") %>%
 # check that there's only 1 entry per quarter (no overlapping slices)
   # group_by(region, date) %>% summarize(n = n(),.groups= 'drop') %>% arrange(desc(n))
transmute(topdistrict, date, n) %>% write_csv("data-products/notifications.csv")

#### Make notifications by TB history ####
### NOTE: these do NOT ADD UP TO THE TOTAL NOTIFICATIONS!  ONLY USABLE AS FRACTIONS ###
df %>%
  filter(outcome == "notified") %>%
  group_by(slice,topdistrict,date,tbhistory,time) %>%
  ## sum over sex, diagnosis and age
  summarize(n = sum(n),.groups = 'drop') %>% 
  filter(slice == "outcomes_diagnosis_tbhistory_sex" |
           slice == "notifications_diagnosis_tbhistory_sex") %>%# pull(tbhistory) %>% unique

  # check that there's only 1 entry per quarter (no overlapping slices)
  # group_by(region, date) %>% summarize(n = length(unique(time)),.groups= 'drop') %>% arrange(desc(n))
  transmute(topdistrict, date, tbhistory, n) %>% write_csv("data-products/notifications-by-tbhistory.csv")


#### Make notifications by sex ####
df %>%
  filter(outcome == "notified") %>%
  filter(sex %in% c("f","m","female","male")) %>%
  mutate(sex = case_when(sex == "f" ~ "female",
                         sex == "m" ~ "male",
                         TRUE ~ sex)) %>% 
  group_by(slice,topdistrict,date,sex,time) %>%
  ## sum over sex, diagnosis and age
  summarize(n_exclNA = sum(n, na.rm = TRUE), 
            has_NA = any(is.na(n)),
    n = sum(n), .groups = 'drop') %>% 
  filter(slice == "outcomes_diagnosis_tbhistory_sex" |
           slice == "notifications_diagnosis_tbhistory_sex") %>%# pull(tbhistory) %>% unique

  # check that there's only 1 entry per quarter (no overlapping slices)
  # group_by(region, date) %>% summarize(n = length(unique(time)),.groups= 'drop') %>% arrange(desc(n))
  transmute(topdistrict, date, sex, n, n_exclNA, has_NA) %>% write_csv("data-products/notifications-by-sex.csv")


#### Make Treatment Outcomes #####
#### Note, these do NOT necessarily add up to the notifications ####
df %>%
  filter(slice == "outcomes_diagnosis_tbhistory" | slice == "outcomes_diagnosis_tbhistory_sex") %>%
  mutate(outcome = case_when(outcome %in% c("cured", "completed") ~ "cured",
                             outcome %in% c("transferred", "failure")~ "other",
                             TRUE ~ outcome)) %>%
  filter(outcome != "notified") %>%
  group_by(date,topdistrict,outcome) %>%
  summarize(has_na = ifelse(is.na(sum(n)), TRUE, FALSE),
    n = sum(n, na.rm = TRUE),
            .groups = 'drop') %>%
  transmute(topdistrict, date, outcome, n, has_na) %>%
  # ggplot(aes(x = date, y = n, col = outcome)) +
  # geom_line() +geom_point() +
  # facet_wrap(~topdistrict) +theme_bw()
  write_csv("data-products/outcomes.csv")

#### Make Treatment Outcomes by History #####
df %>%
  filter(slice == "outcomes_diagnosis_tbhistory" | slice == "outcomes_diagnosis_tbhistory_sex") %>%
  mutate(outcome = case_when(outcome %in% c("cured", "completed") ~ "cured",
                             outcome %in% c("transferred", "failure")~ "other",
                             TRUE ~ outcome)) %>%
  filter(outcome != "notified") %>%
  group_by(date,topdistrict,outcome,tbhistory) %>%
  summarize(has_na = ifelse(is.na(sum(n)), TRUE, FALSE),
            n = sum(n, na.rm = TRUE),
            .groups = 'drop') %>%
  transmute(topdistrict, date, outcome, tbhistory, n, has_na) %>%
  # ggplot(aes(x = date, y = n, col = outcome)) +
  # geom_line() +geom_point() +
  # facet_wrap(~topdistrict) +theme_bw()
  write_csv("data-products/outcomes-by-tbhistory.csv")

