#### Combine clean data
source("R/helper.R")

source("R/clean-tb_20072014.R")
source("R/clean-tb_20152019.R")
source("R/clean-tb_20202024.R")

t1 <- read_csv("data-clean/tb_20072014.csv") %>% mutate(time = "20072014")
t2 <- read_csv("data-clean/tb_20152019.csv") %>% mutate(time = "20152019") 
t3 <- read_csv("data-clean/tb_20202024.csv") %>% mutate(time = "20202024")

df <- bind_rows(t1, t2, t3) %>%
  write_csv("data-clean/tb_total.csv")
