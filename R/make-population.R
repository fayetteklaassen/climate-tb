### Render population data ####
library(tidyverse)

source("R/helper.R") ## region names 

### FILES located in IRD drive (all from public sources)

### 2024 census ####
census1 <- read_csv("data-raw/population/2024-population-district-sex.csv") %>%
  transmute(region = str_to_title(Location),
         pop = Total,
         pop_m = Male,
         pop_f = Female,
         date = 2024,
         source = "2024-census") 

#### Census population totals from 2002, 2014 ####
census2 <- readxl::read_xlsx("data-raw/population/Census_Population_counts_(2002_and_2014)_by_Region,_District_and_Mid-Year_Population_projections_(2015-2021).xlsx", 
                             sheet = 'Sheet3', range = 'C6:E145',
                             col_names = c("region", "2002", "2014")) %>% 
  filter(region %in% subregions$region) %>% 
  mutate(`2002` = as.numeric(`2002`),
         `2014` = as.numeric(`2014`)) %>%
  pivot_longer(cols = c(`2002`, `2014`), names_to = "date", values_to = "pop") %>%
  mutate(date = as.numeric(date),
         source = "2014-census")

# ### Historic census ####
census3 <- readxl::read_xlsx("data-raw/population/Population_by_Census_Year_(1969-2014).xlsx",skip = 2) %>%
  rename(region = 'Region/District') %>%
  filter(region %in% unique(subregions$region)) %>%
  pivot_longer(cols = -"region", values_to = "pop", names_to = "date") %>%
  mutate(date = as.numeric(date),
         source= "historic-census")

### Merge the census data ####
census_full <- rbind(census1 %>% transmute(region, date, pop, source),
                census2, census3) %>%
  left_join(subregions, by = "region") %>%
  group_by(date, source, topdistrict) %>%
  summarize(pop = sum(pop), .groups = 'drop') 

### Plot and review the different sources
ggplot(census_full, aes(x = date, y = pop, col = source)) +
  geom_point() + geom_line() + facet_wrap(~topdistrict) + theme_bw()

## Observations: Kaabong has as only topdistrict an asynchrony between the 2014 census and the historic census.
## Conclusion: trust the 2014 census, as that fits with the general growth line observed

### Generate final population census data  ####

census_final <- census_full %>%
  filter(source != "historic-census") %>%
  pivot_wider(id_cols = c("topdistrict"),
              names_from = "date", values_from = "pop",names_prefix = "pop") %>%
  mutate(growth02_14 = ((pop2014/pop2002)^(1/(12)) - 1) ,
         growth14_24 = ((pop2024/pop2014)^(1/(10)) - 1)) %>% 
  ### Create quarterly dataframe
  right_join(expand.grid("topdistrict" = unique(subregions$topdistrict),
                         "date" = zoo::as.yearqtr(paste0(rep(2002:2024, each = 4), " Q", 1:4))) %>%
               mutate(year = as.numeric(substr(date, 1,4))),
             by = c("topdistrict")) 

 ggplot(census_final, aes(x= date, y = pop_from_census, col = topdistrict)) +
   geom_point() + geom_line() +theme_bw() 

###### Add the birth rates and mortality rates #####

births <- read.csv("data-raw/population/Birthrates/API_SP.DYN.CBRT.IN_DS2_en_csv_v2_126710.csv", skip = 4) %>%
  filter(Country.Name == "Uganda") %>%
  pivot_longer(cols = starts_with("X"), names_to = "year",
               values_to = "birthrateY") %>%
  transmute(year = as.numeric(substr(year,2,5)), birthrateY = birthrateY/1000)

#### Mortality rate historically
deaths <- read.csv("data-raw/population/Deathrates/API_SP.DYN.CDRT.IN_DS2_en_csv_v2_119723.csv",skip = 4) %>%
  filter(Country.Name == "Uganda") %>%
  pivot_longer(cols = starts_with("X"), names_to = "year",
               values_to = "deathrateY") %>%
  transmute(year = as.numeric(substr(year,2,5)), deathrateY = deathrateY/1000)

births %>%
  left_join(deaths, by = c("year")) %>%
  mutate(growthY_rates = birthrateY - deathrateY) -> rates

rates %>%
  ggplot(aes( x= year, y = growthY_rates))+ geom_line()

census_final %>% 
  left_join(rates, by = "year") %>%
  group_by(topdistrict) %>%
  arrange(date) %>%
  transmute(
    date = date,
    pop_obs = case_when(
      date == zoo::as.yearqtr("2002 Q1") ~ pop2002,
      date == zoo::as.yearqtr("2014 Q1") ~ pop2014,
      date == zoo::as.yearqtr("2024 Q1") ~ pop2024),
    growthY_census = case_when(date < zoo::as.yearqtr("2014 Q1") ~ growth02_14,
                              TRUE ~ growth14_24),
    birthrateY_adj = growthY_census + deathrateY,
    growth_quarterly_compound = (1+growthY_census)^(1/4),
    pop_assumed = case_when(date ==zoo::as.yearqtr("2002 Q1") ~ pop2002,
                           TRUE ~ pop2002 * cumprod(growth_quarterly_compound)),
    births = pop_assumed * (1+birthrateY_adj),
    deaths = pop_assumed * (1-deathrateY)) %>%
  ungroup() -> census_complete

ggplot(census_complete, 
       aes(x = date, y = pop_obs)) +
  geom_point() +
  geom_line() +
  geom_point(aes(y = pop_assumed), col = "blue")+
  geom_line(aes(y = pop_assumed), col = "blue")+
  facet_wrap(~topdistrict)

write_csv(census_complete, "data-products/pop-quarterly-assumed.csv")
