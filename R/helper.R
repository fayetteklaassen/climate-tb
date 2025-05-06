##### Libraries #####
library(tidyverse)
library(ggplot2)
library(readxl)


#### Helper data ####
qs <- data.frame("quarter" = c(1,2,3,4), "months" = c("Jan.to.Mar",
                                                      "Apr.to.Jun",
                                                      "Jul.to.Sep",
                                                      "Oct.to.Dec"),
                 "months2" = c("Jan to Mar",
                               "Apr to Jun",
                               "Jul to Sep",
                               "Oct to Dec"))

districts <- c("Abim",
               "Amudat",
               "Kaabong",
               "Kotido",
               "Napak",
               "Nakapiripirit",
               "Nabilatuk",
               "Moroto",
               "Karenga")

subregions <- data.frame(region = districts,
                         topdistrict = districts) %>%
  mutate(topdistrict = case_when(region == "Napak" ~ "Moroto",
                                 region == "Nabilatuk" ~ "Nakapiripirit",
                                 region == "Karenga" ~ "Kaabong",
                                 region == "Amudat" ~ "Nakapiripirit",
                                 TRUE ~ region))
