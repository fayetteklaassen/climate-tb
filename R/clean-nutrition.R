# LT 25/05/2025

# Examine the relationship between TB case numbers in males versus females
library(tidyverse)
library(readxl)
library(fs)

# root of data folder
root_path <- path("data-raw/nutrition/")

nutrition_file <- "Food Insecurity and Nutrition data_Karamoja_Uganda_Revised04062025.xlsx"

ipc_sheet <- "IPC"
fsi_sheet <- "FSNA1"
fcs_sheet <- "FSNA2"


# ---- Food insecurity IPC ----

fs_ipc <- read_excel(path(root_path, nutrition_file),
                     sheet = ipc_sheet, na = "NA")

# for now we are only interested in % of pop. in phase 3+

# rebuild across regions
# 
# this should be moves in a utuils script

# list of Karamoja's districts and map to the original 5 districts
districts <- c(
  "Abim",
  "Amudat",
  "Kaabong",
  "Karenga",
  "Kotido",
  "Moroto",
  "Nabilatuk",
  "Nakapiripirit",
  "Napak")

districts_map <- c(
  Abim = "Abim",
  Amudat = "Nakapiripirit",
  Kaabong = "Kaabong",
  Karenga = "Kaabong",
  Kotido = "Kotido",
  Moroto = "Moroto",
  Nabilatuk = "Nakapiripirit",
  Nakapiripirit = "Nakapiripirit",
  Napak = "Moroto")

# convert % to number of individuals before merging split regions

fs_ipc <- fs_ipc %>% mutate(`n_3+` = `IPC_3+` * Population) %>%
  mutate(Year = Year_data_validation,
         District = `District/Region`) %>%
  group_by(Year, Season, District) %>%
  # if there is both actual data and projection data, keep actual data
  filter(all(Projection == 0) | all(Projection == 1) | Projection == 0)

# check existence of duplicated data: same Year/Season/District but multiple values
fs_ipc %>%  summarise(n = n()) %>% filter(n > 1)
# there is some: 2022.1 and 2022.2
# this is legit (2 reports)
# # for now, take the average

fs_ipc <- fs_ipc %>% summarise(across(matches("^IPC_\\d"), mean))
fs_ipc %>%  summarise(n = n()) %>% filter(n > 1)
        
  
  
  

  filter(any(Projection == 0) | (!any(Projection == 0) & Projection == 1))

         root_district = districts_map[District])

%>%
  group_by(Year, Season, District, root_district) %>% summarize(CN = sum(CN)) %>% ungroup

