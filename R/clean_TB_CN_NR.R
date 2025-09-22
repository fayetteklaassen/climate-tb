# LT 25/04/2025

# rebuild quarterly time-series of TB case numbers and notification rate
# over 2007 - 2014

library(tidyverse)
library(readxl)
library(fs)

# root of data folder
root_path <- "data"

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

# ---- TB case numbers (CN) data 2007-2014 ----

tb_file_1 <- "TB_CN_2007_2014.xlsx"

tb_cn_1 <- read_excel(path(root_path, tb_file_1), na = "NA")

# ---- TB CN 2015 - 2019 ----

tb_file_2 <- "Quarterly_TB 2015_2019.csv"

tb_cn_2 <- read_csv(path(root_path, tb_file_2)) 

tb_cn_2 <- tb_cn_2 |>
  mutate(District = str_extract(organisationunitname , "^\\S+")) |>
  filter(District %in% districts) |>
  select(- starts_with("organisationunit")) |>
  pivot_longer(
    cols = starts_with("NTLP - Total notified TB cases"),  # columns to unpivot
    names_to = "period",            # name of the new 'key' column
    values_to = "CN"           # name of the new 'value' column
) %>%
  mutate(
    # extract Year: first word made of 4 digits
    Year = as.numeric(str_extract(period, "\\b\\d{4}\\b")),
    # extract quarter: 3 letters month " to " 3 letters month
    Quarter_text = str_extract(period, "[A-Za-z]{3} to [A-Za-z]{3}"),
    # convert quarter to numeric code
    Quarter = recode(Quarter_text,
      "Jan to Mar" = 1,
      "Apr to Jun" = 2,
      "Jul to Sep" = 3,
      "Oct to Dec" = 4)) |>
  select(Year, Quarter, District, CN)

# Check missing values
sum(is.na(tb_cn_2$CN))
filter(tb_cn_2, is.na(CN))
# manually checked in original csv file: PASS

# ---- TB CN 2020 - 2024 ----

tb_file_3 <- "Notifications.csv"

tb_cn_3 <- read_csv(path(root_path, tb_file_3)) |>
  
  # all blanks are zeroes
  mutate(across(where(is.numeric), ~ replace_na(., 0))) |>
  
  # get district from organisationunitname
  mutate(District = str_extract(organisationunitname , "^\\S+")) |>
  
  # keep only New and Relapse TB Patients Registered
  select(District, matches(" - TB08\\.")) |>
  
  # pivot data columns in order to split compound columns names into separate variables
  pivot_longer(
    cols = -District,
    names_to = "full_name",
    values_to = "CN"
  ) %>%
  mutate(
    Quarter_text = str_extract(full_name, "[A-Za-z]{3} to [A-Za-z]{3}"),
    Quarter = recode(Quarter_text,
                     "Jan to Mar" = 1,
                     "Apr to Jun" = 2,
                     "Jul to Sep" = 3,
                     "Oct to Dec" = 4),
    Year = as.numeric(str_extract(full_name, "\\b\\d{4}\\b")),
    age_group = str_extract(full_name, "<1Yr|\\d{1,2}\\+Yrs|\\d{1,2}-\\d{1,2}Yrs"),
    gender = str_extract(full_name, "(?<=, )Male|Female")
  ) %>%
  select(Year, Quarter, District, age_group, gender, CN)

# this format could be of use to Fayette

# BUT for this rebuild, I collapse gender and age_group

tb_cn_3 <- tb_cn_3 %>% group_by(Year, Quarter, District) %>%
  summarise(CN = sum(CN, na.rm = TRUE))

  
# ---- Rebuild consistent time-series over 5 districts ----

tb_cn <- bind_rows(select(tb_cn_1, -Population), tb_cn_2, tb_cn_3)
tb_cn <- tb_cn %>% mutate(root_district = districts_map[District]) %>%
  group_by(Year, Quarter, root_district) %>% summarize(CN = sum(CN)) %>% ungroup

# ---- Population size time-series ----

pop_file <- "pop-quarterly-assumed.csv"

pop <- read_csv(path(root_path, pop_file), na = "NA") %>%
  mutate(Year = as.numeric(str_extract(date, "^\\d{4}")),
         Quarter = as.numeric(str_extract(date, "\\d$")),
         root_district = topdistrict,
         pop = pop_assumed)

# calculate notification rate per 100 000 individuals
tb_nr <- tb_cn %>% left_join(pop) %>%
  mutate(nr = CN / pop * 1e5)

# ---- Plotting ----

# Okabe-Ito palette (8 colors)
# cb_palette <- c(
#   "#E69F00", "#56B4E9", "#009E73", "#F0E442",
#   "#0072B2", "#D55E00", "#CC79A7", "#999999"
# )
cb_palette <- c(
  "#E69F00", "#56B4E9", "#009E73",
  "#0072B2", "#D55E00", "#F0E442", "#CC79A7", "#999999"
)

tb_cn %>% 
  mutate(time = paste0(Year, ".", Quarter),
         time = factor(time, levels = unique(time)), # preserve order when plotting
         District = root_district) %>%
  ggplot(aes(x = time, y = CN, color = District, group = District)) +
  geom_line() +
  geom_point() +
  scale_x_discrete(
    labels = function(x) {
      # Show the year every 4th tick, otherwise blank
      ifelse((seq_along(x) - 1) %% 4 == 0, str_extract(x, "\\d{4}"), "")
    }
  ) +
  scale_color_manual(values = cb_palette) +
  theme_minimal() + 
  labs(title = "TB cases per district over time", x = "Time", y = "TB cases") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

tb_nr %>% 
  mutate(time = paste0(Year, ".", Quarter),
         time = factor(time, levels = unique(time)), # preserve order when plotting
         District = root_district) %>%
  ggplot(aes(x = time, y = nr, color = District, group = District)) +
  geom_line() +
  geom_point() +
  scale_x_discrete(
    labels = function(x) {
      # Show the year every 4th tick, otherwise blank
      ifelse((seq_along(x) - 1) %% 4 == 0, str_extract(x, "\\d{4}"), "")
    }
  ) +
  scale_color_manual(values = cb_palette) +
  theme_minimal() + 
  labs(title = "Karamoja: TB notification rate per district over time", x = "Time", y = "TB notification rate per 100 000 individuals") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
