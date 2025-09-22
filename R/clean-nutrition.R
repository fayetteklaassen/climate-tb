# LT 22/09/2025

# clean nutrition data: food insecurity (FCS and FI) 
library(tidyverse)
library(readxl)
library(fs)

# root of data folder
root_path <- path("data")
IPC_file <- "20250827_IPC.csv"

# ---- Food insecurity IPC ----

fs_ipc <- read_csv(path(root_path, IPC_file), na = "NA")

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
  mutate(District = `Area`) %>%
  group_by(Year, Season, District) %>%
  # if there is both actual data and projection data, keep actual data
  filter(all(Projection == 0) | all(Projection == 1) | Projection == 0) %>%
  select(Population, `n_3+`)

# check existence of duplicated data: same Year/Season/District but multiple values
fs_ipc %>%  summarise(n = n()) %>% filter(n > 1)
# there is some: 2022.1 and 2022.2
# this is legit (2 reports)
# for now, take the average
fs_ipc <- fs_ipc %>% summarise_all(mean) %>% group_by(Year, Season, District)
fs_ipc %>%  summarise(n = n()) %>% filter(n > 1)

# deal with district data first
fs_ipc_district <- fs_ipc %>% filter(District != "Karamoja")        
fs_ipc_district <- fs_ipc_district %>%
  mutate(root_district = districts_map[District]) %>%
  group_by(Year, Season, root_district) %>%
  summarize(n = sum(Population), `n_3+` = sum(`n_3+`)) %>% ungroup
fs_ipc_district <- fs_ipc_district %>%
  mutate(IPC = `n_3+`/n, District = root_district)


# fill the gaps with missing values
all_years <- min(fs_ipc_district$Year):max(fs_ipc_district$Year)
fs_ipc_district <- expand_grid(Year = all_years,
            Season = 1:2,
            District = unique(fs_ipc_district$District)) %>%
  left_join(fs_ipc_district, by = c("Year", "Season", "District")) %>%
  mutate(time = Year + (Season - 1)/2)

library(zoo)

# interpolated missing value for plotting
df_interp <- fs_ipc_district %>%
  group_by(District) %>%
  arrange(time) %>%
  mutate(
    IPC_interp = na.approx(IPC, x = time, na.rm = FALSE),
    interpolated = is.na(IPC) & !is.na(IPC_interp)  # TRUE if filled
  ) %>%
  ungroup()

# Okabe-Ito palette (7 colors)

cb_palette <- c(
  "#E69F00", "#56B4E9", "#009E73",
  "#0072B2", "#D55E00", "#F0E442", "#CC79A7", "#999999"
)

ggplot(df_interp, aes(x = time, y = IPC_interp, color = District)) +
  # Observed + interpolated lines
  geom_line() +
  # Points with different shapes
  geom_point(aes(shape = interpolated), size = 2) +
  
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 1),
                     labels = c(`FALSE` = "Observed", `TRUE` = "Interpolated"),
                     name = NULL) + # filled circle vs open circle
  # Force x-axis to show only integer years
  scale_x_continuous(
    breaks = unique(floor(df_interp$time)),  # integer years only
    labels = unique(floor(df_interp$time))
  ) +
  scale_color_manual(values = cb_palette) +

  labs(x = "Year", y = "IPC, proportion in phase 3+",
       title = "Food Insecurity in Karamoja: % population in IPC phase 3+",
       shape = "Interpolated") +
  theme_minimal()


# ---- Food insecurity  ----