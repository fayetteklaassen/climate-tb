# LT 25/05/2025

# Examine the relationship between TB case numbers in males versus females
library(tidyverse)
library(readxl)
library(fs)

# root of data folder
root_path <- "."

# file provided by Fayette
tb_sex_file <- "notifications-by-sex.csv"

tb_cn <- read_csv(path(root_path, tb_sex_file), na = "NA")
tb_cn <- tb_cn %>%
  mutate(date = str_replace(date, " Q", "."))

# Reshape data: wide format with separate male/female columns
tb_wide <- tb_cn %>%
  pivot_wider(names_from = sex, values_from = n)


# Fit a linear model with different slopes per district: female ~ male * district
# and get the R2 
model <- lm(female ~ male, data = tb_wide)
r_squared <- summary(model)$r.squared
r_label <- paste0("R² = ", round(r_squared, 2))

# Plot: male vs female
ggplot(tb_wide, aes(x = male, y = female)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  labs(
    x = "Male notifications",
    y = "Female notifications",
    title = "TB notifications per gender"
  ) +
  theme_minimal()

# Plot: male vs female, colored by district
ggplot(tb_wide, aes(x = male, y = female, color = topdistrict)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  labs(
    x = "Male notifications",
    y = "Female notifications",
    title = "TB notifications per gender",
    color = "District"
  ) +
  annotate("text", x = Inf, y = -Inf, label = r_label, hjust = 1.1, vjust = -1.5, size = 5, color = "black") +
  theme_minimal()

# Plot: male vs female, colored by district, with trend lines
ggplot(tb_wide, aes(x = male, y = female, color = topdistrict)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  labs(
    x = "Male notifications",
    y = "Female notifications",
    title = "TB notifications per gender",
    color = "District"
  ) +
  annotate("text", x = Inf, y = -Inf, label = r_label, hjust = 1.1, vjust = -1.5, size = 5, color = "black") +
  theme_minimal()

# Fit a linear model with different slopes per district: female ~ male * district
model <- lm(female ~ male * topdistrict, data = tb_wide)

# is the slope statistically different between regions ? it is
anova(model)


