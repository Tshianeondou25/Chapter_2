# Chapter 2: Thermal Mismatch Experiment
# This script imports, summarize and calculate PRISM Temperature Data for the Wilson (warm) and Summit (cool) common gardens from Jan 2024– Dec 2025.
# The temperature data will be used to characterize thermal conditions at each garden and inform temperature treatments for thermal tolerance trial and setting up the final temperatures for the Thermal mismatch exp
#The downloaded PRISM data contains min temp, mean temp, max temp and precipitation data in a daily basis from 2024-2025

#Packages
library (tidyverse)

#uploading data
wilson <- read.csv(
  "Field_sites_data/PRISM_Wilson_daily_temp_2024_2025.csv",
  skip = 10,
  check.names = FALSE)

summit <- read.csv(
  "Field_sites_data/PRISM_Summit_daily_temp_2024_2025.csv",
  skip = 10,
  check.names = FALSE)

head(wilson)
head(summit)

 ##dates and temp
wilson <- wilson %>%
  rename(
    date = Date,
    ppt = `ppt (mm)`,
    tmin = `tmin (degrees C)`,
    tmean = `tmean (degrees C)`,
    tmax = `tmax (degrees C)`
  ) %>%
  mutate(
    date = as.Date(date, format = "%m/%d/%Y"),
    garden = "Wilson")

summit <- summit %>%
  rename(
    date = Date,
    ppt = `ppt (mm)`,
    tmin = `tmin (degrees C)`,
    tmean = `tmean (degrees C)`,
    tmax = `tmax (degrees C)`
  ) %>%
  mutate(
    date = as.Date(date, format = "%m/%d/%Y"),
    garden = "Summit" )

# Checking data structure and confirming temp variables are numeric
str(wilson)
str(summit)

# Combining Wilson and Summit PRISM temperature data
prism_temp <- bind_rows(wilson, summit)

#checking combined data
dim(prism_temp)

#Summarize temperature conditions by garden
temp_summary <- prism_temp %>%
  group_by(garden) %>%
  summarise(
    mean_tmin = mean(tmin, na.rm = TRUE),
    mean_tmean = mean(tmean, na.rm = TRUE),
    mean_tmax = mean(tmax, na.rm = TRUE),
    min_temp = min(tmin, na.rm = TRUE),
    max_temp = max(tmax, na.rm = TRUE)
  )

temp_summary
#NB:2024-2025, Wilson was 2.4°C warmer in mean daily temperature.

# Calculate temperature quantiles by garden
temp_quantiles <- prism_temp %>%
  group_by(garden) %>%
  summarise(
    tmean_10 = quantile(tmean, 0.10, na.rm = TRUE),
    tmean_25 = quantile(tmean, 0.25, na.rm = TRUE),
    tmean_50 = quantile(tmean, 0.50, na.rm = TRUE),
    tmean_75 = quantile(tmean, 0.75, na.rm = TRUE),
    tmean_90 = quantile(tmean, 0.90, na.rm = TRUE)
  )
temp_quantiles

# Add year and month for examining seasonal temperature patterns
prism_temp <- prism_temp %>%
  mutate(
    year = lubridate::year(date),
    month = lubridate::month(date)
  )

# Calculate monthly mean temperatures for each garden
monthly_temp <- prism_temp %>%
  group_by(garden, year, month) %>%
  summarise(
    mean_tmin = mean(tmin, na.rm = TRUE),
    mean_tmean = mean(tmean, na.rm = TRUE),
    mean_tmax = mean(tmax, na.rm = TRUE),
    .groups = "drop"
  )

monthly_temp

# Compare monthly mean temperatures between Wilson and Summit
temp_difference <- monthly_temp %>%
  select(garden, year, month, mean_tmean) %>%
  pivot_wider(
    names_from = garden,
    values_from = mean_tmean
  ) %>%
  mutate(
    temp_difference = Wilson - Summit)

temp_difference

# View all monthly temperature differences
print(temp_difference, n = 24)

# Plot monthly temperature difference between Wilson and Summit
ggplot(temp_difference,
       aes(x = month, y = temp_difference,
           group = factor(year), color = factor(year))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb
  ) +
  labs(
    x = "Month",
    y = "Wilson - Summit temperature difference (°C)",
    color = "Year"
  ) +
  theme_classic()

# Plot monthly mean temperature for Wilson and Summit
ggplot(
  monthly_temp,
  aes(
    x = month,
    y = mean_tmean,
    color = garden,
    group = garden
  )
) +
  geom_line() +
  geom_point() +
  facet_wrap(~year) +
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb
  ) +
  labs(
    x = "Month",
    y = "Mean temperature (°C)",
    color = "Garden"
  ) +
  theme_classic()

# Subset temperatures to the likely infection period
spring_temp <- prism_temp %>%
  filter(month %in% 2:5)

# Examine the distribution of daily mean temperatures
temp_distribution <- prism_temp %>%
  summarise(
    min = min(tmean, na.rm = TRUE),
    q05 = quantile(tmean, 0.05, na.rm = TRUE),
    q10 = quantile(tmean, 0.10, na.rm = TRUE),
    q25 = quantile(tmean, 0.25, na.rm = TRUE),
    median = quantile(tmean, 0.50, na.rm = TRUE),
    q75 = quantile(tmean, 0.75, na.rm = TRUE),
    q90 = quantile(tmean, 0.90, na.rm = TRUE),
    q95 = quantile(tmean, 0.95, na.rm = TRUE),
    max = max(tmean, na.rm = TRUE)
  )

temp_distribution

# Examine daily mean temperature distribution separately by garden
temp_distribution_garden <- prism_temp %>%
  group_by(garden) %>%
  summarise(
    min = min(tmean, na.rm = TRUE),
    q05 = quantile(tmean, 0.05, na.rm = TRUE),
    q10 = quantile(tmean, 0.10, na.rm = TRUE),
    q25 = quantile(tmean, 0.25, na.rm = TRUE),
    median = quantile(tmean, 0.50, na.rm = TRUE),
    q75 = quantile(tmean, 0.75, na.rm = TRUE),
    q90 = quantile(tmean, 0.90, na.rm = TRUE),
    q95 = quantile(tmean, 0.95, na.rm = TRUE),
    max = max(tmean, na.rm = TRUE)
  )

temp_distribution_garden

# Proposed temperatures for fungal thermal tolerance exp
treatments <- c(5, 10, 15, 20, 25, 30)

# Plot the number of days at different daily mean temp
ggplot(prism_temp, aes(x = tmean)) +
  geom_histogram(
    binwidth = 2,
    boundary = 0,
    color = "black"
  ) +
  geom_vline(
    xintercept = treatments,
    linetype = "dashed"
  ) +
  facet_wrap(~garden, ncol = 1) +
  scale_x_continuous(
    breaks = seq(-15, 30, 5)
  ) +
  labs(
    title = "Daily temperatures at Summit and Wilson",
    subtitle = "PRISM daily mean temperature, 2024–2025; dashed lines show proposed experimental temperatures",
    x = "Daily mean temperature (°C)",
    y = "Number of days"
  ) +
  theme_classic()


