# HISTORICAL PRISM TEMPERATURE DATA
# All 10 sagebrush sites
# November-April, 1995-2025

# LOAD REQUIRED PACKAGES
library(tidyverse)

# READ OWYHEE HIGH PRISM DATA
owyhee_high <- read_csv(
  "Field_sites_data/OwhyeeHigh_30yr.csv",
  skip = 10
)

head(owyhee_high)

# FIND ALL 10 HISTORICAL PRISM FILES
prism_files <- list.files(
  "Field_sites_data",
  pattern = "_30yr\\.csv$",
  full.names = TRUE
)

prism_files

# IMPORT ALL 10 HISTORICAL PRISM FILES
prism_data <- prism_files %>%
  set_names() %>%
  map_dfr(
    ~ read_csv(.x, skip = 10),
    .id = "file"
  )
head(prism_data)

# CLEAN COLUMN NAMES AND CREATE SITE CODES
prism_data <- prism_data %>%
  rename(
    ppt = `ppt (mm)`,
    tmin = `tmin (degrees C)`,
    tmean = `tmean (degrees C)`,
    tmax = `tmax (degrees C)`
  ) %>%
  mutate(
    site = case_when(
      str_detect(file, "BogusBasin") ~ "BB",
      str_detect(file, "OwhyeeHigh") ~ "OH",
      str_detect(file, "ThreeIsland") ~ "TI",
      str_detect(file, "SnakeRiver") ~ "SR",
      str_detect(file, "HullsGulch") ~ "HG",
      str_detect(file, "HawkinsReserve") ~ "HR",
      str_detect(file, "CastleRocks") ~ "CR",
      str_detect(file, "AlbinoHigh") ~ "AH",
      str_detect(file, "NancyGulch") ~ "NG",
      str_detect(file, "ReynoldsCreek") ~ "RC"
    )
  )
unique(prism_data$site)

# CREATE YEAR AND MONTH COLUMNS
prism_data <- prism_data %>%
  separate(
    Date,
    into = c("year", "month"),
    sep = "-",
    remove = FALSE
  ) %>%
  mutate(
    year = as.integer(year),
    month = as.integer(month)
  )
head(prism_data)

# KEEP ONLY NOVEMBER TO APRIL
prism_winter <- prism_data %>%
  filter(month %in% c(11, 12, 1, 2, 3, 4))

# HISTORICAL TEMPERATURE SUMMARY BY SITE AND MONTH
monthly_summary <- prism_winter %>%
  group_by(site, month) %>%
  summarise(
    mean_tmin = mean(tmin),
    mean_tmean = mean(tmean),
    mean_tmax = mean(tmax),
    .groups = "drop"
  )

monthly_summary

#compare each site
site_summary <- prism_winter %>%
  group_by(site) %>%
  summarise(
    mean_tmin = mean(tmin),
    mean_tmean = mean(tmean),
    mean_tmax = mean(tmax),
    .groups = "drop"
  )

site_summary

# ADD MONTH NAMES IN THE CORRECT ORDER
monthly_summary <- monthly_summary %>%
  mutate(
    month_name = factor(
      month,
      levels = c(11, 12, 1, 2, 3, 4),
      labels = c("Nov", "Dec", "Jan", "Feb", "Mar", "Apr")
    )
  )

# PLOT HISTORICAL MONTHLY MEAN TEMPERATURE
ggplot(
  monthly_summary,
  aes(
    x = month_name,
    y = mean_tmean,
    group = site,
    color = site
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    x = "Month",
    y = "Historical mean temperature (°C)",
    color = "Site"
  ) +
  theme_classic()

# HISTORICAL DISTRIBUTION OF MONTHLY MEAN TEMPERATURE
prism_winter %>%
  summarise(
    minimum = min(tmean),
    q05 = quantile(tmean, 0.05),
    q10 = quantile(tmean, 0.10),
    q25 = quantile(tmean, 0.25),
    median = median(tmean),
    q75 = quantile(tmean, 0.75),
    q90 = quantile(tmean, 0.90),
    q95 = quantile(tmean, 0.95),
    maximum = max(tmean)
  )

# HISTORICAL TEMPERATURE RANGE
prism_winter %>%
  summarise(
    tmin_min = min(tmin),
    tmin_q05 = quantile(tmin, 0.05),
    tmin_median = median(tmin),
    
    tmean_min = min(tmean),
    tmean_median = median(tmean),
    tmean_max = max(tmean),
    
    tmax_median = median(tmax),
    tmax_q95 = quantile(tmax, 0.95),
    tmax_max = max(tmax)
  )

# HISTOGRAM OF HISTORICAL MONTHLY MEAN TEMPERATURES
ggplot(prism_winter, aes(x = tmean, fill = after_stat(x))) +
  geom_histogram(
    binwidth = 1,
    color = "black"
  ) +
  scale_fill_gradient(
    low = "lightblue",
    high = "tomato",
    name = "Temperature (°C)"
  ) +
  labs(
    x = "Monthly mean temperature (°C)",
    y = "Count"
  ) +
  theme_classic()

#figure
# ORDER TEMPERATURE VARIABLES
temp_long <- temp_long %>%
  mutate(
    temperature_type = factor(
      temperature_type,
      levels = c("tmax", "tmean", "tmin")
    )
  )
# OVERALL AVERAGE HISTORICAL TEMPERATURES
prism_winter %>%
  summarise(
    mean_tmin = mean(tmin),
    mean_tmean = mean(tmean),
    mean_tmax = mean(tmax)
  )

##
ggplot(temp_long, aes(x = temperature)) +
  geom_histogram(
    binwidth = 1,
    fill = "grey40",
    color = "black"
  ) +
  facet_wrap(
    ~ temperature_type,
    ncol = 1,
    scales = "free_y"
  ) +
  scale_x_continuous(
    breaks = seq(-15, 25, by = 5),
    limits = c(-15, 25)
  ) +
  labs(
    title = "Historical temperatures across 10 sagebrush sites",
    subtitle = "Monthly PRISM temperatures, November–April, 1995–2025",
    x = "Temperature (°C)",
    y = "Number of observations"
  ) +
  theme_classic()

# FINAL PRISM TEMPERATURE SUMMARY
prism_summary <- prism_winter %>%
  summarise(
    mean_tmin  = mean(tmin, na.rm = TRUE),
    min_tmin   = min(tmin, na.rm = TRUE),
    
    mean_tmean = mean(tmean, na.rm = TRUE),
    min_tmean  = min(tmean, na.rm = TRUE),
    max_tmean  = max(tmean, na.rm = TRUE),
    
    mean_tmax  = mean(tmax, na.rm = TRUE),
    max_tmax   = max(tmax, na.rm = TRUE)
  )

prism_summary
