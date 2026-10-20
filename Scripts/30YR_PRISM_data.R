# LOAD REQUIRED PACKAGES
library(prism)
library(tidyverse) # purrr and dplyr used

# SET THE DATA STORAGE LOCATION
prism_set_dl_dir("Data/", create = TRUE)

# DOWNLOAD HISTORICAL MONTHLY PRISM DATA (1995–2025), NOVEMBER–APRIL
get_prism_monthlys(type = "tmin", years = 1995:2025, mon = c(11, 12, 1, 2, 3, 4), keepZip = TRUE, resolution = "4km")
get_prism_monthlys(type = "tmean", years = 1995:2025, mon = c(11, 12, 1, 2, 3, 4), keepZip = TRUE, resolution = "4km")
get_prism_monthlys(type = "tmax", years = 1995:2025, mon = c(11, 12, 1, 2, 3, 4), keepZip = TRUE, resolution = "4km")

# CREATE SITE LIST
bogus_basin <- c(-116.1478, 43.73065)
owyhee_high <- c(-116.814425, 43.180802)
three_islands <- c(-115.32293, 42.943841)
snake_river <- c(-116.344, 43.23641)
hulls_gulch <- c(-116.194131, 43.641545)
hawkins_reserve <- c(-116.1682, 43.701673)
castle_rocks <- c(-113.666326, 42.127255)
albion_high <- c(-113.56689, 42.3344)
nancy_gulch <- c(-116.71387, 43.17102)
reynolds_creek <- c(-116.737, 43.22752)

site_coords <- list(bogus_basin, owyhee_high, three_islands, snake_river, hulls_gulch, hawkins_reserve, castle_rocks, albion_high, nancy_gulch, reynolds_creek)

# SUBSET DATA SO THAT YOU CAN SLICE IT BY SITE LOCATION
temp_min <- prism_archive_subset("tmin", "monthly", mon = c(11, 12, 1, 2, 3, 4), resolution = "4km")
temp_max <- prism_archive_subset("tmax", "monthly", mon = c(11, 12, 1, 2, 3, 4), resolution = "4km")
temp_mean <- prism_archive_subset("tmean", "monthly", mon = c(11, 12, 1, 2, 3, 4), resolution = "4km")

# CLEAN DATA
# Write function for slicing data by site location
prism_by_site <- function(subset_type, loc_list){
  subset_type %>%
    pd_plot_slice(., loc_list)
}

# map the site list through the user defined function
# Minimum temperature
site_mins <- map(site_coords, 
                 ~prism_by_site(subset_type = temp_min, loc_list = .)
)

# Mean temperature
site_means <- map(site_coords, 
                  ~prism_by_site(subset_type = temp_mean, loc_list = .)
)

# Maximum temperature
site_maxs <- map(site_coords, 
                 ~prism_by_site(subset_type = temp_max, loc_list = .)
)


# extract data from ggplot list
# mins
temp_data_mins <- purrr::imap_dfr(
  site_mins,
  ~ .$data %>%
    mutate(source_id = .y)
)

# means
temp_data_means <- purrr::imap_dfr(
  site_means,
  ~ .$data %>%
    mutate(source_id = .y)
)

# maxs
temp_data_maxs <- purrr::imap_dfr(
  site_maxs,
  ~ .$data %>%
    mutate(source_id = .y)
)

#do your checks
dim(temp_data_mins)
dim(temp_data_means)
dim(temp_data_maxs)

head(temp_data_mins)
head(temp_data_means)
head(temp_data_maxs)

# COMBINE MINIMUM, MEAN, AND MAXIMUM TEMPERATURE DATA
temp_data <- temp_data_mins %>%
  rename(tmin = data) %>%
  left_join(
    temp_data_means %>% rename(tmean = data),
    by = c("date", "source_id")
  ) %>%
  left_join(
    temp_data_maxs %>% rename(tmax = data),
    by = c("date", "source_id")
  )

dim(temp_data)
head(temp_data)


# ADD YEAR AND MONTH
temp_data <- temp_data %>%
  mutate(
    year = lubridate::year(date),
    month = lubridate::month(date)
  )
head(temp_data)

# HISTORICAL MONTHLY TEMPERATURE SUMMARY
monthly_temp_summary <- temp_data %>%
  group_by(source_id, month) %>%
  summarise(
    mean_tmin = mean(tmin, na.rm = TRUE),
    mean_tmean = mean(tmean, na.rm = TRUE),
    mean_tmax = mean(tmax, na.rm = TRUE),
    .groups = "drop"
  )

View(monthly_temp_summary)
dim(monthly_temp_summary)
head(monthly_temp_summary)

# ORDER MONTHS FROM NOVEMBER TO APRIL AND ADD SITE NAMES
monthly_temp_summary <- monthly_temp_summary %>%
  mutate(
    month_name = factor(
      month,
      levels = c(11, 12, 1, 2, 3, 4),
      labels = c("Nov", "Dec", "Jan", "Feb", "Mar", "Apr")
    ),
    site = factor(
      source_id,
      levels = 1:10,
      labels = c(
        "Bogus Basin",
        "Owyhee High",
        "Three Islands",
        "Snake River",
        "Hulls Gulch",
        "Hawkins Reserve",
        "Castle Rocks",
        "Albion High",
        "Nancy Gulch",
        "Reynolds Creek"
      )
    )
  )

head(monthly_temp_summary)

# PLOT THE GRAPH
ggplot(
  monthly_temp_summary,
  aes(
    x = month_name,
    y = mean_tmean,
    group = site,
    color = site
  )
) +
  geom_line() +
  geom_point() +
  labs(
    x = "Month",
    y = "Historical mean temperature (°C)",
    color = "Site"
  ) +
  theme_classic()

# HISTORICAL TEMPERATURE RANGE FOR TMIN, TMEAN, AND TMAX
temperature_ranges <- temp_data %>%
  summarise(
    tmin_min = min(tmin, na.rm = TRUE),
    tmin_q05 = quantile(tmin, 0.05, na.rm = TRUE),
    tmin_median = median(tmin, na.rm = TRUE),
    
    tmean_min = min(tmean, na.rm = TRUE),
    tmean_median = median(tmean, na.rm = TRUE),
    tmean_max = max(tmean, na.rm = TRUE),
    
    tmax_median = median(tmax, na.rm = TRUE),
    tmax_q95 = quantile(tmax, 0.95, na.rm = TRUE),
    tmax_max = max(tmax, na.rm = TRUE)
  )

temperature_ranges

# RESHAPE TEMPERATURE DATA FOR PLOTTING
temp_long <- temp_data %>%
  select(source_id, date, tmin, tmean, tmax) %>%
  pivot_longer(
    cols = c(tmin, tmean, tmax),
    names_to = "temperature_type",
    values_to = "temperature"
  )

# CANDIDATE TEMPERATURE TREATMENTS
treatments <- c(0, 5, 10, 15, 20)

# PLOT HISTORICAL TEMPERATURE DISTRIBUTIONS
ggplot(temp_long, aes(x = temperature)) +
  geom_histogram(
    binwidth = 1,
    color = "black"
  ) +
  geom_vline(
    xintercept = treatments,
    linetype = "dashed"
  ) +
  facet_wrap(
    ~temperature_type,
    ncol = 1,
    scales = "free_y"
  ) +
  scale_x_continuous(
    breaks = seq(-15, 25, 5)
  ) +
  labs(
    title = "Historical temperatures across 10 sagebrush sites",
    subtitle = "Monthly PRISM temperatures, November–April, 1995–2025",
    x = "Temperature (°C)",
    y = "Number of observations"
  ) +
  theme_classic()


 