# description -------------------------------------------------------------

# setup -------------------------------------------------------------------

# R packages on CRAN

library(readr)
library(dplyr)
library(forcats)
library(lubridate)
library(ggplot2)
library(janitor)
library(tidyr)
library(leaflet)
library(mapview)

# R packages not on CRAN

# install.packages("devtools")
# devtools::install_github("hrbrmstr/waffle")
library(waffle)

# read data ---------------------------------------------------------------

# devtools::install_github("Global-Health-Engineering/qechairquality")
library(qechairquality)

# data preparation --------------------------------------------------------

## prepare a vector of names for sensor locations

location_vec <- c("Ward 6B",
                  "Guardian Shelter",
                  "Lighthouse Clinic",
                  "Lions Sight",
                  "Malaria Project",
                  "Mercy James",
                  "uMoyo")

## read data from package and manipulate variables

dat_in <- qechairquality |>
  # move to data cleaning process
  mutate(date = as_date(date_time)) |>
  mutate(hour = hour(date_time)) |>
  # remove values above 1000, as they could not plausibly be this high
  filter(value <= 1000) |>
  # not all sensors were installed on the same day or stopped
  # collecting data on the same day.
  # define a daterange for entire dataset
  filter(date_time >= "2019-10-05 00:00:00",
         date_time <= "2019-11-25 00:00:00") |>
  ## rename locations to meaningful names
  mutate(location = case_when(
    location == "6B" ~ location_vec[[1]],
    location == "guardian" ~ location_vec[[2]],
    location == "Lhouse" ~ location_vec[[3]],
    location == "Lions" ~ location_vec[[4]],
    location == "MJC" ~ location_vec[[6]],
    location == "Moyo" ~ location_vec[[7]],
    TRUE ~ location
  ))

# Prepare hospital location vector

vec_hospital_name <- dat_in |>
  count(location) |>
  pull(location)

# define US EPA levels for air quality
usepa_level_fct <- c("Good",
                     "Moderate",
                     "Unhealthy for Sensitive Groups",
                     "Unhealthy",
                     "Very Unhealthy",
                     "Hazardous")

# define WHO 2021 levels
who_2021_level_fct <- c("Interim target 4",
                        "Interim target 3",
                        "Interim target 2",
                        "Interim target 1",
                        "Greater interim target 1")

# Data transformation
dat_air_quality <- dat_in |>

  mutate(air_quality = case_when(
    indicator == "pm2.5" & value <= 12 ~ "Good",
    indicator == "pm2.5" & value > 12 & indicator == "pm2.5" & value <= 35.4 ~ "Moderate",
    indicator == "pm2.5" & value > 35.4 & indicator == "pm2.5" & value <= 55.4 ~ "Unhealthy for Sensitive Groups",
    indicator == "pm2.5" & value > 55.4 & indicator == "pm2.5" & value <= 150.4 ~ "Unhealthy",
    indicator == "pm2.5" & value > 150.4 & indicator == "pm2.5" & value <= 250.4 ~ "Very Unhealthy",
    indicator == "pm2.5" & value > 250.4 ~ "Hazardous",
    indicator == "pm10" & value <= 54.9 ~ "Good",
    indicator == "pm10" & value > 54.9 & indicator == "pm10" & value <= 154.9 ~ "Moderate",
    indicator == "pm10" & value > 154.9 & indicator == "pm10" & value <= 254.9 ~ "Unhealthy for Sensitive Groups",
    indicator == "pm10" & value > 254.9 & indicator == "pm10" & value <= 354.9 ~ "Unhealthy",
    indicator == "pm10" & value > 354.9 & indicator == "pm10" & value <= 424.9 ~ "Very Unhealthy",
    indicator == "pm10" & value > 424.9 ~ "Hazardous",
  )) |>
  mutate(air_quality =
           fct_relevel(air_quality,
                       usepa_level_fct)) |>
  mutate(air_quality_who_annual = case_when(
    indicator == "pm2.5" & value <= 10 ~ "Interim target 4",
    indicator == "pm2.5" & value > 10 & indicator == "pm2.5" & value <= 15 ~ "Interim target 3",
    indicator == "pm2.5" & value > 15 & indicator == "pm2.5" & value <= 25 ~ "Interim target 2",
    indicator == "pm2.5" & value > 25 & indicator == "pm2.5" & value <= 35 ~ "Interim target 1",
    indicator == "pm2.5" & value > 35 ~ "Greater interim target 1",
    indicator == "pm10" & value <= 20 ~ "Interim target 4",
    indicator == "pm10" & value > 20 & indicator == "pm10" & value <= 30 ~ "Interim target 3",
    indicator == "pm10" & value > 30 & indicator == "pm10" & value <= 50 ~ "Interim target 2",
    indicator == "pm10" & value > 50 & indicator == "pm10" & value <= 70 ~ "Interim target 1",
    indicator == "pm10" & value > 70 ~ "Greater interim target 1",
  )) |>
  mutate(air_quality_who_annual  =
           fct_relevel(air_quality_who_annual,
                       who_2021_level_fct))

# Data transformation - summary statistics

dat_in_sum_day <- dat_in |>
  group_by(date, location, indicator) |>
  summarise(min = min(value),
            median = median(value),
            mean = mean(value),
            sd = sd(value),
            max = max(value))

dat_in_sum_day_night <- dat_in |>
  mutate(daytime = case_when(
    hour >= 6 & hour < 18 ~ "day_time",
    TRUE ~ "night_time"
    #hour >= 18 | hour < 6 ~ "night_time"
  )) |>
  group_by(date, location, daytime, indicator) |>
  summarise(min = min(value),
            median = median(value),
            mean = mean(value),
            sd = sd(value),
            max = max(value))

dat_in_sum_hour <- dat_in |>
  group_by(date, hour, location, indicator) |>
  summarise(min = min(value),
            median = median(value),
            mean = mean(value),
            sd = sd(value),
            max = max(value))


# Data transformation - air quality summary US EPA

dat_air_quality_sum <- dat_air_quality |>
  group_by(location, indicator, air_quality) |>
  count(air_quality) |>
  mutate(exposure_mins = n * 5) |>
  mutate(exposure_hrs = round(exposure_mins / 60)) |>
  select(-n)

# Data transformation - air quality WHO 2021

dat_air_quality_sum_who_2021 <- dat_air_quality |>
  group_by(location, indicator, air_quality_who_annual) |>
  count(air_quality_who_annual) |>
  mutate(exposure_mins = n * 5) |>
  mutate(exposure_hrs = round(exposure_mins / 60)) |>
  select(-n)

# data export -------------------------------------------------------------


