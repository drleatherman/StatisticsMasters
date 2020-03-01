require(tidyverse)

# Numeric fields are read in as strings and empty string ("") don't cast to numerics. 
temps2010.raw <- read.csv("~/Downloads/2055484.csv", na.strings = "", stringsAsFactors = FALSE, strip.white = TRUE, header = TRUE, sep = ",")
temps2000.raw <- read.csv("~/Downloads/2055486.csv", na.strings = "", stringsAsFactors = FALSE, strip.white = TRUE, header = TRUE, sep = ",")
temps1990.raw <- read.csv("~/Downloads/2055488.csv", na.strings = "", stringsAsFactors = FALSE, strip.white = TRUE, header = TRUE, sep = ",")

temps2010 <- 
  temps2010.raw %>%
  as_tibble() %>% 
  filter(trimws(REPORT_TYPE) == "SOD") %>% 
  mutate(dateYYYYMMDD = as.Date(DATE)) %>%
  # Remove fields that cannot be calculated for 1990-2000
  select(-c(DailyWeather, DailyDepartureFromNormalAverageTemperature, DailyHeatingDegreeDays, DailyCoolingDegreeDays, DailySnowDepth, DailySnowfall, DailyPeakWindDirection, DailySustainedWindSpeed)) %>% 
  select(dateYYYYMMDD, starts_with("Daily"), Sunrise, Sunset) %>% 
  mutate_at("DailyPrecipitation", as.numeric) %>% 
  # these particular columns contained "s" in them so removing the s. i.e 46s. 
  mutate(
    num.DailyAverageDryBulbTemperature = str_replace(DailyAverageDryBulbTemperature, "s","") %>% as.numeric,
    num.DailyMaximumDryBulbTemperature = str_replace(DailyMaximumDryBulbTemperature, "s","") %>% as.numeric,
    num.DailyMinimumDryBulbTemperature = str_replace(DailyMinimumDryBulbTemperature, "s","") %>% as.numeric,
    num.DailyPeakWindSpeed = str_replace(DailyPeakWindSpeed, "s","") %>% as.numeric
  ) %>% 
  # Final cleanup for a homogenous looking dataset for the other years
  select(
    dateYYYYMMDD, 
    starts_with("Daily"), 
    Sunrise, 
    Sunset, 
    -DailyAverageDryBulbTemperature,
    -DailyMinimumDryBulbTemperature,
    -DailyMaximumDryBulbTemperature,
    -DailyPeakWindSpeed,
    DailyAverageDryBulbTemperature = num.DailyAverageDryBulbTemperature,
    DailyMaximumDryBulbTemperature = num.DailyMaximumDryBulbTemperature,
    DailyMinimumDryBulbTemperature = num.DailyMinimumDryBulbTemperature,
    DailyPeakWindSpeed = num.DailyPeakWindSpeed
  ) 


temps2000 <-
  temps2000.raw %>%
  as_tibble() %>% 
  filter(trimws(REPORT_TYPE) == "SOD") %>% 
  mutate(dateYYYYMMDD = as.Date(DATE)) %>%
  # Remove fields that cannot be calculated for 1990
  select(-c(DailyWeather, DailyDepartureFromNormalAverageTemperature, DailyHeatingDegreeDays, DailyCoolingDegreeDays, DailySnowDepth, DailySnowfall, DailyPeakWindDirection, DailySustainedWindSpeed)) %>% 
  mutate_at("DailyPrecipitation", as.numeric) %>% 
  # these particular columns contained "s" in them so removing the s. i.e 46s. 
  mutate(
    num.DailyAverageDryBulbTemperature = str_replace(DailyAverageDryBulbTemperature, "s","") %>% as.numeric,
    num.DailyMaximumDryBulbTemperature = str_replace(DailyMaximumDryBulbTemperature, "s","") %>% as.numeric,
    num.DailyMinimumDryBulbTemperature = str_replace(DailyMinimumDryBulbTemperature, "s","") %>% as.numeric,
    num.DailyPeakWindSpeed = str_replace(DailyPeakWindSpeed, "s","") %>% as.numeric
  ) %>% 
  # Final cleanup for a homogenous looking dataset for the other years
  select(
    dateYYYYMMDD, 
    starts_with("Daily"), 
    Sunrise, 
    Sunset, 
    -DailyAverageDryBulbTemperature,
    -DailyMinimumDryBulbTemperature,
    -DailyMaximumDryBulbTemperature,
    -DailyPeakWindSpeed,
    DailyAverageDryBulbTemperature = num.DailyAverageDryBulbTemperature,
    DailyMaximumDryBulbTemperature = num.DailyMaximumDryBulbTemperature,
    DailyMinimumDryBulbTemperature = num.DailyMinimumDryBulbTemperature,
    DailyPeakWindSpeed = num.DailyPeakWindSpeed
  ) 


# Some dates in 1990 used a different reporting structure which didnt contain Daily values
# The daily values are computed by aggregating over each day and applying transfomrs
temps1990 <-
  temps1990.raw %>% 
  mutate_all(list(~na_if(., ""))) %>% 
  as_tibble() %>% 
  mutate(dateYYYYMMDD = as.Date(DATE)) %>%
  select(-c(HourlyPresentWeatherType, HourlySkyConditions, HourlyWindGustSpeed, HourlyWindDirection, HourlyPressureChange, HourlyPressureTendency)) %>% 
  mutate_at(vars(starts_with("Hourly")), as.numeric) %>% 
  group_by(dateYYYYMMDD) %>% 
  summarise_at(
    vars(starts_with("Hourly")), 
    # don't use null datapoints when calculating these values
    funs(mean(., na.rm = TRUE), max(., na.rm = TRUE), min(., na.rm = TRUE))) %>% 
  ungroup()

# Replace "Hourly" with "Daily" since the aggregation is at the day level
colnames(temps1990) <- gsub("^Hourly","Daily",colnames(temps1990))

# Final cleanup for a homogenous looking dataset for the other years
temps1990 <-
  temps1990 %>% 
  select(
    dateYYYYMMDD,
    DailyAverageDewPointTemperature = DailyDewPointTemperature_mean,
    DailyAverageDryBulbTemperature = DailyDryBulbTemperature_mean,
    DailyAverageRelativeHumidity = DailyRelativeHumidity_mean,
    DailyAverageSeaLevelPressure = DailySeaLevelPressure_mean,
    DailyAverageStationPressure = DailyStationPressure_mean,
    DailyAverageWetBulbTemperature = DailyWetBulbTemperature_mean,
    DailyAverageWindSpeed = DailyWindSpeed_mean,
    DailyMaximumDryBulbTemperature = DailyDryBulbTemperature_max,
    DailyMinimumDryBulbTemperature = DailyDryBulbTemperature_min,
    DailyPeakWindSpeed = DailyWindSpeed_max,
    DailyPrecipitation = DailyPrecipitation_mean
  )

# union all the datasets to make a master dataset
final_dataset <- temps1990 %>% bind_rows(temps2000, temps2010)