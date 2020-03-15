require(tidyverse)

# Numeric fields are read in as strings and empty string ("") don't cast to numerics. 
temps2010.raw <- read.csv("~/Downloads/2055484.csv", na.strings = "", stringsAsFactors = FALSE, strip.white = TRUE, header = TRUE, sep = ",")
temps2000.raw <- read.csv("~/Downloads/2055486.csv", na.strings = "", stringsAsFactors = FALSE, strip.white = TRUE, header = TRUE, sep = ",")
temps1990.raw <- read.csv("~/Downloads/2055488.csv", na.strings = "", stringsAsFactors = FALSE, strip.white = TRUE, header = TRUE, sep = ",")

#########################################################################################
####################                                       ##############################
#########################################################################################

temps2010.weekly <-
  temps2010.raw %>%
  as_tibble() %>% 
  filter(trimws(REPORT_TYPE) == "SOD") %>% 
  mutate(dateYYYYMMDD = as.Date(DATE)) %>%
  # Remove fields that cannot be calculated for 1990
  select(-c(DailyWeather, DailyDepartureFromNormalAverageTemperature, DailyHeatingDegreeDays, DailyCoolingDegreeDays, DailySnowDepth, DailySnowfall, DailyPeakWindDirection, DailySustainedWindSpeed)) %>% 
  mutate_at("DailyPrecipitation", as.numeric) %>% 
  # these particular columns contained "s" in them so removing the s. i.e 46s. 
  mutate(
    DailyAverageDryBulbTemperature = str_replace(DailyAverageDryBulbTemperature, "s","") %>% as.numeric,
    DailyMaximumDryBulbTemperature = str_replace(DailyMaximumDryBulbTemperature, "s","") %>% as.numeric,
    DailyMinimumDryBulbTemperature = str_replace(DailyMinimumDryBulbTemperature, "s","") %>% as.numeric,
    DailyPeakWindSpeed = str_replace(DailyPeakWindSpeed, "s","") %>% as.numeric
  ) %>% 
  # convert to timetibble
  as_tbl_time(., index = dateYYYYMMDD) %>% 
  # roll up hourly to weekly
  collapse_by("weekly") %>% 
  group_by(dateYYYYMMDD) %>% 
  summarise_at(
    vars(starts_with("Daily")),  
    funs(mean(., na.rm = TRUE), max(., na.rm = TRUE), min(., na.rm = TRUE))) %>% 
  ungroup

colnames(temps2010.weekly) <- gsub("^Daily","Weekly", colnames(temps2010.weekly))

# Final cleanup for a homogenous looking dataset for the other years
temps2010.weekly.view <-
  temps2010.weekly %>% 
  select(
    dateYYYYMMDD,
    WeeklyAverageDewPointTemperature = WeeklyAverageDewPointTemperature_mean,
    WeeklyAverageDryBulbTemperature = WeeklyAverageDryBulbTemperature_mean,
    WeeklyAverageRelativeHumidity = WeeklyAverageRelativeHumidity_mean,
    WeeklyAverageSeaLevelPressure = WeeklyAverageSeaLevelPressure_mean,
    WeeklyAverageStationPressure = WeeklyAverageStationPressure_mean,
    WeeklyAverageWetBulbTemperature = WeeklyAverageWetBulbTemperature_mean,
    WeeklyAverageWindSpeed = WeeklyAverageWindSpeed_mean,
    WeeklyMaximumDryBulbTemperature = WeeklyMaximumDryBulbTemperature_max,
    WeeklyMinimumDryBulbTemperature = WeeklyMinimumDryBulbTemperature_min,
    WeeklyPeakWindSpeed = WeeklyPeakWindSpeed_max,
    WeeklyPrecipitation = WeeklyPrecipitation_mean
  ) 

temps2010.weekly.raw <-
  temps2010.raw %>% 
  # filter out "SOD" or any other measures
  filter(trimws(REPORT_TYPE) %in% c("FM-15")) %>%
  mutate_all(list(~na_if(., ""))) %>% 
  as_tibble() %>% 
  mutate(dateYYYYMMDD = as.Date(DATE)) %>%
  select(-c(HourlyPresentWeatherType, HourlySkyConditions, HourlyWindGustSpeed, HourlyWindDirection, HourlyPressureChange, HourlyPressureTendency)) %>% 
  mutate_at(vars(starts_with("Hourly")), as.numeric) %>% 
  # convert to timetibble
  as_tbl_time(., index = dateYYYYMMDD) %>% 
  # roll up hourly to weekly
  collapse_by("weekly") %>% 
  group_by(dateYYYYMMDD) %>% 
  summarise_at(
    vars(starts_with("Hourly")),  
    funs(mean(., na.rm = TRUE), max(., na.rm = TRUE), min(., na.rm = TRUE))) %>%
  ungroup

colnames(temps2010.weekly.raw) <- gsub("^Hourly","Weekly", colnames(temps2010.weekly.raw))


# Final cleanup for a homogenous looking dataset for the other years
temps2010.weekly.raw.view <-
  temps2010.weekly.raw %>% 
  select(
    dateYYYYMMDD,
    WeeklyAverageDewPointTemperature = WeeklyDewPointTemperature_mean,
    WeeklyAverageDryBulbTemperature = WeeklyDryBulbTemperature_mean,
    WeeklyAverageRelativeHumidity = WeeklyRelativeHumidity_mean,
    WeeklyAverageSeaLevelPressure = WeeklySeaLevelPressure_mean,
    WeeklyAverageStationPressure = WeeklyStationPressure_mean,
    WeeklyAverageWetBulbTemperature = WeeklyWetBulbTemperature_mean,
    WeeklyAverageWindSpeed = WeeklyWindSpeed_mean,
    WeeklyMaximumDryBulbTemperature = WeeklyDryBulbTemperature_max,
    WeeklyMinimumDryBulbTemperature = WeeklyDryBulbTemperature_min,
    WeeklyPeakWindSpeed = WeeklyWindSpeed_max,
    WeeklyPrecipitation = WeeklyPrecipitation_mean
  ) 



# Final cleanup for a homogenous looking dataset for the other years
temps2010.weekly.view[is.nan.data.frame(temps2010.weekly.view)] <- NA
temps2010.weekly.raw.view[is.nan.data.frame(temps2010.weekly.raw.view)] <- NA

temps2010.final <-
  temps2010.weekly.view %>% 
  left_join(temps2010.weekly.raw.view, by = "dateYYYYMMDD") %>%
  mutate(
    WeeklyAverageDewPointTemperature = coalesce(WeeklyAverageDewPointTemperature.x, WeeklyAverageDewPointTemperature.y),
    WeeklyAverageDryBulbTemperature = coalesce(WeeklyAverageDryBulbTemperature.x, WeeklyAverageDryBulbTemperature.y),
    WeeklyAverageRelativeHumidity = coalesce(WeeklyAverageRelativeHumidity.x, WeeklyAverageRelativeHumidity.y),
    WeeklyAverageSeaLevelPressure = coalesce(WeeklyAverageSeaLevelPressure.x, WeeklyAverageSeaLevelPressure.y),
    WeeklyAverageStationPressure = coalesce(WeeklyAverageStationPressure.x, WeeklyAverageStationPressure.y),
    WeeklyAverageWetBulbTemperature = coalesce(WeeklyAverageWetBulbTemperature.x, WeeklyAverageWetBulbTemperature.y),
    WeeklyAverageWindSpeed = coalesce(WeeklyAverageWindSpeed.x, WeeklyAverageWindSpeed.y),
    WeeklyMaximumDryBulbTemperature = coalesce(WeeklyMaximumDryBulbTemperature.x, WeeklyMaximumDryBulbTemperature.y),
    WeeklyMinimumDryBulbTemperature = coalesce(WeeklyMinimumDryBulbTemperature.x, WeeklyMinimumDryBulbTemperature.y),
    WeeklyPeakWindSpeed = coalesce(WeeklyPeakWindSpeed.x, WeeklyPeakWindSpeed.y),
    WeeklyPrecipitation = coalesce(WeeklyPrecipitation.x, WeeklyPrecipitation.y)
  ) %>% 
  select(
    dateYYYYMMDD,
    WeeklyAverageDewPointTemperature,
    WeeklyAverageDryBulbTemperature,
    WeeklyAverageRelativeHumidity,
    WeeklyAverageSeaLevelPressure,
    WeeklyAverageStationPressure,
    WeeklyAverageWetBulbTemperature,
    WeeklyAverageWindSpeed,
    WeeklyMaximumDryBulbTemperature,
    WeeklyMinimumDryBulbTemperature,
    WeeklyPeakWindSpeed,
    WeeklyPrecipitation
  )
#########################################################################################
####################                                       ##############################
#########################################################################################

temps2000.weekly <-
  temps2000.raw %>%
  as_tibble() %>% 
  filter(trimws(REPORT_TYPE) == "SOD") %>% 
  mutate(dateYYYYMMDD = as.Date(DATE)) %>%
  # Remove fields that cannot be calculated for 1990
  select(-c(DailyWeather, DailyDepartureFromNormalAverageTemperature, DailyHeatingDegreeDays, DailyCoolingDegreeDays, DailySnowDepth, DailySnowfall, DailyPeakWindDirection, DailySustainedWindSpeed)) %>% 
  mutate_at("DailyPrecipitation", as.numeric) %>% 
  # these particular columns contained "s" in them so removing the s. i.e 46s. 
  mutate(
    DailyAverageDryBulbTemperature = str_replace(DailyAverageDryBulbTemperature, "s","") %>% as.numeric,
    DailyMaximumDryBulbTemperature = str_replace(DailyMaximumDryBulbTemperature, "s","") %>% as.numeric,
    DailyMinimumDryBulbTemperature = str_replace(DailyMinimumDryBulbTemperature, "s","") %>% as.numeric,
    DailyPeakWindSpeed = str_replace(DailyPeakWindSpeed, "s","") %>% as.numeric
  ) %>% 
  # convert to timetibble
  as_tbl_time(., index = dateYYYYMMDD) %>% 
  # roll up hourly to weekly
  collapse_by("weekly") %>% 
  group_by(dateYYYYMMDD) %>% 
  summarise_at(
    vars(starts_with("Daily")),  
    funs(mean(., na.rm = TRUE), max(., na.rm = TRUE), min(., na.rm = TRUE))) %>% 
  ungroup

colnames(temps2000.weekly) <- gsub("^Daily","Weekly", colnames(temps2000.weekly))

# Final cleanup for a homogenous looking dataset for the other years
temps2000.weekly.view <-
  temps2000.weekly %>% 
  select(
    dateYYYYMMDD,
    WeeklyAverageDewPointTemperature = WeeklyAverageDewPointTemperature_mean,
    WeeklyAverageDryBulbTemperature = WeeklyAverageDryBulbTemperature_mean,
    WeeklyAverageRelativeHumidity = WeeklyAverageRelativeHumidity_mean,
    WeeklyAverageSeaLevelPressure = WeeklyAverageSeaLevelPressure_mean,
    WeeklyAverageStationPressure = WeeklyAverageStationPressure_mean,
    WeeklyAverageWetBulbTemperature = WeeklyAverageWetBulbTemperature_mean,
    WeeklyAverageWindSpeed = WeeklyAverageWindSpeed_mean,
    WeeklyMaximumDryBulbTemperature = WeeklyMaximumDryBulbTemperature_max,
    WeeklyMinimumDryBulbTemperature = WeeklyMinimumDryBulbTemperature_min,
    WeeklyPeakWindSpeed = WeeklyPeakWindSpeed_max,
    WeeklyPrecipitation = WeeklyPrecipitation_mean
  ) 

temps2000.weekly.raw <-
  temps2000.raw %>% 
  # filter out "SOD" or any other measures
  filter(trimws(REPORT_TYPE) %in% c("FM-15")) %>%
  mutate_all(list(~na_if(., ""))) %>% 
  as_tibble() %>% 
  mutate(dateYYYYMMDD = as.Date(DATE)) %>%
  select(-c(HourlyPresentWeatherType, HourlySkyConditions, HourlyWindGustSpeed, HourlyWindDirection, HourlyPressureChange, HourlyPressureTendency)) %>% 
  mutate_at(vars(starts_with("Hourly")), as.numeric) %>% 
  # convert to timetibble
  as_tbl_time(., index = dateYYYYMMDD) %>% 
  # roll up hourly to weekly
  collapse_by("weekly") %>% 
  group_by(dateYYYYMMDD) %>% 
  summarise_at(
    vars(starts_with("Hourly")),  
    funs(mean(., na.rm = TRUE), max(., na.rm = TRUE), min(., na.rm = TRUE))) %>%
  ungroup

colnames(temps2000.weekly.raw) <- gsub("^Hourly","Weekly", colnames(temps2000.weekly.raw))


# Final cleanup for a homogenous looking dataset for the other years
temps2000.weekly.raw.view <-
  temps2000.weekly.raw %>% 
  select(
    dateYYYYMMDD,
    WeeklyAverageDewPointTemperature = WeeklyDewPointTemperature_mean,
    WeeklyAverageDryBulbTemperature = WeeklyDryBulbTemperature_mean,
    WeeklyAverageRelativeHumidity = WeeklyRelativeHumidity_mean,
    WeeklyAverageSeaLevelPressure = WeeklySeaLevelPressure_mean,
    WeeklyAverageStationPressure = WeeklyStationPressure_mean,
    WeeklyAverageWetBulbTemperature = WeeklyWetBulbTemperature_mean,
    WeeklyAverageWindSpeed = WeeklyWindSpeed_mean,
    WeeklyMaximumDryBulbTemperature = WeeklyDryBulbTemperature_max,
    WeeklyMinimumDryBulbTemperature = WeeklyDryBulbTemperature_min,
    WeeklyPeakWindSpeed = WeeklyWindSpeed_max,
    WeeklyPrecipitation = WeeklyPrecipitation_mean
  ) 



# Final cleanup for a homogenous looking dataset for the other years
temps2000.weekly.view[is.nan.data.frame(temps2000.weekly.view)] <- NA
temps2000.weekly.raw.view[is.nan.data.frame(temps2000.weekly.raw.view)] <- NA

temps2000.final <-
  temps2000.weekly.view %>% 
    left_join(temps2000.weekly.raw.view, by = "dateYYYYMMDD") %>%
    mutate(
      WeeklyAverageDewPointTemperature = coalesce(WeeklyAverageDewPointTemperature.x, WeeklyAverageDewPointTemperature.y),
      WeeklyAverageDryBulbTemperature = coalesce(WeeklyAverageDryBulbTemperature.x, WeeklyAverageDryBulbTemperature.y),
      WeeklyAverageRelativeHumidity = coalesce(WeeklyAverageRelativeHumidity.x, WeeklyAverageRelativeHumidity.y),
      WeeklyAverageSeaLevelPressure = coalesce(WeeklyAverageSeaLevelPressure.x, WeeklyAverageSeaLevelPressure.y),
      WeeklyAverageStationPressure = coalesce(WeeklyAverageStationPressure.x, WeeklyAverageStationPressure.y),
      WeeklyAverageWetBulbTemperature = coalesce(WeeklyAverageWetBulbTemperature.x, WeeklyAverageWetBulbTemperature.y),
      WeeklyAverageWindSpeed = coalesce(WeeklyAverageWindSpeed.x, WeeklyAverageWindSpeed.y),
      WeeklyMaximumDryBulbTemperature = coalesce(WeeklyMaximumDryBulbTemperature.x, WeeklyMaximumDryBulbTemperature.y),
      WeeklyMinimumDryBulbTemperature = coalesce(WeeklyMinimumDryBulbTemperature.x, WeeklyMinimumDryBulbTemperature.y),
      WeeklyPeakWindSpeed = coalesce(WeeklyPeakWindSpeed.x, WeeklyPeakWindSpeed.y),
      WeeklyPrecipitation = coalesce(WeeklyPrecipitation.x, WeeklyPrecipitation.y)
    ) %>% 
  select(
    dateYYYYMMDD,
    WeeklyAverageDewPointTemperature,
    WeeklyAverageDryBulbTemperature,
    WeeklyAverageRelativeHumidity,
    WeeklyAverageSeaLevelPressure,
    WeeklyAverageStationPressure,
    WeeklyAverageWetBulbTemperature,
    WeeklyAverageWindSpeed,
    WeeklyMaximumDryBulbTemperature,
    WeeklyMinimumDryBulbTemperature,
    WeeklyPeakWindSpeed,
    WeeklyPrecipitation
)
#########################################################################################
####################                                       ##############################
#########################################################################################

# Some dates in 1990 used a different reporting structure which didnt contain Daily values
# The daily values are computed by aggregating over each day and applying transfomrs
temps1990.weekly <-
  temps1990.raw %>% 
  # filter out "SOD" or any other measures
  filter(trimws(REPORT_TYPE) %in% c("FM-15", "SAO")) %>%
  mutate_all(list(~na_if(., ""))) %>% 
  as_tibble() %>% 
  mutate(dateYYYYMMDD = as.Date(DATE)) %>%
  select(-c(HourlyPresentWeatherType, HourlySkyConditions, HourlyWindGustSpeed, HourlyWindDirection, HourlyPressureChange, HourlyPressureTendency)) %>% 
  mutate_at(vars(starts_with("Hourly")), as.numeric) %>% 
  # convert to timetibble
  as_tbl_time(., index = dateYYYYMMDD) %>% 
  # roll up hourly to weekly
  collapse_by("weekly") %>% 
  group_by(dateYYYYMMDD) %>% 
  summarise_at(
    vars(starts_with("Hourly")),  
    funs(mean(., na.rm = TRUE), max(., na.rm = TRUE), min(., na.rm = TRUE))) %>% 
  ungroup()

# Replace "Hourly" with "Weekly" since the aggregation is at the week level
colnames(temps1990.weekly) <- gsub("^Hourly","Weekly", colnames(temps1990.weekly))

# Final cleanup for a homogenous looking dataset for the other years
temps1990 <-
  temps1990.weekly %>% 
  select(
    dateYYYYMMDD,
    WeeklyAverageDewPointTemperature = WeeklyDewPointTemperature_mean,
    WeeklyAverageDryBulbTemperature = WeeklyDryBulbTemperature_mean,
    WeeklyAverageRelativeHumidity = WeeklyRelativeHumidity_mean,
    WeeklyAverageSeaLevelPressure = WeeklySeaLevelPressure_mean,
    WeeklyAverageStationPressure = WeeklyStationPressure_mean,
    WeeklyAverageWetBulbTemperature = WeeklyWetBulbTemperature_mean,
    WeeklyAverageWindSpeed = WeeklyWindSpeed_mean,
    WeeklyMaximumDryBulbTemperature = WeeklyDryBulbTemperature_max,
    WeeklyMinimumDryBulbTemperature = WeeklyDryBulbTemperature_min,
    WeeklyPeakWindSpeed = WeeklyWindSpeed_max,
    WeeklyPrecipitation = WeeklyPrecipitation_mean
  )

# union all the datasets to make a master dataset
final_dataset <- temps1990 %>% bind_rows(temps2000.final, temps2010.final)
write.csv(final_dataset, file = "~/projects/StatisticsMasters/TimeSeries/chicago_weather2.csv")
