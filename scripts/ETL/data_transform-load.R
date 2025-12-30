# packages
library(tidyverse)
library(zoo)


# loading data
ace_data_raw <- read_csv("data/aemet_ace_data_raw.csv", locale = locale(decimal_mark = ","))


# glimpse at the data
glimpse(ace_data_raw)


# first 6 rows
head(ace_data_raw)


# last 6 rows
tail(ace_data_raw)


# missing data
ace_data_raw %>%
  summarise_all(~sum(is.na(.))) %>%
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "missing_values") %>%
  arrange(desc(missing_values)) %>%
  print(n = nrow(.))


# data wrangling
ace_data <- ace_data_raw %>%
  # renaming columns (to English)
  rename(date = fecha, station_code = indicativo, name = nombre, province = provincia,
         altitude = altitud, tmean = tmed, time_tmin = horatmin, time_tmax = horatmax,
         wind_dir = dir, wind_mean_speed = velmedia, wind_gust = racha, time_gust = horaracha,
         sun_hours = sol, rh_mean = hrMedia, pres_max = presMax, time_pres_max = horaPresMax,
         pres_min = presMin, time_pres_min = horaPresMin, rh_max = hrMax, time_rh_max = horaHrMax,
         rh_min = hrMin, time_rh_min = horaHrMin) %>%
  # wrangling / creating variables
  mutate(name = str_to_sentence(name),
         prec = if_else(prec == "Ip",
                        "0,0",
                        prec),
         prec = as.numeric(str_replace_all(prec, ",", ".")),
         day = day(date),
         month = month(date),
         year = year(date),
         day_month = make_date(month = month, day = day, year = 2020)) %>%
  # filling missing values in the columns tmean, tmax, tmmin and prec by interpolation
  mutate_at(vars(tmean, tmax, tmin, prec), ~na.approx(.)) %>%
  # selecting the variables I need for the analysis
  select(date, station_code, name, tmean, prec, tmin, tmax, day_month, day, month, year)


# checking the first few rows of the tidy data
head(ace_data)


# checking the last few rows of the tidy data
tail(ace_data)


# glimpse
glimpse(ace_data)


# checking missing values
ace_data %>%
  summarise_all(~sum(is.na(.)))


# saving tidy data
write_csv(ace_data, "data/aemet_ace_data_clean.csv")





























