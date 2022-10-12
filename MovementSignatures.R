library(tidyverse)
library(vultureUtils) # package developed by Kaija to make network analyses easier. github.com/kaijagahm/vultureUtils. Use devtools::install_github("kaijagahm/vultureUtils") to install.
library(feather) # for faster writing/reading of data
library(sf) # for shapefiles and spatial analysis

# LOAD THE SHAPEFILES
roostPolygons <- sf::st_read("data/AllRoostPolygons.kml")
mask <- sf::st_read("data/CutOffRegion.kml")

# LOG INTO MOVEBANK
# Movebank login information
load("movebankCredentials/pw.Rda")
MB.LoginObject <- movebankLogin(username = 'kaijagahm', password = pw)

# GET DATA: DECEMBER 2020 TO JUNE 2021
breeding2020_2021 <- vultureUtils::downloadVultures(loginObject = MB.LoginObject, dateTimeStartUTC = "2020-12-01 00:00:00", dateTimeEndUTC = "2021-06-30 11:59:00", quiet = T)
feather::write_feather(breeding2020_2021, "data/breeding2020_2021.feather")
breeding2020_2021 <- feather::read_feather("data/breeding2020_2021.feather")

# For simplicity, I'm going to refer to the dataset "breeding2020_2021" from now on as "dat" unless we start working with something more complicated.
dat <- breeding2020_2021

# CLEAN THE DATA
dat <- vultureUtils::cleanData(dataset = dat, mask = mask, inMaskThreshold = 0.33, crs = "WGS84")
dim(dat)

# GET JUST FLIGHT DATA
# Define flight as speed > 5m/s
dat <- dat %>%
  mutate(flight = case_when(ground_speed > 5 ~ TRUE,
                            TRUE ~ FALSE))

# CALCULATE MOVEMENT METRICS
# Max displacement on each day
dailyFlightDisplacements <- dat %>%
  filter(flight == TRUE) %>%
  group_by(trackId, dateOnly) %>%
  summarize(
    first = geometry[1],
    displacement = st_distance(geometry, first, by_element = T),
  )
maxDailyFlightDisplacement <- dailyFlightDisplacements %>%
  sf::st_drop_geometry() %>%
  group_by(trackId, dateOnly) %>%
  summarize(maxDisplacement_m = max(as.numeric(displacement)))

# Total flight duration on each day
dailyFlightDuration <- dat %>%
  sf::st_drop_geometry() %>%
  mutate(timediff = timestamp - lag(timestamp)) %>%
  filter(flight == TRUE) %>%
  group_by(trackId, dateOnly) %>%
  summarize(dailyFlightDuration_secs = as.numeric(sum(timediff))) %>%
  mutate(dailyFlightDuration_mins = dailyFlightDuration_secs/60) %>%
  mutate(dailyFlightDuration_hours = dailyFlightDuration_mins/60)

# Combine movement metrics
movementMetrics_breeding2020_2021 <- left_join(maxDailyFlightDisplacement, dailyFlightDuration) %>%
  dplyr::select(trackId, dateOnly, maxDisplacement_m, dailyFlightDuration_hours)

# SAVE THE DATA
write_feather(movementMetrics_breeding2020_2021, "data/movementMetrics_breeding2020_2021.feather")