## ---- message = FALSE, warning = FALSE, eval = FALSE, echo = TRUE--------
#  library("ropenaq")
#  
#  countMeasures <- aq_measurements(city = "Delhi",
#                                       parameter = "pm25",
#                                       date_from = "2016-03-01",
#                                   date_to = "2016-03-31",
#                                       limit = 1)$meta$found
#  measurementsDelhi <- NULL
#  for (page in 1:ceiling(countMeasures/1000)){
#    measurementsDelhi <- rbind(measurementsDelhi,
#                               aq_measurements(city = "Delhi",
#                                       parameter = "pm25",
#                                       date_from = "2016-03-01",
#                                   date_to = "2016-03-31",
#                                       limit = 1000,
#                                       page = page)$results)
#  }
#  save(measurementsDelhi, file = "data/measurementsDelhi.RData")

## ---- message = FALSE, warning = FALSE, eval = TRUE, echo = TRUE---------
library("dplyr")
load("data/measurementsDelhi.RData")
measurementsDelhi %>% head() %>% knitr::kable()
measurementsDelhi <- filter(measurementsDelhi, value > 0)

## ---- message = FALSE, warning = FALSE-----------------------------------
# only keep stations with geographical information
measurementsDelhi <- filter(measurementsDelhi, !is.na(latitude))
# now transform to daily data
measurementsDelhi <- measurementsDelhi %>% 
  mutate(day = as.Date(dateLocal)) %>%
  group_by(location, day) %>%
  summarize(value = mean(value),
            longitude = longitude[1],
            latitude = latitude[1]) %>%
  ungroup()
measurementsDelhi %>% head() %>% knitr::kable()


## ---- eval = FALSE, echo = TRUE------------------------------------------
#  library("rnoaa")
#  station_data <- ghcnd_stations()[[1]]
#  lat_lon_df <- select(measurementsDelhi,
#                       location,
#                       latitude,
#                       longitude) %>% unique() %>%
#    ungroup() %>%
#    rename(id = location) %>%
#    mutate(id = factor(id))
#  
#  stationsDelhi <- meteo_nearby_stations(lat_lon_df = as.data.frame(lat_lon_df),
#                                         station_data = station_data,
#                                         radius = 15,
#                                         year_min = 2016,
#                                         var = c("TAVG", "PRCP"))
#  stationsDelhi <- unique(bind_rows(stationsDelhi) %>% select(- distance))
#  
#  save(stationsDelhi, file = "data/stationsDelhi.RData")
#  

## ------------------------------------------------------------------------
load("data/stationsDelhi.RData")
stationsDelhi %>% knitr::kable()

## ---- message = FALSE, warning = FALSE, fig.width = 10, fig.height = 10----
library("ggmap")
map <- get_map(location = "Delhi", zoom = 11)
ggmap(map) + 
  geom_point(aes(x = longitude, y = latitude),
             data = stationsDelhi, col = "blue", size = 4)+ 
  geom_point(aes(x = longitude, y = latitude),
             data = measurementsDelhi, col = "red", size = 4)

## ------------------------------------------------------------------------
library("rnoaa")
monitors <- stationsDelhi$id
all_monitors_clean <- meteo_pull_monitors(monitors,
                                      date_min = "2016-03-01",
                                     date_max = "2016-03-31") %>%
  rename(day = date,
         location = id)
all_monitors_clean %>% head() %>% knitr::kable()


## ------------------------------------------------------------------------
measurementsDelhi <- bind_rows(measurementsDelhi, all_monitors_clean)
measurementsDelhi %>% head() %>% knitr::kable()

## ---- fig.width = 10, fig.height = 5, warning = FALSE--------------------
data_plot <- measurementsDelhi %>% 
  rename(pm25 = value) %>%
  select(- longitude, - latitude, - tmax, - tmin) %>%
  tidyr::gather(parameter, value, pm25:prcp)

library("ggplot2")
ggplot(data_plot) +
  geom_line(aes(x = day, y = value, col = location)) +
  facet_grid(parameter ~ ., scales = "free_y")


