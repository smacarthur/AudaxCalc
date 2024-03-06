library(data.table)
library(lubridate)
library(plotly)
library(here)
library(magrittr)
library(tidyverse)
source(here("strava","training","functions.R"))

updatePlan <- function(){
events <<- fread(here("strava","training","events.txt"))
events <<- events[event.date %>% str_detect("#") %>% not,]
events[,event.date:=as_date(event.date)]
events <<- events
input <<- fread(here("strava","training","ldch_plan.csv"))

mean.speed.kph <- 22.5 ## KPH
mean.speed.mph <- round(mean.speed.kph/1.609,1)

glue::glue("Average speed {mean.speed.kph}kph ({mean.speed.mph}mph)")

events[, event.date:=event.date %>% round_date(unit = "week")]
events[, wday:=lubridate::wday(event.date, label = TRUE)]

y <- apply(events, 1, function(x) calculateTaining(x[2], x[1], x[3])) %>% rbindlist()
y[, total_distance_km:=max(total_distance_km, na.rm = TRUE), by = .(year, weekOfYear)]
y[, maximum_ride_distance_km:=max(maximum_ride_distance_km, na.rm = TRUE), by = .(year, weekOfYear)]
y <- y[!is.na(total_distance_km) & is.finite(total_distance_km),]
y <- y[order(week),sum:=cumsum(total_distance_km)]
y[,time_hrs:=total_distance_km / mean.speed.kph]
return(y)
}

plotPlan <- function(y=y){
p <- y %>% 
  filter(week >= today()) %>% 
  pivot_longer(cols = ends_with("distance_km"), names_to = "DistanceType", values_to = "distance" ) %>% 
  filter(DistanceType=="maximum_ride_distance_km") %>% 
  ggplot(aes(x = week, y = distance)) +
  geom_point(aes(shape = DistanceType)) +
  geom_line(aes(group = week))+
  geom_vline(xintercept = date(c("2023-12-31","2024-12-31")), linetype = "dotted") +
  geom_hline(yintercept = c(100,200,300,400,600), linetype = "dotted")+
  geom_point(data = events,
             aes(
               x = event.date, 
               y = actual.length, col = event.length), size = 4 ) +
  ylab("Distance (km)") +
  scale_y_continuous(sec.axis = sec_axis(trans = ~./1.609, name = "Total Distance (miles)"), name = "Total Distance (km)")
ggplotly(p)
}

y <- updatePlan()
plotPlan(y)

merged <- rbind(y[,.(week, event, maximum_ride_distance_km)], events[,.(week=event.date, event, maximum_ride_distance_km=actual.length)])

merged %<>% 
mutate(month = lubridate::month(week, label = TRUE, abbr = FALSE)) %>% 
  mutate(year = lubridate::year(week)) %>% 
  mutate(weekOfYear = lubridate::isoweek(week)) %>% 
  mutate(dayOfWeek = lubridate::wday(week, abbr = FALSE, label = TRUE)) %>% 
  mutate(time_hrs = maximum_ride_distance_km / mean.speed.kph)


merged[order(weekOfYear),.(week = weekOfYear, sum = cumsum(maximum_ride_distance_km)),by = year] %>% ggplot(aes(x = week, y = sum)) + geom_line(aes(col = factor(year)))

merged[,.(maximum_ride_distance_km = sum(maximum_ride_distance_km), weekOfYear = unique(weekOfYear) , time_hrs =sum(time_hrs)),by = .(week, year)] %>% 
  ggplot(aes(x = weekOfYear, y = time_hrs)) +
  geom_point(aes(col = factor(year))) + 
  geom_line(aes(col = factor(year)))
