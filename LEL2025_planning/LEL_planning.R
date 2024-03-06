library(data.table)
library(tidyverse)
library(lubridate)
library(leaflet)
library(here)
source(here::here("LEL2025_planning/functions.R"))

## DESCRIPTION : This script is an early draft of planning where to stop and for how long on LEL2025

## Parameters
ctrl.min.time.h          <- 20/60
desired.move.ratio       <- 0.8
desired.moving.speed.kph <- 20
desired.finish.time      <- 110

event.distance.km        <- 1530
event.max.time           <- 125


## Load Controls
ctrls <- loadControls(file.path(here::here(),"LEL2025/LEL_2025_Ctrls.csv"))

## Load Route GPX
lel <- loadGPX(gpx.file = file.path(here::here(), "LEL2025_planning/2023-10-11_1344721464_LEL 2025 Controls & stops (2).gpx"))
plotGPX(lel)

ctrls[, lel[which.min(abs(lel$totalDistance.km-cumulativeDistance)),Ctrl:=endID], by = endID]

lel %>% 
  filter(!is.na(Ctrl)) %>% 
  ggplot(aes(x = totalDistance.km, y = ele.gain)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = lel[!is.na(Ctrl), totalDistance.km])

ctrls[, ele.gain:=lel[match(ctrls$endID, lel$Ctrl),ele.gain]]
ctrls[, legGain.m:=diff(c(0,ele.gain))]
ctrls[, legDifficulty:=legDistance*(legGain.m/1000)]

ctrls[, legID:=str_c((START %>% str_split("_") %>% map(~.x[1]) %>% unlist), (END %>% str_split("_") %>% map(~.x[1]) %>% unlist), sep = "->")]

leaflet() %>% 
  addTiles() %>% 
  addPolylines(lng = lel$Longitude, lat = lel$Latitude) %>%
  addAwesomeMarkers(
    lng = lel$Longitude[lel$Ctrl %>% is.na %>% not], 
    lat = lel$Latitude[lel$Ctrl %>% is.na %>% not]) %>% 
  addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addLayersControl(position = 'topright',
                   baseGroups = c("Topographical", "Satellite"),
                   options = layersControlOptions(collapsed = FALSE))

ctrls %>% 
  ggplot(aes(x = cumulativeDistance, y = ele.gain)) +
  geom_point(aes(col = legDifficulty, size = legDifficulty)) +
  ggrepel::geom_text_repel(aes(label = legID))


## TODO Use 2022 activities not personal logs



## First Method to use personal Strava Log
x <- loadActivities(here("LEL2025_planning/activities.csv"))

fit <- loess(speed_kph~Difficulty, data = x)
ctrls[,legPredictedSpeed:=predict(fit, newdata = data.table(Difficulty=ctrls$legDifficulty))]
ctrls[,legPredictedSpeed:=legPredictedSpeed + (desired.moving.speed.kph - weighted.mean(legPredictedSpeed, w = ctrls$legDifficulty))]

fit <- loess(moveRatio~Difficulty, data =x)
ctrls[,legPredictedMoveRario:=predict(fit, newdata = data.table(Difficulty=ctrls$legDifficulty))]
ctrls[,legPredictedMoveRario:=legPredictedMoveRario + (desired.move.ratio - weighted.mean(legPredictedMoveRario, w = ctrls$legDifficulty))]
ctrls[,legPredictedTime:=(legDistance/legPredictedSpeed)/legPredictedMoveRario]
ctrls[,ctrlTime:=ctrl.min.time.h]
ctrls[,totalTime := (legPredictedTime+ctrlTime) %>% cumsum]

total.sleep <- desired.finish.time - (ctrls$totalTime %>% max)
number.of.sleeps <- 4
sleep.per.rest <- (total.sleep / (number.of.sleeps))
time.between.sleep <- desired.finish.time / (number.of.sleeps+1)

for (t in seq(from = time.between.sleep, to = desired.finish.time - time.between.sleep, by = time.between.sleep)){
  ctrls[which.min(abs(t-totalTime)),ctrlTime:=ctrlTime+sleep.per.rest]
  ctrls[,totalTime := (legPredictedTime+ctrlTime) %>% cumsum]
}

cycling.speed <- round(event.distance.km/sum(ctrls$legPredictedTime * ctrls$legPredictedMoveRario), 1)
leg.speed     <- round(event.distance.km / (ctrls$legPredictedTime %>% sum), 1)
overall.speed <- round(event.distance.km / (ctrls$totalTime %>% max), 1)
total.time <- ctrls$totalTime %>% max %>% round(1)

ctrls %>% 
  ggplot(aes(x = totalTime, y = cumulativeDistance )) +
  geom_point(aes(col = legDifficulty, size = ctrlTime)) +
  ggrepel::geom_text_repel(aes(label = legID)) +
  geom_abline(slope = event.distance.km/event.max.time, intercept = 0) +
  geom_vline(xintercept = event.max.time) +
  geom_hline(yintercept = event.distance.km) +
  xlab("Time (Hours)") + ylab("Distance (km)") +
  ggtitle(glue::glue("{cycling.speed}kph cycling, {leg.speed}kph leg, {overall.speed}kph overall, {total.time} hrs, {total.sleep %>% round(1)} hrs sleep"))

ctrls %>% 
  ggplot(aes(x = cumulativeDistance, y = legPredictedSpeed)) +
  geom_point()

