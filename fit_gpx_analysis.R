library(gpx)
library(leaflet)
library(FITfileR)
library(tidyverse)
library(data.table)

source("LEL2025_planning/functions.R")

x <- list(planned= loadGPX("~/Downloads/Cambridge Pork Pie Perm 200km.gpx"),
actual=loadGPX("~/Downloads/2022-03-19_708894810_Cycling.gpx"))

p <- FITfileR::readFitFile("~/Downloads/Popping_out_for_a_pork_pie.fit") %>% 
  records %>% 
  bind_rows() %>% 
  arrange(timestamp) %>% 
  data.table

p %>% 
  filter(power <2000) %>%
  filter(grade!=0 & power != 0) %>% 
  ggplot(aes(x = power, y = speed))+
  geom_point(alpha = 0.1, aes(col = grade)) +
  geom_smooth(method = "lm")


  leaflet() %>% 
    addTiles() %>% 
    addPolylines(lng = x$actual$Longitude, lat = x$actual$Latitude, color = "white") %>%
    addPolylines(lng = x$planned$Longitude, lat = x$planned$Latitude, color = "green") %>%
    addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
    addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
    addLayersControl(
      position = 'topright',
      baseGroups = c("OpenStreetMap", "Topographical", "Satellite"),
      options = layersControlOptions(collapsed = FALSE)
    )



