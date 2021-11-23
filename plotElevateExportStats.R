library(tidyverse)
library(magrittr)
library(data.table)
library(plotly)

activities <- fread("~/Downloads/elevate_activities_export.2021.11.16-17.55.31.csv")
activities[,`Avg Total Speed (kph)`:=`Avg Total Speed (kph)` %>% as.numeric()]
activities[,`Avg Moving Speed (kph)`:=`Avg Moving Speed (kph)` %>% as.numeric()]

activities <- activities[
  Type == "Ride" & 
    `Distance (km)`>50 & 
    `Elevation Gain (m)` > 0 &
    !(`Avg Total Speed (kph)`<14 & `Distance (km)`<100) ,]
activities[,difficulty:= `Distance (km)` * (`Elevation Gain (m)`/1000)]

ggplotly(activities %>% 
  ggplot(aes(x = Date, y = `Distance (km)`)) +
  geom_point(aes(Name = Name)))

ggplotly(activities %>% 
           ggplot(aes(x = `Distance (km)`, y = `Avg Total Speed (kph)`)) +
           geom_point(aes(Name = Name)))


a <- activities$`Avg Total Speed (kph)`
b <- activities$`Distance (km)`
#b <- activities$difficulty
fit <- loess(a~b)

activities %>% 
  ggplot(aes(x = b, y =a))+
  geom_abline(intercept = fit$coefficients[1], slope = fit$coefficients[2]) +
  geom_smooth(method = "lm") +
  geom_point(aes(Name = Name))

newdata = data.frame(b=seq(50,400,by=50))
cbind(newdata,predict(fit, newdata = newdata))



distances.to.check <- c(25,50,75,100,150,200)

last.occurance <- distances.to.check %>% 
  map(~(activities$`Distance (km)` >= .x)) %>% 
  map(~activities[which(.x) %>% head(1),Date %>% lubridate::as_date()]) %>% unlist %>% lubridate::as_date()

ride.distance <- distances.to.check %>% 
  map(~(activities$`Distance (km)` >= .x)) %>% 
  map_dbl(~activities[which(.x) %>% head(1), `Distance (km)`])

ride.name <- distances.to.check %>% 
  map(~(activities$`Distance (km)` >= .x)) %>% 
  map_chr(~activities[which(.x) %>% head(1), Name])

results <- data.table(
  km = distances.to.check, 
  miles = distances.to.check %>% divide_by(1.609) %>% round(2), 
  last.occurance,
  ride.name,
  ride.km = ride.distance,
  ride.miles = ride.distance %>% divide_by(1.609) %>% round(2))
results[,lastRide:=lubridate::today() - last.occurance]
results
