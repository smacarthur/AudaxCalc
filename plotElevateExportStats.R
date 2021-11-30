library(tidyverse)
library(magrittr)
library(data.table)
library(plotly)
library(lubridate)

activities <- fread("~/Downloads/elevate_activities_export.2021.11.30-13.35.00.csv")
activities[,`Avg Total Speed (kph)`:=`Avg Total Speed (kph)` %>% as.numeric()]
activities[,`Avg Moving Speed (kph)`:=`Avg Moving Speed (kph)` %>% as.numeric()]

plotFormAndFitness <- function(activities, forcast.length = 30, history.length = 180) {
tmp <- data.table(
  date = as.Date(activities$Date), 
  stress = activities$`Power Stress Score` %>% as.numeric(),
  name=activities$Name)
  
tmp <- merge(tmp,data.table(date=seq(from=min(tmp$date),to=today()+forcast.length,by = "1 day")), all.y = TRUE)
tmp[stress %>% is.na,stress:= 0]


calculateForm <- function(tmp){
fitness <- rep(0,nrow(tmp))
form <- rep(0,nrow(tmp))
fatigue <- rep(0,nrow(tmp))
stress <- tmp$stress

for (idx in 2:nrow(tmp)){
fitness[idx] <- fitness[idx-1] + (stress[idx] - fitness[idx-1]) * (1- exp(-1/42))
fatigue[idx] <- fatigue[idx-1] + (stress[idx] - fatigue[idx-1]) * (1- exp(-1/7))
form[idx] <- fitness[idx-1] - fatigue[idx-1]
}
tmp$form    <- form
tmp$fitness <- fitness
tmp$fatigue <- fatigue
return(tmp)
}
tmp <- calculateForm(tmp)
tmp$timeFilter <- lubridate::today() %>% subtract(tmp$date) %>% is_less_than(history.length)
tmp[,inFuture:=date>today()]

rects <- data.frame(
  xmin=rep(min(tmp[(timeFilter),date]),3),
  xmax=rep(max(tmp[(timeFilter),date]),3),
  ymin=c(-30,-10,5),
  ymax=c(-10,5,24),
  fill = c("green","orange","blue"),
  stringsAsFactors=FALSE)

p1 <- rects %>% ggplot() +
  geom_rect(data = rects, aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax, fill = fill), alpha = 0.25) +
  geom_line(data = tmp %>% 
              filter(timeFilter),
            aes(x = date, y = form, linetype = inFuture)) +
  theme(legend.position = "none")
max.fitness <- tmp[(timeFilter),max(fitness)]
todays.fitness <- tmp[date==today(),max(fitness)]
fit.diff <- todays.fitness %>% subtract(max.fitness) %>% round(2)
p2 <- tmp %>% 
  filter(timeFilter) %>% 
  ggplot(aes(x = date, y = fitness)) + 
  geom_line(aes(linetype = inFuture), col = "blue") +
  geom_hline(aes(yintercept = max.fitness), linetype = "dotted") +
  geom_hline(aes(yintercept = todays.fitness), linetype = "dotted") +
  theme(legend.position = "none") + 
  ggtitle(paste("Fitness is", fit.diff, "points from maximum in last", history.length, "days"))
p3 <- tmp %>% 
  filter(timeFilter) %>% 
  ggplot(aes(x = date, y = fatigue)) + 
  geom_line(aes(linetype = inFuture), col = "red") +
  theme(legend.position = "none")
gridExtra::grid.arrange(p1,p2,p3)
}

plotFormAndFitness(activities, forcast.length = 60, history.length = 180)



  
  




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
