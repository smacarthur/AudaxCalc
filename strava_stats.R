library(rStrava)
library(lubridate)
library(tidyverse)
library(magrittr)
library(data.table)

## Authentication ####
if (as_datetime(oauth$credentials$expires_at)<now()){
oauth <- rStrava::strava_oauth(
  app_name = "My API Application", 
  app_client_id = "14098", 
  app_secret = "0fddc99747c0007a21ad717c0703983dfc8d5643", 
  app_scope = "activity:read_all",
  cache = FALSE)
}
message(as_datetime(oauth$credentials$expires_at))

cache.file <- paste0("~/cache_",oauth$credentials$athlete$id,".csv")

## Load All Data ####
if (file.exists(cache.file)){
  message("Using Cache")
  comp.activities <- fread(cache.file)
  last.ride.date <- date(comp.activities$start_date) %>% max()
  if (date(today())>last.ride.date){
  message("Getting New Data")
  comp.activities.update <- rStrava::get_activity_list(oauth, after =  as_date(max(comp.activities$start_date, na.rm = TRUE))) %>% 
    compile_activities %>% 
    data.table

  tmp <- merge(comp.activities %>% map(~.x %>% class) %>% as.data.frame %>% t %>% data.table(keep.rownames = TRUE) %>% .[,.(rn,V1)],
  comp.activities.update %>% map(~.x %>% class) %>% as.data.frame %>% t %>% data.table(keep.rownames = TRUE) %>% .[,.(rn,V1)], by = "rn", all.y=TRUE)
  tmp <- tmp[match(names(comp.activities.update), tmp$rn),]
  
  tmp.file <- tempfile()
  fwrite(comp.activities.update, file = tmp.file)
  comp.activities.update <- fread(tmp.file, colClasses = tmp$V1.x)
  unlink(tmp.file)  
  
comp.activities <- rbindlist(list(comp.activities, comp.activities.update), fill = TRUE) %>% unique
fwrite(comp.activities, file = cache.file)
}
} else {
## Load All Activities - First time user
  message("No Cache - Loading all data")
  comp.activities <- rStrava::get_activity_list(oauth) %>% 
    compile_activities %>% 
    data.table
fwrite(comp.activities, file = cache.file)
}

## Filter and Clean Data ####
message(nrow(comp.activities)," activities")
comp.activities <- comp.activities[type %in% c("Ride","VirtualRide"),]
message(nrow(comp.activities)," cycling activities")
comp.activities <- comp.activities[moving_time>15,]
message(nrow(comp.activities)," cycling activities after cleanup")

comp.activities[name %>% str_detect("^Wattbike"),type:="VirtualRide"]
comp.activities[trainer=="TRUE", type:="VirtualRide"]

## Add derived data ####
comp.activities[,year:=year(start_date)]
comp.activities[,isoweek:=isoweek(start_date)]
comp.activities[,month:=month(start_date)]
comp.activities[,month_name:=factor(lubridate::month(start_date, label = TRUE))]
comp.activities[,day:=day(start_date)]
comp.activities[,kcal:=kilojoules/4.184]

comp.activities[,difficulty:=distance * (total_elevation_gain / 1000)]


## Useful globals ####
year.range <- c(comp.activities[,min(year)] : comp.activities[,max(year)])

## Plotting Defaults ####
global_size <- 15
default.theme <- theme_gray(base_size = global_size)
theme_set(default.theme)
# 
# list(actual = comp.activities[year == 2024,.(max=max(distance), total = sum(distance)), by = .(year,isoweek)]  ,
# plan = y[year==2024,.(year = year, isoweek = weekOfYear, max = maximum_ride_distance_km, total = total_distance_km)]) %>% 
#   rbindlist(idcol = "type") %>% 
#   pivot_longer(cols = c("max", "total")) %>% 
#   ggplot(aes(x = isoweek, y = value)) +
#   geom_bar(aes(fill = type), position = "identity", stat = "identity", alpha = 0.5) +
#   facet_wrap(~name)


## Plot Distance by Year ####
comp.activities[,.(totalDistance= sum(distance)), by = .(year, type)] %>% 
  ggplot(aes(x = year, y = totalDistance)) +
  geom_bar(stat = "identity", aes(fill = type)) +
  scale_x_continuous(breaks = year.range)+
  geom_point(data = comp.activities[isoweek <= isoweek(today()),.(totalDistance= sum(distance)), by = year],
, col = "red", size = 3) +
  geom_hline(yintercept = comp.activities[year == year(today()),sum(distance)], col = "red") +
  scale_y_continuous(sec.axis = sec_axis(trans = ~./1.609, name = "Total Distance (miles)"), name = "Total Distance (km)")

## Plot Time by Year ####
comp.activities[,.(totalTime= sum(moving_time)/60/60), by = .(year, type)] %>% 
  ggplot(aes(x = year, y = totalTime)) +
  geom_bar(stat = "identity", aes(fill = type)) +
  scale_x_continuous(breaks = year.range)+
  geom_point(data = comp.activities[isoweek <= isoweek(today()),.(totalTime= sum(moving_time)/60/60), by = year],
             , col = "red", size = 3) +
  geom_hline(yintercept = comp.activities[year == year(today()),sum(moving_time)/60/60], col = "red") + 
  ylab("Total Time (hours)")

## Plot Elevation by Year ####
comp.activities[,.(totalEle= sum(total_elevation_gain)), by = .(year, type)] %>% 
  ggplot(aes(x = year, y = totalEle)) +
  geom_bar(stat = "identity", aes(fill = type)) +
  scale_x_continuous(breaks = year.range)+
  geom_point(data = comp.activities[isoweek <= isoweek(today()),.(totalEle= sum(total_elevation_gain)), by = year],
             , col = "red", size = 3) +
  geom_hline(yintercept = comp.activities[year == year(today()),sum(total_elevation_gain)], col = "red") +
  scale_y_continuous(sec.axis = sec_axis(trans = ~.*3.281, name = "Total Elevation Gain (feet)"), name = "Total Elevation Gain (metres)")

##Plot suffer score by Year ####
comp.activities[,.(totalEle= sum(suffer_score, na.rm = TRUE)), by = year] %>% 
  ggplot(aes(x = year, y = totalEle)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = year.range)+
  ylab("Suffer Score") +
  geom_point(data = comp.activities[isoweek <= isoweek(today()),.(totalEle= sum(suffer_score, na.rm = TRUE)), by = year],
             , col = "red", size = 3) +
  geom_hline(yintercept = comp.activities[year == year(today()),sum(suffer_score, na.rm = TRUE)], col = "red")


## Plot cumulative distance of rides and virtual rides ####
p <- lapply(list("Ride","VirtualRide",c("Ride","VirtualRide")), function(t){
  tmp <- comp.activities[type %in% t,.(sum = sum(distance, na.rm = TRUE)),by = year]
  comp.activities %>% 
    filter(type %in% t) %>% 
    .[order(start_date), .(weekDistance = sum(distance, na.rm = TRUE)), by = .(year(start_date), isoweek(start_date))] %>% 
    .[order(isoweek),.(isoweek, cumsum = cumsum(weekDistance)), by = .(year)] %>% 
    ggplot(aes(x = isoweek, y = cumsum)) +
    geom_line(aes(col = factor(year), group = year)) +
    geom_vline(xintercept = isoweek(today()), col = "red", linetype = "dotted")+ 
    xlab("Week") + 
    ylab("Cumulative Distance (km)") +
    theme(legend.title=element_blank())+
    scale_y_continuous(sec.axis = sec_axis(~ ., breaks = tmp$sum,labels = tmp$year )) +
    ggtitle(str_c(t,collapse = "&"))})
cowplot::plot_grid(plotlist = p, ncol = 1)

## Plot cumulative time of rides and virtual rides ####
p <- lapply(list("Ride","VirtualRide",c("Ride","VirtualRide")), function(t){
  tmp <- comp.activities[type %in% t,.(sum = sum(moving_time, na.rm = TRUE)/60/60),by = year]
  comp.activities %>% 
    filter(type %in% t) %>% 
    .[order(start_date), .(weekDistance = sum(moving_time, na.rm = TRUE)/60/60), by = .(year(start_date), isoweek(start_date))] %>% 
    .[order(isoweek),.(isoweek, cumsum = cumsum(weekDistance)), by = .(year)] %>% 
    ggplot(aes(x = isoweek, y = cumsum)) +
    geom_line(aes(col = factor(year), group = year)) +
    geom_vline(xintercept = isoweek(today()), col = "red", linetype = "dotted")+ 
    xlab("Week") + 
    ylab("Cumulative Moving Time") +
    theme(legend.title=element_blank())+
    scale_y_continuous(sec.axis = sec_axis(~ ., breaks = tmp$sum,labels = tmp$year )) +
    ggtitle(str_c(t,collapse = "&"))})
cowplot::plot_grid(plotlist = p, ncol = 1)


## Plot evelation gain time of rides and virtual rides ####
p <- lapply(list("Ride","VirtualRide",c("Ride","VirtualRide")), function(t){
  tmp <- comp.activities[type %in% t,.(sum = sum(total_elevation_gain, na.rm = TRUE)),by = year]
  comp.activities %>% 
    filter(type %in% t) %>% 
    .[order(start_date), .(weekDistance = sum(total_elevation_gain, na.rm = TRUE)), by = .(year(start_date), isoweek(start_date))] %>% 
    .[order(isoweek),.(isoweek, cumsum = cumsum(weekDistance)), by = .(year)] %>% 
    ggplot(aes(x = isoweek, y = cumsum)) +
    geom_line(aes(col = factor(year), group = year)) +
    geom_vline(xintercept = isoweek(today()), col = "red", linetype = "dotted")+ 
    xlab("Week") + 
    ylab("Cumulative Elevation Gain") +
    theme(legend.title=element_blank())+
    scale_y_continuous(sec.axis = sec_axis(~ ., breaks = tmp$sum,labels = tmp$year )) +
    ggtitle(str_c(t,collapse = "&"))})
cowplot::plot_grid(plotlist = p, ncol = 1)

p <- lapply(list("Ride","VirtualRide",c("Ride","VirtualRide")), function(t){
  tmp <- comp.activities[type %in% t,.(sum = sum(suffer_score, na.rm = TRUE)),by = year]
  comp.activities %>% 
    filter(type %in% t) %>% 
    .[order(start_date), .(weekDistance = sum(suffer_score, na.rm = TRUE)), by = .(year(start_date), isoweek(start_date))] %>% 
    .[order(isoweek),.(isoweek, cumsum = cumsum(weekDistance)), by = .(year)] %>% 
    ggplot(aes(x = isoweek, y = cumsum)) +
    geom_line(aes(col = factor(year), group = year)) +
    geom_vline(xintercept = isoweek(today()), col = "red", linetype = "dotted")+ 
    xlab("Week") + 
    ylab("Cumulative Suffer Score") +
    theme(legend.title=element_blank())+
    scale_y_continuous(sec.axis = sec_axis(~ ., breaks = tmp$sum,labels = tmp$year )) +
    ggtitle(str_c(t,collapse = "&"))})
cowplot::plot_grid(plotlist = p, ncol = 1)

## Plot Temperature by Month
comp.activities %>% 
  filter(type == "Ride") %>% 
  ggplot(aes(x= month_name, y = as.numeric(average_temp))) +
  geom_jitter()+
  geom_boxplot() +
  ylab("Mean Temperatuce (°C)") +
  xlab("Month")

comp.activities[,hour:=hour(as_datetime(start_date))]
comp.activities[,wday:=factor(lubridate::wday(comp.activities$start_date, label = TRUE, week_start = 1) )]

## Distance by Hour of Day ####
comp.activities %>% 
  ggplot(aes(x = hour, y = distance))+
  geom_bar(aes(fill = type), stat = "sum", position = "dodge") +
  xlab("Hour of Day") +
  scale_y_continuous(sec.axis = sec_axis(trans = ~./1.609, name = "Total Distance (miles)"), name = "Total Distance (km)") +
  ggtitle("Distance by Time of Day")

## Distance by Month ####
comp.activities %>% 
  ggplot(aes(x = month_name, y = distance))+
  geom_bar(aes(fill = type), stat = "sum", position = "dodge") +
  xlab("Month of Year") +
  scale_y_continuous(sec.axis = sec_axis(trans = ~./1.609, name = "Total Distance (miles)"), name = "Total Distance (km)")

## Distance by Day of Week
comp.activities %>% 
  ggplot(aes(x = wday, y = distance))+
  geom_bar(aes(fill = type), stat = "sum", position = "dodge") +
  xlab("Day of Week") +
  scale_y_continuous(sec.axis = sec_axis(trans = ~./1.609, name = "Total Distance (miles)"), name = "Total Distance (km)")


## Plot Speed vs Power ####
p <- comp.activities %>% 
  filter(type %in% c("Ride","VirtualRide")) %>% 
  filter(distance>20) %>% 
  ggplot(aes(x = average_speed, y = average_watts)) +
  geom_point(aes(col = type, size = total_elevation_gain, name = name, date = start_date)) + 
  xlab("Average Speed (kph)") +
  ylab("Average Power (Watts)") +
  ggtitle("Speeds vs Power (rides >20km)")
show(p)

## Distance vs Move Ratio ####
p <- comp.activities %>% 
  filter(type %in% c("Ride")) %>% 
  filter(distance>50) %>% 
  ggplot(aes(col = average_speed, x = distance, y = moving_time/elapsed_time)) +
  geom_point(aes(size = distance, name = name, date = start_date)) + 
  xlab("Distance (km)") +
  ylab("Move Ratio") +
  ggtitle("Distance vs Move Ratio (rides > 50km)")
plotly::ggplotly(p)

ftp <- 250
comp.activities[,IF:=as.numeric(weighted_average_watts)/ftp]
comp.activities[,TSS:= (moving_time/60/60) * ((IF^2) * 100)]

forcast.length <- 30
tmp <- data.frame(
  date = as.Date(comp.activities$start_date), 
  stress= as.numeric(comp.activities$suffer_score), 
  name = comp.activities$name)
tmp <- merge(tmp,data.table(date=seq(from=min(tmp$date),to=max(tmp$date)+forcast.length,by = "1 day")), all.y = TRUE) %>% data.table
tmp[stress %>% is.na,stress:= 0]
tmp[,stress:=as.numeric(stress)]

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
  tmp[,idx:=1:length(form)]
  tmp$form    <- form
  tmp$fitness <- fitness
  tmp$fatigue <- fatigue
  tmp[,forcast:=date>max(comp.activities$start_date,na.rm=TRUE)]
  return(tmp)
}

tmp <- calculateForm(tmp)
tmp[,weekBefore:=date-7]

tmp <- merge(tmp, tmp[,.(fitnessWeekBefore=fitness, weekBefore=date-7)], by.x = "date", by.y = "weekBefore", all = TRUE)
tmp <- tmp[!is.na(fitness) & !is.na(fitnessWeekBefore),]

p <- tmp %>% 
  ggplot(aes(x = date, y = form)) +
  geom_line(aes(linetype = forcast)) 
plotly::ggplotly(p)

glue::glue("Maximum form in {as.numeric(as_date(tmp[(forcast),][which.max(form),date])-as_date(today()))} days")

comp.activities[,date:=as.Date(start_date)]
comp.activities <- merge(comp.activities, tmp[!(forcast),], by="date", all = TRUE)

comp.activities %>% 
  filter(distance>=40) %>% 
  ggplot(aes(x = distance, y = fitnessWeekBefore)) +
  geom_point() + 
  geom_hline(yintercept = comp.activities[which.max(date),fitness], col = "green") +
  geom_hline(yintercept = comp.activities[which.max(date),fitnessWeekBefore], col = "orange")

howFit <- lapply(c(100, 200, 300, 400, 600, 1000, 1500), function(d)
  data.table(
    distance   = d,
    minFit     = comp.activities[distance>=d,suppressWarnings(min(fitness))],
    currentFit = comp.activities[which.max(date),fitness])) %>% 
  rbindlist() %>% 
  .[,canRide:=currentFit>(minFit*0.80)] %>% 
  .[is.finite(minFit),] 

glue::glue("You should be able to complete a {howFit[(canRide),max(distance)]}km ride, based on past performance")

howFit %>% 
  knitr::kable()

comp.activities[(commute) & year >= 2023,] %>% 
    ggplot(aes(x=date, y = average_heartrate)) +
    geom_point() + 
    geom_smooth(method = "lm")

comp.activities[(commute) & year >= 2023,] %>% 
  ggplot(aes(x=date, y = average_speed)) +
  geom_point() + 
  geom_smooth(method = "lm")

comp.activities[(commute) & year >= 2023,] %>% 
  ggplot(aes(x=date, y = suffer_score)) +
  geom_point() + 
  geom_smooth(method = "lm")

dgTime2HM <- . %>% {str_c(floor(.),"h",round(60*(.-floor(.)),2),"m")}


getStats <- function(x, dp = 1, time = FALSE) 
  {
  if (time){
    data.table(
      min = as.character(round(min(x),dp) %>% dgTime2HM()), 
      mean = as.character(round(mean(x), dp) %>% dgTime2HM()), 
      max = as.character(round(max(x),dp) %>% dgTime2HM()))
  } else {
  data.table(
    min = as.character(round(min(x),dp)), 
    mean = as.character(round(mean(x), dp)), 
    max = as.character(round(max(x),dp)))
  }
}


query.distance <- 200
distance.range <- 0.10 ## Fraction of query distance to include query.distance +/- (distance.range*query.distance)
tmp <- comp.activities[type == "Ride" & abs(query.distance-distance)<(query.distance*distance.range), ] 
tmp[,estMoveTime:=query.distance/average_speed]
tmp[,estTotalTime:=estMoveTime/(moving_time/elapsed_time)]
tmp[,estFinish:=estTotalTime+8]

glue::glue("Found {nrow(tmp)} activities")

list(
  "Moving Time (hrs)"   = tmp[, estMoveTime]                %>% getStats(time = TRUE),
  "Elapsed Time (hrs)"  = tmp[, estTotalTime]               %>% getStats(time = TRUE),
  "Estimated Finish (6:30)" = tmp[, estFinish]               %>% getStats(time = TRUE),
  "Move Ratio"          = tmp[, moving_time / elapsed_time] %>% getStats(dp = 2),
  "Speed(kpm)"          = tmp[, average_speed]              %>% getStats(),
  "Speed(mph)"          = tmp[, average_speed / 1.609]      %>% getStats(),
  "HR (bpm)"            = tmp[, na.omit(average_heartrate)] %>% getStats(dp = 0),
  "Power (W)"           = tmp[, na.omit(average_watts)]     %>% getStats(dp = 0),
  "Cadence (rpm)"       = tmp[, na.omit(average_cadence)]   %>% getStats(dp = 0),
  "kjoules"             = tmp[, kilojoules]                 %>% getStats(dp = 0 ),
  "Distance (km)"       = tmp[, distance]                   %>% getStats()
  ) %>% 
  rbindlist(idcol = paste0("Metric (N=", as.character(nrow(tmp)),")"))




