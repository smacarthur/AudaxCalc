library(data.table)
library(magrittr)
library(tidyverse)
library(lubridate)
library(ggrepel)

## User Parameters
total_distance_km         <- 1530
maximum_time_hours        <- 125
target_time_hours         <- 120
target_moving_speed_kph   <- 20
target_onbike_move_ratio  <- 0.8
dont.sleep.if.over        <- 0.95
min.dist.before.sleep     <- 300
wake.up.time.before.start <- 2
start_time                <- lubridate::as_datetime("2025-08-03 06:00:00.00")

## Calculated Parameters
onbike_cycling_hours <- total_distance_km / target_moving_speed_kph
onbike_total_hours   <- onbike_cycling_hours / target_onbike_move_ratio
onbike_resting_hours <- onbike_total_hours - onbike_cycling_hours
offbike_total_hours  <- target_time_hours - onbike_total_hours
total_move_ratio     <- onbike_cycling_hours / target_time_hours
onbike_speed         <- total_distance_km / onbike_total_hours
cyclingUnits         <- floor(target_time_hours / 24)

## Table Generation
ctrls                    <- fread("LEL_2025_Ctrls.csv")
ctrls[,START:=str_c(START,"_",ID)]
ctrls$legDistance        <- c(diff(ctrls$Distance), 0)
ctrls$END                <- c(ctrls$START[2:nrow(ctrls)], NA)
ctrls$destinationDormBeds <- c(ctrls$DormitoryBeds[2:nrow(ctrls)],NA)
ctrls[,DormitoryBeds:=NULL]
ctrls$cumulativeDistance <- ctrls$legDistance + ctrls$Distance


ctrls %>% 
  filter(ID %>% str_detect("^C")) %>% 
  ggplot(aes(x = 1:maximum_time_hours, y = 1:total_distance_km)) +
  geom_hline(yintercept = ctrls$Distance, alpha = 0.5) +
  geom_vline(xintercept = maximum_time_hours) +
  geom_text(aes(x=-5, y= Distance, label = START), size = 5) +
  xlim(-10, maximum_time_hours) +
  geom_abline(slope = total_distance_km/maximum_time_hours) +
  xlab("Time (Hours)") +
  ylab("Distance (km)") +
  theme(text =element_text(size =16))


ctrls[, Distance := NULL]
ctrls <- ctrls[ID != "C21", ]
ctrls[, legDuration := legDistance / onbike_speed]
ctrls[, cumulativeOnBikeDuration := cumsum(legDuration)]
ctrls[ID %>% str_detect("^C"), controlTime := 0.25]
ctrls[ID %>% str_detect("^C") %>% not, controlTime := 0]
ctrls[, sleep := 0]
ctrls[, cumulativeControlTime := cumsum(controlTime)]
ctrls[, cumulativeTotalTime := cumulativeOnBikeDuration + cumulativeControlTime]
ctrls[, timeWithOutSleep := cumulativeTotalTime + wake.up.time.before.start]
for (day in 1:cyclingUnits) {
  message(day)
  last.sleep <- ctrls[sleep == 1, START] %>% tail(1)
  if (!length(last.sleep))
    last.sleep <- ctrls$START[1]
  message("Last sleep was at ", last.sleep)
  ctrl.to.sleep.at <- ctrls[match(last.sleep, START):(.N - 1)] %>%
    .[destinationDormBeds != 0,] %>% 
    .[cumulativeDistance >= min.dist.before.sleep,] %>% 
    .[which.min(abs((onbike_total_hours / cyclingUnits) - timeWithOutSleep  )), START] # Find closest control to cycling "day" time
  message("Sleep at ", ctrl.to.sleep.at)

  if ((ctrls[START == ctrl.to.sleep.at,cumulativeDistance]/total_distance_km) < dont.sleep.if.over){
  ctrls[START == ctrl.to.sleep.at, sleep := 1]
  ctrls[sleep == 1, timeWithOutSleep := 0] # Reset time without sleep
  ctrls[(match(ctrl.to.sleep.at, START) + 1):.N, timeWithOutSleep := cumsum(legDuration)]
  ctrls[, cumulativeControlTime := cumsum(controlTime)]
  ctrls[, cumulativeTotalTime := cumulativeOnBikeDuration + cumulativeControlTime]
  } else {
    message("Skipping Sleep, almost there")
  }
}
total.control.time <- ctrls$controlTime %>% sum
ctrls[sleep == 1, controlTime := controlTime + (offbike_total_hours-total.control.time) / sum(sleep)] # Allocate sleep
ctrls[, cumulativeControlTime := cumsum(controlTime)]
ctrls[, cumulativeTotalTime := cumulativeOnBikeDuration + cumulativeControlTime]
ctrls[, arrivalTime:=(start_time+duration(cumulativeTotalTime-controlTime, "hours"))]
ctrls[, departureTime:=(start_time+duration(cumulativeTotalTime, "hours"))]
ctrls
fwrite(ctrls, file = "timings.csv")

ctrls[,mainControl:=ID %>% str_detect("^C")]

ctrls %>% 
  ggplot(aes(x = 1: maximum_time_hours, y = 1:total_distance_km)) +
  geom_hline(yintercept = )

suntimes <- list(
  sunrise = lapply(0:(cyclingUnits-1), function(d) 
  data.table(
    stop = lubridate::as_datetime("2025-08-03 05:29:00.00") + duration(d+1, "days"), 
    start =  lubridate::as_datetime("2025-08-03 20:42:00.00") + duration(d, "days"))) %>% rbindlist(),
astronomical =  lapply(0:(cyclingUnits-1), function(d) 
  data.table(
    stop = lubridate::as_datetime("2025-08-03 02:47:00.00") + duration(d+1, "days"), 
    start =  lubridate::as_datetime("2025-08-03 23:22:00.00") + duration(d, "days"))) %>% rbindlist(),
nautical = lapply(0:(cyclingUnits-1), function(d) 
  data.table(
    stop = lubridate::as_datetime("2025-08-03 03:57:00.00") + duration(d+1, "days"), 
    start =  lubridate::as_datetime("2025-08-03 22:14:00.00") + duration(d, "days"))) %>% rbindlist(),
 civil = lapply(0:(cyclingUnits-1), function(d) 
   data.table(
     stop = lubridate::as_datetime("2025-08-03 04:50:00.00") + duration(d+1, "days"), 
     start =  lubridate::as_datetime("2025-08-03 21:21:00.00") + duration(d, "days"))) %>% rbindlist()) %>% rbindlist(idcol = "Type")
 
ctrls.long <- ctrls %>% 
  pivot_longer(cols = c(arrivalTime, departureTime)) %>% 
  data.table
  
ctrls.long %>% 
  ggplot(aes(x = value, y = cumulativeDistance))+
  geom_rect(data=suntimes, aes(xmin = start, xmax = stop, ymin = 0, ymax = Inf), fill = "black", inherit.aes = FALSE, alpha = 0.25) +
  geom_line(alpha = 0.5, col = "green") +
  geom_line(aes(group = ID), col = "green")+
  geom_point(aes(col = name, shape = mainControl)) +
  geom_vline(xintercept = start_time + duration(target_time_hours,"hours"), linetype = "dotted") +
  geom_vline(xintercept = start_time + duration(maximum_time_hours,"hours")) +
  geom_hline(yintercept = total_distance_km) +
  ggrepel::geom_label_repel(data=ctrls.long[sleep==1 & name == "arrivalTime",], aes(label = END)) +
  xlab("Time") + ylab("Distance")



