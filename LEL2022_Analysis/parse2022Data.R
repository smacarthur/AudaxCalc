#processLEL2022Data
library(data.table)
library(tidyverse)
library(magrittr)
library(lubridate)
library(scales)

## Functions
to_date <- . %>%  parse_date_time(., "dmY HM")
km_to_m <- . %>% divide_by(1.609)

ctrls <- fread("2022Controls", sep = ",")
ctrls[,distance := distance %>% str_remove(" km") %>% as.numeric()]
ctrls[,idx:=1:.N]
ctrls[,distFromLast:=c(0,distance %>% diff)]
total.distance <- ctrls[,distance[.N]]



## Load and Process Raw Data
## Read in file and use "NULL" to be NA - Meaning no data
x <- fread("2022 rider tracking data.csv", na.strings = "NULL")
x <- x[`Start Location`=="Debden",] ## Only keep Debden Starters
x[,`Start Location`:=NULL] ## Only keeping the debden starts
x <- x[!is.na(Start),] ## Only keep people with start times
x <- x[,lapply(.SD, to_date)] ## Convert all columns to date time
x <- x[Start %>% date %>% equals("2022-08-07"),] ## Filter out start dates on the wrong day
x %<>% set_colnames(str_c(names(x),"_",1:ncol(x)))

x[,rider:=1:.N]
x[, fastGroup        := Start_1 %>% hour %>% equals(5)] 
x[, timeAllowed      := c(128+(20/60), 100)[fastGroup %>% as.numeric() %>% add(1)]]
x[, finished         := !is.na(DebdenFinish_21)]
x[, finishedInTime   := DebdenFinish_21<=Start_1+duration(timeAllowed, "hours")]
x[, success := finished & finishedInTime]
x[, totalTime        := interval(Start_1,DebdenFinish_21) %>% as.duration() %>% as.numeric(.,"hours")]
x[, averageSpeed_kph := total.distance/totalTime]

y <- x %>% pivot_longer( cols = c(contains("bound"),"Start_1","DebdenFinish_21","Dunfermline_10")) %>% data.table
y[name %>% str_detect("Dunfermline"),direction:= "H"]
y[name %>% str_detect("Start|North"),direction:= "N"]
y[name %>% str_detect("Finish|South"), direction:="S"]
y[,control:=name %>% str_split("_") %>% map(~.[1]) %>% unlist]
y[,controlIDX:=name %>% str_split("_") %>% map(~.[2]) %>% unlist %>% as.integer()]
y <- merge(y, ctrls, by.x="controlIDX", by.y="idx")
y[, timeFromLast               := c(NA,.SD[order(controlIDX),value] %>% diff %>% as.numeric(.,"hours")),by = rider]
y[, avgSpeedFromArrivalAtLast := distFromLast/timeFromLast]
y[, runningTime               := interval(.SD[controlIDX==1,value], value) %>% as.duration() %>% as.numeric(.,"hours"),by = rider]
y[, runningAvgSpeed           := distance/runningTime]
y[, leg1Speed                 := avgSpeedFromArrivalAtLast[controlIDX==2], by = rider]
y[, estStopTime:=c((round(timeFromLast-(distFromLast/leg1Speed),1))[2:.N],NA), by = rider]
y[, estTotalControlTime:=estStopTime %>% sum(na.rm = TRUE), by = rider]
y[, estTotalCyclingTime:=totalTime-estTotalControlTime, by = rider]
y[, estAvgCyclingSpeed:=total_distance_km/estTotalCyclingTime, by =rider]
y[, estMoveRatio := estTotalCyclingTime / totalTime, by =rider]

y[,lastControl:=controlIDX==(is.na(value) %>% which %>% head(1) %>% subtract(1)), by = rider]

y[,.(success=success %>% sum %>% divide_by(.N),.N), by = leg1Speed %>% round] %>% 
  ggplot(aes(x = leg1Speed  , y = success)) + geom_point() + geom_smooth() +
  scale_y_continuous(label = percent) + 
  ylab("Percent of Riders Completed") +
  geom_hline(yintercept = sum(x$success)/nrow(x)) +
  xlab("Average speed to St Ives (kph)") +
  theme(text =element_text(size =16))

y[name == "StIvesNorthbound_2",.(success=success %>% sum %>% divide_by(.N),.N), by = runningTime %>% round(1)] %>% 
  ggplot(aes(x = runningTime , y = success)) + geom_point() + geom_smooth() +
  scale_y_continuous(label = percent) + 
  ylab("Percent of Riders Completed") +
  xlab("Average Time to St Ives (hours)") +
  theme(text =element_text(size =16)) +
  xlim(3,6)

y[,name:=factor(name, levels = y[,.(name,controlIDX)] %>% unique %>% .[order(controlIDX),name])]

y[,.(success=success %>% sum %>% divide_by(.N),.N), by = .(name, runningTime %>% round(0))] %>% 
  ggplot(aes(x = runningTime , y = success)) + 
  geom_point() + 
  geom_smooth(aes(group = name)) +
  scale_y_continuous(label = percent, limits = c(0,1)) + 
  ylab("Percent of Riders Completed") +
  xlab("Average Time to Malton") +
  facet_wrap(~name, scale = "free_x") + 
  geom_hline(yintercept = 0.5) +
  theme(text =element_text(size =16))

y %>% ggplot(aes(x = totalTime, y = estMoveRatio)) +
   geom_point(aes(col = success)) +
  theme(text =element_text(size =16))


y %>% 
  filter(!fastGroup) %>% 
  ggplot(aes(x = runningTime, y = distance)) + 
  geom_line(aes(group = rider, col = success), alpha = 0.2) +
  geom_vline(xintercept = 128.3333) +
  geom_hline(yintercept = total_distance_km) +
  geom_abline(slope = total_distance_km/128.33, linetype = "dotted") +
  xlab("Total Time") + 
  ylab("Distance") + 
  facet_wrap(~success) +
  geom_smooth(method = "loess") +
  theme(text = element_text(size = 16)) 




