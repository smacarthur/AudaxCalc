## Cycling Day Planner

total.time <- 125
resting.percent <- 0.1
break.every.x.hours <- 3
hours.sleep.per.day <- 3


number.of.sleeps <- (total.time/24)-1
total.sleep.time <- number.of.sleeps * hours.sleep.per.day

resting.time <- (total.time * resting.percent) - total.sleep.time
cycling.time <- total.time - resting.time - total.sleep.time


number.of.breaks <- (total.time / break.every.x.hours)-2 ## Minus two as one break is at the end and no break at start
length.of.breaks <- resting.time / number.of.breaks

cycling.stints <- number.of.breaks+1
length.of.stint <- cycling.time / cycling.stints

a <- rep("C",cycling.stints)
b <- rep("B", number.of.breaks)

x <- data.table(time.type = c(sapply(seq_along(a), function(i) append(a[i], b[i], i))),
duration = c(length.of.stint, length.of.breaks)) %>% 
  .[,timeSum:=cumsum(duration)]
x[,idx:=1:.N]
sleep.idx <- x[time.type=="B",c(0,floor(timeSum/24) %>% diff) %>% equals(1) %>% which]
sleep.idx <- x[time.type=="B"] %>% .[sleep.idx,idx]
x[sleep.idx,time.type:="S"]
x[sleep.idx,duration:=duration+hours.sleep.per.day]

x %>% 
  .[,segmentEnd:=cumsum(duration)] %>% 
  .[,segmentStart:=segmentEnd-duration]

total.cycling <- x[time.type == "C", sum(duration)]
total.break <- x[time.type == "B", sum(duration)]
total.sleep <- x[time.type == "S", sum(duration)]


x %>% 
  filter(time.type %>% is.na %>% not) %>% 
  ggplot(aes(x = 1, y = duration)) + 
  geom_bar(aes(fill = time.type, group = segmentStart), stat = "identity") + 
  coord_flip() + 
  theme_minimal() +
  theme(axis.text.y = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  xlab(NULL) +
  ggtitle(str_c(
    "Cycling = ",cycling.stints, " stints of ", length.of.stint %>% round(2), " hours  = ", total.cycling, " hours total\n",
    "Break = ", number.of.breaks, " breaks of ", length.of.breaks %>% round(2), " hours = ", total.break %>% round(2)," hours break\n",
    "Sleep = ", number.of.sleeps, " sleeps of ", hours.sleep.per.day, " hours = ", total.sleep %>% round(2), " hours sleep"))
  

