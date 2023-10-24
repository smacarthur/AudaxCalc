library(data.table)
library(lubridate)
library(plotly)
source("functions.R")
events <- fread("events.txt")
input <- fread("ldch_plan.csv")
mean.speed <- 22.5
events[,event.date:=event.date %>% round_date(unit = "week")]
events[, wday:=wday(event.date, label = TRUE)]
events[, .(wday,event,event.date)]
y <- apply(events, 1, function(x) calculateTaining(x[2], x[1], x[3])) %>% rbindlist()
y[, value:=max(value, na.rm = TRUE), by = .(year, weekOfYear)]
y <- y[!is.na(value),]
y <- y[,.SD[which.max(value)], by = week]
y <- y[order(week),sum:=cumsum(value)]
y[,time:=value / mean.speed]


p <- y %>% 
  ggplot(aes(x = week, y = value)) +
  geom_point(aes(col = value)) +
  geom_vline(xintercept = date(c("2023-12-31","2024-12-31")), linetype = "dotted") +
  geom_hline(yintercept = c(100,200,300,400,600), linetype = "dotted")+
  geom_point(data = events,
             aes(
               x = event.date, 
               y = actual.length, col = actual.length), size = 4 ) +
  scale_color_continuous(low = "green", high = "red") +
  ylab("Distance (km)")
show(p)

y[,sum(value),by = ym] %>% 
  ggplot(aes(x = ym, y = V1)) +
  geom_point()
