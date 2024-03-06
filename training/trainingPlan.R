
## Load Libraries
library(data.table)
library(lubridate)
library(plotly)
library(here)
library(magrittr)
library(tidyverse)

## Load Functions Files
here <- file.path(here(),"strava","training")
source(file.path(here(), "src", "functions.R"))

## Load in cycling calendar from downloaded ical file - Exported from Google Calendar
cal.file <- list.files("~/Downloads/", pattern = "*.ics", full.names = TRUE)
if (length(cal.file)!=1) stop("More than one calendar file found")
cal <- ic_list(readLines(cal.file))
cal.x <- lapply(cal, function(x) 
  data.table(
    date = x %>% str_match("DTSTART.*:([:digit:]*)") %>% .[,2] %>% na.omit, 
    event = x %>% str_match("SUMMARY:(.*$)") %>% .[,2] %>% na.omit)) %>% 
  rbindlist()
cal.x[, event.date := lubridate::as_date(date)]
cal.x <- cal.x[event.date>(today()-14),][order(event.date)] # Only keep things within the last two weeks and future
cal.x[, event.length := event %>% str_match("([:digit:]+km)") %>% .[,2]]
cal.x <- cal.x[!is.na(event.length),]
cal.x[, actual.length := event.length %>% str_remove("km") %>% as.numeric]
cal.x[, wday := lubridate::wday(event.date, label = TRUE)]

cal.x[, uniqueEvent := event %>% str_remove("Day[:space:]*[:digit:]+")] ## Remove Trailing Day X
cal.x[, event.length:=str_c(sum(actual.length),"km"), by = uniqueEvent]
cal.x[, event.date := max(event.date), by = uniqueEvent]
cal.x[, event.date := event.date %>% floor_date(unit = "week")]
cal.x[, .(wday, event, event.date)]

events <- cal.x


input <- fread(file.path(here(), "referenceFiles", "ldch_plan.csv"))

## Hacks
events[event.length=="1500km", event.length:="1000km"]
events[event.length=="100km", event.length:="200km"]

if(!all(events$event.length %in% names(input))) stop("Can't find plan for distance")

mean.speed <- 22.5
y <- lapply(1:nrow(events), function(x) calculateTaining(events$event.length[x], events$event.date[x], events$event[x], plan = input)) %>% rbindlist()
y <- y[!is.na(value),]
y <- y[,.(actual.length = max(value), event = "Training"),by = .(week)]
y <- list(event = events[,.(week = event.date, event, actual.length)],
training = y[,.(week, event, actual.length)]) %>% rbindlist(idcol = "type")

y %<>% mutate(month = lubridate::month(week, label = TRUE, abbr = FALSE))
y %<>% mutate(year = lubridate::year(week))
y %<>% mutate(weekOfYear = lubridate::isoweek(week))
y %<>% mutate(dayOfWeek = lubridate::wday(week, abbr = FALSE, label = TRUE))
y$WeekOfMonth <- sapply(y$week, function(x) stringi::stri_datetime_fields(x)$WeekOfMonth)
y$ym <- ym(str_c(year(y$week), month(y$week),sep="-"))

y[,trainingDistance:=max(0,.SD[type == "training", sum(actual.length)]-.SD[type == "event", sum(actual.length)]), by = week]
y[type=="event", eventDistance:=max(actual.length, na.rm = TRUE), by = week]

p <- y[,.(total = sum(trainingDistance, eventDistance, na.rm=TRUE),event), by = .(week, type)] %>% 
  ggplot(aes(x = week, y= total)) +
  geom_point(aes(col = type, name = week, event = event)) +
  geom_hline(yintercept = c(100,200,300,400,600), alpha = 0.5, linetype = "dotted") +
  geom_vline(xintercept = as_date("2024-01-01"), linetype = "dotted") +
  geom_vline(xintercept = as_date("2025-01-01"), linetype = "dotted") +
  geom_hline(yintercept = 90)
p
plotly::ggplotly(p)

