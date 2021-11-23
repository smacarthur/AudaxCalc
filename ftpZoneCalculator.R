library(tidyverse)
library(magrittr)
library(data.table)

ftp <- 251

zone.name <- c(
  "Z1 Active Recovery",
  "Z2  Endurance",
  "Z3  Tempo",
  "Z4 Threshold",
  "Z5 VO2 max",
  "Z6 Anaerobic capacity",
  "Z7 Neuromuscular Power",
  "Sweet Spot")

zone.lower <- c(   0, 0.56, 0.76, 0.91, 1.06, 1.21, 1.50, 0.88)
zone.upper <- c(0.55, 0.75, 0.90, 1.05, 1.20, 1.50, Inf, 0.95)

resultsTable <- data.table(
  Zone = zone.name,
  Range = str_c(zone.lower %>% multiply_by(100), "-", zone.upper %>% multiply_by(100)),
  `Min Power` = ftp %>% multiply_by(zone.lower) %>% round(0),
  `Max Power` = ftp %>% multiply_by(zone.upper) %>% round(0))
resultsTable[,`Mid Power`:=((`Min Power` + `Max Power`)/2) %>% round(0)]

DT::datatable(resultsTable)

