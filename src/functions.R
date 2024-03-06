#' Plot a GPX using Leaflet
plotGPX <- function(x) {
  leaflet() %>% addTiles() %>% addPolylines(lng = x$Longitude, lat = x$Latitude) %>%
    addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
    addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
    addLayersControl(
      position = 'topright',
      baseGroups = c("Topographical", "Satellite"),
      options = layersControlOptions(collapsed = FALSE)
    )
}


#' Load GPX File
loadGPX <- function(gpx.file) {
  x <- gpx::read_gpx(file = gpx.file)$tracks[[1]] %>% data.table
  x$distance.metres <-
    c(0, geosphere::distHaversine(p1 = x[, c("Longitude", "Latitude")]))
  x[, distance.km := x$distance.metres / 1000]
  x[, distance.mile := x$distance.km / 1.609]
  x[, totalDistance.m := distance.metres %>% cumsum]
  x[, totalDistance.km := totalDistance.m / 1000]
  x[, ele.change := c(0, Elevation %>% diff())]
  tmp <- x$ele.change
  tmp[tmp <= 0] <- 0
  x[, ele.gain := tmp %>% cumsum]
  tmp <- x$ele.change
  tmp[tmp > 0] <- 0
  x[, ele.loss := tmp %>% cumsum]
  return(x)
}

#' Load Control
loadControls <- function(ctrl.file, justMain = TRUE) {
  ctrls <- fread(ctrl.file)
  ctrls[, mainCtrl := ID %>% str_detect("^C"), ]
  if (justMain) {
    ctrls <- ctrls[(mainCtrl), ]
  }
  ctrls[, START := str_c(START, "_", ID)]
  ctrls$legDistance        <- c(diff(ctrls$Distance), 0)
  ctrls$END                <- c(ctrls$START[2:nrow(ctrls)], NA)
  ctrls[, endID := END %>% str_split("_") %>% map( ~ .[2]) %>% unlist]
  ctrls$destinationDormBeds <-
    c(ctrls$DormitoryBeds[2:nrow(ctrls)], NA)
  ctrls[, DormitoryBeds := NULL]
  ctrls$cumulativeDistance <- ctrls$legDistance + ctrls$Distance
  ctrls[, Distance := NULL]
  ctrls <- ctrls[ID != "C21",]
  ctrls
}

#' Load Activities
loadActivities <- function(file.name) {
  x <- fread(file.name)
  x <- x[, names(x) %>% unique(), with = FALSE]
  x <- x[`Activity Type` == "Ride", ]
  #x <- x[Distance>50,]
  x <- x[`Elevation Gain` != 0, ]
  x <- x[!is.na(`Elevation Gain`), ]
  x[, year := `Activity Date` %>% str_extract("20..")]
  names.to.fix <- names(x) %>% str_detect(">.*<")
  names(x)[names.to.fix] <-
    names(x)[names.to.fix] %>% str_extract(">.*<") %>% str_remove_all(">|<")
  x[, apply(x, 2, function(x)
    is.na(x) %>% all) %>% not, with = FALSE]
  x[, Duration := duration(`Elapsed Time`, units = "seconds")]
  x[, speed_kph := Distance / (`Moving Time` / 60 / 60)]
  x[, Difficulty := Distance * (`Elevation Gain` / 1000)]
  x[, moveRatio := `Moving Time` / `Elapsed Time`]
  x
}


calculateTaining <- function(event.length, event.date, event.name){
  if (event.length %in% names(input) %>% not) stop("Can't find plan for distance")
  x <- input[,c("Weeks Before", event.length), with = FALSE]
  x[,week:=date(event.date) - duration(`Weeks Before`, units = "week")]
  
  y <- x %>% 
    filter(!is.na(week)) %>% 
    pivot_longer(cols = ends_with("km")) %>% 
    mutate(month = lubridate::month(week, label = TRUE, abbr = FALSE)) %>% 
    mutate(year = lubridate::year(week)) %>% 
    mutate(weekOfYear = lubridate::isoweek(week)) %>% 
    mutate(dayOfWeek = lubridate::wday(week, abbr = FALSE, label = TRUE))
  y$event <- event.name
  y$WeekOfMonth <- sapply(y$week, function(x) stringi::stri_datetime_fields(x)$WeekOfMonth)
  y$ym <- ym(str_c(year(y$week), month(y$week),sep="-"))
  y$name <- factor(y$name,levels=(unique(y$name)[y$name %>% unique %>% str_remove("km") %>% as.numeric() %>% order]))
  y %<>% mutate(total_distance_km=value*2)
  y %<>% mutate(maximum_ride_distance_km=value)
  y %<>% select(-value)
  return(y)
}

calculateTaining <- function(event.length, event.date, event.name, plan = input){
  if (event.length %in% names(plan) %>% not) stop("Can't find plan for distance")
  x <- plan[,c("Weeks Before", event.length), with = FALSE]
  x[,week:=date(event.date) - duration(`Weeks Before`, units = "week")]
  y <- x %>% 
    filter(!is.na(week)) %>% 
    pivot_longer(cols = ends_with("km"))
  y$event <- event.name
  return(y)
}




ic_list <- function (x, pattern = ":VEVENT", include_pattern = FALSE) 
{
  locations <- grepl(pattern = pattern, x = x)
  locations_int <- which(locations)
  list_length <- length(locations_int)/2
  list_seq <- seq_len(list_length)
  locations_start <- list_seq * 2 - 1
  locations_end <- list_seq * 2
  if (include_pattern) {
    lapply(list_seq, function(i) {
      x[locations_int[locations_start[i]]:locations_int[locations_end[i]]]
    })
  }
  else {
    lapply(list_seq, function(i) {
      x[(locations_int[locations_start[i]] + 1):(locations_int[locations_end[i]] - 
                                                   1)]
    })
  }
}







