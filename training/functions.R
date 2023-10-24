calculateTaining <- function(event.length, event.date, event.name){
  if (event.length %in% names(input) %>% not) stop("Can't find plan for distance")
  x <- input[,c("Weeks Before", event.length), with = FALSE]
  x[,week:=date(event.date) - duration(`Weeks Before`, units = "week")]
  
  y <- x %>% 
    filter(!is.na(week)) %>% 
    pivot_longer(cols = ends_with("km")) %>% 
    mutate(month = month(week, label = TRUE, abbr = FALSE)) %>% 
    mutate(year = year(week)) %>% 
    mutate(weekOfYear = week(week)) %>% 
    mutate(dayOfWeek = wday(week, abbr = FALSE, label = TRUE))
  y$event <- event.name
  y$WeekOfMonth <- sapply(y$week, function(x) stringi::stri_datetime_fields(x)$WeekOfMonth)
  y$ym <- ym(str_c(year(y$week), month(y$week),sep="-"))
  y$name <- factor(y$name,levels=(unique(y$name)[y$name %>% unique %>% str_remove("km") %>% as.numeric() %>% order]))
  return(y)}
