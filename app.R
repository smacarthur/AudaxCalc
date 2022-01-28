library(shiny)
library(shinyBS)
library(tidyverse)
library(magrittr)
library(plotly)
library(data.table)
library(DT)
library(lubridate)
library(forecast)
library(gpx)
library(geosphere)
library(zoo)
library(scales)
library(leaflet)
#remotes::install_github("hrbrmstr/daybrdayeak")
library(daybreak)
library(shinyTime)
library(shinydashboard)
#remotes::install_github("onofriAndreaPG/aomisc")
library(aomisc)


# Define UI for application that draws a histogram
ui <- dashboardPage(

    # Application title
  dashboardHeader(title = "AUDAX Calculator"),

    # Sidebar with a slider input for number of bins 
  dashboardSidebar(
    sidebarMenu(
      menuItem(text = "Basic Settings", startExpanded = TRUE, icon = icon("cog"),
            selectInput("preset", label = "Preset Rides", selected = "None", choices = 
                            c("None",
                              "LEL 2022",
                              "PBP 2019",
                              "LEJOG Stew 2019",
                              "Cambridge Pork Pie 200",
                              "Fenland Friends 600 2022")),
            radioButtons("distanceUnit","Distance Units", choices = c("KM","Miles")),
            numericInput("total.distance",     "Distance",                 value = 200,    min = 1),
            numericInput("total.time.allowed", "Maximum time (hours)",     value = 10,     min = 1),
            numericInput("target.speed","Target Speed (mph)",              value = 15,      min = 1),
            numericInput("rider.weight",       "Rider Weight (KG)",        value = 95,      min = 1),
            numericInput("bike.weight",        "Bike Weight (KG)",         value = 20,      min = 1)),
      menuItem(text = "Advanced Settings", icon = icon("cogs"),
            numericInput("height.cm",          "Rider Height (CM)",        value = 185,     min = 1),
            numericInput("age",                "Rider Age",                value = 44,      min = 1),
            numericInput("ftp",                "Rider FTP",                value = 297,     min = 1),
            numericInput("grade",              "Average Grade (%)",        value = 0.81,     min = 0, max=100),
            numericInput("headwind",           "Headwind (mph)",           value = 0, min = 0, max = 100),
            numericInput("gain",               "Height Gain (metres)",     value = 1900),
            numericInput("rolling.resistance", "Rolling Resistance (Crr)", value = 0.005,   min = 0),
            numericInput("drag",               "Drag coefficient (Cd)",    value = 0.63,    min = 0),
            numericInput("area",               "Frontal Area (M2)",        value = 0.509,   min = 0),
            numericInput("rho",                "Air density Rho (kg/m3)",  value = 1.22601, min = 0),
            numericInput("dt.loss",            "Drive Train Loss (%)",     value = 2,       min = 0, max = 100)),
      menuItem(text = "Other Settings", icon = icon("compass"),
            numericInput("break.every.x.hours",          "Break very X hours",        value = 2.5,     min = 0),
            dateInput("date", "Date:", weekstart = 1),
            timeInput("start.time", "Start Time:", seconds = FALSE, value = strptime("08:00:00", "%T")),
            numericInput("lat", "Latitude", value = 54.093409),
            numericInput("long", "Longitude", value = , -2.89479))
        )),
        dashboardBody(
           tabBox(width = "100%",
           tabPanel("Planning", width = "100%",
                    bsCollapse(open = c("Summary Text", "Ride Summary"), multiple = TRUE,
                      bsCollapsePanel("Summary Text",
                    box(verbatimTextOutput("summaryText"), width = "100%")),
                    bsCollapsePanel("Day/Night",
                    box(
                        plotOutput("timeOfDay", height = 150), 
                        shinydashboard::valueBoxOutput("sunTimeValue"),
                        shinydashboard::valueBoxOutput("dayTimeValue"), 
                        width = "100%")),
                    bsCollapsePanel("Ride Summary",
                    box(
                        shinydashboard::valueBoxOutput("rideDistance"),
                        shinydashboard::valueBoxOutput("rideElevation"),
                        shinydashboard::valueBoxOutput("difficultyValue"),
                        shinydashboard::valueBoxOutput("difficultyGradeValue"),
                        width = "100%")),
                    bsCollapsePanel("Ride Metrics",
                   box(
                     shinydashboard::valueBoxOutput("powerValue"),
                   shinydashboard::valueBoxOutput("caloriesValue"),
                   shinydashboard::valueBoxOutput("cyclingTimeValue"),
                   shinydashboard::valueBoxOutput("totalTimeValue"),
                   shinydashboard::valueBoxOutput("restingTimeValue"),
                   shinydashboard::valueBoxOutput("moveRatioValue"),
                   width = "100%")),
                   bsCollapsePanel("Suggested Values",
                   box(
                     shinydashboard::valueBoxOutput("suggestedSpeedValue"),
                     shinydashboard::valueBoxOutput("suggestedMoveRatio"),
                     shinydashboard::valueBoxOutput("suggestedTotalTime")
                     , width = "100%"))
                   )),
           tabPanel("Grids",
                    tabBox(width = "100%",
           tabPanel("Mean Speed",plotOutput("meanSpeed")),
           tabPanel("Total Time",plotOutput("totalTime")),
           tabPanel("Resting Time",plotOutput("RestingTime")),
           tabPanel("Move Ratio", plotOutput("moveRatio")),
           tabPanel("Average Power",plotOutput("averagePower")),
           tabPanel("Normalised Power",plotOutput("normPower")),
           tabPanel("Intensity Factor",plotOutput("intensityFactor")),
           tabPanel("Total Calories",plotOutput("caloriesTotal")),
           tabPanel("TSS",plotOutput("tss")))),
           tabPanel("Power Pie", plotOutput("powerPie")),
           tabPanel("TSS vs Rest",plotlyOutput("tssRest")),
           #tabPanel("Calories", plotOutput("calories")),
           tabPanel("Power Zone Table", verbatimTextOutput("ftpText"), dataTableOutput("zoneTable")),
           tabPanel("Activities",
             fileInput("file1", "Choose CSV File Exported from Elevate - All Fields",
                       multiple = FALSE,
                       accept = c("text/csv",
                                  "text/comma-separated-values,text/plain",
                                  ".csv")),
             tabBox(width = "100%",
               tabPanel("Fitness and Freshness", 
           numericInput("forcast.length", "Forcast Length", value = 14, min = 0, max = 365),
           numericInput("history.length", "Plot last X days", value = 60, min = 7),
           plotOutput("freshness")))),
           tabPanel("PlotGPX",
           fileInput("gpxFile", "Choose GPX File",
           multiple = FALSE,
           accept = c(".gpx")),
           textOutput("gpxSummary"),
           leafletOutput("leafletPlot"),
           numericInput("window.size", "Elevation Smoothing Window (metres)", value = 250, min = 1),
           plotOutput("elePlot")))
))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
  digitalTimeConvert <- function(x, type = "time"){
    if (type %in% c("time","duration") %>% not) stop("Type should be one of time or duration")
    if (type == "time"){
      paste0(floor(x),":",sprintf("%02d",round((x-floor(x)) * 60,0)))
    } else {
      paste0(floor(x),"h",round((x-floor(x)) * 60,0),"m")
    }
  }  
  
  #https://stackoverflow.com/questions/40039903/r-add-th-rd-and-nd-to-dates
  append_date_suffix <- function(dates){
    dayy <- day(dates)
    suff <- case_when(dayy %in% c(11,12,13) ~ "th",
                      dayy %% 10 == 1 ~ 'st',
                      dayy %% 10 == 2 ~ 'nd',
                      dayy %% 10 == 3 ~'rd',
                      TRUE ~ "th")
    paste0(dayy, suff)
  }
  
#'  Turn a date object in to a long string e.g. Monthday 11th January 2022
  longDate <- function(date){
    paste(
      wday(date, label = TRUE, abbr = FALSE), 
      append_date_suffix(date), month(date, label = TRUE, abbr = FALSE), 
      year(date))
  }
  
  distance <- reactive({
        switch(input$distanceUnit,
               KM = input$total.distance/1.609,
               Miles = input$total.distance)
    })
  
  getSunTimes <- function(){
    daybreak::sun_rise_set(date = input$date, lon = input$long, lat = input$lat) %>% map(~digitalTimeConvert(.x))
  }
    
    calculatePowerFromSpeed <- function(velocity){
      velocity.mps <- (velocity * 1.609) / 3.6
      windspeed.mps <- (input$headwind * 1.609) / 3.6
      f.gravity <-  9.8067 * sin(tanh(input$grade/100))*(input$rider.weight+input$bike.weight)
      f.rolling <-  9.8067 * cos(tanh(input$grade/100))* (input$rider.weight + input$bike.weight) * input$rolling.resistance
      f.drag <- 0.5 * input$drag * input$area * input$rho * ((velocity.mps+windspeed.mps)^2)
      f.resist <- f.gravity + f.rolling + f.drag
      p.legs <- (((1 - (input$dt.loss / 100))^-1) * f.resist * velocity.mps) %>% round(1)
      p.rolling <- f.rolling %>% multiply_by(velocity.mps) %>% round(1)
      p.gravity <- f.gravity %>% multiply_by(velocity.mps) %>% round(1)
      p.drag <- f.drag %>% multiply_by(velocity.mps) %>% round(1)
      p.dt <- p.legs %>% multiply_by (input$dt.loss / 100) %>% round(2)
      return(data.table(p.legs,p.rolling,p.gravity,p.drag,p.dt))
    }
    
    
    observeEvent(input$preset,{
        if (input$preset == "LEL 2022"){
            updateRadioButtons(session, inputId = "distanceUnit", selected = "KM")
            updateNumericInput(session, inputId = "total.distance", value = 1500)
            updateNumericInput(session, inputId = "total.time.allowed", value = 125)
            updateNumericInput(session, inputId = "gain", value = 12200)
            updateDateInput(session, inputId = "date", value = "2022-08-07")
        }
        if (input$preset == "PBP 2019"){
            updateRadioButtons(session, inputId = "distanceUnit", selected = "KM")
            updateNumericInput(session, inputId = "total.distance", value = 1200)
            updateNumericInput(session, inputId = "total.time.allowed", value = 90)
            updateNumericInput(session, inputId = "gain", value = 12235 )
            updateDateInput(session, inputId = "date", value = "2019-08-18")
        }
        
        if (input$preset == "LEJOG Stew 2019"){
            updateRadioButtons(session, inputId = "distanceUnit", selected = "Miles")
            updateNumericInput(session, inputId = "total.distance", value = 974)
            updateNumericInput(session, inputId = "total.time.allowed", value = 188)
            updateNumericInput(session, inputId = "gain", value = 18290)
            updateDateInput(session, inputId = "date", value = "2019-07-05")
        }  
      
        if (input$preset == "Fenland Friends 600 2022"){
          updateRadioButtons(session, inputId = "distanceUnit", selected = "KM")
          updateNumericInput(session, inputId = "total.distance", value = 600)
          updateNumericInput(session, inputId = "total.time.allowed", value = 40)
          updateNumericInput(session, inputId = "gain", value = 1073)
          updateDateInput(session, inputId = "date", value = "2022-06-25")
          updateTimeInput(session, inputId = "start.time", value =  strptime("06:00:00", "%T"))
          
        }
        
        if (input$preset == "Cambridge Pork Pie 200"){
            updateRadioButtons(session, inputId = "distanceUnit", selected = "KM")
            updateNumericInput(session, inputId = "total.distance", value = 214)
            updateNumericInput(session, inputId = "total.time.allowed", value = 13.5)
            updateNumericInput(session, inputId = "gain", value = 1900)
            updateDateInput(session, inputId = "date", value = "2022-03-19")
        }
    })
    
    observeEvent(input$gain,{
    updateNumericInput(session, inputId = "grade", value =((input$gain/(distance()*1609))*100) %>% round(2))
           })
 
#    observeEvent(input$total.distance,{
#       updateNumericInput(session, inputId = "gain", value =(input$grade/100)*(distance()*1609))
#   })
    
        
    tmp <- reactive({
        BMR <-  66 + (13.7 * input$rider.weight) + (5 * input$height.cm) - (6.8 * input$age)
    daily.calories <- BMR * 1.2
    break.per.hour <- seq(0, 45, 2.5)
    average.cycling.speed <- c(input$target.speed, seq(8,25,1)) %>% sort
    tmp <- expand.grid(average.cycling.speed, break.per.hour) %>% 
        set_colnames(c("Average Cycling Speed (mph)", "Break Per Hour (minutes)")) %>% 
        mutate(`Average Travel Time (mph)`=`Average Cycling Speed (mph)` * (1 - (`Break Per Hour (minutes)` / 60))) %>% 
        mutate(`Cycling Time (hours)`= distance()/`Average Cycling Speed (mph)`) %>% 
        mutate(`Resting Time (hours)` = `Cycling Time (hours)` * (`Break Per Hour (minutes)` / 60)) %>% 
        mutate(`Total Time (hours)` = distance() / `Average Travel Time (mph)`) %>% 
        mutate(`Complete in Time` = `Total Time (hours)`<= input$total.time.allowed) %>% 
        mutate("Average Power (watts)" = `Average Cycling Speed (mph)`  %>% map_dbl(~calculatePowerFromSpeed(.x)$p.legs)) %>% 
        mutate("Total Calories (kcal)" = (`Average Power (watts)` * `Cycling Time (hours)` * 3.6) + ((`Total Time (hours)`/24) * daily.calories)) 
    
     tmp$`Normalized Power (watts)` <- sapply(1:nrow(tmp), function(x) weighted.mean(c(tmp$`Average Power (watts)`[x],0), w = c(tmp$`Cycling Time (hours)`[x], tmp$`Resting Time (hours)`[x])))
     tmp$`Intensity Factor` <- tmp$`Normalized Power (watts)` / input$ftp
     tmp$`TSS` <- tmp$`Total Time (hours)` * ((tmp$`Intensity Factor`^2) * 100)
    return(tmp)
    })
    ## TSS = (sec x NP® x IF®)/(FTP x 3600) x 100
    
    output$powerValue <- shinydashboard::renderValueBox({
      power <- tmp() %>% data.table %>% 
        .[(`Complete in Time`)] %>% 
        .[`Average Cycling Speed (mph)` == input$target.speed,] %>% 
        .[which.max(`Break Per Hour (minutes)`),`Average Power (watts)`] %>% 
        round(0)
      shinydashboard::valueBox(value = power, subtitle = "Power", color = "yellow", icon = icon("battery-half"))
    })
    
    output$caloriesValue <- shinydashboard::renderValueBox({
      power <- tmp() %>% data.table %>% 
        .[(`Complete in Time`)] %>% 
        .[`Average Cycling Speed (mph)` == input$target.speed,] %>% 
        .[which.max(`Break Per Hour (minutes)`),`Total Calories (kcal)`] %>% 
        round(0)
      shinydashboard::valueBox(value = power, subtitle = "Calories", color = "yellow", icon = icon("hamburger"))
    })
    
    output$totalTimeValue <- shinydashboard::renderValueBox({
      time <- tmp() %>% data.table %>% 
        .[(`Complete in Time`)] %>% 
        .[`Average Cycling Speed (mph)` == input$target.speed,] %>% 
        .[which.max(`Break Per Hour (minutes)`),`Total Time (hours)`] %>% 
        digitalTimeConvert()
      shinydashboard::valueBox(value = time, subtitle = "Total Time", color = "yellow", icon = icon("clock"))
    })
    
    output$cyclingTimeValue <- shinydashboard::renderValueBox({
      time <- tmp() %>% data.table %>% 
        .[(`Complete in Time`)] %>% 
        .[`Average Cycling Speed (mph)` == input$target.speed,] %>% 
        .[which.max(`Break Per Hour (minutes)`),`Cycling Time (hours)`] %>% 
        digitalTimeConvert()
      shinydashboard::valueBox(value = time, subtitle = "Cycling Time", color = "yellow", icon = icon("bicycle"))
    })
    
    output$restingTimeValue <- shinydashboard::renderValueBox({
      time <- tmp() %>% data.table %>% 
        .[(`Complete in Time`)] %>% 
        .[`Average Cycling Speed (mph)` == input$target.speed,] %>% 
        .[which.max(`Break Per Hour (minutes)`),`Resting Time (hours)`] %>% 
        digitalTimeConvert()
      shinydashboard::valueBox(value = time, subtitle = "Resting Time", color = "yellow", icon = icon("bed"))
    })
    
    
    output$sunTimeValue <- shinydashboard::renderValueBox({
      sun <- daybreak::sun_rise_set(date = input$date, lon = input$long, lat = input$lat) %>% map(~digitalTimeConvert(.x))
      rise <- sun$rise 
      set <- sun$set 
      time <- paste(rise,"/",set)
            shinydashboard::valueBox(value = time, subtitle = "Rise / Set", color = "yellow", icon = icon("sun"))
    })
    
    output$dayTimeValue <- shinydashboard::renderValueBox({
      day.length <- day_length(date = input$date, lon = input$long, lat = input$lat) %>% digitalTimeConvert(type = "duration")
      shinydashboard::valueBox(value = day.length, subtitle = "Day Length", color = "yellow", icon = icon("calendar-day"))
    })
    
    
    output$moveRatioValue <- shinydashboard::renderValueBox({
      time <- tmp() %>% data.table %>% 
        .[(`Complete in Time`)] %>% 
        .[`Average Cycling Speed (mph)` == input$target.speed,] %>% 
        .[which.max(`Break Per Hour (minutes)`),]
     moveRatio <- time[,`Cycling Time (hours)`] %>% divide_by(time[,`Total Time (hours)`]) %>% round(1)
      shinydashboard::valueBox(value = moveRatio, subtitle = "Move Ratio", color = "yellow", icon = icon("percentage"))
    })
    
    
    output$difficultyGradeValue <- shinydashboard::renderValueBox({
      difficultyGrade <-  (distance() %>% multiply_by(1.609) * (input$gain/1000)) %>% divide_by(100) %>% floor() %>% add(1)
      if (difficultyGrade<=6){
      rating <- c("Moderate", "Challenging", "Hard", "V Hard", "Extreme", "Audacious")[difficultyGrade]
      } else {
      rating <-   paste(paste(rep("V",difficultyGrade-6), collapse = " "), "Audacious")
      }
        
      shinydashboard::valueBox(value = rating, subtitle = "Difficulty Rating", color = "yellow", icon = icon("trophy"))
    })
    
    output$difficultyValue <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(value = (distance()*1.609 * (input$gain/1000)) %>% round(0), subtitle = "Difficulty Score", color = "yellow", icon = icon("balance-scale"))
    })
    
    suggestedSpeed.mph <- reactive({
      req(input$file1)
      activities <- loadActivities()
      df <- activities[,.(Speed = max(`Avg Moving Speed (kph)`), difficulty = median(difficulty)), by = difficulty %>% cut_interval(10)] %>% 
        .[,.(Speed,difficulty)]
      model <- drm(Speed ~ difficulty, fct = DRC.asymReg(),
                   data = df)
      pred <- data.table(difficulty= (distance()*1.609 * (input$gain/1000)))
      pred$Speed <- predict(model,newdata = pred)
      return((pred$Speed/1.609) %>% round(1))
    })
    
    suggestedMoveRatio <- reactive({
      req(input$file1)
      activities <- loadActivities()
      df <- activities[,.(MoveRatio = max(`Move Ratio`), difficulty = median(difficulty)), by = difficulty %>% cut_interval(10)] %>% 
        .[,.(MoveRatio,difficulty)]
            model <- drm(MoveRatio ~ difficulty, fct = DRC.asymReg(),
                   data = df)
      pred <- data.table(difficulty= (distance()*1.609 * (input$gain/1000)))
      model.moveRatio <- drm(MoveRatio ~ difficulty, fct = DRC.asymReg(), data = df)
      pred$MoveRatio <- predict(model.moveRatio,newdata = pred)
      suggestedMoveRatio <- pred$MoveRatio %>% round(1)
      return(suggestedMoveRatio)
    })
    
    suggestedTotalTime <- reactive({
      req(input$file1)
      distance() %>% divide_by(suggestedSpeed.mph()) %>%  divide_by(suggestedMoveRatio()) %>% digitalTimeConvert
    })
    
    output$suggestedSpeedValue <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(value = suggestedSpeed.mph(), subtitle = "Suggested Speed (mph)", color = "green", icon = icon("tachometer-alt"))
    })
    
    output$suggestedMoveRatio <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(value = suggestedMoveRatio(), subtitle = "Suggested Move Ratio", color = "green", icon = icon("percentage"))
    })
    
    output$suggestedTotalTime <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(value = suggestedTotalTime(), subtitle = "Suggested Total Time", color = "green", icon = icon("clock"))
    })
    
    output$rideDistance <- shinydashboard::renderValueBox({
      display.value <- switch(input$distanceUnit,
             KM = paste(round(distance()*1.609,1),"km"),
             Miles = paste(distance() %>% round(1), "miles"))
      shinydashboard::valueBox(value = display.value, subtitle = "Distance", color = "yellow", icon = icon("ruler"))
    })
    
    output$rideElevation <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(value = paste(input$gain,"m"), subtitle = "Elevation", color = "yellow", icon = icon("mountain"))
    })
  
    
    output$summaryText <- renderText({
      power <- calculatePowerFromSpeed(input$target.speed)
        tmp <- tmp() %>% data.table
        subset <- tmp[(`Complete in Time`)] %>% 
            .[`Average Cycling Speed (mph)` == input$target.speed,] %>% 
            .[which.max(`Break Per Hour (minutes)`),]
        if (nrow(subset)){
          hours.rest <- subset[,`Resting Time (hours)`] %>% floor
          minutes.rest <- ((subset[,`Resting Time (hours)`] - hours.rest) * 60) %>% round(0)
          number.of.breaks <- floor(( input$total.time.allowed /input$break.every.x.hours)-1)
          length.of.breaks.minutes  <- (subset[,`Resting Time (hours)`] / number.of.breaks) %>% multiply_by(60) %>% round(0)
         dark.light <- ifelse(subset[,`Total Time (hours)`]>day_length(lat = input$lat, lon = input$long, date = input$date), 
                yes = "This ride will require cycling in darkness", no = "This ride can be completed in daylight hours")

        c(" Cycling", distance() %>% round(0), "miles (",distance()*1.609,"km ) in", input$total.time.allowed, "hours requires a total average speed of", distance() %>% divide_by(input$total.time.allowed) %>% round(2), "mph (",distance() %>% divide_by(input$total.time.allowed) %>% multiply_by(1.609) %>% round(2), "kph)\n",
          "Cycling at", input$target.speed, "mph(",input$target.speed %>% multiply_by(1.609) %>% round(1),"kph) will allow for ", hours.rest ,"hours", minutes.rest, "minutes rest (", subset[,`Break Per Hour (minutes)`], "mins per hour), completing the route in", subset[,`Total Time (hours)`] %>% round(1), "hours","\n",
          "Taking a break every", input$break.every.x.hours, "hours allows for", number.of.breaks, "breaks of ", length.of.breaks.minutes," minutes each\n",
          "This will require an average of", subset[,`Average Power (watts)`] %>% round(0),"watts and burn", subset[,`Total Calories (kcal)`] %>% round(0), "calories\n",
          "Assuming a headwind of",input$headwind,"mph and a total of",input$gain %>% round(0), "metres climbing\n",
          "On",input$date %>% longDate(),"at the specified location the day is",  day_length(lat = input$lat, lon = input$long, date = input$date) %>% digitalTimeConvert(type = "duration"),"long\n",
          "The sun rises at",getSunTimes()$rise,"and sets at",getSunTimes()$set,"\n",
          dark.light
        )
         
        } else {
            c("Increase Target Speed")
        }
    })
    
    output$timeOfDay <- renderPlot({
      start.time <- input$start.time$hour + (input$start.time$min/60)
      total.time <- tmp() %>% data.table %>% 
        .[(`Complete in Time`)] %>% 
        .[`Average Cycling Speed (mph)` == input$target.speed,] %>% 
        .[which.max(`Break Per Hour (minutes)`),] %>% 
        pull(`Total Time (hours)`)
      total.days <- total.time %>% add(start.time) %>% divide_by(24) %>% floor %>% add(1)
      
      cycling.times <- data.table(begin.cycling=start.time, duration = total.time)
      cycling.times[,end.cycling:=begin.cycling + duration]
    
      
      twilights <- c("Night","Astronomical","Nautical","Civil")
      
      day.length <- day_length(date = input$date, lon = input$long, lat = input$lat) %>% digitalTimeConvert(type = "duration")
      sun <- daybreak::sun_rise_set(date = input$date, lon = input$long, lat = input$lat) %>% map(~digitalTimeConvert(.x))
      ## paste0("Length: ", day.length, " / Rise: ",sun$rise, " / Set: ",sun$set)  
      light <- data.table(start.time =
                            c(0,
                              astronomical_twilight(input$date, lon = input$long, lat = input$lat)$start,                
                              nautical_twilight(input$date, lon = input$long, lat = input$lat)$start,
                              civil_twilight(input$date, lon = input$long, lat = input$lat)$start,                 
                              sun_rise_set(input$date, lon = input$long, lat = input$lat)$rise,               
                              sun_rise_set(input$date, lon = input$long, lat = input$lat)$set,
                              civil_twilight(input$date, lon = input$long, lat = input$lat)$end,                
                              nautical_twilight(input$date, lon = input$long, lat = input$lat)$end,
                              astronomical_twilight(input$date, lon = input$long, lat = input$lat)$end,
                              24
                            ),
                          time.of.day = c(twilights,"Day",rev(twilights),"Night"))
      light <- light[!is.na(start.time),]
      light[,end.time:=c(start.time[-1],24)]
      light <- light[-nrow(light),] ## remove last line, don't need it now
      
      light <- lapply(1:total.days, function(x) light %>% 
                        mutate(day=x) %>% 
                        mutate(start.time=start.time+(24*(x-1))) %>% 
                        mutate(end.time=end.time+(24*(x-1)))) %>% 
        rbindlist()
      light$time.of.day <- factor(light$time.of.day, levels = c("Night","Astronomical", "Nautical", "Civil","Day"))
      
      cols <- c(rgb(222/255,233/255,253/255), rgb(140/255,164/255,207/255), rgb(80/255,116/255,182/255), rgb(42/255,62/255,99/255), rgb(32/255,37/255,44/255)) %>% rev
      
      light %>% 
        ggplot() + 
        geom_rect(aes(xmin=start.time, xmax = end.time, fill = time.of.day), ymin=0,ymax=1) +
        geom_rect(data = cycling.times, aes(xmin = begin.cycling, xmax = end.cycling), fill = col2hcl("yellowgreen"), ymin=0.25, ymax=0.75, inherit.aes = FALSE) +
        scale_fill_manual(aesthetics = "fill", values = cols) + 
        xlab("Hours") +
        ggtitle(paste("Starting at",start.time %>% digitalTimeConvert()))
      
    })
    
    #' Mean Speed ####
    output$meanSpeed <- renderPlot({
        tmp() %>% 
        ggplot(aes(x = `Break Per Hour (minutes)`, y = `Average Cycling Speed (mph)`)) + 
        geom_tile(aes(fill = `Average Travel Time (mph)`)) +
        geom_text(aes(label = `Average Travel Time (mph)` %>% round(1), col = `Complete in Time`)) + 
        scale_colour_manual(values = c("red", "white")) +
#        theme(legend.position = "none") + 
            scale_x_continuous(n.breaks = tmp()$`Break Per Hour (minutes)` %>% unique %>% length) + 
            scale_y_continuous(n.breaks = tmp()$`Average Cycling Speed (mph)` %>% unique %>% length) + 
            ggtitle("Average Overall Speed")
    })
    
    #' Total Time #### 
    output$totalTime <- renderPlot({
        tmp() %>% 
            ggplot(aes(x = `Break Per Hour (minutes)`, y = `Average Cycling Speed (mph)`)) + 
            geom_tile(aes(fill = `Total Time (hours)`)) +
            geom_text(aes(label = `Total Time (hours)` %>% round(1), col = `Complete in Time`)) + 
            scale_colour_manual(values = c("red", "white")) +
            #        theme(legend.position = "none") + 
            scale_x_continuous(n.breaks = tmp()$`Break Per Hour (minutes)` %>% unique %>% length) + 
            scale_y_continuous(n.breaks = tmp()$`Average Cycling Speed (mph)` %>% unique %>% length) + 
            ggtitle("Total Time (Hours)")
    })
    
    output$RestingTime <- renderPlot({
        tmp() %>% 
            ggplot(aes(x = `Break Per Hour (minutes)`, y = `Average Cycling Speed (mph)`)) + 
            geom_tile(aes(fill = `Resting Time (hours)`)) +
            geom_text(aes(label = `Resting Time (hours)` %>% round(1), col = `Complete in Time`)) + 
            scale_colour_manual(values = c("red", "white")) +
            #        theme(legend.position = "none") + 
            scale_x_continuous(n.breaks = tmp()$`Break Per Hour (minutes)` %>% unique %>% length) + 
            scale_y_continuous(n.breaks = tmp()$`Average Cycling Speed (mph)` %>% unique %>% length) + 
            ggtitle("Resting Time (Hours)")
    })
    
    output$moveRatio <- renderPlot({
      tmp() %>% 
        ggplot(aes(x = `Break Per Hour (minutes)`, y = `Average Cycling Speed (mph)`)) + 
        geom_tile(aes(fill = (`Cycling Time (hours)`/`Total Time (hours)`) %>% round(1))) +
        geom_text(aes(label =  (`Cycling Time (hours)`/`Total Time (hours)`) %>% round(1), col = `Complete in Time`)) + 
        scale_colour_manual(values = c("red", "white")) +
        #        theme(legend.position = "none") + 
        scale_x_continuous(n.breaks = tmp()$`Break Per Hour (minutes)` %>% unique %>% length) + 
        scale_y_continuous(n.breaks = tmp()$`Average Cycling Speed (mph)` %>% unique %>% length) + 
        ggtitle("Move Ratio")
    })
    
    output$averagePower<- renderPlot({
        tmp() %>% 
            ggplot(aes(x = `Break Per Hour (minutes)`, y = `Average Cycling Speed (mph)`)) + 
            geom_tile(aes(fill = `Average Power (watts)`)) +
            geom_text(aes(label = `Average Power (watts)` %>% round(0), col = `Complete in Time`)) + 
            scale_colour_manual(values = c("red", "white")) +
            #        theme(legend.position = "none") + 
            scale_x_continuous(n.breaks = tmp()$`Break Per Hour (minutes)` %>% unique %>% length) + 
            scale_y_continuous(n.breaks = tmp()$`Average Cycling Speed (mph)` %>% unique %>% length) + 
            ggtitle("Average Power (Watts)")
    })
    
    output$normPower<- renderPlot({
        tmp() %>% 
            ggplot(aes(x = `Break Per Hour (minutes)`, y = `Average Cycling Speed (mph)`)) + 
            geom_tile(aes(fill = `Normalized Power (watts)`)) +
            geom_text(aes(label = `Normalized Power (watts)` %>% round(0), col = `Complete in Time`)) + 
            scale_colour_manual(values = c("red", "white")) +
            #        theme(legend.position = "none") + 
            scale_x_continuous(n.breaks = tmp()$`Break Per Hour (minutes)` %>% unique %>% length) + 
            scale_y_continuous(n.breaks = tmp()$`Average Cycling Speed (mph)` %>% unique %>% length) + 
            ggtitle("Normalised Power (Watts)")
    })
    
    output$caloriesTotal <- renderPlot({
        tmp() %>% 
            ggplot(aes(x = `Break Per Hour (minutes)`, y = `Average Cycling Speed (mph)`)) + 
            geom_tile(aes(fill = `Total Calories (kcal)`)) +
            geom_text(aes(label =`Total Calories (kcal)` %>% divide_by(1000) %>% round(1), col = `Complete in Time`)) + 
            scale_colour_manual(values = c("red", "white")) +
            #        theme(legend.position = "none") + 
            scale_x_continuous(n.breaks = tmp()$`Break Per Hour (minutes)` %>% unique %>% length) + 
            scale_y_continuous(n.breaks = tmp()$`Average Cycling Speed (mph)` %>% unique %>% length) + 
            ggtitle("Total Calories 1000 kcal, e.g 2.5 = 2500kcal")
    })
    
    output$intensityFactor <- renderPlot({
        tmp() %>% 
            ggplot(aes(x = `Break Per Hour (minutes)`, y = `Average Cycling Speed (mph)`)) + 
            geom_tile(aes(fill = `Intensity Factor`)) +
            geom_text(aes(label =`Intensity Factor` %>% round(1), col = `Complete in Time`)) + 
            scale_colour_manual(values = c("red", "white")) +
            #        theme(legend.position = "none") + 
            scale_x_continuous(n.breaks = tmp()$`Break Per Hour (minutes)` %>% unique %>% length) + 
            scale_y_continuous(n.breaks = tmp()$`Average Cycling Speed (mph)` %>% unique %>% length) + 
            ggtitle("Intensity Factor")
    })
    
    output$tss <- renderPlot({
        tmp() %>% 
            ggplot(aes(x = `Break Per Hour (minutes)`, y = `Average Cycling Speed (mph)`)) + 
            geom_tile(aes(fill = `TSS`)) +
            geom_text(aes(label =`TSS` %>% round(0), col = `Complete in Time`)) + 
            scale_colour_manual(values = c("red", "white")) +
            #        theme(legend.position = "none") + 
            scale_x_continuous(n.breaks = tmp()$`Break Per Hour (minutes)` %>% unique %>% length) + 
            scale_y_continuous(n.breaks = tmp()$`Average Cycling Speed (mph)` %>% unique %>% length) + 
            ggtitle("Training Stress Score ")
    })
    
    output$tssRest <- renderPlotly({
        p <- tmp() %>% 
            ggplot(aes(x = `TSS`, y = `Resting Time (hours)`)) + 
            geom_point(aes(col = `Complete in Time`,
                           text =  paste(`Break Per Hour (minutes)`,"rest", `Average Cycling Speed (mph)`,"mph"))) +
                        scale_colour_manual(values = c("red", "black")) +
            ggtitle("TSS vs Total Rest Time") 
        ggplotly(p)
    })

    output$calories <- renderPlot({
        tmp() %>%  
            ggplot(aes( x= `Average Cycling Speed (mph)`, y = `Total Calories (kcal)`)) +
            geom_point(aes(col = `Complete in Time`)) + 
            scale_colour_manual(values = c("red", "black")) + 
            facet_wrap(~`Break Per Hour (minutes)`)
    })
    
    
    output$powerPie <- renderPlot({
        power <- calculatePowerFromSpeed(input$target.speed)
        p <- power[,names(power) != "p.legs",with = FALSE] %>% unlist
        names(p) %<>% str_remove("p\\.")
        pie(p, labels = paste(names(p), p %>% divide_by(sum(p)) %>% multiply_by(100) %>% round(1),"%"))
    })
    
    output$ftpText <- renderText(str_c("FTP of ",input$ftp, ". Endurance pace is around ",(mean(c(0.56,0.75)*input$ftp) %>% round(0))))
    
    output$zoneTable <- renderDataTable({
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
            `Min Power` = input$ftp %>% multiply_by(zone.lower) %>% round(1),
            `Max Power` = input$ftp %>% multiply_by(zone.upper) %>% round(1))
        resultsTable[,`Mid Power`:=((`Min Power` + `Max Power`)/2) %>% round(0)]
        resultsTable %>% data.frame
    })
    
    getGPX <- reactive({
      req(input$gpxFile)
      gpx::read_gpx(input$gpxFile$datapath)$tracks[[1]] %>% data.table
    })
    
    output$leafletPlot <- renderLeaflet({
      req(input$gpxFile)
      x <- getGPX()
      leaflet() %>% addTiles() %>% addPolylines(lng = x$Longitude, lat = x$Latitude) %>%
        addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
        addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
        addLayersControl(position = 'topright',
                         baseGroups = c("Topographical", "Satellite"),
                         options = layersControlOptions(collapsed = FALSE))
      })
    
    parseGPX <- reactive({
      tmp <- getGPX()
      tmp$distance.metres <- c(0,geosphere::distHaversine(p1=tmp[,c("Longitude","Latitude")]))
      tmp[,distance.km:=tmp$distance.metres/1000]
      tmp[,distance.mile:=tmp$distance.km/1.609]
      tmp[,totalDistance:=distance.metres %>% cumsum]
      ele.change <- tmp$Elevation %>% diff()

      updateRadioButtons(session, inputId = "distanceUnit", selected = "KM")
      updateNumericInput(session, inputId = "total.distance", value =  tmp$distance.km %>% sum() %>% round(1))
      updateNumericInput(session, inputId = "gain", value =  ele.change[ele.change > 0.2] %>% sum)
      
      tmp[,window:=cut_interval(totalDistance, length = input$window.size)]
      tmp[,windowGain:=Elevation %>% diff %>% keep(Elevation %>% diff %>% is_greater_than(0)) %>% sum(),by = window]
      tmp[,windowLoss:=Elevation %>% diff %>% keep(Elevation %>% diff %>% is_less_than(0)) %>% sum(),by = window]
      tmp[,windowDistance:= distance.metres %>% sum, by = window]
      tmp[,windowElevation:= Elevation %>% mean, by = window]
      tmp[,windowGrade:= windowGain %>% add(windowLoss) %>% divide_by(windowDistance) %>% multiply_by(100)]
      tmp[,windowAbsGrade:= windowGain %>% add(windowLoss) %>% divide_by(windowDistance) %>% multiply_by(100) %>% abs]
      return(tmp)
    })
    
    output$gpxSummary <- renderText({
      req(input$gpxFile)
      tmp <- parseGPX()
      ele.change <- tmp$Elevation %>% diff()
      dist.km <- tmp$distance.km %>% sum() %>% round(1)
      ele.gain <- ele.change[ele.change > 0.2] %>% sum
      ele.lost <- ele.change[ele.change < 0.2] %>% sum %>% abs
      
      est.duration <- (dist.km/(input$target.speed * 1.609)) %>% round(1)
      ele.highest <- tmp$Elevation %>% max %>% round(0)
      ele.lowest <- tmp$Elevation %>% min %>% round(0)
      
      paste(dist.km,"km (",tmp$distance.km %>% sum %>% divide_by(1.609) %>% round(1),"miles)","\n",
      ele.gain %>% round(0),"m climbing","\n",
      ele.lost %>% round(0),"m descent","\n",
      ele.highest,"m highest and ", ele.lowest,"m lowest elevation","\n",
      "Moving time is",est.duration,"hours")

      
    })
    
    output$elePlot <- renderPlot({
      req(input$gpxFile)
      tmp <- parseGPX()
      tmp2 <- tmp %>% 
        filter(windowDistance > 0) %>% 
        select(starts_with("window")) %>% 
        unique 
      tmp2[,windowTotalDistance:=windowDistance %>% cumsum]
      
      tmp2 %>% 
        ggplot(aes(x = windowTotalDistance/1000, y = windowElevation)) +
        #  geom_point(aes(col = windowGrade)) + 
        geom_line(aes(col = windowGrade, group = 1), size = 2) +
        scale_colour_gradient2(low = "green", mid = "black", high = "red") +
        xlab("Distance (km)") + 
        ylab("Elevation") +
        scale_x_continuous(label = comma)
      
    })
    
    loadActivities <- reactive({
      req(input$file1)
      activities <- fread(file = input$file1$datapath, header = TRUE)
      activities[,`Avg Total Speed (kph)`:=`Avg Total Speed (kph)` %>% as.numeric()]
      activities[,`Avg Moving Speed (kph)`:=`Avg Moving Speed (kph)` %>% as.numeric()]
      activities[,`Move Ratio`:=`Move Ratio` %>% as.numeric()]
      activities[,difficulty:= `Distance (km)` * (`Elevation Gain (m)`/1000)]
      return(activities)
    })
    
    output$freshness <- renderPlot({
      req(input$file1)
        activities <- loadActivities()
        
        plotFormAndFitness <- function(activities, forcast.length = 30, history.length = 180) {
            tmp <- data.table(
                date = as.Date(activities$Date), 
                stress = activities$`Power Stress Score` %>% as.numeric(),
                name=activities$Name)
            
            tmp <- merge(tmp,data.table(date=seq(from=min(tmp$date),to=input$date+forcast.length,by = "1 day")), all.y = TRUE)
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
            tmp$timeFilter <- input$date %>% subtract(tmp$date) %>% is_less_than(history.length)
            tmp[,inFuture:=date>input$date]
            
            rects <- data.frame(
                xmin=rep(min(tmp[(timeFilter),date]),3),
                xmax=rep(max(tmp[(timeFilter),date]),3),
                ymin=c(-30,-10,5),
                ymax=c(-10,5,24),
                fill = c("green","orange","blue"),
                stringsAsFactors=FALSE)
            max.form.date <- tmp[(inFuture),] %>% .[form == max(form),date] %>% subtract(input$date)
            p1 <- rects %>% ggplot() +
                geom_rect(data = rects, aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax, fill = fill), alpha = 0.25) +
                geom_line(data = tmp %>% 
                              filter(timeFilter),
                          aes(x = date, y = form, linetype = inFuture)) +
                theme(legend.position = "none") +
                ggtitle(paste("Maximum form in",max.form.date,"days"))
            max.fitness <- tmp[(timeFilter),max(fitness)]
            todays.fitness <- tmp[date==input$date,max(fitness)]
            fit.diff <- todays.fitness %>% subtract(max.fitness) %>% round(2)
            forcast.arima <- cbind(
                data.table(
                    date = seq(from = input$date+1, to = input$date+forcast.length, by = "1 day"),
                    auto.arima(tmp[(timeFilter)&!(inFuture),fitness]) %>% 
                        forecast(h = forcast.length, level = 0.95) %>% as.data.table))
            p2 <- tmp %>% 
                filter(timeFilter) %>% 
                ggplot(aes(x = date, y = fitness)) + 
                geom_line(aes(linetype = inFuture), col = "blue") +
                geom_hline(aes(yintercept = max.fitness), linetype = "dotted") +
                geom_hline(aes(yintercept = todays.fitness), linetype = "dotted") +
                theme(legend.position = "none") + 
                geom_smooth(data=tmp[(timeFilter) & !(inFuture),], alpha = 0.25, fullrange = TRUE ) +
                geom_line(data = forcast.arima, aes(y = `Point Forecast`), col = "blue") +
                geom_ribbon(data = forcast.arima, 
                            aes(x = date, 
                                y = `Point Forecast`, 
                                ymin = `Lo 95`, 
                                ymax = `Hi 95`), linetype=2, alpha = 0.1) +
                ggtitle(paste("Fitness is", fit.diff, "points from maximum in last", history.length, "days"))
            p3 <- tmp %>% 
                filter(timeFilter) %>% 
                ggplot(aes(x = date, y = fatigue)) + 
                geom_line(aes(linetype = inFuture), col = "red") +
                theme(legend.position = "none")
            gridExtra::grid.arrange(p1,p2,p3)
        }
        
        plotFormAndFitness(activities, forcast.length = input$forcast.length, history.length = input$history.length)
        
    }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
