library(shiny)
library(data.table)
library(magrittr)
library(tidyverse)
library(lubridate)
library(ggrepel)
library(shinydashboard)
library(DT)

### DESCRIPTION - This is is designed to plan where to sleep on LEL2025.

## User Parameters

# Define UI for application that draws a histogram
ui <- dashboardPage(

    # Application title
  dashboardHeader(title = "LEL 2025"),

    # Sidebar with a slider input for number of bins 
  dashboardSidebar(
    sidebarMenu(
            #numericInput(inputId = "total_distance_km",         label = "Total Distance (Km) ",                     value = 1530 ),
            #numericInput(inputId = "maximum_time_hours",        label = "Maximum Finish Time (Hours)",              value = 125),
            numericInput(inputId = "target_time_hours",         label = "Target Finish Time (Hours)",               value = 120),
            numericInput(inputId = "target_moving_speed_kph",   label = "Target Moving Speed (Kph)",                value = 20),
            numericInput(inputId = "target_onbike_move_ratio",  label = "On Bike Move Ratio (0-1)",                 value = 0.8, max = 1, min =0, step = 0.05),
            numericInput(inputId = "dont.sleep.if.over",        label = "Dont Sleep if over x*100% Complete (0-1)", value = 0.95, max =1, min =0, step = 0.01),
            numericInput(inputId = "min.dist.before.sleep",     label = "Minimum Distance before first sleep (Km)", value = 300, max = 1530, min =0),
            numericInput(inputId = "min.ctrl.time.hours",       label = "Minimum Time at Controls (Minutes)",       value = 15, max = 1440, min =0, step = 1),
            numericInput(inputId = "wake.up.time.before.start", label = "Hours awake before start (hours)",         value = 2, max = 24, min = 1),
            numericInput(inputId = "start_time_digital",        label = "Start time e.g 6 = 6am, 18.5 = 6:30pm",    value = 6, max = 23, min = 0, step = 0.25),
            numericInput(inputId = "override_sleeps",           label = "More of Less Sleep Breaks",                value = 0, max = 5, min = -5, step = 1)
        )),
        # Show a plot of the generated distribution
    dashboardBody(
      tabBox(width = "100%",
             box(
               shinydashboard::valueBoxOutput("totalCyclingTime"),
               shinydashboard::valueBoxOutput("totalRoadTime"),
               shinydashboard::valueBoxOutput("numberOfSleeps"),
               shinydashboard::valueBoxOutput("maxTimeWithoutSleeps"),
               shinydashboard::valueBoxOutput("maxTimeBetweenCtrls"),
               shinydashboard::valueBoxOutput("totalSleep")
               , width = "100%"),
             tabPanel("Planning", width = "100%",
           plotOutput("mainPlot")),
           tabPanel("Table", width = "100%",
           DTOutput("summaryTable")))
        )
    )


# Define server logic required to draw a histogram
server <- function(input, output) {
  global <- list()
  global$total_distance_km <- 1530
  global$maximum_time_hours <- 125
  global$inHandSpeed <- global$total_distance_km/global$maximum_time_hours
  
  getSuntimes <- reactive({
    cyclingUnits         <- floor(input$target_time_hours / 24) 
    list(
      sunrise = lapply(0:(cyclingUnits), function(d) 
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
    })
  
   makeCtrlsTable <- reactive({
     onbike_cycling_hours <- global$total_distance_km / input$target_moving_speed_kph
     onbike_total_hours   <- (global$total_distance_km / input$target_moving_speed_kph) / input$target_onbike_move_ratio
     onbike_resting_hours <- onbike_total_hours - onbike_cycling_hours
     offbike_total_hours  <- input$target_time_hours - onbike_total_hours
     total_move_ratio     <- onbike_cycling_hours / input$target_time_hours
     onbike_speed         <- global$total_distance_km / onbike_total_hours
     cyclingUnits         <- floor(input$target_time_hours / 24) + input$override_sleeps
     start_time           <- lubridate::as_datetime(str_c("2025-08-03 ",format(as_datetime(duration(input$start_time_digital,"hours")),"%H:%M:%S")))

     ## Table Generation
     ctrls                    <- fread("LEL_2025_Ctrls.csv")
     ctrls[,START:=str_c(START,"_",ID)]
     ctrls$legDistance        <- c(diff(ctrls$Distance), 0)
     ctrls$END                <- c(ctrls$START[2:nrow(ctrls)], NA)
     ctrls$destinationDormBeds <- c(ctrls$DormitoryBeds[2:nrow(ctrls)],NA)
     ctrls[,DormitoryBeds:=NULL]
     ctrls$cumulativeDistance <- ctrls$legDistance + ctrls$Distance
     ctrls[, Distance := NULL]
     ctrls <- ctrls[ID != "C21", ]
     ctrls[, legDuration := legDistance / onbike_speed]
     ctrls[, cumulativeOnBikeDuration := cumsum(legDuration)]
     ctrls[ID %>% str_detect("^C"), controlTime := input$min.ctrl.time.hours/60]
     ctrls[ID %>% str_detect("^C") %>% not, controlTime := 0]
     ctrls[, sleep := 0]
     ctrls[, cumulativeControlTime := cumsum(controlTime)]
     ctrls[, cumulativeTotalTime := cumulativeOnBikeDuration + cumulativeControlTime]
     ctrls[, timeWithOutSleep := cumulativeTotalTime + input$wake.up.time.before.start]
     for (day in 1:cyclingUnits) {
       message(day)
       last.sleep <- ctrls[sleep == 1, START] %>% tail(1)
       if (!length(last.sleep))
         last.sleep <- ctrls$START[1]
       message("Last sleep was at ", last.sleep)
       ctrl.to.sleep.at <- ctrls[match(last.sleep, START):(.N - 1)] %>%
         .[destinationDormBeds != 0,] %>% 
         .[cumulativeDistance >= input$min.dist.before.sleep,] %>% 
         .[which.min(abs((onbike_total_hours / cyclingUnits) - timeWithOutSleep  )), START] # Find closest control to cycling "day" time
       message("Sleep at ", ctrl.to.sleep.at)
       
       if ((ctrls[START == ctrl.to.sleep.at,cumulativeDistance]/global$total_distance_km) < input$dont.sleep.if.over){
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
     ctrls[,mainControl:=ID %>% str_detect("^C")]
     return(ctrls)
   })
  
   output$suntimes <- renderTable({
     suntimes <- getSuntimes()
     suntimes[,start:=start %>% as.character()]
     suntimes[,stop:=stop %>% as.character()]
     suntimes
   })
   
    output$summaryTable <- renderDT({
        # generate bins based on input$bins from ui.R
      ctrls <- makeCtrlsTable()
      ctrls[,arrivalTime:=arrivalTime %>% as.character()]
      ctrls[,departureTime:=departureTime %>% as.character()]
      DT::datatable(data = ctrls,
                    filter = "top",
                    options = list(scrollX = TRUE)
      )
    })
    
    output$totalCyclingTime <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(value = (global$total_distance_km / input$target_moving_speed_kph) %>% round(1), subtitle = "Moving Time", color = "green", icon = icon("clock"))
    })
    
    output$totalRoadTime <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(value =  ((global$total_distance_km / input$target_moving_speed_kph) / input$target_onbike_move_ratio) %>% round(1), subtitle = "On Road Time", color = "green", icon = icon("clock"))
    })
    
    output$numberOfSleeps <- shinydashboard::renderValueBox({
      ctrls <- makeCtrlsTable()
      shinydashboard::valueBox(value =  ctrls$sleep %>% sum, subtitle = "# Sleeps", color = "green", icon = icon("bed"))
    })
   
    output$maxTimeWithoutSleeps <- shinydashboard::renderValueBox({
      ctrls <- makeCtrlsTable()
      shinydashboard::valueBox(value =  max(ctrls$timeWithOutSleep, na.rm = TRUE) %>% round(1), subtitle = "MaxNoSleep", color = "green", icon = icon("bed"))
    })
    
    output$maxTimeBetweenCtrls <- shinydashboard::renderValueBox({
      ctrls <- makeCtrlsTable()
      shinydashboard::valueBox(value =  max(ctrls$legDuration, na.rm = TRUE) %>% round(1), subtitle = "MaxTimeBetweenCtrls", color = "green", icon = icon("clock"))
    })
    
    output$totalSleep <- shinydashboard::renderValueBox({
      ctrls <- makeCtrlsTable()
      shinydashboard::valueBox(value = (ctrls[sleep==1, sum(controlTime)] - (ctrls[,sum(sleep)] * (input$min.ctrl.time.hours/60))) %>% round(1), subtitle = "Total Sleep", color = "green", icon("bed"))
    })
    
    output$mainPlot <- renderPlot({
      ctrls.long <- makeCtrlsTable() %>% 
        pivot_longer(cols = c(arrivalTime, departureTime)) %>% 
        data.table
      suntimes <- getSuntimes()
      start_time       <- lubridate::as_datetime(str_c("2025-08-03 ",format(as_datetime(duration(input$start_time_digital,"hours")),"%H:%M:%S")))

      max.time.line    <- (start_time+duration( global$maximum_time_hours, "hours"))
      target.time.line <- (start_time+duration( input$target_time_hours,   "hours"))

      inHand <- data.table(
        time = start_time + duration(ctrls.long$cumulativeDistance/(global$total_distance_km/global$maximum_time_hours), "hours"),
        distance = ctrls.long$cumulativeDistance)
    
      p <- ctrls.long %>% 
        ggplot(aes(x = as.POSIXct(value), y = cumulativeDistance))+
        geom_line(data=inHand, aes(x = as.POSIXct(time), y = distance), col = "red", linetype="dotted", size = 2) +
        geom_rect(data=suntimes[start<max(ctrls.long$value),], aes(xmin = start, xmax = stop, ymin = 0, ymax = Inf), fill = "black", inherit.aes = FALSE, alpha = 0.25) +
        geom_line(alpha = 0.5, col = "green", size = 3) +
        geom_line(aes(group = ID), col = "green" , size = 3)+
        geom_point(aes(col = name, shape = mainControl)) +
        geom_vline(xintercept = as.POSIXct(target.time.line), linetype = "dotted") +
        geom_vline(xintercept = as.POSIXct(max.time.line)) +
        geom_hline(yintercept = global$total_distance_km) +
        ggrepel::geom_label_repel(data=ctrls.long[sleep==1 & name == "arrivalTime",], aes(label = END)) +
        xlab("Time") + ylab("Distance")
      p
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
