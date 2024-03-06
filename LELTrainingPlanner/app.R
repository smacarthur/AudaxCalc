#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinydashboard)
library(rStrava)
library(lubridate)
library(tidyverse)
library(magrittr)
library(data.table)
oauth <- NULL
comp.activities <- NULL

#' Functions ####

## Authentication ####
authenticate <- function(){
  oauth <<- rStrava::strava_oauth(
    app_name = "My API Application", 
    app_client_id = "14098", 
    app_secret = "0fddc99747c0007a21ad717c0703983dfc8d5643", 
    app_scope = "activity:read_all",
    cache = FALSE)
  message(as_datetime(oauth$credentials$expires_at))
  return(oauth)
}

loadActivitiesFromAPI <- function(){
  comp.activities <- rStrava::get_activity_list(oauth) %>% 
    compile_activities %>% 
    data.table
  ## Filter and Clean Data ####
  message(nrow(comp.activities)," activities")
  comp.activities <- comp.activities[type %in% c("Ride","VirtualRide"),]
  message(nrow(comp.activities)," cycling activities")
  comp.activities <- comp.activities[moving_time>15,]
  message(nrow(comp.activities)," cycling activities after cleanup")
  
  comp.activities[name %>% str_detect("^Wattbike"),type:="VirtualRide"]
  comp.activities[trainer=="TRUE", type:="VirtualRide"]
  
  ## Add derived data ####
  comp.activities[,year:=year(start_date)]
  comp.activities[,isoweek:=isoweek(start_date)]
  comp.activities[,month:=month(start_date)]
  comp.activities[,month_name:=factor(lubridate::month(start_date, label = TRUE))]
  comp.activities[,day:=day(start_date)]
  comp.activities[,kcal:=kilojoules/4.184]
  year.range <<- c(comp.activities[,min(year)] : comp.activities[,max(year)])
  return(comp.activities)
}

getData <- function(){
  return(comp.activities)
}

# Define UI for application that draws a histogram
ui <- dashboardPage(title = ("Strava Training Tracker"), 
                    header = dashboardHeader(),
            sidebar = dashboardSidebar(
            actionButton("auth", "1: Authenticate"),
            actionButton("loadData","2: Load Rides")
        ), body = dashboardBody(
          infoBoxOutput("approvalBox"),
          plotOutput("plotDistByYear")
        )
    )

# Define server logic required to draw a histogram
server <- function(input, output) {

  observeEvent(input$auth, {  
    oauth <<- authenticate()
  })
  
  
  observeEvent(input$loadData, {
   comp.activities <<- loadActivitiesFromAPI()
  })
  
  output$plotDistByYear <- renderPlot({
    comp.activities <- getData()
    comp.activities[,.(totalDistance= sum(distance)), by = year] %>% 
      ggplot(aes(x = year, y = totalDistance)) +
      geom_bar(stat = "identity") +
      scale_x_continuous(breaks = year.range)+
      geom_point(data = comp.activities[isoweek <= isoweek(today()),.(totalDistance= sum(distance)), by = year],
                 , col = "red", size = 3) +
      geom_hline(yintercept = comp.activities[year == year(today()),sum(distance)], col = "red") +
      scale_y_continuous(sec.axis = sec_axis(trans = ~./1.609, name = "Total Distance (miles)"), name = "Total Distance (km)")
  })
  
  output$approvalBox <- renderInfoBox({
    if (!is.null(oauth)){
      colour = "green" 
      } else {
        colour = "red"
      }
    infoBox(
      "Approval", "100%", icon = icon("thumbs-up", lib = "glyphicon"),
      color = colour, fill = TRUE
    )
  })
  
  
}
# Run the application 
shinyApp(ui = ui, server = server)
