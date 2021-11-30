library(shiny)
library(tidyverse)
library(magrittr)
library(plotly)
library(data.table)
library(DT)
library(lubridate)
library(forecast)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("AUDAX Calculator"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            h2("Basic Settings"),
            selectInput("preset", label = "Preset Rides", selected = "None", choices = 
                            c("None",
                              "LEL 2022",
                              "PBP 2019",
                              "LEJOG Stew 2019",
                              "Cambridge Pork Pie 200")),
            radioButtons("distanceUnit","Distance Units", choices = c("KM","Miles")),
            numericInput("total.distance",     "Distance",                 value = 1500,    min = 1),
            numericInput("total.time.allowed", "Maximum time (hours)",     value = 125,     min = 1),
            numericInput("target.speed","Target Speed (mph)",              value = 12,      min = 1),
            numericInput("rider.weight",       "Rider Weight (KG)",        value = 90,      min = 1),
            numericInput("bike.weight",        "Bike Weight (KG)",         value = 20,      min = 1),
            hr(),
            h2("Advanced Settings"),
            numericInput("height.cm",          "Rider Height (CM)",        value = 185,     min = 1),
            numericInput("age",                "Rider Age",                value = 44,      min = 1),
            numericInput("ftp",                "Rider FTP",                value = 280,     min = 1),
            numericInput("grade",              "Average Grade (%)",        value = 0.81,     min = 0, max=100),
            numericInput("gain",               "Height Gain (metres)",     value = 12200),
            numericInput("rolling.resistance", "Rolling Resistance (Crr)", value = 0.005,   min = 0),
            numericInput("drag",               "Drag coefficient (Cd)",    value = 0.63,    min = 0),
            numericInput("area",               "Frontal Area (M2)",        value = 0.509,   min = 0),
            numericInput("rho",                "Air density Rho (kg/m3)",  value = 1.22601, min = 0),
            numericInput("dt.loss",            "Drive Train Loss (%)",     value = 2,       min = 0, max = 100)
        ),
        mainPanel(
           verbatimTextOutput("summaryText"),
           tabsetPanel(
           tabPanel("Mean Speed",plotOutput("meanSpeed")),
           tabPanel("Total Time",plotOutput("totalTime")),
           tabPanel("Resting Time",plotOutput("RestingTime")),
           tabPanel("Average Power",plotOutput("averagePower")),
           tabPanel("Normalised Power",plotOutput("normPower")),
           tabPanel("Intensity Factor",plotOutput("intensityFactor")),
           tabPanel("TSS",plotOutput("tss")),
           tabPanel("Total Calories",plotOutput("caloriesTotal")),
           tabPanel("TSS vs Rest",plotlyOutput("tssRest")),
           tabPanel("Calories", plotOutput("calories")),
           tabPanel("Power Zone Table", verbatimTextOutput("ftpText"), dataTableOutput("zoneTable")),
           tabPanel("Fitness and Freshness", 
           fileInput("file1", "Choose CSV File",
                     multiple = FALSE,
                     accept = c("text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv")),
           numericInput("forcast.length", "Forcast Length", value = 30, min = 0, max = 365),
           numericInput("history.length", "Plot last X days", value = 180, min = 7),
           plotOutput("freshness")))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    distance <- reactive({
        switch(input$distanceUnit,
               KM = input$total.distance/1.609,
               Miles = input$total.distance)
    })
    
    observeEvent(input$preset,{
if (input$preset == "LEL 2022"){
    updateRadioButtons(session, inputId = "distanceUnit", selected = "KM")
    updateNumericInput(session, inputId = "total.distance", value = 1500)
    updateNumericInput(session, inputId = "total.time.allowed", value = 125)
    updateNumericInput(session, inputId = "gain", value = 12200 )
}
        if (input$preset == "PBP 2019"){
            updateRadioButtons(session, inputId = "distanceUnit", selected = "KM")
            updateNumericInput(session, inputId = "total.distance", value = 1200)
            updateNumericInput(session, inputId = "total.time.allowed", value = 90)
            updateNumericInput(session, inputId = "gain", value = 12235 )
        }
        
        if (input$preset == "LEJOG Stew 2019"){
            updateRadioButtons(session, inputId = "distanceUnit", selected = "Miles")
            updateNumericInput(session, inputId = "total.distance", value = 974)
            updateNumericInput(session, inputId = "total.time.allowed", value = 188)
            updateNumericInput(session, inputId = "gain", value = 60007)
        }  
        
        if (input$preset == "Cambridge Pork Pie 200"){
            updateRadioButtons(session, inputId = "distanceUnit", selected = "KM")
            updateNumericInput(session, inputId = "total.distance", value = 214)
            updateNumericInput(session, inputId = "total.time.allowed", value = 13.5)
            updateNumericInput(session, inputId = "gain", value = 1900)
        }
    })
    
    observeEvent(input$gain,{
    updateNumericInput(session, inputId = "grade", value =((input$gain/(distance()*1609))*100) %>% round(2))
           })
 
    observeEvent(input$total.distance,{
       updateNumericInput(session, inputId = "gain", value =(input$grade/100)*(distance()*1609))
   })
        
    tmp <- reactive({
        BMR <-  66 + (13.7 * input$rider.weight) + (5 * input$height.cm) - (6.8 * input$age)
    daily.calories <- BMR * 1.2
    break.per.hour <- seq(0, 45, 2.5)
    average.cycling.speed <- c(input$target.speed, seq(8,25,1)) %>% sort
    calculatePowerFromSpeed <- function(velocity){
        velocity.mps <- (velocity * 1.609) / 3.6
        f.gravity <-  9.8067 * sin(tanh(input$grade/100))*(input$rider.weight+input$bike.weight)
        f.rolling <-  9.8067 * cos(tanh(input$grade/100))* (input$rider.weight + input$bike.weight) * input$rolling.resistance
        f.drag <- 0.5 * input$drag * input$area * input$rho * (velocity.mps^2)
        f.resist <- f.gravity + f.rolling + f.drag
        ((1 - (input$dt.loss / 100))^-1) * f.resist * velocity.mps 
    }
    tmp <- expand.grid(average.cycling.speed, break.per.hour) %>% 
        set_colnames(c("Average Cycling Speed (mph)", "Break Per Hour (minutes)")) %>% 
        mutate(`Average Travel Time (mph)`=`Average Cycling Speed (mph)` * (1 - (`Break Per Hour (minutes)` / 60))) %>% 
        mutate(`Cycling Time (hours)`= distance()/`Average Cycling Speed (mph)`) %>% 
        mutate(`Resting Time (hours)` = `Cycling Time (hours)` * (`Break Per Hour (minutes)` / 60)) %>% 
        mutate(`Total Time (hours)` = distance() / `Average Travel Time (mph)`) %>% 
        mutate(`Complete in Time` = `Total Time (hours)`<= input$total.time.allowed) %>% 
        mutate("Average Power (watts)" = `Average Cycling Speed (mph)` %>% map_dbl(~calculatePowerFromSpeed(.x))) %>% 
        mutate("Total Calories (kcal)" = (`Average Power (watts)` * `Cycling Time (hours)` * 3.6) + ((`Total Time (hours)`/24) * daily.calories)) 
    
     tmp$`Normalized Power (watts)` <- sapply(1:nrow(tmp), function(x) weighted.mean(c(tmp$`Average Power (watts)`[x],0), w = c(tmp$`Cycling Time (hours)`[x], tmp$`Resting Time (hours)`[x])))
     tmp$`Intensity Factor` <- tmp$`Normalized Power (watts)` / input$ftp
     tmp$`TSS` <- tmp$`Total Time (hours)` * ((tmp$`Intensity Factor`^2) * 100)
    return(tmp)
    })
    
    ## TSS = (sec x NP® x IF®)/(FTP x 3600) x 100
    
    output$summaryText <- renderText({
        tmp <- tmp() %>% data.table
        subset <- tmp[(`Complete in Time`)] %>% 
            .[`Average Cycling Speed (mph)` == input$target.speed,] %>% 
            .[which.max(`Break Per Hour (minutes)`),]
        if (nrow(subset)){
        c(" Cycling", distance() %>% round(0), "miles (",distance()*1.609,"km) in", input$total.time.allowed, "hours requires a total average speed of ", distance() %>% divide_by(input$total.time.allowed) %>% round(2), "mph", "\n",
          "Cycling at", input$target.speed, "mph will allow for ", subset[,`Resting Time (hours)`] %>% round(1) ,"hours rest (", subset[,`Break Per Hour (minutes)`], "mins per hour), completing the route in", subset[,`Total Time (hours)`] %>% round(1), "hours","\n",
          "This will require an average of ", subset[,`Average Power (watts)`] %>% round(0),"watts and burn", subset[,`Total Calories (kcal)`] %>% round(0), "calories"
        )
         
        } else {
            c("Increase Target Speed")
        }
    })
    
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
    
    output$averagePower<- renderPlot({
        tmp() %>% 
            ggplot(aes(x = `Break Per Hour (minutes)`, y = `Average Cycling Speed (mph)`)) + 
            geom_tile(aes(fill = `Average Power (watts)`)) +
            geom_text(aes(label = `Average Power (watts)` %>% round(1), col = `Complete in Time`)) + 
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
            geom_text(aes(label = `Normalized Power (watts)` %>% round(1), col = `Complete in Time`)) + 
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
            geom_text(aes(label =`Total Calories (kcal)` %>% round(0), col = `Complete in Time`)) + 
            scale_colour_manual(values = c("red", "white")) +
            #        theme(legend.position = "none") + 
            scale_x_continuous(n.breaks = tmp()$`Break Per Hour (minutes)` %>% unique %>% length) + 
            scale_y_continuous(n.breaks = tmp()$`Average Cycling Speed (mph)` %>% unique %>% length) + 
            ggtitle("Total Calories (kcal)")
    })
    
    output$intensityFactor <- renderPlot({
        tmp() %>% 
            ggplot(aes(x = `Break Per Hour (minutes)`, y = `Average Cycling Speed (mph)`)) + 
            geom_tile(aes(fill = `Intensity Factor`)) +
            geom_text(aes(label =`Intensity Factor` %>% round(2), col = `Complete in Time`)) + 
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
            geom_text(aes(label =`TSS` %>% round(2), col = `Complete in Time`)) + 
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
    
    output$freshness <- renderPlot({
        req(input$file1)
        activities <- fread(file = input$file1$datapath, header = TRUE)
        
        plotFormAndFitness <- function(activities, forcast.length = 30, history.length = 180) {
            tmp <- data.table(
                date = as.Date(activities$Date), 
                stress = activities$`Power Stress Score` %>% as.numeric(),
                name=activities$Name)
            
            tmp <- merge(tmp,data.table(date=seq(from=min(tmp$date),to=today()+forcast.length,by = "1 day")), all.y = TRUE)
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
            tmp$timeFilter <- lubridate::today() %>% subtract(tmp$date) %>% is_less_than(history.length)
            tmp[,inFuture:=date>today()]
            
            rects <- data.frame(
                xmin=rep(min(tmp[(timeFilter),date]),3),
                xmax=rep(max(tmp[(timeFilter),date]),3),
                ymin=c(-30,-10,5),
                ymax=c(-10,5,24),
                fill = c("green","orange","blue"),
                stringsAsFactors=FALSE)
            max.form.date <- tmp[(inFuture),] %>% .[form == max(form),date] %>% subtract(today())
            p1 <- rects %>% ggplot() +
                geom_rect(data = rects, aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax, fill = fill), alpha = 0.25) +
                geom_line(data = tmp %>% 
                              filter(timeFilter),
                          aes(x = date, y = form, linetype = inFuture)) +
                theme(legend.position = "none") +
                ggtitle(paste("Maximum form in",max.form.date,"days"))
            max.fitness <- tmp[(timeFilter),max(fitness)]
            todays.fitness <- tmp[date==today(),max(fitness)]
            fit.diff <- todays.fitness %>% subtract(max.fitness) %>% round(2)
            forcast.arima <- cbind(
                data.table(
                    date = seq(from = today()+1, to = today()+forcast.length, by = "1 day"),
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
