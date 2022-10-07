# columbia_dashboard


library(shiny)
library(shinydashboard)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(plotly)
library(sf)
library(tidyr)
library(htmltools)
library(rgdal)

library(httr)
library(jsonlite)
library(dplyr)
library(stringr)
options(digits=9)


incidents <- read.csv("NYC_incidents_current.csv")
shapeData <- readOGR("./boundary",'boundary')
shapeData <- spTransform(shapeData, CRS("+proj=longlat +ellps=GRS80"))

last_update <- max(incidents$Date)

#UI
ui <- dashboardPage(
    dashboardHeader(title = "Columbia Crime"),
    dashboardSidebar(width = 240,
                     
                     textOutput("text"),
                     selectInput("campus", "Select Campus:",
                                 c("Main", "Manhattanville", "Med School")),
                     
                     selectInput("dataset", "Select Dataset:",
                                 c("Crime", "311")),
                     
                     selectInput("crime_type", "Select Crime Type:",
                                 choices = c("All", unique(incidents$Category))),
                     
                     dateRangeInput("date", "Date range:",
                                    min = "2022-01-01",
                                    start = "2022-09-01",
                                    end   = "2022-12-31")
                     
                     
                     ),
    dashboardBody(
        # Boxes need to be put in a row (or column)
        fluidRow(
            box(width = 9,
                tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
                leafletOutput("map")
            ),
            column(width = 3,
                    box(width = NULL,
                        plotlyOutput("crime_bar")
                        ),
                    box(width = NULL,
                        plotlyOutput("crime_bar2")
                    )
                )
            )
        )
    )



#Server
server <- function(input, output, session) {
    
    output$text <- renderText({paste("Last Update:", last_update) })
    
    observeEvent(input$crime_type, {
        print(input$crime_type)
    })
    
    observeEvent(input$campus, {
        print(input$campus)
    })
    
    observeEvent(input$date, {
        print(input$date)
    })
    


    lat <- reactive({
        if (input$campus== "Main") {
            lat <- 40.809
        }

        else if (input$campus == "Manhattanville") {
            lat <- 40.818
        }

        else if (input$campus == "Med School") {
            lat <- 40.840
        }

    })

    lon <- reactive({
        if (input$campus== "Main") {
            lon <- -73.961
        }
        
        else if (input$campus == "Manhattanville") {
            lon <- -73.957
        }
        
        else if (input$campus == "Med School") {
            lon <- -73.942
        }
        
    })

# Spatial Filter ----------------------------------------------------------
    
    bounds <- reactive({
        if (input$campus== "Main") {
                xmin <- -73.95389
                xmax <- -73.96876
                ymin <-  40.80164
                ymax <-  40.81476 
                bounds <- data.frame(xmin, xmax, ymin, ymax)
        }
        
        else if (input$campus== "Manhattanville") {
                xmin <- -73.95615
                xmax <- -73.96156
                ymin <-  40.81560
                ymax <-  40.82018  
                bounds <- data.frame(xmin, xmax, ymin, ymax)
        }
        
        else if (input$campus== "Med School") {
                xmin <- -73.9396805
                xmax <- -73.9465531
                ymin <-  40.8395324
                ymax <-  40.8411665
                bounds <- data.frame(xmin, xmax, ymin, ymax)
        }
    })
 

    filt_bbox <- reactive({
        filt_bbox <- sf::st_bbox(c(xmin = ifelse(is.na(bounds()$xmin), -180, bounds()$xmin), 
                                   ymin = ifelse(is.na(bounds()$ymin),  -90,  bounds()$ymin), 
                                   xmax = ifelse(is.na(bounds()$xmax), +180, bounds()$xmax), 
                                   ymax = ifelse(is.na(bounds()$ymax),  +90, bounds()$ymax)), 
                                 crs = st_crs(4326)) %>% 
            sf::st_as_sfc(.)
        
        
    })
    

    observe({
        print(filt_bbox())
    })
    
    crime_data_filtered <- reactive({
        incidents <- read.csv("NYC_incidents_current.csv")
        if (input$crime_type == "All"){crime_data_filtered <- incidents}
        else (crime_data_filtered <- incidents %>%
                  filter(Category == input$crime_type))

        crime_data_filtered <- crime_data_filtered %>%
            filter(Date > input$date[1] & Date < input$date[2])

    })
    crime_data_filtered2 <- reactive({

        projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
        df <- st_as_sf(x = crime_data_filtered(),
                       coords = c("Lon", "Lat"),
                       crs = projcrs)


        find_data <- sf::st_within(df, filt_bbox())
        crime_data_filtered2 <- df[which(lengths(find_data) !=0),]
        
    })
    crime_data_filtered3 <- reactive({ 
        options(digits=9)
        crime_data_filtered3 <- as_tibble(crime_data_filtered2()) 
        crime_data_filtered3$geometry2 <- as.character(crime_data_filtered3$geometry)
        
        crime_data_filtered3$geometry2 <- gsub("[()]", "", crime_data_filtered3$geometry2)
        crime_data_filtered3$geometry2 <- gsub("c", "", crime_data_filtered3$geometry2)
        crime_data_filtered3$geometry2 <- gsub(",", "", crime_data_filtered3$geometry2)
        crime_data_filtered3 <- separate(data = crime_data_filtered3, col = geometry2, into = c("Lon", "Lat"), sep = " ")
        crime_data_filtered3$Lat <- as.numeric(crime_data_filtered3$Lat)
        crime_data_filtered3$Lon <- as.numeric(crime_data_filtered3$Lon)
        crime_data_filtered3 <- crime_data_filtered3 

        

    })
    
    observe({
        print(crime_data_filtered3())
    })
    

# Map Generator -----------------------------------------------------------
    
    
    output$map <- renderLeaflet(
        
        leaflet() %>%
            addProviderTiles("CartoDB.Positron") %>%
            addHeatmap(lng = crime_data_filtered3()$Lon, lat = crime_data_filtered3()$Lat, blur = 40, max = 0.05, radius = 30, group = "Heatmap") %>%
            addCircleMarkers(data = crime_data_filtered3(),
                             lat = ~Lat, lng = ~Lon, popup = paste(crime_data_filtered3()$Category, "<br>",
                                                                   crime_data_filtered3()$Date, "<br>",
                                                                   crime_data_filtered3()$Time, "<br>"),
                             color = 'black', radius = 5, stroke = FALSE,
                             opacity = 0, fillOpacity = 1,
                             clusterOptions = markerClusterOptions(spiderfyDistanceMultiplier=1.5, group = "Points")) %>%
            addPolygons(data=shapeData,weight=5, col = 'black', fillColor = "transparent") %>%
            setView(lng = lon(), lat = lat(), zoom = 16) %>%
            addLayersControl(
                overlayGroups = c("Points", "Heatmap"),
                options = layersControlOptions(collapsed = FALSE)) %>%
            hideGroup("Heatmap")

    )

# Plots Generator ---------------------------------------------------------

    
    output$crime_bar<-renderPlotly({
        sum_tab <- crime_data_filtered3() %>%
            group_by(crime_data_filtered3()[2]) %>%
            summarise(count = n())
        
       plot_ly() %>%
            add_trace(x = sum_tab$Category, y = sum_tab$count, type = "bar")
    })
    
    output$crime_bar2<-renderPlotly({
        sum_tab_date <- crime_data_filtered3() %>%
            group_by(crime_data_filtered3()[3]) %>%
            summarise(count = n())
        sum_tab_date$Date <- as.Date(sum_tab_date$Date)
        plot_ly() %>%
            add_trace(x = sum_tab_date$Date, y = sum_tab_date$count, type = "scatter", mode = "lines")
    })
    

    
}
shinyApp(ui, server) 
