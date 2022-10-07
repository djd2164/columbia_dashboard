
library(shiny)
library(shinydashboard)
library(leaflet)
library(leaflet.extras)


#UI
ui <- dashboardPage(
    dashboardHeader(title = "Basic dashboard"),
    dashboardSidebar(width = 200),
    dashboardBody(
        # Boxes need to be put in a row (or column)
        fluidRow(
            box(
                tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
                leafletOutput("map")
            ),
            
            box(
                title = "Controls",
                sliderInput("slider", "Number of observations:", 1, 100, 50)
            )
        )
    )
)



#Server
server <- function(input, output) {
    
    output$map <- renderLeaflet(
        leaflet() %>%
            addProviderTiles("CartoDB.Positron") %>%
            setView(lng = -166, lat = 58.0, zoom = 5) %>%
            addDrawToolbar(
                targetGroup='draw',
                editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions(),
                )
            )  %>%
            addLayersControl(overlayGroups = c('draw'), options =
                                 layersControlOptions(collapsed=FALSE))
    )
    
    observeEvent(input$map_draw_new_feature,{
        feature <- input$map_draw_new_feature
        
        print(feature)
        
    })
    
}
shinyApp(ui, server)