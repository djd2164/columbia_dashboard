library(shiny)
library(leaflet)
library(leaflet.extras)
# Define UI 
ui <- fluidPage(
    leafletOutput("mymap",height=800)
)

# Define server logic 
server <- function(input, output) {
    
    output$mymap <- renderLeaflet(
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
    
    observeEvent(input$mymap_draw_new_feature,{
        feature <- input$mymap_draw_new_feature
        
        print(feature)
        
    })
    
}

# Run the application
shinyApp(ui = ui, server = server)

