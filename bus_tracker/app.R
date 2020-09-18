library(shiny)
library(jsonlite)
library(leaflet)
library(tidyverse)
bus_icon <- makeAwesomeIcon(icon = "bus",
                             library = "fa")

ui <- fluidPage(
    titlePanel("Where is the Madison Bike Week bus?"),
    leafletOutput("mymap"),
    p(),
    actionButton("recalc", "Refresh position")
)

server <- function(input, output, session) {
    
    veh_pos <- eventReactive(input$recalc, {
        pos <- read_json("http://transitdata.cityofmadison.com/Vehicle/VehiclePositions.json", simplifyVector = T)
        pos <- pos$entity
        #filter out bus of interest 
                bus <- pos %>%
                    filter(vehicle$vehicle$label == 148)
                #extract position coordinates as sf object
                cbind(bus$vehicle$position$longitude, bus$vehicle$position$latitude)
                
    }, ignoreNULL = FALSE)
    
    output$mymap <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE)
            ) %>%
            addAwesomeMarkers(data = veh_pos(), icon = bus_icon)
    })
}

shinyApp(ui, server)
