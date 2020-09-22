library(shiny)
library(jsonlite)
library(leaflet)
library(tidyverse)
library(tidytransit)
# set map icon for bus
bus_icon <- makeAwesomeIcon(icon = "bus",
                             library = "fa")
# read route information RDS file
# This has been extracted from the complete GTFS file for 
# faster processing
routes <- readRDS("data/routes.RDS")

ui <- fluidPage(
    titlePanel("Where is the Madison Bike Week bus?"),
    leafletOutput("mymap"),
    p(),
    actionButton("recalc", "Refresh position"),
    p("If you see a blank world map, the bus is not in service. Try again later."),
    p("Source code:", a("https://github.com/vgXhc/bus_tracker", href="https://github.com/vgXhc/bus_tracker"))
)

server <- function(input, output, session) {
    ## create popup text by joining vehicle position data with route data
    
    lab <- eventReactive(input$recalc, {
        pos <- read_json("http://transitdata.cityofmadison.com/Vehicle/VehiclePositions.json", simplifyVector = T)
        pos <- pos$entity
        bus <- pos %>%
            filter(vehicle$vehicle$label == "148")
        route <- bus$vehicle$trip %>%
            left_join(routes, by = "route_id")
        x <- paste0("Route: ", route$route_short_name, "<br>",
                       route$route_service_name)
        x #return value
    } , ignoreNULL = FALSE)


    veh_pos <- eventReactive(input$recalc, {
        pos <- read_json("http://transitdata.cityofmadison.com/Vehicle/VehiclePositions.json", simplifyVector = T)
        pos <- pos$entity
        #filter out bus of interest 
        bus <- pos %>%
            filter(vehicle$vehicle$label == "148")
        #return value: long/lat of bus position
        cbind(bus$vehicle$position$longitude, bus$vehicle$position$latitude)
        
    }, ignoreNULL = FALSE)
    
  
        
    output$mymap <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE)
            ) %>%
            addAwesomeMarkers(data = veh_pos(), 
                              icon = bus_icon,
                              popup = lab())
    })
}

shinyApp(ui, server)
