library(shiny)
library(jsonlite)
library(leaflet)
library(tidyverse)
library(tidytransit)


# get route information from GTFS file
# download.file("http://transitdata.cityofmadison.com/GTFS/mmt_gtfs.zip", destfile = "data/mmt_gtfs.zip")

gtfs <- read_gtfs("data/mmt_gtfs.zip")
routes <- gtfs$routes

# read route information RDS file
# This has been extracted from the complete GTFS file for 
# faster processing
#routes <- readRDS("data/routes.RDS")

#create list of all vehicle IDs currently in service
tmp <- read_json("http://transitdata.cityofmadison.com/Vehicle/VehiclePositions.json", simplifyVector = T)
vehicles_in_service <- tmp$entity$vehicle$vehicle$label

ui <- fluidPage(
    titlePanel("Where is the Metro bus?"),
    p("Do want to track a specific Metro bus, for example one of the new battery-electric buses (vehicle IDs 2001, 2002, 2003)? You can do so here, as long as you know the vehicle ID."),
    checkboxGroupInput("vehicle_input", "Which of these vehicles do you want to display?", 
                       vehicles_in_service, 
                       inline = T,
                       selected = vehicles_in_service[1]
                          ),
    actionButton("recalc", "Confirm/refresh"),
    leafletOutput("mymap"),
    p(),

    p("Source code:", a("https://github.com/vgXhc/bus_tracker", href="https://github.com/vgXhc/bus_tracker")),
    p("Help me cover the hosting costs for this project with a tip", HTML("<a href='https://ko-fi.com/X8X5EIPI8' target='_blank'><img height='36' style='border:0px;height:36px;' src='https://cdn.ko-fi.com/cdn/kofi2.png?v=3' border='0' alt='Buy Me a Coffee at ko-fi.com' /></a>"))
)

server <- function(input, output, session) {

  
    ## create popup text by joining vehicle position data with route data
    
    lab <- eventReactive(input$recalc, {
        pos <- read_json("http://transitdata.cityofmadison.com/Vehicle/VehiclePositions.json", simplifyVector = T)
        pos <- pos$entity
        bus <- pos %>%
            filter(vehicle$vehicle$label %in% input$vehicle_input)
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
            filter(vehicle$vehicle$label %in% input$vehicle_input)
        bus_icon <- makeAwesomeIcon(icon = "arrow-up",
                                    library = "fa",
                                    iconRotate = bus$vehicle$position$bearing)
        #return value: long/lat of bus position
        list(cbind(bus$vehicle$position$longitude, bus$vehicle$position$latitude), bus_icon)
        
    }, ignoreNULL = FALSE)
    

    
  
        
    output$mymap <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE)
            ) %>%
            addAwesomeMarkers(data = veh_pos()[[1]], 
                              icon = veh_pos()[[2]],
                              popup = lab())
    })
}

shinyApp(ui, server)
