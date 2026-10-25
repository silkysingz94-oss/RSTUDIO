# -------------------------------
# GAMBIA EA INTERACTIVE DASHBOARD
# Author: Ansu Singhateh, GIS Analyst, Gambia Bureau of Statistics
# -------------------------------

library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(RColorBrewer)

# Load shapefile
EA_shp <- st_read("C:/Users/user/Desktop/RCoding/Mapping_Census/RawData/EA_Pop.shp")

# Transform to WGS84 (long-lat)
if(st_crs(EA_shp)$epsg != 4326){
  EA_shp <- st_transform(EA_shp, crs = 4326)
}

# UI
ui <- fluidPage(
  titlePanel("Interactive Gambia EA Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("lga", "Select LGA:", choices = c("All", unique(EA_shp$lganame))),
      selectInput("district", "Select District:", choices = NULL),
      selectInput("ward", "Select Ward:", choices = NULL),
      selectInput("field", "Select Field to Map:", 
                  choices = c("map_hhs","map_pop","ls_hhs","ls_pop","enum_hhs","enum_pop",
                              "comp_map","build_map","comp_ls","build_ls")),
      sliderInput("polyOpacity", "Polygon Opacity:", min = 0, max = 1, value = 0.6),
      sliderInput("pointOpacity", "Point Opacity:", min = 0, max = 1, value = 0.8)
    ),
    
    mainPanel(
      leafletOutput("map", height = 700)
    )
  )
)

# SERVER
server <- function(input, output, session) {
  
  # Update district choices based on LGA
  observeEvent(input$lga, {
    if(input$lga == "All"){
      updateSelectInput(session, "district", choices = c("All", unique(EA_shp$distname)))
    } else {
      filtered <- EA_shp %>% filter(lganame == input$lga)
      updateSelectInput(session, "district", choices = c("All", unique(filtered$distname)))
    }
  })
  
  # Update ward choices based on district
  observeEvent(input$district, {
    if(input$district == "All"){
      updateSelectInput(session, "ward", choices = c("All", unique(EA_shp$wardname)))
    } else {
      filtered <- EA_shp %>% filter(distname == input$district)
      updateSelectInput(session, "ward", choices = c("All", unique(filtered$wardname)))
    }
  })
  
  # Reactive dataset based on selections
  filteredData <- reactive({
    df <- EA_shp
    if(input$lga != "All") df <- df %>% filter(lganame == input$lga)
    if(input$district != "All") df <- df %>% filter(distname == input$district)
    if(input$ward != "All") df <- df %>% filter(wardname == input$ward)
    df
  })
  
  # Color palette
  pal <- reactive({
    data_vals <- filteredData()[[input$field]]
    colorNumeric(palette = "YlOrRd", domain = data_vals, na.color = "transparent")
  })
  
  # Render Leaflet Map
  output$map <- renderLeaflet({
    # Initial map centered on Gambia
    bbox <- st_bbox(EA_shp)
    leaflet() %>% 
      addProviderTiles(providers$OpenStreetMap) %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Light") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      setView(lng = mean(c(bbox["xmin"], bbox["xmax"])), 
              lat = mean(c(bbox["ymin"], bbox["ymax"])), zoom = 8) %>%
      addLayersControl(
        baseGroups = c("OpenStreetMap", "Light", "Satellite"),
        overlayGroups = c("EAs"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addLegend("bottomright", pal = pal(), values = filteredData()[[input$field]],
                title = input$field)
  })
  
  # Update polygons when selection changes
  observe({
    data <- filteredData()
    leafletProxy("map", data = data) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(fillColor = ~pal()(data[[input$field]]),
                  fillOpacity = input$polyOpacity,
                  color = "black",
                  weight = 1,
                  label = ~paste0(wardname, ": ", data[[input$field]])) %>%
      addLegend("bottomright", pal = pal(), values = data[[input$field]], title = input$field)
    
    # Zoom to selected area
    if(nrow(data) > 0){
      bbox <- st_bbox(data)
      leafletProxy("map") %>% flyToBounds(
        lng1 = bbox["xmin"], lat1 = bbox["ymin"],
        lng2 = bbox["xmax"], lat2 = bbox["ymax"]
      )
    }
  })
}

# Run App
shinyApp(ui, server)

