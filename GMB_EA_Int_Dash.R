# ----------------------------------------
# Interactive Gambia EA Dashboard - Shiny App
# Produced by: Ansu Singhateh, GIS Analyst, Gambia Bureau of Statistics
# ----------------------------------------

# ----------------------------------------
# Interactive Gambia EA Dashboard - Full Shiny App
# Produced by: Ansu Singhateh, GIS Analyst, Gambia Bureau of Statistics
# ----------------------------------------

library(shiny)
library(sf)
library(dplyr)
library(leaflet)
library(RColorBrewer)

# Load shapefile and fix duplicate column names
ea_sf<- st_read("C:/Users/ansu.s/Desktop/RCoding/Mapping_Census/RawData/EA_Pop.shp")

# Fix duplicate column names
names(ea_sf) <- make.names(names(ea_sf), unique = TRUE)

# Transform to WGS84 for leaflet and ensure validity
ea_sf <- ea_sf %>%
  st_transform(crs = 4326) %>%
  st_make_valid() %>%
  st_simplify(dTolerance = 50)

# Available fields to visualize
value_fields <- c(
  "map_hhs","map_pop","ls_hhs","ls_pop",
  "enum_hhs","enum_pop","comp_map","build_map",
  "comp_ls","build_ls"
)

# UI
ui <- fluidPage(
  titlePanel("Gambia EA Interactive Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("lga", "Select LGA:", choices = sort(unique(ea_sf$lganame))),
      selectInput("district", "Select District:", choices = NULL),
      selectInput("ward", "Select Ward:", choices = NULL),
      selectInput("field", "Select Field to Visualize:", choices = value_fields),
      checkboxInput("show_centroids", "Show Centroids", value = TRUE),
      sliderInput("opacity_poly", "Polygon Opacity:", min = 0, max = 1, value = 0.6, step = 0.05),
      sliderInput("opacity_point", "Centroid/Point Opacity:", min = 0, max = 1, value = 0.7, step = 0.05),
      hr(),
      p("Produced by: Ansu Singhateh, GIS Analyst, Gambia Bureau of Statistics")
    ),
    mainPanel(
      leafletOutput("map", height = 700)
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Update district based on LGA
  observe({
    req(input$lga)
    districts <- ea_sf %>% filter(lganame == input$lga) %>% pull(distname) %>% unique() %>% sort()
    updateSelectInput(session, "district", choices = districts)
    updateSelectInput(session, "ward", choices = NULL)
  })
  
  # Update ward based on district
  observe({
    req(input$lga, input$district)
    wards <- ea_sf %>%
      filter(lganame == input$lga, distname == input$district) %>%
      pull(wardname) %>% unique() %>% sort()
    updateSelectInput(session, "ward", choices = wards)
  })
  
  # Reactive filtered data
  filtered_data <- reactive({
    sf <- ea_sf
    if(!is.null(input$lga) && input$lga != ""){
      sf <- sf %>% filter(lganame == input$lga)
    }
    if(!is.null(input$district) && input$district != ""){
      sf <- sf %>% filter(distname == input$district)
    }
    if(!is.null(input$ward) && input$ward != ""){
      sf <- sf %>% filter(wardname == input$ward)
    }
    sf
  })
  
  # Color palette
  pal_fun <- reactive({
    vals <- filtered_data()[[input$field]]
    colorNumeric("YlOrRd", domain = vals, na.color = "transparent")
  })
  
  # Render Leaflet map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 13.5, lat = 16.5, zoom = 8)
  })
  
  # Update map whenever selection changes
  observe({
    req(filtered_data(), input$field)
    data <- filtered_data()
    pal <- pal_fun()
    
    leafletProxy("map", data = data) %>%
      clearShapes() %>%
      clearMarkers() %>%
      clearControls() %>%  # remove old legend
      addPolygons(
        fillColor = ~pal(get(input$field)),
        fillOpacity = input$opacity_poly,
        color = "black",
        weight = 1,
        popup = ~paste0(
          "<b>EA Code:</b> ", EA_code, "<br>",
          "<b>Ward:</b> ", wardname, "<br>",
          "<b>District:</b> ", distname, "<br>",
          "<b>LGA:</b> ", lganame, "<br>",
          "<b>", input$field, ":</b> ", get(input$field)
        )
      ) %>%
      {
        if(input$show_centroids){
          addCircleMarkers(., data = st_centroid(data),
                           radius = 4,
                           fillColor = "blue",
                           fillOpacity = input$opacity_point,
                           color = "white",
                           weight = 1)
        } else { . }
      } %>%
      flyToBounds(
        lng1 = st_bbox(data)$xmin,
        lat1 = st_bbox(data)$ymin,
        lng2 = st_bbox(data)$xmax,
        lat2 = st_bbox(data)$ymax
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = data[[input$field]],
        title = paste("Legend:", input$field),
        opacity = 0.7
      )
  })
}

# Run App
shinyApp(ui, server)

