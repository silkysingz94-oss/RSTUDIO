# ==========================================
# 🇬🇲 GAMBIA DISABILITY DASHBOARD (FINAL)
# ==========================================

library(shiny)
library(shinydashboard)
library(sf)
library(dplyr)
library(leaflet)
library(plotly)
library(RColorBrewer)
library(tidyr)

# ------------------------------------------
# 1️⃣ LOAD & CLEAN DATA
# ------------------------------------------

# Set your shapefile path
shp_path <- "C:/Users/user/Desktop/RCoding/Mapping_Census/RawData/Disability_SexRatio.shp"

if(!file.exists(shp_path)){
  stop("Shapefile not found! Place all .shp/.dbf/.shx/.prj files in the folder and update the path.")
}

EA <- st_read(shp_path, quiet = TRUE) %>%
  st_transform(4326)

# Clean column names (remove duplicates)
colnames(EA) <- make.names(colnames(EA), unique = TRUE)

# Detect numeric fields automatically
base_fields <- c("Pop_2013","Pop_2024","Sex_Ratio_2013","Sex_Ratio_2024")
disability_fields <- grep("Seeing|Hearing|Physical|Learning|Self|Speech", colnames(EA), value = TRUE)
numeric_fields <- unique(c(base_fields, disability_fields))
numeric_fields <- intersect(numeric_fields, colnames(EA))  # Only existing columns

# Convert numeric fields safely
for(f in numeric_fields){
  EA[[f]] <- as.numeric(gsub(",", "", EA[[f]]))
}

# ------------------------------------------
# 2️⃣ UI
# ------------------------------------------
ui <- dashboardPage(
  
  skin = "black",
  
  # Centered, responsive title
  dashboardHeader(
    title = tags$div(
      style = "width:100%; text-align:center; font-size:25px; font-weight:bold;",
      "GAMBIA POPULATION & DISABILITY DASHBOARD"
    ),
    titleWidth = "100%"
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Map", tabName = "map", icon = icon("map")),
      menuItem("Charts", tabName = "charts", icon = icon("chart-bar"))
    ),
    
    hr(),
    
    selectInput("metric", "Select Indicator", choices = numeric_fields),
    
    selectInput("lga", "Select LGA",
                choices = c("All", unique(EA$lganame))),
    
    sliderInput("opacity", "Polygon Opacity", 0, 1, 0.7),
    
    checkboxInput("centroids", "Show Centroids", TRUE),
    checkboxInput("hotspots", "Show Hotspots", TRUE),
    
    selectInput("basemap", "Basemap",
                choices = c("Light","Dark","Satellite"))
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML("
      body {background-color:#111; color:#eee;}
      .box {background-color:#222; color:#fff;}
      .content-wrapper {background-color:#111;}
      @media(max-width:768px){ .main-sidebar {width: 0px;} }
    "))),
    
    tabItems(
      
      tabItem(tabName = "map",
              fluidRow(
                box(width = 12, leafletOutput("map", height = 600))
              )
      ),
      
      tabItem(tabName = "charts",
              fluidRow(
                valueBoxOutput("avg"),
                valueBoxOutput("total"),
                valueBoxOutput("count")
              ),
              
              fluidRow(
                box(width = 6, plotlyOutput("bar")),
                box(width = 6, plotlyOutput("pie"))
              )
      )
    )
  )
)

# ------------------------------------------
# 3️⃣ SERVER
# ------------------------------------------
server <- function(input, output, session) {
  
  # Filter data
  data_filt <- reactive({
    d <- EA
    if(input$lga != "All"){
      d <- d %>% filter(lganame == input$lga)
    }
    d
  })
  
  # Safe color palette
  pal <- reactive({
    vals <- data_filt()[[input$metric]]
    vals <- vals[!is.na(vals)]
    if(length(vals) == 0) vals <- 0
    colorNumeric("YlOrRd", domain = vals)
  })
  
  # KPI boxes
  output$avg <- renderValueBox({
    valueBox(
      round(mean(data_filt()[[input$metric]], na.rm = TRUE),2),
      paste("Average", input$metric),
      icon = icon("chart-line"),
      color = "blue"
    )
  })
  
  output$total <- renderValueBox({
    valueBox(
      round(sum(data_filt()[[input$metric]], na.rm = TRUE),0),
      paste("Total", input$metric),
      icon = icon("calculator"),
      color = "green"
    )
  })
  
  output$count <- renderValueBox({
    valueBox(
      length(unique(data_filt()$lganame)),
      "Number of LGAs",
      icon = icon("map"),
      color = "yellow"
    )
  })
  
  # Initial map
  output$map <- renderLeaflet({
    leaflet() %>% addProviderTiles(providers$CartoDB.Positron)
  })
  
  # Update map dynamically
  observe({
    d <- data_filt()
    p <- pal()
    
    base <- switch(input$basemap,
                   "Light" = providers$CartoDB.Positron,
                   "Dark" = providers$CartoDB.DarkMatter,
                   "Satellite" = providers$Esri.WorldImagery)
    
    # Safe centroids, suppress warnings
    centroids <- suppressWarnings(
      d %>% st_transform(3857) %>% st_centroid() %>% st_transform(4326)
    )
    
    leafletProxy("map", data = d) %>%
      clearShapes() %>% clearMarkers() %>% clearControls() %>%
      addProviderTiles(base) %>%
      
      addPolygons(
        fillColor = ~p(get(input$metric)),
        fillOpacity = input$opacity,
        color = "black",
        weight = 1,
        popup = ~paste0("<b>LGA:</b> ", lganame,
                        "<br><b>", input$metric, ":</b> ", get(input$metric))
      ) %>%
      
      {if(input$centroids)
        addCircleMarkers(., data = centroids,
                         radius = 5, color = "blue")
        else .} %>%
      
      {if(input$hotspots)
        addCircles(., data = centroids,
                   radius = sqrt(pmax(replace_na(d[[input$metric]],0),0))*500,
                   fillColor = "red",
                   fillOpacity = 0.3,
                   stroke = FALSE)
        else .} %>%
      
      addLegend("bottomright",
                pal = p,
                values = d[[input$metric]],
                title = input$metric)
  })
  
  # BAR CHART
  output$bar <- renderPlotly({
    df <- data_filt() %>%
      st_drop_geometry() %>%
      filter(!is.na(.data[[input$metric]])) %>%
      group_by(lganame) %>%
      summarise(val = mean(.data[[input$metric]], na.rm = TRUE), .groups="drop")
    
    validate(need(nrow(df) > 0, "No data available"))
    
    plot_ly(df, x = ~lganame, y = ~val, type = "bar")
  })
  
  # PIE CHART
  output$pie <- renderPlotly({
    df <- data_filt() %>%
      st_drop_geometry() %>%
      filter(!is.na(.data[[input$metric]])) %>%
      group_by(lganame) %>%
      summarise(val = mean(.data[[input$metric]], na.rm = TRUE), .groups="drop")
    
    validate(need(nrow(df) > 0, "No data available"))
    
    plot_ly(df, labels = ~lganame, values = ~val, type = "pie")
  })
}

# ------------------------------------------
# 4️⃣ RUN APP
# ------------------------------------------
shinyApp(ui, server)
