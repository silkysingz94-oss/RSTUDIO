# ==========================================
# 🇬🇲 GAMBIA ADVANCED EA DASHBOARD - UPGRADED
# Produced by: Ansu Singhateh, GIS Analyst, GBoS
# Features: Multi-layer map, KPIs, charts, hotspots, points, ward-level totals
# ==========================================

library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(plotly)
library(shinydashboard)
library(ggplot2)

# -----------------------------
# LOAD DATA
# -----------------------------
EA <- st_read("C:/Users/ansu.s/Desktop/RCoding/Mapping_Census/RawData/EA_Pop.shp", quiet = TRUE)

if (is.na(st_crs(EA))) st_crs(EA) <- 3857
EA <- st_transform(EA, 4326)
names(EA) <- make.unique(names(EA))

# Ensure all required columns exist
cols <- c("lganame","distname","wardname","map_pop","map_hhs",
          "enum_hhs","enum_pop","comp_map","build_map","comp_ls","build_ls","EA_code")
for(cn in cols){ if(!cn %in% names(EA)) EA[[cn]] <- 0 }

centroids <- st_point_on_surface(EA)

# -----------------------------
# UI
# -----------------------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {background-color: #111; color: #eee;}
      .well {background-color: #222; border: none;}
      .sidebar {background-color: #222; color: #fff;}
      .box {background-color: #222; color: #fff;}
      #title-bar {background: linear-gradient(to right, #CE1126, #14B53A, #003893); 
                  color: #fff; text-align: center; padding: 15px; font-size: 28px; font-weight:bold;}
      .value-box {margin-bottom:10px;}
    "))
  ),
  
  # TITLE BAR
  div(id="title-bar", "GAMBIA MAPPING & CENSUS DASHBOARD: HOUSEHOLDS, POPULATION, BUILDINGS, COMPOUNDS"),
  
  fluidRow(
    # LEFT PANEL - Filters & Layers
    column(3, style="height: 90vh; overflow-y: auto; padding:10px;",
           h3("📍 Filters & Layers"),
           selectInput("lga", "LGA", c("All", unique(EA$lganame))),
           uiOutput("dist_ui"),
           uiOutput("ward_ui"),
           selectInput("field", "Map Variable",
                       c("map_hhs","map_pop","ls_hhs","ls_pop",
                         "enum_hhs","enum_pop","comp_map","build_map","comp_ls","build_ls")),
           sliderInput("polyOpacity","Polygon Opacity",0,1,0.6),
           sliderInput("pointOpacity","Point Opacity",0,1,0.8),
           checkboxInput("points","Show Points", TRUE),
           checkboxInput("hotspot","Show Hotspots", TRUE),
           checkboxGroupInput("layers", "Additional Layers",
                              c("Households"="map_hhs",
                                "Population"="map_pop",
                                "Enumerated HHS"="enum_hhs",
                                "Enumerated Pop"="enum_pop",
                                "Compounds"="comp_map",
                                "Buildings"="build_map")),
           selectInput("basemap","Basemap",
                       c("OpenStreetMap","Light","Dark","Satellite")),
           hr(),
           strong("Produced by Ansu Singhateh"),
           p("GIS Analyst, Gambia Bureau of Statistics")
    ),
    
    # CENTER PANEL - Map
    column(6, leafletOutput("map", height = "90vh")),
    
    # RIGHT PANEL - KPIs & Charts
    column(3, style="height: 90vh; overflow-y: auto; padding:10px;",
           h3("📊 Summaries & Charts"),
           valueBoxOutput("pop_box"),
           valueBoxOutput("hh_box"),
           valueBoxOutput("ea_box"),
           hr(),
           plotlyOutput("bar_chart", height = 300),
           hr(),
           plotlyOutput("pie_chart", height = 300)
    )
  )
)

# -----------------------------
# SERVER
# -----------------------------
server <- function(input, output, session){
  
  # -----------------------------
  # DYNAMIC FILTERS
  # -----------------------------
  output$dist_ui <- renderUI({
    req(input$lga)
    d <- if(input$lga=="All") EA else filter(EA, lganame==input$lga)
    selectInput("dist","District", c("All", unique(d$distname)))
  })
  
  output$ward_ui <- renderUI({
    req(input$dist)
    d <- EA
    if(input$lga!="All") d <- filter(d, lganame==input$lga)
    if(input$dist!="All") d <- filter(d, distname==input$dist)
    selectInput("ward","Ward", c("All", unique(d$wardname)))
  })
  
  # -----------------------------
  # FILTERED DATA
  # -----------------------------
  data_filt <- reactive({
    d <- EA
    if(input$lga!="All") d <- filter(d, lganame==input$lga)
    if(!is.null(input$dist) && input$dist!="All") d <- filter(d, distname==input$dist)
    if(!is.null(input$ward) && input$ward!="All") d <- filter(d, wardname==input$ward)
    validate(need(nrow(d)>0,"No data available"))
    d
  })
  
  # -----------------------------
  # PALETTE
  # -----------------------------
  pal <- reactive({
    df <- st_drop_geometry(data_filt()) %>%
      group_by(wardname) %>%
      summarise(value = sum(as.numeric(get(input$field)), na.rm=TRUE))
    colorNumeric("YlOrRd", domain = df$value, na.color="transparent")
  })
  
  # -----------------------------
  # KPI BOXES
  # -----------------------------
  output$pop_box <- renderValueBox({
    valueBox(
      format(sum(data_filt()$map_pop, na.rm=TRUE), big.mark=","),
      "Total Population", icon = icon("users"), color = "red"
    )
  })
  
  output$hh_box <- renderValueBox({
    valueBox(
      format(sum(data_filt()$map_hhs, na.rm=TRUE), big.mark=","),
      "Total Households", icon = icon("home"), color = "blue"
    )
  })
  
  output$ea_box <- renderValueBox({
    valueBox(
      nrow(data_filt()),
      "Total EAs", icon = icon("map"), color = "green"
    )
  })
  
  # -----------------------------
  # INITIAL MAP
  # -----------------------------
  output$map <- renderLeaflet({
    leaflet() %>% addProviderTiles(providers$OpenStreetMap) %>%
      setView(lng=-15.5, lat=13.4, zoom=9)
  })
  
  # -----------------------------
  # UPDATE MAP
  # -----------------------------
  observe({
    d <- data_filt()
    p <- pal()
    
    base <- switch(input$basemap,
                   "OpenStreetMap" = providers$OpenStreetMap,
                   "Light" = providers$CartoDB.Positron,
                   "Dark" = providers$CartoDB.DarkMatter,
                   "Satellite" = providers$Esri.WorldImagery)
    
    cent <- st_point_on_surface(d)
    
    map <- leafletProxy("map", data = d) %>%
      clearShapes() %>% clearMarkers() %>% clearControls() %>% clearTiles() %>%
      addProviderTiles(base)
    
    # MULTI-LAYER DISPLAY
    if(length(input$layers)>0){
      for(layer in input$layers){
        map <- map %>% addPolygons(
          data = d,
          fillColor = ~colorNumeric("YlOrRd", d[[layer]])(d[[layer]]),
          fillOpacity = input$polyOpacity,
          color = "black",
          weight = 1,
          group = layer,
          popup = ~paste0("<b>EA:</b> ", EA_code,
                          "<br><b>LGA:</b> ", lganame,
                          "<br><b>District:</b> ", distname,
                          "<br><b>Ward:</b> ", wardname,
                          "<br><b>", layer, ":</b> ", get(layer))
        )
      }
      map <- map %>% addLayersControl(
        overlayGroups = input$layers,
        options = layersControlOptions(collapsed = FALSE)
      )
    }
    
    # POINTS
    if(input$points){
      map <- map %>% addCircleMarkers(
        data = cent,
        radius = 4,
        fillOpacity = input$pointOpacity,
        color = "blue",
        group = "Points"
      )
    }
    
    # HOTSPOTS
    if(input$hotspot){
      map <- map %>% addCircles(
        data = cent,
        radius = sqrt(d[[input$field]])*50,
        fillColor = "red",
        fillOpacity = 0.2,
        stroke = FALSE,
        group = "Hotspots"
      )
    }
    
    # ZOOM TO FILTERED AREA
    bbox <- st_bbox(d)
    if(all(is.finite(bbox))){
      map <- map %>% flyToBounds(lng1=bbox["xmin"], lat1=bbox["ymin"],
                                 lng2=bbox["xmax"], lat2=bbox["ymax"])
    }
    
    # LEGEND
    map %>% addLegend("bottomright",
                      pal = colorNumeric("YlOrRd", d[[input$field]]),
                      values = d[[input$field]],
                      title = input$field)
  })
  
  # -----------------------------
  # BAR CHART
  # -----------------------------
  output$bar_chart <- renderPlotly({
    df <- st_drop_geometry(data_filt()) %>%
      group_by(wardname) %>%
      summarise(value = sum(as.numeric(get(input$field)), na.rm=TRUE))
    
    plot_ly(df, x=~wardname, y=~value, type="bar",
            marker=list(color=~pal()(value))) %>%
      layout(xaxis=list(title="Ward"), yaxis=list(title=input$field))
  })
  
  # -----------------------------
  # PIE CHART
  # -----------------------------
  output$pie_chart <- renderPlotly({
    df <- st_drop_geometry(data_filt()) %>%
      group_by(wardname) %>%
      summarise(value = sum(as.numeric(get(input$field)), na.rm=TRUE))
    
    plot_ly(df, labels=~wardname, values=~value, type="pie",
            marker=list(colors=pal()(df$value)))
  })
}

# -----------------------------
# RUN APP
# -----------------------------
shinyApp(ui, server)
