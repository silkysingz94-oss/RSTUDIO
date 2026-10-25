# ==========================================
# 🇬🇲 GAMBIA ADVANCED EA DASHBOARD
# Produced by: Ansu Singhateh, GIS Analyst, GBoS
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
EA <- st_read("C:/Users/user/Desktop/RCoding/Mapping_Census/RawData/EA_Pop.shp", quiet = TRUE)

# Ensure CRS is longlat WGS84
if (is.na(st_crs(EA))) st_crs(EA) <- 3857
EA <- st_transform(EA, 4326)
names(EA) <- make.unique(names(EA))

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
    "))
  ),
  
  # TITLE BAR
  div(id="title-bar", "GAMBIA MAPPING & CENSUS DASHBOARD: HOUSEHOLDS, POPULATION, BUILDINGS, COMPOUNDS"),
  
  fluidRow(
    # LEFT PANEL - Filters & Controls
    column(3, style="height: 90vh; overflow-y: auto; padding:10px;",
           h3("📍 Filters & Style"),
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
           selectInput("basemap","Basemap",
                       c("OpenStreetMap","Light","Dark","Satellite")),
           hr(),
           strong("Produced by Ansu Singhateh"),
           p("GIS Analyst, Gambia Bureau of Statistics")
    ),
    
    # CENTER PANEL - Map
    column(6, leafletOutput("map", height = "90vh")),
    
    # RIGHT PANEL - Charts & Summaries
    column(3, style="height: 90vh; overflow-y: auto; padding:10px;",
           h3("📊 Summaries & Charts"),
           fluidRow(
             valueBoxOutput("pop_box"),
             valueBoxOutput("hh_box"),
             valueBoxOutput("ea_box")
           ),
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
  # PALETTE FOR WARD-LEVEL AGGREGATION
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
      setView(lng=-15.5, lat=13.4, zoom=8)
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
    
    # WARD-LEVEL COLOR MATCH
    ward_sums <- d %>%
      group_by(wardname) %>%
      summarise(value = sum(as.numeric(get(input$field)), na.rm = TRUE)) %>%
      st_as_sf()
    
    map <- map %>%
      addPolygons(
        fillColor = ~p(ward_sums$value[match(wardname, ward_sums$wardname)]),
        fillOpacity = input$polyOpacity,
        color = "black",
        weight = 1,
        popup = ~paste0(
          "<b>EA:</b> ", EA_code, "<br>",
          "<b>LGA:</b> ", lganame, "<br>",
          "<b>District:</b> ", distname, "<br>",
          "<b>Ward:</b> ", wardname, "<br>",
          "<b>", input$field, " (Ward Total):</b> ", 
          ward_sums$value[match(wardname, ward_sums$wardname)]
        )
      )
    
    if(input$points){
      map <- map %>%
        addCircleMarkers(
          data = cent,
          radius = 4,
          fillOpacity = input$pointOpacity,
          color = "blue"
        )
    }
    
    if(input$hotspot){
      map <- map %>%
        addCircles(
          data = cent,
          radius = sqrt(d[[input$field]])*50,
          fillColor = "red",
          fillOpacity = 0.2,
          stroke = FALSE
        )
    }
    
    # ZOOM TO SELECTION
    bbox <- st_bbox(d)
    if(all(is.finite(bbox))){
      map <- map %>% flyToBounds(lng1=bbox["xmin"], lat1=bbox["ymin"],
                                 lng2=bbox["xmax"], lat2=bbox["ymax"])
    }
    
    # LEGEND
    map %>% addLegend(
      position = "bottomright",
      pal = p,
      values = ward_sums$value,
      title = paste(input$field, "(Ward Total)")
    )
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
