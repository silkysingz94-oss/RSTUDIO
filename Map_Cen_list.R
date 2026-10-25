# GAMBIA ADVANCED EA DASHBOARD
# Mapping & Census: Households, Population, Buildings, Compounds
# Produced by: Ansu Singhateh, GIS Analyst, GBoS
# ==========================================

library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(plotly)
library(shinydashboard)

# -----------------------------
# LOAD DATA
# -----------------------------
EA <- st_read("C:/Users/ansu.s/Desktop/RCoding/Mapping_Census/Scripts/Map_Cen_list.R", quiet = TRUE)

# Ensure CRS is longlat WGS84
if (is.na(st_crs(EA))) st_crs(EA) <- 4326
EA <- st_transform(EA, 4326)
names(EA) <- make.unique(names(EA))

# Sample AQ data (replace with real Sentinel-5P data)
set.seed(123)
aq_sf <- st_centroid(EA) %>%
  cbind(data.frame(
    date = rep(seq.Date(Sys.Date()-6, Sys.Date(), by="day"), length.out = nrow(EA)),
    CO = runif(nrow(EA), 0, 10),
    NO2 = runif(nrow(EA), 0, 20),
    SO2 = runif(nrow(EA), 0, 5),
    O3 = runif(nrow(EA), 0, 8),
    CH4 = runif(nrow(EA), 0, 15)
  ))

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
  div(id="title-bar", "MAPPING & CENSUS: HOUSEHOLDS, POPULATION, BUILDINGS, COMPOUNDS"),
  
  fluidRow(
    # LEFT PANEL
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
           h3("🌫️ Air Quality"),
           selectInput("aq_pollutants","Select Pollutants",
                       choices = c("CO","NO2","SO2","O3","CH4"), multiple = TRUE),
           sliderInput("aq_date","Select Date", min=min(aq_sf$date), max=max(aq_sf$date),
                       value=min(aq_sf$date), step=1, animate=animationOptions(interval=1000, loop=TRUE)),
           actionButton("aq_play","▶ Play"),
           actionButton("aq_pause","⏸ Pause"),
           downloadButton("download_aq","⬇ Download AQ Data"),
           hr(),
           strong("Produced by Ansu Singhateh"),
           p("GIS Analyst, Gambia Bureau of Statistics")
    ),
    
    # CENTER PANEL
    column(6, leafletOutput("map", height = "90vh")),
    
    # RIGHT PANEL
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
  # KPI BOXES
  # -----------------------------
  output$pop_box <- renderValueBox({
    valueBox(format(sum(data_filt()$map_pop, na.rm=TRUE), big.mark=","),
             "Total Population", icon = icon("users"), color = "red")
  })
  
  output$hh_box <- renderValueBox({
    valueBox(format(sum(data_filt()$map_hhs, na.rm=TRUE), big.mark=","),
             "Total Households", icon = icon("home"), color = "blue")
  })
  
  output$ea_box <- renderValueBox({
    valueBox(nrow(data_filt()), "Total EAs", icon = icon("map"), color = "green")
  })
  
  # -----------------------------
  # INITIAL MAP
  # -----------------------------
  output$map <- renderLeaflet({
    leaflet() %>% addProviderTiles(providers$OpenStreetMap) %>%
      setView(lng=-15.5, lat=13.4, zoom=8)
  })
  
  # -----------------------------
  # MAP UPDATE
  # -----------------------------
  observe({
    d <- data_filt()
    pal <- colorNumeric("YlOrRd", d[[input$field]], na.color="transparent")
    
    base <- switch(input$basemap,
                   "OpenStreetMap" = providers$OpenStreetMap,
                   "Light" = providers$CartoDB.Positron,
                   "Dark" = providers$CartoDB.DarkMatter,
                   "Satellite" = providers$Esri.WorldImagery)
    
    cent <- st_point_on_surface(d)
    
    leafletProxy("map", data=d) %>%
      clearShapes() %>% clearMarkers() %>% clearControls() %>% clearTiles() %>%
      addProviderTiles(base) %>%
      addPolygons(fillColor = ~pal(get(input$field)), fillOpacity = input$polyOpacity,
                  color="white", weight=1,
                  popup = ~paste0("<b>EA:</b> ", EA_code,
                                  "<br><b>LGA:</b> ", lganame,
                                  "<br><b>District:</b> ", distname,
                                  "<br><b>Ward:</b> ", wardname,
                                  "<br><b>", input$field, ":</b> ", get(input$field))) %>%
      { if(input$points) addCircleMarkers(., data=cent, radius=4, fillOpacity=input$pointOpacity, color="blue") else . } %>%
      { if(input$hotspot) addCircles(., data=cent, radius=sqrt(d[[input$field]])*50,
                                     fillColor="red", fillOpacity=0.2, stroke=FALSE) else . } %>%
      addLegend(position="bottomright", pal=pal, values=d[[input$field]], title=input$field)
  })
  
  # -----------------------------
  # BAR CHART
  # -----------------------------
  output$bar_chart <- renderPlotly({
    df <- st_drop_geometry(data_filt()) %>%
      group_by(wardname) %>%
      summarise(value=sum(as.numeric(get(input$field)), na.rm=TRUE))
    pal <- colorNumeric("YlOrRd", df$value)
    
    plot_ly(df, x=~wardname, y=~value, type="bar",
            marker=list(color=pal(df$value))) %>%
      layout(xaxis=list(title="Ward"), yaxis=list(title=input$field))
  })
  
  # -----------------------------
  # PIE CHART
  # -----------------------------
  output$pie_chart <- renderPlotly({
    df <- st_drop_geometry(data_filt()) %>%
      group_by(wardname) %>%
      summarise(value=sum(as.numeric(get(input$field)), na.rm=TRUE))
    pal <- colorNumeric("YlOrRd", df$value)
    
    plot_ly(df, labels=~wardname, values=~value, type="pie",
            marker=list(colors=pal(df$value)))
  })
  
  # -----------------------------
  # AQ ANIMATION
  # -----------------------------
  aq_day <- reactiveVal(min(aq_sf$date))
  play_status <- reactiveVal(FALSE)
  
  observeEvent(input$aq_play, { play_status(TRUE) })
  observeEvent(input$aq_pause, { play_status(FALSE) })
  
  observe({
    req(input$aq_pollutants)
    d <- aq_sf %>% filter(date == aq_day())
    
    map <- leafletProxy("map") %>% clearGroup("AQ") %>% clearControls()
    
    for(poll in input$aq_pollutants){
      pal_aq <- colorNumeric("RdYlBu", d[[poll]], na.color="transparent")
      radius_vals <- sqrt(d[[poll]] / max(d[[poll]], na.rm=TRUE)) * 20
      
      map <- map %>% addCircleMarkers(data=d,
                                      radius=radius_vals,
                                      color=~pal_aq(get(poll)),
                                      fillOpacity=0.7,
                                      stroke=FALSE,
                                      group=poll,
                                      label=~paste0("<b>EA:</b> ", EA_code,
                                                    "<br><b>Ward:</b> ", wardname,
                                                    "<br><b>", poll, ":</b> ", round(get(poll),2)))
    }
    
    map %>% addLayersControl(overlayGroups=input$aq_pollutants,
                             options=layersControlOptions(collapsed=FALSE))
  })
  
  # Animate AQ day forward
  observe({
    if(play_status()){
      invalidateLater(1000, session)
      next_day <- aq_day() + 1
      if(next_day > max(aq_sf$date)) next_day <- min(aq_sf$date)
      aq_day(next_day)
      updateSliderInput(session, "aq_date", value=next_day)
    }
  })
  
  # AQ download
  output$download_aq <- downloadHandler(
    filename = "aq_data.csv",
    content = function(file){
      write.csv(st_drop_geometry(aq_sf), file, row.names=FALSE)
    }
  )
}

# -----------------------------
# RUN APP
# -----------------------------
shinyApp(ui, server)

