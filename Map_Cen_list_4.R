# =========================================================
# 🌍 GEOAI v19 AI GIS SYSTEM (TIME SLIDER + SATELLITE AI)
# =========================================================

library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(plotly)
library(shinydashboard)

# =========================================================
# LOAD DATA
# =========================================================
shp_path <- "C:/Users/ansu.s/Desktop/RCoding/Mapping_Census/RawData/GMB_Population.shp"

if(!file.exists(shp_path)){
  stop("❌ Shapefile not found")
}

EA <- st_read(shp_path, quiet = TRUE) %>% st_transform(4326)
names(EA) <- make.unique(names(EA))

# SAFE FIELDS
EA$region   <- as.character(EA$lganame)
EA$district <- as.character(EA$distname)
EA$ward     <- as.character(EA$wardname)

EA$pop <- as.numeric(EA$map_pop)
EA$hh  <- as.numeric(EA$map_hhs)

CENT <- st_point_on_surface(EA)

# =========================================================
# TIME SERIES AIR QUALITY (CORE UPGRADE)
# =========================================================
set.seed(42)

days <- paste0("Day_", 1:14)   # 🔥 extended time slider
pollutants <- c("CO","NO2","SO2","O3","PM25")

AQ <- do.call(rbind, lapply(days, function(d){
  
  data.frame(
    id = 1:nrow(EA),
    region = EA$region,
    district = EA$district,
    day = d,
    geometry = st_geometry(CENT),
    
    CO   = runif(nrow(EA), 0, 10),
    NO2  = runif(nrow(EA), 0, 25),
    SO2  = runif(nrow(EA), 0, 8),
    O3   = runif(nrow(EA), 0, 12),
    PM25 = runif(nrow(EA), 0, 40)
  )
}))

AQ <- st_as_sf(AQ, sf_column_name="geometry", crs=4326)

# AQ CLASSIFICATION
aq_class <- function(x){
  ifelse(x < 5, "Good",
         ifelse(x < 10, "Moderate",
                ifelse(x < 20, "High", "Very High")))
}

AQ_COLORS <- c(
  "Good"="#2ECC71",
  "Moderate"="#F1C40F",
  "High"="#E67E22",
  "Very High"="#E74C3C"
)

# =========================================================
# UI
# =========================================================
ui <- dashboardPage(
  
  dashboardHeader(title="🌍 GEOAI VERSION 19 AI GIS SYSTEM"),
  
  dashboardSidebar(
    
    selectInput("region","Region", c("All", unique(EA$region))),
    uiOutput("district_ui"),
    uiOutput("ward_ui"),
    
    selectInput("field","Variable", c("pop","hh")),
    selectInput("pollutant","Air Quality", pollutants),
    
    # 🕒 TIME SLIDER (KEY FEATURE)
    sliderInput("day","Time Slider (Days)",
                min=1, max=length(days),
                value=1, step=1,
                animate = animationOptions(interval = 1200, loop = TRUE)),
    
    checkboxInput("show_aq","Show AQ Layer", TRUE),
    
    radioButtons("view","View Mode",
                 choices=c("Map","Chart"),
                 selected="Map"),
    
    actionButton("run","🚀 RUN"),
    actionButton("reset","🔄 RESET")
  ),
  
  dashboardBody(
    
    fluidRow(
      valueBoxOutput("v1"),
      valueBoxOutput("v2"),
      valueBoxOutput("v3")
    ),
    
    fluidRow(
      column(8, uiOutput("main")),
      column(4,
             box(
               title="📘 GEOAI CREDIT",
               width=12,
               status="primary",
               solidHeader=TRUE,
               HTML("
               <b>GEOAI v19 AI GIS SYSTEM</b><br>
               Developer: Ansu Singhateh<br>
               Institution: Gambia Bureau of Statistics<br>
               Engine: R + Shiny + SpatioTemporal AI<br>
               Feature: Animated Air Quality Time Series
               ")
             )
      )
    )
  )
)

# =========================================================
# SERVER
# =========================================================
server <- function(input, output, session){
  
  # ---------------- REGION → DISTRICT FIX ----------------
  output$district_ui <- renderUI({
    
    d <- EA
    if(input$region != "All"){
      d <- d %>% filter(region == input$region)
    }
    
    selectInput("district","District",
                c("All", unique(d$district)))
  })
  
  # ---------------- DISTRICT → WARD FIX ----------------
  output$ward_ui <- renderUI({
    
    d <- EA
    if(input$region != "All") d <- d %>% filter(region == input$region)
    if(!is.null(input$district) && input$district != "All")
      d <- d %>% filter(district == input$district)
    
    selectInput("ward","Ward",
                c("All", unique(d$ward)))
  })
  
  # ---------------- FILTER ----------------
  filtered <- reactive({
    
    d <- EA
    
    if(input$region != "All")
      d <- d %>% filter(region == input$region)
    
    if(!is.null(input$district) && input$district != "All")
      d <- d %>% filter(district == input$district)
    
    if(!is.null(input$ward) && input$ward != "All")
      d <- d %>% filter(ward == input$ward)
    
    d
  })
  
  run_data <- eventReactive(input$run, filtered())
  
  # =========================================================
  # RESET
  # =========================================================
  observeEvent(input$reset,{
    leafletProxy("map") %>% setView(-15.5,13.4,8)
  })
  
  # =========================================================
  # VALUE BOXES
  # =========================================================
  output$v1 <- renderValueBox({
    valueBox(nrow(run_data()), "EAs", icon("map"), color="blue")
  })
  
  output$v2 <- renderValueBox({
    valueBox(sum(run_data()$pop, na.rm=TRUE), "Population",
             icon("users"), color="red")
  })
  
  output$v3 <- renderValueBox({
    valueBox(length(unique(run_data()$district)),
             "Districts", icon("globe"), color="green")
  })
  
  # =========================================================
  # VIEW SWITCH
  # =========================================================
  output$main <- renderUI({
    if(input$view == "Map"){
      leafletOutput("map", height=700)
    } else {
      plotlyOutput("chart", height=700)
    }
  })
  
  # =========================================================
  # BASE MAP
  # =========================================================
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      setView(-15.5,13.4,8)
  })
  
  # =========================================================
  # MAP + TIME ANIMATION (CORE FEATURE)
  # =========================================================
  observeEvent(input$run, {
    
    d <- run_data()
    
    pal <- colorNumeric("YlOrRd", d[[input$field]])
    
    AQ_day <- AQ %>%
      filter(day == days[input$day]) %>%
      filter(region %in% d$region)
    
    AQ_day$val <- AQ_day[[input$pollutant]]
    AQ_day$class <- aq_class(AQ_day$val)
    
    leafletProxy("map") %>%
      clearShapes() %>%
      clearMarkers() %>%
      clearControls()
    
    leafletProxy("map") %>%
      addPolygons(
        data=d,
        fillColor=~pal(get(input$field)),
        fillOpacity=0.6,
        color="white",
        popup=~paste0(
          "Region: ", region,
          "<br>District: ", district,
          "<br>Ward: ", ward,
          "<br>Pop: ", pop
        )
      )
    
    if(input$show_aq){
      
      leafletProxy("map") %>%
        addCircleMarkers(
          data=AQ_day,
          radius=5,
          color=~AQ_COLORS[class],
          fillOpacity=0.8,
          popup=~paste0(
            "Day: ", day,
            "<br>AQ: ", round(val,2),
            "<br>Status: ", class
          )
        )
    }
  })
  
  # =========================================================
  # CHART VIEW
  # =========================================================
  output$chart <- renderPlotly({
    
    d <- run_data() %>%
      group_by(district) %>%
      summarise(v = sum(.data[[input$field]], na.rm=TRUE))
    
    plot_ly(d, x=~district, y=~v, type="bar")
    
  })
  
  
  
  
}

# =========================================================
# RUN APP
# =========================================================
shinyApp(ui, server)
