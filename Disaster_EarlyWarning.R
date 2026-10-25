# ============================
# PACKAGES
# ============================
library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(plotly)
library(RColorBrewer)

# ============================
# LOAD DATA
# ============================
shp_path <- "C:/Users/ansu.s/Desktop/RCoding/Mapping_Census/RawData/GMB_Population.shp"
gmb <- st_read(shp_path, quiet = TRUE)

if (is.na(st_crs(gmb)$epsg)) st_crs(gmb) <- 4326
gmb <- st_transform(gmb, 4326)

safe_num <- function(x) as.numeric(as.character(x))

# ============================
# DISASTER RISK ENGINE
# ============================
generate_risk <- function(df){
  
  n <- nrow(df)
  
  df$rain  <- runif(n, 0, 350)
  df$flood <- runif(n, 0, 1)
  df$pop   <- safe_num(df$map_pop)
  
  df$vulnerability <- (df$pop - min(df$pop, na.rm=TRUE)) /
    (max(df$pop, na.rm=TRUE) - min(df$pop, na.rm=TRUE))
  
  df$risk_index <-
    (df$rain/350)*0.35 +
    df$flood*0.40 +
    df$vulnerability*0.25
  
  df$alert <- cut(df$risk_index,
                  breaks = c(0,0.3,0.6,0.8,1),
                  labels = c("GREEN","YELLOW","ORANGE","RED"))
  
  df
}

# ============================
# UI
# ============================
ui <- fluidPage(
  
  tags$head(tags$style(HTML("

    body { background:#07111c; color:white; }

    .header {
      background:#0b2a44;
      padding:12px;
      border-radius:10px;
      margin-bottom:10px;
    }

    .panel {
      height: 900px;
      overflow-y: auto;
      background:#0f1f33;
      padding:10px;
      border-radius:10px;
    }

    .mapbox {
      height: 900px;
      border-radius:10px;
    }

    .alert-red { color:red; font-weight:bold; }
    .alert-orange { color:orange; font-weight:bold; }
    .alert-yellow { color:yellow; }
    .alert-green { color:lightgreen; }

    .credit {
      position:fixed;
      bottom:0;
      width:100%;
      background:black;
      text-align:center;
      font-size:12px;
      padding:6px;
    }

  "))),
  
  div(class="header",
      h2("🚨🌍 DISASTER EARLY WARNING SYSTEM VERSION 25"),
      p("Flood • Rainfall • Vulnerability Fusion | National Emergency Decision Support System")
  ),
  
  sidebarLayout(
    
    sidebarPanel(width = 3,
                 
                 selectInput("lga", "LGA:",
                             c("", sort(unique(gmb$lganame)))),
                 
                 selectInput("dist", "District:", c("")),
                 selectInput("var", "Risk Layer:",
                             c("Risk Index"="risk_index",
                               "Rainfall"="rain",
                               "Flood Probability"="flood",
                               "Population Vulnerability"="vulnerability")),
                 
                 checkboxInput("live", "Live Monitoring", TRUE),
                 sliderInput("speed", "Refresh Speed (sec)", 1, 10, 3),
                 
                 actionButton("reset", "Reset System")
    ),
    
    mainPanel(
      
      fluidRow(
        
        # ============================
        # LEFT PANEL (CONTROL CENTER)
        # ============================
        column(
          4,
          
          div(class="panel",
              
              h3("📡 EMERGENCY CONTROL CENTER"),
              
              h4("🚨 ALERT SUMMARY"),
              textOutput("alert_summary"),
              
              hr(),
              
              h4("📊 Risk Statistics"),
              textOutput("mean"),
              textOutput("max"),
              textOutput("high_count"),
              
              hr(),
              
              h4("🏆 Highest Risk Areas"),
              tableOutput("top"),
              
              hr(),
              
              h4("📈 Distribution"),
              plotlyOutput("hist", height=200),
              
              h4("📦 Spread Analysis"),
              plotlyOutput("box", height=200),
              
              hr(),
              
              h4("🧠 AI Interpretation"),
              textOutput("insight")
          )
        ),
        
        # ============================
        # MAP PANEL
        # ============================
        column(
          8,
          div(class="mapbox",
              leafletOutput("map", height="900px")
          )
        )
      )
    )
  ),
  
  div(class="credit",
      "Produced by SILKYSING GIS/Carto | Disaster Early Warning System v25 | National Risk Intelligence Unit"
  )
)

# ============================
# SERVER
# ============================
server <- function(input, output, session){
  
  data <- reactiveVal()
  
  # ============================
  # LIVE ENGINE
  # ============================
  observe({
    
    invalidateLater(input$speed * 1000, session)
    
    df <- generate_risk(gmb)
    
    if(input$lga != "")
      df <- df %>% filter(lganame == input$lga)
    
    if(input$dist != "")
      df <- df %>% filter(distname == input$dist)
    
    data(df)
  })
  
  # ============================
  # ALERT SUMMARY
  # ============================
  output$alert_summary <- renderText({
    
    df <- data()
    
    paste0(
      "RED:", sum(df$alert=="RED"), " | ",
      "ORANGE:", sum(df$alert=="ORANGE"), " | ",
      "YELLOW:", sum(df$alert=="YELLOW"), " | ",
      "GREEN:", sum(df$alert=="GREEN")
    )
  })
  
  # ============================
  # STATS
  # ============================
  output$mean <- renderText({
    paste("Mean Risk:", round(mean(data()$risk_index),3))
  })
  
  output$max <- renderText({
    paste("Max Risk:", round(max(data()$risk_index),3))
  })
  
  output$high_count <- renderText({
    paste("Critical Zones:", sum(data()$risk_index > 0.8))
  })
  
  # ============================
  # TOP AREAS
  # ============================
  output$top <- renderTable({
    
    df <- data() %>%
      st_drop_geometry() %>%
      group_by(lganame) %>%
      summarise(risk = mean(risk_index, na.rm=TRUE)) %>%
      arrange(desc(risk)) %>%
      head(10)
    
    df
  })
  
  # ============================
  # INSIGHT ENGINE
  # ============================
  output$insight <- renderText({
    
    df <- data()
    
    if(mean(df$risk_index) > 0.6){
      "HIGH ALERT: Widespread elevated disaster risk detected. Immediate preparedness recommended."
    } else {
      "Moderate risk conditions. Monitoring continues."
    }
  })
  
  # ============================
  # CHARTS
  # ============================
  output$hist <- renderPlotly({
    plot_ly(x = data()$risk_index, type="histogram")
  })
  
  output$box <- renderPlotly({
    plot_ly(y = data()$risk_index, type="box")
  })
  
  # ============================
  # MAP
  # ============================
  output$map <- renderLeaflet({
    
    df <- data()
    
    pal <- colorNumeric("YlOrRd", df$risk_index)
    
    leaflet(df) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(risk_index),
        fillOpacity = 0.85,
        color = "white",
        weight = 1,
        label = ~paste(lganame, "Risk:", round(risk_index,3))
      ) %>%
      addLegend("bottomright", pal = pal, values = df$risk_index,
                title = "Disaster Risk Index")
  })
  
  # ============================
  # RESET
  # ============================
  observeEvent(input$reset, {
    updateSelectInput(session, "lga", selected="")
    updateSelectInput(session, "dist", selected="")
  })
  
}

# ============================
# RUN APP
# ============================
shinyApp(ui, server)
