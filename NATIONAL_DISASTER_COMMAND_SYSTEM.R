# ============================================================
# 🚨 NATIONAL DISASTER COMMAND SYSTEM v27
# ============================================================

library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(plotly)
library(RColorBrewer)

# ============================================================
# 📦 DATA LOAD
# ============================================================
shp_path <- "C:/Users/ansu.s/Desktop/RCoding/Mapping_Census/RawData/GMB_Population.shp"
gmb <- st_read(shp_path, quiet = TRUE)

if (is.na(st_crs(gmb)$epsg)) st_crs(gmb) <- 4326
gmb <- st_transform(gmb, 4326)

safe_num <- function(x) as.numeric(as.character(x))

# ============================================================
# 🧠 DATA ENGINE (CLEAN + VALIDATE)
# ============================================================
clean_data <- function(df){
  
  df <- df[!st_is_empty(df), ]
  
  df <- df %>%
    filter(!is.na(lganame),
           !is.na(distname),
           !is.na(wardname))
  
  df <- df %>%
    distinct(feacode, .keep_all = TRUE)
  
  df
}

gmb <- clean_data(gmb)

# ============================================================
# 🚨 AI RISK ENGINE
# ============================================================
risk_engine <- function(df){
  
  n <- nrow(df)
  
  df$rain  <- runif(n, 0, 350)
  df$flood <- runif(n, 0, 1)
  df$pop   <- safe_num(df$map_pop)
  
  df$pop_norm <- (df$pop - min(df$pop, na.rm=TRUE)) /
    (max(df$pop, na.rm=TRUE) - min(df$pop, na.rm=TRUE))
  
  df$risk <- (df$rain/350)*0.35 +
    df$flood*0.40 +
    df$pop_norm*0.25
  
  df$alert <- cut(df$risk,
                  breaks=c(0,0.3,0.6,0.8,1),
                  labels=c("GREEN","YELLOW","ORANGE","RED"))
  
  df
}

# ============================================================
# 🧾 UI
# ============================================================
ui <- fluidPage(
  
  tags$head(tags$style(HTML("

    body { background:#07111c; color:white; }

    .panel {
      height: 900px;
      overflow-y: auto;
      background:#0f1f33;
      padding:10px;
      border-radius:10px;
    }

    .mapbox { height: 900px; }

    .credit {
      position:fixed;
      bottom:0;
      width:100%;
      background:black;
      text-align:center;
      font-size:12px;
      padding:5px;
    }

    .red { color:red; font-weight:bold; }
    .orange { color:orange; }
    .yellow { color:yellow; }
    .green { color:lightgreen; }

  "))),
  
  titlePanel("🚨🌍 NATIONAL DISASTER COMMAND SYSTEM v27"),
  
  sidebarLayout(
    
    sidebarPanel(width=3,
                 
                 h4("🧠 CONTROL PANEL"),
                 
                 selectInput("lga","LGA", c("", unique(gmb$lganame))),
                 selectInput("dist","District", c("")),
                 selectInput("ward","Ward", c("")),
                 
                 selectInput("var","Risk Layer",
                             c("Risk Index"="risk",
                               "Rainfall"="rain",
                               "Flood"="flood",
                               "Population"="pop_norm")
                 ),
                 
                 actionButton("run","▶ RUN SYSTEM"),
                 actionButton("reset","🔄 RESET"),
                 downloadButton("download","📥 DOWNLOAD REPORT"),
                 
                 hr(),
                 
                 actionButton("sms","📲 SEND SMS ALERT")
    ),
    
    mainPanel(
      
      fluidRow(
        
        # ============================
        # LEFT ANALYTICS PANEL
        # ============================
        column(4,
               
               div(class="panel",
                   
                   h3("📊 COMMAND ANALYTICS"),
                   
                   textOutput("summary"),
                   textOutput("mean"),
                   textOutput("max"),
                   textOutput("critical"),
                   
                   hr(),
                   
                   h4("🚨 ALERT BREAKDOWN"),
                   textOutput("alert_count"),
                   
                   hr(),
                   
                   plotlyOutput("hist", height=180),
                   plotlyOutput("box", height=180),
                   plotlyOutput("rank", height=220),
                   
                   hr(),
                   
                   h4("🧠 AI INSIGHT"),
                   textOutput("insight"),
                   
                   hr(),
                   
                   h4("📲 SMS STATUS"),
                   textOutput("sms_status")
               )
        ),
        
        # ============================
        # MAP PANEL
        # ============================
        column(8,
               leafletOutput("map", height="900px")
        )
      )
    )
  ),
  
  div(class="credit",
      "Produced by SILKYSING GIS/Carto | National Disaster Command System v27"
  )
)

# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session){
  
  data <- reactiveVal()
  
  # ============================
  # RUN SYSTEM
  # ============================
  observeEvent(input$run, {
    
    df <- risk_engine(gmb)
    
    if(input$lga != "")
      df <- df %>% filter(lganame == input$lga)
    
    if(input$dist != "")
      df <- df %>% filter(distname == input$dist)
    
    if(input$ward != "")
      df <- df %>% filter(wardname == input$ward)
    
    data(df)
  })
  
  # ============================
  # RESET
  # ============================
  observeEvent(input$reset, {
    updateSelectInput(session,"lga",selected="")
    updateSelectInput(session,"dist",selected="")
    updateSelectInput(session,"ward",selected="")
    data(NULL)
  })
  
  # ============================
  # SMS SIMULATION
  # ============================
  observeEvent(input$sms, {
    
    showNotification("🚨 SMS ALERT SENT TO NATIONAL DISASTER UNITS",
                     type="warning")
    
  })
  
  output$sms_status <- renderText({
    "SMS Gateway: READY (SIMULATED MODE)"
  })
  
  # ============================
  # SUMMARY
  # ============================
  output$summary <- renderText({
    
    req(data())
    paste("Total Zones:", nrow(data()))
  })
  
  output$mean <- renderText({
    req(data())
    paste("Mean Risk:", round(mean(data()$risk),3))
  })
  
  output$max <- renderText({
    req(data())
    paste("Max Risk:", round(max(data()$risk),3))
  })
  
  output$critical <- renderText({
    req(data())
    sum(data()$risk > 0.8)
  })
  
  output$alert_count <- renderText({
    req(data())
    paste(table(data()$alert), collapse=" | ")
  })
  
  # ============================
  # INSIGHT ENGINE
  # ============================
  output$insight <- renderText({
    
    req(data())
    
    if(mean(data()$risk) > 0.6){
      "🚨 CRITICAL: National-level disaster exposure detected. Immediate coordination required."
    } else {
      "🟡 Moderate conditions. Monitoring continues."
    }
  })
  
  # ============================
  # CHARTS
  # ============================
  output$hist <- renderPlotly({
    req(data())
    plot_ly(x=data()[[input$var]], type="histogram")
  })
  
  output$box <- renderPlotly({
    req(data())
    plot_ly(y=data()[[input$var]], type="box")
  })
  
  output$rank <- renderPlotly({
    
    req(data())
    
    df <- data() %>%
      st_drop_geometry() %>%
      group_by(lganame) %>%
      summarise(score=mean(.data[[input$var]],na.rm=TRUE)) %>%
      arrange(desc(score))
    
    plot_ly(df,
            x=~reorder(lganame,score),
            y=~score,
            type="bar")
  })
  
  # ============================
  # MAP
  # ============================
  output$map <- renderLeaflet({
    
    req(data())
    
    df <- data()
    
    pal <- colorNumeric("YlOrRd", df[[input$var]])
    
    leaflet(df) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(df[[input$var]]),
        fillOpacity = 0.85,
        color="white",
        weight=1,
        label=~paste(lganame,"Risk:",round(df[[input$var]],3))
      ) %>%
      addLegend("bottomright", pal=pal, values=df[[input$var]])
  })
  
  # ============================
  # DOWNLOAD
  # ============================
  output$download <- downloadHandler(
    filename=function() "disaster_report.csv",
    content=function(file){
      write.csv(st_drop_geometry(data()),file,row.names=FALSE)
    }
  )
  
}

# ============================================================
# RUN APP
# ============================================================
shinyApp(ui, server)

