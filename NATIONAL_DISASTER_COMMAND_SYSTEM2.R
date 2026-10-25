# ============================================================
# 🚨 NATIONAL DISASTER COMMAND SYSTEM v29 (PRODUCTION)
# ============================================================

library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(plotly)
library(RColorBrewer)

# ============================================================
# LOAD DATA
# ============================================================
shp_path <- "C:/Users/user/Desktop/RCoding/Mapping_Census/RawData/GMB_Population.shp"
gmb <- st_read(shp_path, quiet = TRUE)

if (is.na(st_crs(gmb)$epsg)) st_crs(gmb) <- 4326
gmb <- st_transform(gmb, 4326)

# ============================================================
# CLEAN DATA (CRITICAL FIX)
# ============================================================
gmb$lganame  <- as.character(trimws(gmb$lganame))
gmb$distname <- as.character(trimws(gmb$distname))
gmb$wardname <- as.character(trimws(gmb$wardname))

gmb <- gmb %>%
  filter(!is.na(lganame),
         !is.na(distname),
         !is.na(wardname))

safe_num <- function(x) as.numeric(as.character(x))

# ============================================================
# AI RISK ENGINE
# ============================================================
risk_engine <- function(df){
  
  n <- nrow(df)
  
  df$rain  <- runif(n, 0, 400)
  df$flood <- runif(n, 0, 1)
  
  df$pop <- safe_num(df$map_pop)
  
  df$pop_norm <- (df$pop - min(df$pop, na.rm=TRUE)) /
    (max(df$pop, na.rm=TRUE) - min(df$pop, na.rm=TRUE))
  
  df$risk <- (df$rain/400)*0.35 +
    df$flood*0.40 +
    df$pop_norm*0.25
  
  df$alert <- cut(df$risk,
                  breaks=c(0,0.3,0.6,0.8,1),
                  labels=c("🟢 LOW","🟡 MEDIUM","🟠 HIGH","🔴 CRITICAL"))
  
  df
}

# ============================================================
# UI
# ============================================================
ui <- fluidPage(
  
  tags$head(tags$style(HTML("

    body { background:#0b1220; color:white; }

    .panel {
      height: 900px;
      overflow-y:auto;
      background:#111a2e;
      padding:12px;
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
      padding:6px;
    }

  "))),
  
  titlePanel("🚨🌍 NATIONAL DISASTER COMMAND SYSTEM v29"),
  
  sidebarLayout(
    
    sidebarPanel(width = 3,
                 
                 h4("🧠 CONTROL PANEL"),
                 
                 selectInput("lga","LGA", choices=c("")),
                 selectInput("dist","District", choices=c("")),
                 selectInput("ward","Ward", choices=c("")),
                 
                 selectInput("var","Variable",
                             c("Risk Index"="risk",
                               "Rainfall"="rain",
                               "Flood"="flood",
                               "Population"="pop_norm")),
                 
                 actionButton("run","▶ RUN SYSTEM"),
                 actionButton("reset","🔄 RESET"),
                 downloadButton("download","📥 DOWNLOAD REPORT")
    ),
    
    mainPanel(
      
      fluidRow(
        
        # ============================
        # LEFT PANEL (STATS + CHARTS)
        # ============================
        column(4,
               
               div(class="panel",
                   
                   h3("📊 STATISTICS ENGINE"),
                   
                   textOutput("total"),
                   textOutput("mean"),
                   textOutput("sd"),
                   textOutput("max"),
                   textOutput("cv"),
                   
                   hr(),
                   
                   h4("🚨 ALERT SUMMARY"),
                   tableOutput("alert_table"),
                   
                   hr(),
                   
                   plotlyOutput("hist", height=180),
                   plotlyOutput("box", height=180),
                   plotlyOutput("rank", height=220),
                   
                   hr(),
                   
                   h4("🧠 AI INSIGHT"),
                   textOutput("insight")
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
      "Produced by SILKYSING GIS/Carto | Census Mapping 2023–2024 | National Disaster Command System v29"
  )
)

# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session){
  
  base <- reactiveVal(gmb)
  data <- reactiveVal(NULL)
  
  # ============================================================
  # FIXED CASCADING FILTERS
  # ============================================================
  observe({
    updateSelectInput(session,"lga",
                      choices=c("", sort(unique(base()$lganame))))
  })
  
  observe({
    df <- base()
    if(input$lga!="") df <- df[df$lganame==input$lga,]
    updateSelectInput(session,"dist",
                      choices=c("", sort(unique(df$distname))))
  })
  
  observe({
    df <- base()
    if(input$lga!="") df <- df[df$lganame==input$lga,]
    if(input$dist!="") df <- df[df$distname==input$dist,]
    updateSelectInput(session,"ward",
                      choices=c("", sort(unique(df$wardname))))
  })
  
  # ============================================================
  # RUN SYSTEM
  # ============================================================
  observeEvent(input$run, {
    
    df <- base()
    
    if(input$lga!="") df <- df[df$lganame==input$lga,]
    if(input$dist!="") df <- df[df$distname==input$dist,]
    if(input$ward!="") df <- df[df$wardname==input$ward,]
    
    df <- risk_engine(df)
    
    data(df)
  })
  
  # ============================================================
  # RESET
  # ============================================================
  observeEvent(input$reset, {
    
    updateSelectInput(session,"lga",selected="")
    updateSelectInput(session,"dist",selected="")
    updateSelectInput(session,"ward",selected="")
    
    data(NULL)
  })
  
  # ============================================================
  # STATS
  # ============================================================
  output$total <- renderText({
    req(data())
    paste("Zones:", nrow(data()))
  })
  
  output$mean <- renderText({
    req(data())
    paste("Mean Risk:", round(mean(data()$risk),3))
  })
  
  output$sd <- renderText({
    req(data())
    paste("SD:", round(sd(data()$risk),3))
  })
  
  output$max <- renderText({
    req(data())
    paste("Max Risk:", round(max(data()$risk),3))
  })
  
  output$cv <- renderText({
    req(data())
    paste("CV:", round(sd(data()$risk)/mean(data()$risk),3))
  })
  
  output$insight <- renderText({
    req(data())
    
    if(mean(data()$risk) > 0.6){
      "🚨 HIGH NATIONAL RISK DETECTED — Immediate response required"
    } else {
      "🟡 Moderate conditions — monitoring active"
    }
  })
  
  # ============================================================
  # ALERT TABLE
  # ============================================================
  output$alert_table <- renderTable({
    req(data())
    as.data.frame(table(data()$alert))
  })
  
  # ============================================================
  # CHARTS
  # ============================================================
  output$hist <- renderPlotly({
    req(data())
    plot_ly(x=data()$risk, type="histogram")
  })
  
  output$box <- renderPlotly({
    req(data())
    plot_ly(y=data()$risk, type="box")
  })
  
  output$rank <- renderPlotly({
    
    req(data())
    
    df <- data() %>%
      st_drop_geometry() %>%
      group_by(lganame) %>%
      summarise(score=mean(risk,na.rm=TRUE)) %>%
      arrange(desc(score))
    
    plot_ly(df,
            x=~reorder(lganame,score),
            y=~score,
            type="bar")
  })
  
  # ============================================================
  # MAP
  # ============================================================
  output$map <- renderLeaflet({
    
    req(data())
    
    df <- data()
    
    pal <- colorNumeric("YlOrRd", df$risk)
    
    leaflet(df) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(risk),
        fillOpacity = 0.8,
        weight = 1,
        color = "white",
        label = ~paste(lganame, distname, round(risk,3))
      ) %>%
      addLegend("bottomright", pal=pal, values=df$risk)
  })
  
  # ============================================================
  # DOWNLOAD
  # ============================================================
  output$download <- downloadHandler(
    filename="disaster_report.csv",
    content=function(file){
      write.csv(st_drop_geometry(data()),file,row.names=FALSE)
    }
  )
  
}

# ============================================================
# RUN
# ============================================================
shinyApp(ui, server)
