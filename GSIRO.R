# ============================================================
# 🇬🇲 GSIRO v17 — FULL NATIONAL GIS PLATFORM (FIXED + ADVANCED)
# ============================================================

library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(plotly)
library(RColorBrewer)

# =========================
# LOAD DATA
# =========================
gmb_sf <- st_read("C:/Users/ansu.s/Desktop/RCoding/Mapping_Census/RawData/GMB_Population.shp", quiet = TRUE)

gmb_sf <- st_transform(gmb_sf, 4326)
gmb_sf <- gmb_sf[!st_is_empty(gmb_sf), ]

gmb_sf$lganame  <- ifelse(is.na(gmb_sf$lganame), "Unknown", gmb_sf$lganame)
gmb_sf$distname <- ifelse(is.na(gmb_sf$distname), gmb_sf$lganame, gmb_sf$distname)
gmb_sf$wardname <- ifelse(is.na(gmb_sf$wardname), gmb_sf$distname, gmb_sf$wardname)

# =========================
# NORMALIZATION
# =========================
norm01 <- function(x){
  if (all(is.na(x))) return(rep(0, length(x)))
  (x - min(x, na.rm=TRUE)) /
    (max(x, na.rm=TRUE) - min(x, na.rm=TRUE) + 1e-9)
}

# =========================
# RISK ENGINE
# =========================
gmb_sf$flood_risk      <- norm01(gmb_sf$comp_map)
gmb_sf$poverty_risk    <- norm01(gmb_sf$ls_pop)
gmb_sf$population_risk <- norm01(gmb_sf$enum_pop)

gmb_sf$composite_risk <-
  0.40*gmb_sf$flood_risk +
  0.35*gmb_sf$poverty_risk +
  0.25*gmb_sf$population_risk

gmb_sf$risk_class <- cut(
  gmb_sf$composite_risk,
  breaks=c(0,0.2,0.4,0.6,0.8,1),
  labels=c("Very Low","Low","Moderate","High","Extreme"),
  include.lowest=TRUE
)

# ============================================================
# UI
# ============================================================
ui <- fluidPage(
  
  tags$head(tags$style(HTML("
    html, body {height:100%; margin:0; overflow:hidden;}

    /* 🇬🇲 FLAG BACKGROUND (SAFE) */
    body::before {
      content:'';
      position:fixed;
      top:0; left:0;
      width:100%; height:100%;
      z-index:-100;
      opacity:0.12;
      background: linear-gradient(
        to bottom,
        #CE1126 0%, #CE1126 33%,
        #FFFFFF 33%, #FFFFFF 38%,
        #0C1C8C 38%, #0C1C8C 62%,
        #FFFFFF 62%, #FFFFFF 67%,
        #3A7728 67%, #3A7728 100%
      );
    }

    /* TITLE FIX */
    .title-wrap{
      position:sticky;
      top:0;
      z-index:9999;
      background:white;
      padding:10px;
      font-weight:bold;
      text-align:center;
      border-bottom:1px solid #ccc;
    }

    .sidebar {
      height:100vh;
      overflow-y:auto;
      background:rgba(255,255,255,0.95);
      padding:15px;
    }

    .mapbox {height:100vh;}

  "))),
  
  div(class="title-wrap",
      "GSIRO — NATIONAL GIS RISK INTELLIGENCE PLATFORM"
  ),
  
  fluidRow(
    
    # ================= LEFT PANEL =================
    column(3,
           
           div(class="sidebar",
               
               h4("CONTROLS"),
               
               selectInput("lga","LGA", c("All", unique(gmb_sf$lganame))),
               selectInput("dist","District", c("All")),
               selectInput("ward","Ward", c("All")),
               
               radioButtons("view","View Mode",
                            c("Map","Charts","Stats","Radar")),
               
               selectInput("chartType","Chart Type",
                           c("Bar","Pie")),
               
               selectInput("basemap","Basemap",
                           c("OpenStreetMap","Esri Satellite","Dark")),
               
               selectInput("report","System Report",
                           c("Overview","Methodology","Risk Model","Credits")),
               
               actionButton("reset","🔄 Reset"),
               
               downloadButton("dl","⬇ Filtered"),
               downloadButton("dl_all","⬇ Full"),
               
               hr(),
               uiOutput("report_text")
           )
    ),
    
    # ================= RIGHT PANEL =================
    column(9,
           
           conditionalPanel(
             "input.view=='Map'",
             leafletOutput("map", height="100vh")
           ),
           
           conditionalPanel(
             "input.view=='Charts'",
             plotlyOutput("chart", height="100vh")
           ),
           
           conditionalPanel(
             "input.view=='Stats'",
             verbatimTextOutput("stats")
           ),
           
           conditionalPanel(
             "input.view=='Radar'",
             plotlyOutput("radar", height="100vh")
           )
    )
  )
)

# ============================================================
# SERVER (FULL FIXED LOGIC)
# ============================================================
server <- function(input, output, session){
  
  # RESET
  observeEvent(input$reset,{
    updateSelectInput(session,"lga", selected="All")
    updateSelectInput(session,"dist", selected="All")
    updateSelectInput(session,"ward", selected="All")
  })
  
  # ================= CASCADING FIX =================
  observe({
    updateSelectInput(session,"lga",
                      choices=c("All", sort(unique(gmb_sf$lganame))))
  })
  
  observe({
    df <- gmb_sf
    if(input$lga!="All") df <- df[df$lganame==input$lga,]
    updateSelectInput(session,"dist",
                      choices=c("All", sort(unique(df$distname))))
  })
  
  observe({
    df <- gmb_sf
    if(input$lga!="All") df <- df[df$lganame==input$lga,]
    if(input$dist!="All") df <- df[df$distname==input$dist,]
    updateSelectInput(session,"ward",
                      choices=c("All", sort(unique(df$wardname))))
  })
  
  # ================= FILTER =================
  filtered_sf <- reactive({
    df <- gmb_sf
    if(input$lga!="All") df <- df[df$lganame==input$lga,]
    if(input$dist!="All") df <- df[df$distname==input$dist,]
    if(input$ward!="All") df <- df[df$wardname==input$ward,]
    df
  })
  
  # ================= MAP =================
  output$map <- renderLeaflet({
    df <- filtered_sf()
    
    pal <- colorFactor("YlOrRd", df$risk_class)
    
    base <- switch(input$basemap,
                   "OpenStreetMap" = leaflet() %>% addTiles(),
                   "Esri Satellite" = leaflet() %>% addProviderTiles("Esri.WorldImagery"),
                   "Dark" = leaflet() %>% addProviderTiles("CartoDB.DarkMatter")
    )
    
    base %>%
      addPolygons(data=df,
                  fillColor=~pal(risk_class),
                  fillOpacity=0.7,
                  color="white",
                  weight=1) %>%
      addLegend("bottomright", pal=pal, values=df$risk_class)
  })
  
  # ================= CHART SWITCH =================
  output$chart <- renderPlotly({
    df <- filtered_sf() %>%
      group_by(lganame) %>%
      summarise(risk=mean(composite_risk, na.rm=TRUE))
    
    if(input$chartType=="Bar"){
      plot_ly(df, x=~lganame, y=~risk, type="bar")
    } else {
      plot_ly(df, labels=~lganame, values=~risk, type="pie")
    }
  })
  
  # ================= RADAR =================
  output$radar <- renderPlotly({
    df <- filtered_sf()
    
    plot_ly(
      type='scatterpolar',
      r=c(mean(df$flood_risk), mean(df$poverty_risk),
          mean(df$population_risk), mean(df$composite_risk)),
      theta=c("Flood","Poverty","Population","Composite"),
      fill='toself'
    )
  })
  
  # ================= REPORT =================
  output$report_text <- renderUI({
    switch(input$report,
           "Overview" = HTML("National GIS Risk Intelligence Platform for The Gambia."),
           "Methodology" = HTML("Weighted model: Flood 40%, Poverty 35%, Population 25%"),
           "Risk Model" = HTML("Normalized composite risk index (0–1)."),
           "Credits" = HTML("Developed by GIS Unit — Gambia Bureau of Statistics")
    )
  })
  
  # ================= STATS =================
  output$stats <- renderText({
    df <- filtered_sf()
    
    paste0(
      "Records: ", nrow(df), "\n",
      "Mean Risk: ", round(mean(df$composite_risk),3), "\n",
      "Median Risk: ", round(median(df$composite_risk),3), "\n",
      "Max Risk: ", round(max(df$composite_risk),3), "\n",
      "Min Risk: ", round(min(df$composite_risk),3)
    )
  })
  
  # ================= DOWNLOAD =================
  output$dl <- downloadHandler(
    filename=function() "filtered.csv",
    content=function(file){
      write.csv(st_drop_geometry(filtered_sf()), file, row.names=FALSE)
    }
  )
  
  output$dl_all <- downloadHandler(
    filename=function() "full.csv",
    content=function(file){
      write.csv(st_drop_geometry(gmb_sf), file, row.names=FALSE)
    }
  )
}

shinyApp(ui, server)
