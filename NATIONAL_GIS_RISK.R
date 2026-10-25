# ============================================================
# 🇬🇲 GSIRO — NATIONAL GIS + EARLY WARNING PLATFORM (STABLE)
# ============================================================

library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(plotly)

# =========================
# LOAD DATA
# =========================

gmb_sf <- st_read( "C:/Users/ansu.s/Desktop/RCoding/Mapping_Census/RawData/GMB_Population.shp", quiet = TRUE)

gmb_sf <- st_transform(gmb_sf, 4326)
gmb_sf <- gmb_sf[!st_is_empty(gmb_sf), ]

gmb_sf$lganame  <- ifelse(is.na(gmb_sf$lganame), "Unknown", gmb_sf$lganame)
gmb_sf$distname <- ifelse(is.na(gmb_sf$distname), gmb_sf$lganame, gmb_sf$distname)
gmb_sf$wardname <- ifelse(is.na(gmb_sf$wardname), gmb_sf$distname, gmb_sf$wardname)

# =========================
# NORMALIZATION
# =========================
norm01 <- function(x){
  if(all(is.na(x))) return(rep(0,length(x)))
  (x - min(x,na.rm=TRUE)) / (max(x,na.rm=TRUE) - min(x,na.rm=TRUE) + 1e-9)
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

    body::before{
      content:'';
      position:fixed;
      width:100%; height:100%;
      z-index:-10;
      background:linear-gradient(to bottom,
        #CE1126 0%, #CE1126 33%,
        #FFFFFF 33%, #FFFFFF 38%,
        #0C1C8C 38%, #0C1C8C 62%,
        #FFFFFF 62%, #FFFFFF 67%,
        #3A7728 67%, #3A7728 100%);
      opacity:0.12;
    }

    .title{
      position:sticky;
      top:0;
      z-index:9999;
      background:white;
      padding:10px;
      font-weight:bold;
      text-align:center;
      border-bottom:1px solid #ddd;
    }

    .sidebar{
      height:100vh;
      overflow-y:auto;
      background:rgba(255,255,255,0.95);
      padding:15px;
    }

    .mapbox{
      height:100vh;
      position:relative;
    }

    .leaflet-container{
      height:100% !important;
      width:100% !important;
    }
  "))),
  
  div(class="title",
      "🇬🇲 GSIRO — NATIONAL GIS RISK & EARLY WARNING PLATFORM"
  ),
  
  fluidRow(
    
    # ================= LEFT =================
    column(3,
           
           div(class="sidebar",
               
               h4("CONTROL PANEL"),
               
               selectInput("lga","LGA", choices=c("All")),
               selectInput("dist","District", choices=c("All")),
               selectInput("ward","Ward", choices=c("All")),
               
               selectInput("basemap","Basemap",
                           choices=c("OSM","Satellite","Dark")),
               
               selectInput("report","Report Section",
                           choices=c("Overview","Methodology","Risk Model","Credits")),
               
               actionButton("reset","🔄 Reset"),
               downloadButton("dl","⬇ Filtered"),
               downloadButton("dl_all","⬇ Full Dataset"),
               
               hr(),
               uiOutput("report_text")
           )
    ),
    
    # ================= RIGHT =================
    column(9,
           
           div(class="mapbox",
               leafletOutput("map"),
               plotlyOutput("chart", height="300px")
           )
    )
  )
)

# ============================================================
# SERVER (FULL FIXED LOGIC)
# ============================================================

server <- function(input, output, session){
  
  # ---------------- INIT DROPDOWNS ----------------
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
  
  # ---------------- RESET ----------------
  observeEvent(input$reset,{
    updateSelectInput(session,"lga",selected="All")
    updateSelectInput(session,"dist",selected="All")
    updateSelectInput(session,"ward",selected="All")
  })
  
  # ---------------- FILTER ----------------
  filtered_sf <- reactive({
    df <- gmb_sf
    if(input$lga!="All") df <- df[df$lganame==input$lga,]
    if(input$dist!="All") df <- df[df$distname==input$dist,]
    if(input$ward!="All") df <- df[df$wardname==input$ward,]
    df
  })
  
  # ---------------- BASEMAP FIX ----------------
  base_map <- reactive({
    switch(input$basemap,
           "OSM" = leaflet() %>% addTiles(),
           "Satellite" = leaflet() %>% addProviderTiles("Esri.WorldImagery"),
           "Dark" = leaflet() %>% addProviderTiles("CartoDB.DarkMatter")
    )
  })
  
  # ---------------- MAP ----------------
  output$map <- renderLeaflet({
    
    df <- filtered_sf()
    pal <- colorFactor("YlOrRd", df$risk_class)
    
    base_map() %>%
      addPolygons(
        data=df,
        fillColor=~pal(risk_class),
        fillOpacity=0.7,
        color="white",
        weight=1,
        popup=~paste0(lganame," | ",risk_class,
                      "<br>Risk: ",round(composite_risk,3))
      ) %>%
      addLegend("bottomright",
                pal=pal,
                values=df$risk_class,
                title="Risk Level")
  })
  
  # ---------------- REPORT (ALWAYS WORKS) ----------------
  output$report_text <- renderUI({
    
    req(input$report)
    
    if(input$report=="Overview"){
      HTML("National GIS platform for climate risk, population vulnerability & flood analysis.")
    } else if(input$report=="Methodology"){
      HTML("Weighted model: Flood (40%) + Poverty (35%) + Population (25%)")
    } else if(input$report=="Risk Model"){
      HTML("Min-max normalization used to create composite risk index (0–1).")
    } else {
      HTML("Credits: GIS Unit — The Gambia Bureau of Statistics (GBoS)")
    }
  })
  
  # ---------------- CHART (WORKING) ----------------
  output$chart <- renderPlotly({
    df <- filtered_sf()
    
    plot_ly(df, x=~composite_risk, type="histogram") %>%
      layout(title="Risk Distribution")
  })
  
  # ---------------- DOWNLOAD FILTERED ----------------
  output$dl <- downloadHandler(
    filename=function() "filtered_gsiro.csv",
    content=function(file){
      write.csv(st_drop_geometry(filtered_sf()), file, row.names=FALSE)
    }
  )
  
  # ---------------- DOWNLOAD FULL ----------------
  output$dl_all <- downloadHandler(
    filename=function() "full_gsiro.csv",
    content=function(file){
      write.csv(st_drop_geometry(gmb_sf), file, row.names=FALSE)
    }
  )
  
}

shinyApp(ui, server)# ============================================================
# 🇬🇲 GSIRO v18 — ADVANCED NATIONAL GIS + ANALYTICS PLATFORM
# ============================================================

library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(plotly)

# =========================
# LOAD DATA
# =========================

gmb_sf <- st_read("C:/Users/user/Desktop/RCoding/Mapping_Census/RawData/GMB_Population.shp", quiet = TRUE)

gmb_sf <- st_transform(gmb_sf, 4326)
gmb_sf <- gmb_sf[!st_is_empty(gmb_sf), ]

gmb_sf$lganame  <- ifelse(is.na(gmb_sf$lganame), "Unknown", gmb_sf$lganame)
gmb_sf$distname <- ifelse(is.na(gmb_sf$distname), gmb_sf$lganame, gmb_sf$distname)
gmb_sf$wardname <- ifelse(is.na(gmb_sf$wardname), gmb_sf$distname, gmb_sf$wardname)

# =========================
# NORMALIZATION
# =========================
norm01 <- function(x){
  if(all(is.na(x))) return(rep(0,length(x)))
  (x-min(x,na.rm=TRUE))/(max(x,na.rm=TRUE)-min(x,na.rm=TRUE)+1e-9)
}

# =========================
# RISK ENGINE
# =========================
gmb_sf$flood_risk <- norm01(gmb_sf$comp_map)
gmb_sf$poverty_risk <- norm01(gmb_sf$ls_pop)
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

# =========================
# UI
# =========================

ui <- fluidPage(
  
  tags$head(tags$style(HTML("
    html,body{height:100%;margin:0;overflow:hidden;}

    body::before{
      content:'';
      position:fixed;
      width:100%;height:100%;
      z-index:-10;
      opacity:0.12;
      background:linear-gradient(to bottom,
        #CE1126 0%,#CE1126 33%,
        #FFFFFF 33%,#FFFFFF 38%,
        #0C1C8C 38%,#0C1C8C 62%,
        #FFFFFF 62%,#FFFFFF 67%,
        #3A7728 67%,#3A7728 100%);
    }

    .title{
      position:sticky;
      top:0;
      z-index:9999;
      background:white;
      padding:10px;
      font-weight:bold;
      text-align:center;
      border-bottom:1px solid #ddd;
    }

    .sidebar{
      height:100vh;
      overflow-y:auto;
      background:white;
      padding:15px;
    }

    .mapbox{height:100vh;}
  "))),
  
  div(class="title",
      "🇬🇲 GSIRO — NATIONAL GIS INTELLIGENCE & EARLY WARNING SYSTEM"
  ),
  
  fluidRow(
    
    column(3,
           
           div(class="sidebar",
               
               h4("CONTROL PANEL"),
               
               selectInput("lga","LGA",choices=c("All")),
               selectInput("dist","District",choices=c("All")),
               selectInput("ward","Ward",choices=c("All")),
               
               selectInput("basemap","Basemap",
                           choices=c("OSM","Satellite","Dark")),
               
               selectInput("chart","Analytics View",
                           choices=c("Bar Chart","Pie Chart","Histogram","Heatmap")),
               
               selectInput("report","System Report",
                           choices=c("Overview","Methodology","Risk Model","Early Warning")),
               
               actionButton("reset","Reset"),
               downloadButton("dl","Filtered"),
               downloadButton("dl_all","Full Data"),
               
               hr(),
               uiOutput("report_text"),
               
               hr(),
               verbatimTextOutput("stats"),
               
               hr(),
               uiOutput("warning")
           )
    ),
    
    column(9,
           
           leafletOutput("map",height="65vh"),
           plotlyOutput("chart_out",height="35vh")
    )
  )
)

# =========================
# SERVER
# =========================

server <- function(input, output, session){
  
  # ---------- SAFE CHOICES ----------
  safe <- function(x){
    x <- unique(na.omit(x))
    if(length(x)==0) return(c("All"))
    c("All",sort(x))
  }
  
  observe({
    updateSelectInput(session,"lga",choices=safe(gmb_sf$lganame))
  })
  
  observeEvent(input$lga,{
    df <- gmb_sf
    if(input$lga!="All") df <- df[df$lganame==input$lga,]
    updateSelectInput(session,"dist",choices=safe(df$distname))
    updateSelectInput(session,"ward",choices=c("All"))
  })
  
  observeEvent(input$dist,{
    df <- gmb_sf
    if(input$lga!="All") df <- df[df$lganame==input$lga,]
    if(input$dist!="All") df <- df[df$distname==input$dist,]
    updateSelectInput(session,"ward",choices=safe(df$wardname))
  })
  
  observeEvent(input$reset,{
    updateSelectInput(session,"lga",selected="All")
    updateSelectInput(session,"dist",selected="All")
    updateSelectInput(session,"ward",selected="All")
  })
  
  # ---------- FILTER ----------
  filtered <- reactive({
    df <- gmb_sf
    if(input$lga!="All") df <- df[df$lganame==input$lga,]
    if(input$dist!="All") df <- df[df$distname==input$dist,]
    if(input$ward!="All") df <- df[df$wardname==input$ward,]
    df
  })
  
  # ---------- BASEMAP ----------
  base <- reactive({
    switch(input$basemap,
           "OSM"=leaflet()%>%addTiles(),
           "Satellite"=leaflet()%>%addProviderTiles("Esri.WorldImagery"),
           "Dark"=leaflet()%>%addProviderTiles("CartoDB.DarkMatter"))
  })
  
  # ---------- MAP ----------
  output$map <- renderLeaflet({
    df <- filtered()
    pal <- colorFactor("YlOrRd", df$risk_class)
    
    base() %>%
      addPolygons(data=df,
                  fillColor=~pal(risk_class),
                  fillOpacity=0.7,
                  color="white",
                  weight=1) %>%
      addLegend("bottomright", pal=pal, values=df$risk_class)
  })
  
  # ---------- STATS ----------
  output$stats <- renderText({
    df <- filtered()
    paste0(
      "Records: ",nrow(df),"\n",
      "Mean Risk: ",round(mean(df$composite_risk,na.rm=TRUE),3),"\n",
      "Max Risk: ",round(max(df$composite_risk,na.rm=TRUE),3)
    )
  })
  
  # ---------- REPORT ----------
  output$report_text <- renderUI({
    switch(input$report,
           "Overview"=HTML("National GIS intelligence system for risk monitoring."),
           "Methodology"=HTML("Weighted flood + poverty + population model."),
           "Risk Model"=HTML("Normalized composite index 0–1."),
           "Early Warning"=HTML("High risk zones flagged automatically for alerts."))
  })
  
  # ---------- WARNING PANEL ----------
  output$warning <- renderUI({
    df <- filtered()
    hrisk <- sum(df$risk_class=="High",na.rm=TRUE)
    
    if(hrisk>0){
      HTML(paste0("⚠ HIGH RISK AREAS DETECTED: ",hrisk))
    } else {
      HTML("✔ System Stable")
    }
  })
  
  # ---------- CHART SWITCH ----------
  output$chart_out <- renderPlotly({
    
    df <- filtered()
    
    if(input$chart=="Histogram"){
      plot_ly(df,x=~composite_risk,type="histogram")
      
    } else if(input$chart=="Bar Chart"){
      d <- df %>% group_by(lganame) %>%
        summarise(v=mean(composite_risk,na.rm=TRUE))
      
      plot_ly(d,x=~lganame,y=~v,type="bar")
      
    } else if(input$chart=="Pie Chart"){
      d <- df %>% count(risk_class)
      
      plot_ly(d,labels=~risk_class,values=~n,type="pie")
      
    } else {
      mat <- cor(st_drop_geometry(df)[,c("flood_risk","poverty_risk","population_risk","composite_risk")])
      plot_ly(x=colnames(mat),y=rownames(mat),z=mat,type="heatmap")
    }
  })
  
  # ---------- DOWNLOAD ----------
  output$dl <- downloadHandler(
    filename=function() "filtered.csv",
    content=function(file){
      write.csv(st_drop_geometry(filtered()),file,row.names=FALSE)
    }
  )
  
  output$dl_all <- downloadHandler(
    filename=function() "full.csv",
    content=function(file){
      write.csv(st_drop_geometry(gmb_sf),file,row.names=FALSE)
    }
  )
  
}

shinyApp(ui, server)
