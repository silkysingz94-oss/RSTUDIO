
# ==========================================
# GAMBIA ADVANCED EA DASHBOARD WITH AQ ANIMATION
# Produced by: Ansu Singhateh, GIS Analyst, GBoS
# ==========================================

library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(plotly)
library(shinydashboard)
library(DT)

# -----------------------------
# LOAD DATA
# -----------------------------
EA <- st_read("C:/Users/ansu.s/Desktop/RCoding/Mapping_Census/RawData/EA_Pop.shp", quiet = TRUE)
if (is.na(st_crs(EA))) st_crs(EA) <- 3857
EA <- st_transform(EA, 4326)
names(EA) <- make.unique(names(EA))

# -----------------------------
# Simulate AQ Multi-Day Data
# -----------------------------
set.seed(123)
days <- paste0("Day_", 1:7) # 7-day period
pollutants <- c("CO","NO2","SO2","O3","CH4")

aq_list <- lapply(days, function(day){
  st_centroid(EA) %>%
    cbind(data.frame(
      day = day,
      CO = runif(nrow(EA), 0, 10),
      NO2 = runif(nrow(EA), 0, 20),
      SO2 = runif(nrow(EA), 0, 5),
      O3 = runif(nrow(EA), 0, 8),
      CH4 = runif(nrow(EA), 0, 15)
    ))
})
aq_sf <- do.call(rbind, aq_list)

# -----------------------------
# UI
# -----------------------------
ui <- fluidPage(
  tags$head(tags$style(HTML("
    body {background-color:#111;color:#eee;}
    .box{background-color:#222;color:#fff;}
    #title-bar{background: linear-gradient(to right,#CE1126,#14B53A,#003893);
               color:#fff;text-align:center;padding:15px;font-size:26px;font-weight:bold;}
  "))),
  
  # TITLE
  div(id="title-bar", "MAPPING (2023) & CENSUS(2024): HOUSEHOLDS, POPULATION, BUILDINGS, COMPOUNDS DATA"),
  
  fluidRow(
    # LEFT PANEL
    column(3, style="height:90vh;overflow-y:auto;padding:10px;",
           h3("📍 Filters & Style"),
           selectInput("lga", "LGA", c("All", unique(EA$lganame))),
           uiOutput("dist_ui"),
           uiOutput("ward_ui"),
           selectInput("field","Map Variable",
                       c("map_hhs","map_pop","ls_hhs","ls_pop",
                         "enum_hhs","enum_pop","comp_map","build_map","comp_ls","build_ls")),
           sliderInput("polyOpacity","Polygon Opacity",0,1,0.6),
           sliderInput("pointOpacity","Point Opacity",0,1,0.8),
           checkboxInput("points","Show Points", TRUE),
           checkboxInput("hotspot","Show Hotspots", TRUE),
           selectInput("basemap","Basemap",
                       c("OpenStreetMap","Light","Dark","Satellite")),
           hr(),
           h3("🌫 Air Quality"),
           selectInput("aq_pollutants","Select Pollutants",
                       choices=pollutants,multiple=TRUE),
           sliderInput("aq_day","Select Day", min=1, max=length(days), value=1, step=1, animate=animationOptions(interval=2000, loop=TRUE)),
           downloadButton("download_aq","⬇ Download AQ Data"),
           hr(),
           strong("Produced by Ansu Singhateh"),
           p("GIS Analyst, Gambia Bureau of Statistics")
    ),
    
    # CENTER PANEL
    column(6, leafletOutput("map", height="90vh")),
    
    # RIGHT PANEL
    column(3, style="height:90vh;overflow-y:auto;padding:10px;",
           h3("📊 Summaries & Charts"),
           fluidRow(
             valueBoxOutput("pop_box", width=12),
             valueBoxOutput("hh_box", width=12),
             valueBoxOutput("ea_box", width=12)
           ),
           br(),
           box(width=NULL, plotlyOutput("bar_chart", height=300)),
           br(),
           box(width=NULL, plotlyOutput("pie_chart", height=300))
    )
  )
)

# -----------------------------
# SERVER
# -----------------------------
server <- function(input, output, session){
  
  # Dynamic filters
  output$dist_ui <- renderUI({
    req(input$lga)
    d <- if(input$lga=="All") EA else filter(EA,lganame==input$lga)
    selectInput("dist","District", c("All", unique(d$distname)))
  })
  output$ward_ui <- renderUI({
    req(input$dist)
    d <- EA
    if(input$lga!="All") d <- filter(d,lganame==input$lga)
    if(input$dist!="All") d <- filter(d,distname==input$dist)
    selectInput("ward","Ward", c("All", unique(d$wardname)))
  })
  
  # Filtered EA data
  data_filt <- reactive({
    d <- EA
    if(input$lga!="All") d <- filter(d,lganame==input$lga)
    if(!is.null(input$dist) && input$dist!="All") d <- filter(d,distname==input$dist)
    if(!is.null(input$ward) && input$ward!="All") d <- filter(d,wardname==input$ward)
    validate(need(nrow(d)>0,"No data available"))
    d
  })
  
  # Palette for ward-level map
  pal <- reactive({
    df <- st_drop_geometry(data_filt()) %>%
      group_by(wardname) %>%
      summarise(value=sum(as.numeric(get(input$field)), na.rm=TRUE))
    colorNumeric("YlOrRd", domain=df$value, na.color="transparent")
  })
  
  # KPI Boxes
  output$pop_box <- renderValueBox({valueBox(format(sum(data_filt()$map_pop, na.rm=TRUE),big.mark=","),"Total Population",icon=icon("users"),color="red")})
  output$hh_box <- renderValueBox({valueBox(format(sum(data_filt()$map_hhs, na.rm=TRUE),big.mark=","),"Total Households",icon=icon("home"),color="blue")})
  output$ea_box <- renderValueBox({valueBox(nrow(data_filt()),"Total EAs",icon=icon("map"),color="green")})
  
  # Initial map
  output$map <- renderLeaflet({leaflet() %>% addProviderTiles(providers$OpenStreetMap) %>% setView(lng=-15.5, lat=13.4, zoom=8)})
  
  # Update map (polygons + points)
  observe({
    d <- data_filt()
    p <- pal()
    base <- switch(input$basemap,"OpenStreetMap"=providers$OpenStreetMap,
                   "Light"=providers$CartoDB.Positron,
                   "Dark"=providers$CartoDB.DarkMatter,
                   "Satellite"=providers$Esri.WorldImagery)
    cent <- st_point_on_surface(d)
    
    map <- leafletProxy("map", data=d) %>% clearShapes() %>% clearMarkers() %>% clearControls() %>% clearTiles() %>% addProviderTiles(base)
    
    ward_sums <- d %>% group_by(wardname) %>% summarise(value=sum(as.numeric(get(input$field)), na.rm=TRUE)) %>% st_as_sf()
    
    map <- map %>% addPolygons(
      fillColor = ~p(ward_sums$value[match(wardname, ward_sums$wardname)]),
      fillOpacity = input$polyOpacity,
      color = "white",
      weight = 1,
      popup = ~paste0("<b>EA:</b>", EA_code,"<br><b>LGA:</b>",lganame,"<br><b>District:</b>",distname,"<br><b>Ward:</b>",wardname,"<br><b>",input$field," (Ward Total):</b>", ward_sums$value[match(wardname, ward_sums$wardname)])
    )
    
    if(input$points){map <- map %>% addCircleMarkers(data=cent,radius=4,fillOpacity=input$pointOpacity,color="blue")}
    if(input$hotspot){map <- map %>% addCircles(data=cent,radius=sqrt(d[[input$field]])*50,fillColor="red",fillOpacity=0.2,stroke=FALSE)}
    
    bbox <- st_bbox(d)
    if(all(is.finite(bbox))){map <- map %>% flyToBounds(lng1=bbox["xmin"],lat1=bbox["ymin"],lng2=bbox["xmax"],lat2=bbox["ymax"])}
    map %>% addLegend(position="bottomright",pal=p,values=ward_sums$value,title=paste(input$field,"(Ward Total)"))
  })
  
  # Charts
  output$bar_chart <- renderPlotly({
    df <- st_drop_geometry(data_filt()) %>% group_by(wardname) %>% summarise(value=sum(as.numeric(get(input$field)),na.rm=TRUE))
    plot_ly(df,x=~wardname,y=~value,type="bar",marker=list(color=~pal()(value))) %>% layout(xaxis=list(title="Ward"),yaxis=list(title=input$field))
  })
  output$pie_chart <- renderPlotly({
    df <- st_drop_geometry(data_filt()) %>% group_by(wardname) %>% summarise(value=sum(as.numeric(get(input$field)),na.rm=TRUE))
    plot_ly(df,labels=~wardname,values=~value,type="pie",marker=list(colors=pal()(df$value)))
  })
  
  # AQ animation
  observe({
    req(input$aq_pollutants)
    day_sel <- days[input$aq_day]
    d <- aq_sf[aq_sf$day==day_sel,]
    map <- leafletProxy("map") %>% clearGroup("AirQuality") %>% clearControls()
    
    for(poll in input$aq_pollutants){
      pal_aq <- colorNumeric("RdYlBu", d[[poll]], na.color="transparent")
      radius_vals <- sqrt(d[[poll]] / max(d[[poll]], na.rm=TRUE)) * 20
      map <- map %>% addCircleMarkers(data=d, radius=radius_vals, color=~pal_aq(get(poll)),
                                      fillOpacity=0.7, stroke=FALSE, group=poll,
                                      popup=~paste0("<b>EA:</b>",EA$EA_code,"<br><b>",poll,":</b>",get(poll)))
    }
    
    map <- map %>% addLayersControl(overlayGroups=input$aq_pollutants, options=layersControlOptions(collapsed=FALSE)) %>%
      addLegend(position="bottomleft", pal=pal_aq, values=d[[input$aq_pollutants[1]]], title=paste("AQ:",input$aq_pollutants[1]))
  })
  
  # Download AQ
  output$download_aq <- downloadHandler(
    filename="gambia_aq_data.csv",
    content=function(file){write.csv(st_drop_geometry(aq_sf),file,row.names=FALSE)}
  )
}

# Run App
shinyApp(ui, server)
