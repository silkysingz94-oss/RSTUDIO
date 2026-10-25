library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(plotly)
library(shinydashboard)

# -----------------------------
# LOAD DATA
# -----------------------------
LGA  <- st_read("C:/Users/user/Desktop/RCoding/Mapping_Census/RawData/LGASexRatio.shp", quiet = TRUE)
DIST <- st_read("C:/Users/user/Desktop/RCoding/Mapping_Census/RawData/DistrictSexRatio.shp", quiet = TRUE)
WARD <- st_read("C:/Users/user/Desktop/RCoding/Mapping_Census/RawData/WardSexRatio.shp", quiet = TRUE)

# Ensure all have CRS 4326
datasets <- list(LGA=LGA, DIST=DIST, WARD=WARD)
datasets <- lapply(datasets, function(x){
  if(is.na(st_crs(x))) st_crs(x) <- 4326
  st_transform(x, 4326)
})

LGA  <- datasets$LGA
DIST <- datasets$DIST
WARD <- datasets$WARD

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
                  color: #fff; text-align: center; padding: 15px; font-size: 26px; font-weight:bold;}
    "))
  ),
  
  div(id="title-bar", "GAMBIA SEX RATIO DASHBOARD — POPULATION AND HOUSING CENSUS 2024"),
  
  fluidRow(
    column(3, style="height: 90vh; overflow-y: auto; padding:10px;",
           h3("📍 Controls"),
           selectInput("dataset", "Dataset", choices=c("LGA","District","Ward")),
           uiOutput("lga_ui"),
           uiOutput("dist_ui"),
           uiOutput("ward_ui"),
           selectInput("variable","Variable", choices=c("Male","Female","Total"), selected="Total"),
           sliderInput("polyOpacity","Polygon Opacity",0,1,0.7),
           checkboxInput("show_points","Show Points", TRUE),
           radioButtons("view_mode","View Mode", choices=c("Map","Charts"), inline=TRUE),
           selectInput("basemap","Basemap",
                       c("OpenStreetMap","Light","Dark","Satellite")),
           hr(),
           strong("Produced by Ansu Singhateh, GIS Analyst, GBoS")
    ),
    
    column(6, uiOutput("main_view")),
    
    column(3, style="height: 90vh; overflow-y: auto; padding:10px;",
           h3("📊 Statistics & Charts"),
           conditionalPanel(
             condition="input.view_mode=='Charts'",
             tableOutput("stats"),
             hr(),
             plotlyOutput("bar_chart", height=300),
             hr(),
             plotlyOutput("pie_chart", height=300)
           )
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
  output$lga_ui <- renderUI({
    df <- switch(input$dataset, "LGA"=LGA, "District"=DIST, "Ward"=WARD)
    if("LGA" %in% names(df)){
      selectInput("lga_sel","Select LGA", choices=c("All", unique(df$LGA)), selected="All")
    }
  })
  
  output$dist_ui <- renderUI({
    df <- switch(input$dataset, "lganame"=LGA, "District"=DIST, "Ward"=WARD)
    if("distname" %in% names(df)){
      choices <- c("All", unique(df$distname))
      selectInput("dist_sel","Select District", choices=choices, selected="All")
    }
  })
  
  output$ward_ui <- renderUI({
    df <- switch(input$dataset, "LGA"=LGA, "District"=DIST, "Ward"=WARD)
    if("wardname" %in% names(df)){
      choices <- c("All", unique(df$wardname))
      selectInput("ward_sel","Select Ward", choices=choices, selected="All")
    }
  })
  
  # -----------------------------
  # FILTERED DATA
  # -----------------------------
  data_filt <- reactive({
    df <- switch(input$dataset, "LGA"=LGA, "District"=DIST, "Ward"=WARD)
    
    if(!is.null(input$lga_sel) && input$lga_sel != "All" && "LGA" %in% names(df)){
      df <- filter(df, LGA == input$lga_sel)
    }
    if(!is.null(input$dist_sel) && input$dist_sel != "All" && "distname" %in% names(df)){
      df <- filter(df, distname == input$dist_sel)
    }
    if(!is.null(input$ward_sel) && input$ward_sel != "All" && "wardname" %in% names(df)){
      df <- filter(df, wardname == input$ward_sel)
    }
    validate(need(nrow(df)>0,"No data available"))
    df
  })
  
  # -----------------------------
  # STATISTICS TABLE
  # -----------------------------
  output$stats <- renderTable({
    df <- data_filt()
    var <- input$variable
    data.frame(
      Mean = round(mean(df[[var]], na.rm=TRUE),0),
      Min  = min(df[[var]], na.rm=TRUE),
      Max  = max(df[[var]], na.rm=TRUE),
      Count= nrow(df)
    )
  })
  
  # -----------------------------
  # COLOR PALETTE
  # -----------------------------
  pal <- reactive({
    df <- st_drop_geometry(data_filt())
    colorNumeric("YlGnBu", domain=df[[input$variable]], na.color="transparent")
  })
  
  # -----------------------------
  # MAIN VIEW TOGGLE
  # -----------------------------
  output$main_view <- renderUI({
    if(input$view_mode=="Map"){
      leafletOutput("map", height="90vh")
    } else {
      tagList(
        plotlyOutput("bar_chart", height=300),
        hr(),
        plotlyOutput("pie_chart", height=300)
      )
    }
  })
  
  # -----------------------------
  # LEAFLET MAP
  # -----------------------------
  output$map <- renderLeaflet({
    leaflet() %>% addProviderTiles(providers$OpenStreetMap) %>% 
      setView(lng=-15.5, lat=13.4, zoom=8)
  })
  
  observe({
    req(input$view_mode=="Map")
    df <- data_filt()
    
    # Precompute value and layerId to avoid n() errors
    df <- df %>% mutate(
      val = !!sym(input$variable),
      layer_id = row_number()
    )
    
    p <- pal()
    
    base <- switch(input$basemap,
                   "OpenStreetMap" = providers$OpenStreetMap,
                   "Light" = providers$CartoDB.Positron,
                   "Dark" = providers$CartoDB.DarkMatter,
                   "Satellite" = providers$Esri.WorldImagery)
    
    map <- leafletProxy("map", data=df) %>%
      clearShapes() %>% clearMarkers() %>% clearControls() %>%
      addProviderTiles(base)
    
    map <- map %>%
      addPolygons(
        fillColor = ~p(val),
        fillOpacity = input$polyOpacity,
        color = "white",
        weight = 1,
        layerId = ~layer_id,
        popup = ~paste0(
          if("LGA" %in% names(df)) paste0("<b>LGA:</b> ", LGA, "<br>"),
          if("distname" %in% names(df)) paste0("<b>District:</b> ", distname, "<br>"),
          if("wardname" %in% names(df)) paste0("<b>Ward:</b> ", wardname, "<br>"),
          "<b>", input$variable, ":</b> ", val
        )
      )
    
    if(input$show_points){
      centroids <- st_centroid(df)
      map <- map %>% addCircleMarkers(
        data = centroids,
        radius = 4,
        color = "red",
        fillOpacity = 0.8
      )
    }
    
    # Cascading zoom
    observeEvent(input$map_shape_click,{
      click <- input$map_shape_click
      if(!is.null(click$id)){
        row <- df[df$layer_id == click$id, ]
        bbox <- st_bbox(row)
        leafletProxy("map") %>% flyToBounds(bbox["xmin"], bbox["ymin"], bbox["xmax"], bbox["ymax"])
      }
    })
    
    map %>% addLegend(
      position = "bottomright",
      pal = p,
      values = df$val,
      title = input$variable
    )
  })
  
  # -----------------------------
  # BAR CHART
  # -----------------------------
  output$bar_chart <- renderPlotly({
    df <- st_drop_geometry(data_filt())
    name_col <- if("LGA" %in% names(df)) "LGA" else if("distname" %in% names(df)) "distname" else "wardname"
    plot_ly(df, x=~get(name_col), y=~get(input$variable), type="bar",
            marker=list(color=~pal()(get(input$variable)))) %>%
      layout(xaxis=list(title=name_col), yaxis=list(title=input$variable))
  })
  
  # -----------------------------
  # PIE CHART
  # -----------------------------
  output$pie_chart <- renderPlotly({
    df <- st_drop_geometry(data_filt())
    name_col <- if("LGA" %in% names(df)) "LGA" else if("distname" %in% names(df)) "distname" else "wardname"
    plot_ly(df, labels=~get(name_col), values=~get(input$variable), type="pie",
            marker=list(colors=pal()(df[[input$variable]])))
  })
  
}

# -----------------------------
# RUN APP
# -----------------------------
shinyApp(ui, server)
