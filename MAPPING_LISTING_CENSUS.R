library(shiny)
library(leaflet)
library(sf)          # ✅ REQUIRED (fix for st_read)
library(dplyr)
library(RColorBrewer)
library(plotly)

# ----------------------------
# 📁 SMART SHAPEFILE LOADER

local_path <- "C:/Users/ansu.s/Desktop/RCoding/Mapping_Census/RawData/GMB_Population.shp"
app_path   <- "GMB_Population.shp"

if(file.exists(local_path)){
  shp_path <- local_path
  message("✅ Using LOCAL shapefile")
  
} else if(file.exists(app_path)){
  shp_path <- app_path
  message("✅ Using APP FOLDER shapefile")
  
} else {
  stop("❌ Shapefile not found.
👉 Put it in app folder OR check C:/CENSUSWEB path")
}

# ----------------------------
# LOAD DATA
gmb_data <- st_read(shp_path, quiet = TRUE)

if(st_crs(gmb_data)$epsg != 4326){
  gmb_data <- st_transform(gmb_data, 4326)
}

# ----------------------------
# UI
ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      .chart-bg {
        background-image: url('https://upload.wikimedia.org/wikipedia/commons/7/77/Flag_of_The_Gambia.svg');
        background-size: cover;
        background-position: center;
        padding: 10px;
      }
      .footer {
        position: fixed;
        bottom: 0;
        width: 100%;
        background: #111;
        color: white;
        text-align: center;
        padding: 6px;
        font-size: 12px;
      }
    "))
  ),
  
  titlePanel("POPULATION DASHBOARD FOR THE MAPPING, LISTING AND CENSUS — THE GAMBIA"),
  
  sidebarLayout(
    
    sidebarPanel(
      width = 3,
      
      selectInput("lgaSelect", "LGA:", choices=c("", sort(unique(gmb_data$lganame)))),
      selectInput("distSelect", "District:", choices=NULL),
      selectInput("wardSelect", "Ward:", choices=NULL),
      selectInput("feaSelect", "EA Code:", choices=NULL),
      
      selectInput("variableSelect", "Variable:",
                  choices=c("enum_pop","enum_hhs","ls_pop","map_pop")),
      
      selectInput("variable2Select", "Compare:",
                  choices=c("", "enum_pop","enum_hhs","ls_pop","map_pop")),
      
      radioButtons("viewMode", "View Mode:",
                   choices = c("Map View"="map", "Chart View"="chart"),
                   selected = "map"),
      
      actionButton("resetButton", "🔄 Reset"),
      downloadButton("exportData", "📥 Export CSV")
    ),
    
    mainPanel(
      width = 9,
      
      conditionalPanel(
        condition = "input.viewMode == 'map'",
        leafletOutput("map", height = "650px")
      ),
      
      conditionalPanel(
        condition = "input.viewMode == 'chart'",
        div(class="chart-bg",
            plotlyOutput("chart", height="300px"),
            br(),
            plotlyOutput("rankChart", height="300px")
        )
      ),
      
      br(),
      verbatimTextOutput("info")
    )
  ),
  
  div(class="footer",
      "Produced by: Ansu Singhateh | GIS Analyst | GBoS © 2026")
)

# ----------------------------
# SERVER
server <- function(input, output, session){
  
  # Cascading filters
  observe({
    req(input$lgaSelect)
    updateSelectInput(session, "distSelect",
                      choices=c("", sort(unique(gmb_data$distname[gmb_data$lganame==input$lgaSelect]))))
  })
  
  observe({
    req(input$distSelect)
    updateSelectInput(session, "wardSelect",
                      choices=c("", sort(unique(gmb_data$wardname[gmb_data$distname==input$distSelect]))))
  })
  
  observe({
    req(input$wardSelect)
    updateSelectInput(session, "feaSelect",
                      choices=c("", sort(unique(gmb_data$feacode[gmb_data$wardname==input$wardSelect]))))
  })
  
  # Filter data
  filteredData <- reactive({
    df <- gmb_data
    if(input$lgaSelect!="") df <- df %>% filter(lganame==input$lgaSelect)
    if(input$distSelect!="") df <- df %>% filter(distname==input$distSelect)
    if(input$wardSelect!="") df <- df %>% filter(wardname==input$wardSelect)
    if(input$feaSelect!="") df <- df %>% filter(feacode==input$feaSelect)
    df
  })
  
  # ----------------------------
  # MAP
  output$map <- renderLeaflet({
    df <- filteredData()
    req(nrow(df)>0)
    
    pal <- colorNumeric("Blues", df[[input$variableSelect]])
    
    leaflet(df) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(df[[input$variableSelect]]),
        fillOpacity = 0.7,
        color = "gray",
        weight = 1,
        layerId = ~feacode,
        label = ~paste0(eaname, ": ", df[[input$variableSelect]])
      ) %>%
      addLegend("bottomright",
                pal = pal,
                values = df[[input$variableSelect]],
                title = input$variableSelect)
  })
  
  # ----------------------------
  # CHART
  output$chart <- renderPlotly({
    df <- filteredData()
    req(nrow(df)>0)
    
    p <- plot_ly(df, x=~eaname)
    
    vars <- c(input$variableSelect)
    if(input$variable2Select!="") vars <- c(vars, input$variable2Select)
    
    for(v in vars){
      p <- add_trace(p, y=as.numeric(df[[v]]), type='bar', name=v)
    }
    
    layout(p, barmode='group')
  })
  
  # ----------------------------
  # TOP 10
  output$rankChart <- renderPlotly({
    df <- filteredData()
    req(nrow(df)>0)
    
    df <- df %>%
      mutate(val = as.numeric(.data[[input$variableSelect]])) %>%
      filter(!is.na(val)) %>%
      arrange(desc(val)) %>%
      slice(1:10)
    
    plot_ly(df,
            x=~reorder(eaname, val),
            y=~val,
            type='bar')
  })
  
  # ----------------------------
  # EXPORT
  output$exportData <- downloadHandler(
    filename = function(){"GMB_Data.csv"},
    content = function(file){
      write.csv(filteredData(), file, row.names=FALSE)
    }
  )
  
  # RESET
  observeEvent(input$resetButton,{
    updateSelectInput(session,"lgaSelect",selected="")
    updateSelectInput(session,"distSelect",choices=NULL)
    updateSelectInput(session,"wardSelect",choices=NULL)
    updateSelectInput(session,"feaSelect",choices=NULL)
  })
}

# ----------------------------
shinyApp(ui, server)
