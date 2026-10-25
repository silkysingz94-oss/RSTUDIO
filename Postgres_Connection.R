# ----------------------------
# LIBRARIES
library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(RColorBrewer)
library(plotly)
library(RPostgres)  # for PostGIS connection

# ----------------------------
# DATABASE CONNECTION
db <- dbConnect(
  RPostgres::Postgres(),
  dbname = "censusdata",
  host = "localhost",       # e.g., "localhost"
  port = 5432,              # default Postgres port
  user = "Postgres",
  password = "2092443a"
)

# ----------------------------
# READ SPATIAL DATA DIRECTLY
# Replace 'gambia_population' with your PostGIS table
gmb_data <- st_read(db, query = "SELECT * FROM gambia_population")

# Ensure EPSG:4326 for Leaflet
if(st_crs(gmb_data)$epsg != 4326){
  gmb_data <- st_transform(gmb_data, 4326)
}

# ----------------------------
# UI
ui <- fluidPage(
  titlePanel("Population Dashboard — The Gambia 🇬🇲"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("lgaSelect", "LGA:", choices=c("", sort(unique(gmb_data$lganame)))),
      selectInput("distSelect", "District:", choices=NULL),
      selectInput("wardSelect", "Ward:", choices=NULL),
      selectInput("feaSelect", "EA Code:", choices=NULL),
      
      selectInput("variableSelect", "Variable:", choices=c("enum_pop","enum_hhs","ls_pop","map_pop")),
      selectInput("variable2Select", "Compare:", choices=c("", "enum_pop","enum_hhs","ls_pop","map_pop")),
      
      radioButtons("viewMode", "View Mode:", choices = c("Map View"="map", "Chart View"="chart"), selected = "map"),
      actionButton("resetButton", "🔄 Reset"),
      downloadButton("exportData", "📥 Export CSV")
    ),
    
    mainPanel(
      width = 9,
      conditionalPanel(condition = "input.viewMode == 'map'",
                       leafletOutput("map", height = "650px")),
      conditionalPanel(condition = "input.viewMode == 'chart'",
                       div(style="background-image: url('https://upload.wikimedia.org/wikipedia/commons/7/77/Flag_of_The_Gambia.svg'); background-size: cover; padding:10px;",
                           plotlyOutput("chart", height="300px"),
                           plotlyOutput("rankChart", height="300px"))),
      verbatimTextOutput("info")
    )
  )
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
  # CHARTS
  output$chart <- renderPlotly({
    df <- filteredData()
    req(nrow(df)>0)
    vars <- c(input$variableSelect)
    if(input$variable2Select!="") vars <- c(vars, input$variable2Select)
    
    p <- plot_ly(df, x=~eaname)
    for(v in vars){
      p <- add_trace(p, y=as.numeric(df[[v]]), type='bar', name=v)
    }
    layout(p, barmode='group')
  })
  
  output$rankChart <- renderPlotly({
    df <- filteredData()
    req(nrow(df)>0)
    df <- df %>% mutate(val=as.numeric(.data[[input$variableSelect]])) %>% filter(!is.na(val)) %>% arrange(desc(val)) %>% slice(1:10)
    plot_ly(df, x=~reorder(eaname,val), y=~val, type='bar')
  })
  
  # ----------------------------
  # CLICK INFO
  observeEvent(input$map_shape_click,{
    df <- filteredData() %>% filter(feacode==input$map_shape_click$id)
    if(nrow(df)>0){
      output$info <- renderText({paste0("📍 ", df$eaname, "\nValue: ", df[[input$variableSelect]])})
    }
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
