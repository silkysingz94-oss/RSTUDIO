library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(RColorBrewer)
library(plotly)

# ----------------------------
# Load shapefile
shp_path <- "C:/Users/user/Desktop/RCoding/Mapping_Census/RawData/GMB_Population.shp"
gmb_data <- st_read(shp_path)

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
        background-repeat: no-repeat;
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
  
  titlePanel("POPULATION, HOUSEHOLDS & INFRASTRUCTURE DASHBOARD — THE GAMBIA "),
  
  sidebarLayout(
    
    sidebarPanel(
      width = 3,
      
      img(src="https://upload.wikimedia.org/wikipedia/commons/7/7e/Coat_of_arms_of_The_Gambia.svg", width="120px"),
      br(), br(),
      
      selectInput("lgaSelect", "LGA:", choices=c("", sort(unique(gmb_data$lganame)))),
      selectInput("distSelect", "District:", choices=NULL),
      selectInput("wardSelect", "Ward:", choices=NULL),
      selectInput("feaSelect", "EA Code:", choices=NULL),
      
      selectInput("variableSelect", "Variable:",
                  choices=c("enum_pop","enum_hhs","ls_pop","map_pop")),
      
      selectInput("variable2Select", "Compare:",
                  choices=c("", "enum_pop","enum_hhs","ls_pop","map_pop")),
      
      actionButton("runButton", "▶️ Update"),
      actionButton("resetButton", "🔄 Reset"),
      
      br(), br(),
      
      radioButtons("viewMode", "View Mode:",
                   choices = c("Map View"="map", "Chart View"="chart"),
                   selected = "map"),
      
      br(),
      downloadButton("exportData", "📥 Export CSV")
    ),
    
    mainPanel(
      width = 9,
      
      # ----------------------------
      # MAP VIEW
      conditionalPanel(
        condition = "input.viewMode == 'map'",
        leafletOutput("map", height = "650px")
      ),
      
      # ----------------------------
      # CHART VIEW
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
  
  # ----------------------------
  # FOOTER CREDIT
  div(class="footer",
      "Produced by: |Produced By: Ansu Singhateh| GIS Analyst| GIS/Carto Unit | Gambia Bureau of Statistics | © 2026|")
)

# ----------------------------
# SERVER
server <- function(input, output, session){
  
  # Cascading dropdowns
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
  # MAP WITH LEGEND
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
      addLegend(
        position = "bottomright",
        pal = pal,
        values = df[[input$variableSelect]],
        title = input$variableSelect,
        opacity = 0.7
      )
  })
  
  # ----------------------------
  # MAIN CHART
  output$chart <- renderPlotly({
    df <- filteredData()
    req(nrow(df)>0)
    
    vars <- c(input$variableSelect)
    if(input$variable2Select!="") vars <- c(vars, input$variable2Select)
    
    p <- plot_ly(df, x=~eaname)
    
    for(v in vars){
      p <- add_trace(p, y=as.numeric(df[[v]]), type='bar', name=v)
    }
    
    layout(p, barmode='group', title="Feature Values")
  })
  
  # ----------------------------
  # TOP 10
  output$rankChart <- renderPlotly({
    df <- filteredData()
    req(nrow(df)>0)
    
    df <- df %>%
      mutate(val = as.numeric(.data[[input$variableSelect]])) %>%
      filter(!is.na(val))
    
    df_top <- df %>% arrange(desc(val)) %>% slice(1:10)
    
    plot_ly(df_top,
            x=~reorder(eaname, val),
            y=~val,
            type='bar',
            name="Top 10") %>%
      layout(title="Top 10 Areas")
  })
  
  # ----------------------------
  # CLICK INFO
  observeEvent(input$map_shape_click,{
    df <- filteredData() %>% filter(feacode==input$map_shape_click$id)
    if(nrow(df)>0){
      output$info <- renderText({
        paste0("📍 ", df$eaname, "\nValue: ", df[[input$variableSelect]])
      })
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
