
library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(RColorBrewer)
library(plotly)

# ----------------------------
# 📁 SMART SHAPEFILE LOADER
# ----------------------------
local_path <- "C:/Users/ansu.s/Desktop/RCoding/Mapping_Census/RawData/GMB_Population.shp"
app_path   <- "C:/Users/ansu.s/Desktop/RCoding/Mapping_Census/RawData/GMB_Population.shp"

if (file.exists(local_path)) {
  shp_path <- local_path
  message("✅ Using LOCAL shapefile")
} else if (file.exists(app_path)) {
  shp_path <- app_path
  message("✅ Using APP FOLDER shapefile")
} else {
  stop("❌ Shapefile not found in expected paths")
}

# ----------------------------
# LOAD DATA (SAFE CRS)
# ----------------------------
gmb_data <- st_read(shp_path, quiet = TRUE)

if (is.na(st_crs(gmb_data)$epsg)) {
  st_crs(gmb_data) <- 4326
}
gmb_data <- st_transform(gmb_data, 4326)

# ----------------------------
# SAFE NUMERIC FUNCTION
# ----------------------------
safe_num <- function(x) as.numeric(as.character(x))

# ----------------------------
# UI
# ----------------------------
ui <- fluidPage(
  
  tags$head(tags$style(HTML("
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
  "))),
  
  titlePanel("POPULATION DASHBOARD — THE GAMBIA"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      width = 3,
      
      selectInput("lgaSelect", "LGA:",
                  choices = c("", sort(unique(gmb_data$lganame)))),
      
      selectInput("distSelect", "District:", choices = ""),
      selectInput("wardSelect", "Ward:", choices = ""),
      selectInput("feaSelect", "EA Code:", choices = ""),
      
      selectInput("variableSelect", "Variable:",
                  choices = c("enum_pop","enum_hhs","ls_pop","map_pop")),
      
      selectInput("variable2Select", "Compare:",
                  choices = c("", "enum_pop","enum_hhs","ls_pop","map_pop")),
      
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
        leafletOutput("map", height = 650)
      ),
      
      conditionalPanel(
        condition = "input.viewMode == 'chart'",
        div(class="chart-bg",
            plotlyOutput("chart", height = 300),
            plotlyOutput("rankChart", height = 300)
        )
      ),
      
      verbatimTextOutput("info")
    )
  ),
  
  div(class="footer",
      "Produced by Ansu Singhateh | GIS Analyst | GBoS © 2026")
)

# ----------------------------
# SERVER
# ----------------------------
server <- function(input, output, session){
  
  # ----------------------------
  # CASCADING FILTERS (SAFE)
  # ----------------------------
  
  observe({
    if (!is.null(input$lgaSelect) && input$lgaSelect != "") {
      updateSelectInput(session, "distSelect",
                        choices = c("",
                                    sort(unique(gmb_data$distname[
                                      gmb_data$lganame == input$lgaSelect
                                    ]))))
    }
  })
  
  observe({
    if (!is.null(input$distSelect) && input$distSelect != "") {
      updateSelectInput(session, "wardSelect",
                        choices = c("",
                                    sort(unique(gmb_data$wardname[
                                      gmb_data$distname == input$distSelect
                                    ]))))
    }
  })
  
  observe({
    if (!is.null(input$wardSelect) && input$wardSelect != "") {
      updateSelectInput(session, "feaSelect",
                        choices = c("",
                                    sort(unique(gmb_data$feacode[
                                      gmb_data$wardname == input$wardSelect
                                    ]))))
    }
  })
  
  # ----------------------------
  # FILTERED DATA
  # ----------------------------
  filteredData <- reactive({
    
    df <- gmb_data
    
    if (!is.null(input$lgaSelect) && input$lgaSelect != "")
      df <- df %>% filter(lganame == input$lgaSelect)
    
    if (!is.null(input$distSelect) && input$distSelect != "")
      df <- df %>% filter(distname == input$distSelect)
    
    if (!is.null(input$wardSelect) && input$wardSelect != "")
      df <- df %>% filter(wardname == input$wardSelect)
    
    if (!is.null(input$feaSelect) && input$feaSelect != "")
      df <- df %>% filter(feacode == input$feaSelect)
    
    req(nrow(df) > 0)
    df
  })
  
  # ----------------------------
  # PALETTE (SAFE)
  # ----------------------------
  pal <- reactive({
    df <- filteredData()
    vals <- safe_num(df[[input$variableSelect]])
    colorNumeric("Blues", vals, na.color = "transparent")
  })
  
  # ----------------------------
  # MAP (FIXED sf SAFE RENDERING)
  # ----------------------------
  output$map <- renderLeaflet({
    
    df <- filteredData()
    
    df$val <- safe_num(df[[input$variableSelect]])
    
    p <- pal()
    
    leaflet(df) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~p(val),
        fillOpacity = 0.7,
        color = "white",
        weight = 1,
        layerId = ~feacode,
        label = ~paste0(eaname, ": ", val)
      ) %>%
      addLegend("bottomright",
                pal = p,
                values = df$val,
                title = input$variableSelect)
  })
  
  # ----------------------------
  # CHART (FIXED TYPE SAFETY)
  # ----------------------------
  output$chart <- renderPlotly({
    
    df <- st_drop_geometry(filteredData())
    
    df[[input$variableSelect]] <- safe_num(df[[input$variableSelect]])
    
    vars <- c(input$variableSelect)
    if (input$variable2Select != "")
      vars <- c(vars, input$variable2Select)
    
    p <- plot_ly(df, x = ~eaname)
    
    for (v in vars) {
      df[[v]] <- safe_num(df[[v]])
      p <- add_trace(p, y = df[[v]], type = "bar", name = v)
    }
    
    layout(p, barmode = "group")
  })
  
  # ----------------------------
  # TOP 10 (FIXED SORTING)
  # ----------------------------
  output$rankChart <- renderPlotly({
    
    df <- st_drop_geometry(filteredData())
    df$val <- safe_num(df[[input$variableSelect]])
    
    df <- df %>%
      filter(!is.na(val)) %>%
      arrange(desc(val)) %>%
      slice(1:10)
    
    plot_ly(df,
            x = ~reorder(eaname, val),
            y = ~val,
            type = "bar")
  })
  
  # ----------------------------
  # CLICK INFO
  # ----------------------------
  observeEvent(input$map_shape_click, {
    
    df <- filteredData()
    
    row <- df[df$feacode == input$map_shape_click$id, ]
    
    if (nrow(row) > 0) {
      output$info <- renderText({
        paste0("📍 ", row$eaname,
               "\nValue: ", row[[input$variableSelect]])
      })
    }
  })
  
  # ----------------------------
  # EXPORT
  # ----------------------------
  output$exportData <- downloadHandler(
    filename = function() "GMB_Data.csv",
    content = function(file) {
      write.csv(st_drop_geometry(filteredData()), file, row.names = FALSE)
    }
  )
  
  # ----------------------------
  # RESET (FIXED SAFE RESET)
  # ----------------------------
  observeEvent(input$resetButton, {
    
    updateSelectInput(session, "lgaSelect", selected = "")
    updateSelectInput(session, "distSelect", choices = c(""))
    updateSelectInput(session, "wardSelect", choices = c(""))
    updateSelectInput(session, "feaSelect", choices = c(""))
  })
}

# ----------------------------
shinyApp(ui, server)
