library(shiny)
library(shinydashboard)
library(sf)
library(dplyr)
library(leaflet)
library(plotly)
library(readxl)
library(tidyr)

# ------------------------------------------
# 1️⃣ LOAD DATA
# ------------------------------------------
shp_path <- "C:/Users/ansu.s/Desktop/RCoding/Mapping_Census/RawData/Buildings.xlsx"
excel_path <- "C:/Users/ansu.s/Desktop/RCoding/Mapping_Census/RawData/Buildings.xlsx"

# Load shapefile
buildings_sf <- st_read(shp_path, quiet = TRUE) %>% st_transform(4326)

# Load Excel data
excel_data <- read_excel(excel_path, sheet = 1, col_names = FALSE)

# Use first row as variable names, second row as descriptions
var_names <- as.character(excel_data[1, ])
var_descs <- as.character(excel_data[2, ])

# Remaining rows are actual data
excel_data <- excel_data[-c(1,2), ]
colnames(excel_data) <- var_names

# Clean numeric fields
num_cols <- names(excel_data)[sapply(excel_data, function(x) all(grepl("^[0-9,\\.]*$", x) | is.na(x)))]
for(col in num_cols){
  excel_data[[col]] <- as.numeric(gsub(",", "", excel_data[[col]]))
}

# Ensure 'lganame' exists as character
excel_data$lganame <- as.character(excel_data$lganame)

# Merge shapefile and Excel
buildings_sf <- buildings_sf %>%
  left_join(excel_data, by = "lganame")

# Metrics for selection
numeric_fields <- num_cols

# ------------------------------------------
# 2️⃣ UI
# ------------------------------------------
ui <- dashboardPage(
  dashboardHeader(
    title = tags$div("2024 POPULATION & HOUSING CENSUS BUILDING DASHBOARD", style = "text-align:center; width:100%; font-size:27px;"),
    titleWidth = "100%"
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Map", tabName = "map", icon = icon("map")),
      menuItem("Charts", tabName = "charts", icon = icon("chart-bar"))
    ),
    hr(),
    selectInput("metric", "Select Indicator", choices = numeric_fields),
    selectInput("lga", "Select LGA", choices = c("All", unique(buildings_sf$lganame))),
    sliderInput("opacity", "Polygon Opacity", 0, 1, 0.7),
    checkboxInput("centroids", "Show Centroids", TRUE),
    checkboxInput("hotspots", "Show Hotspots", TRUE),
    checkboxInput("dynamic_hotspots", "Dynamic Hotspot Size by Metric", TRUE),
    selectInput("basemap", "Basemap", choices = c("Light","Dark","Satellite"))
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML("
      body {background-color:#111; color:#eee;}
      .box {background-color:#222; color:#fff;}
    "))),
    tabItems(
      tabItem(tabName = "map",
              fluidRow(
                box(width = 12, leafletOutput("map", height = 600))
              )
      ),
      tabItem(tabName = "charts",
              fluidRow(
                valueBoxOutput("avg"),
                valueBoxOutput("total"),
                valueBoxOutput("count")
              ),
              fluidRow(
                box(width = 6, plotlyOutput("bar")),
                box(width = 6, plotlyOutput("pie"))
              ),
              fluidRow(
                box(width = 12,
                    h4("Indicator Description"),
                    textOutput("indicator_desc")
                )
              )
      )
    )
  )
)

# ------------------------------------------
# 3️⃣ SERVER
# ------------------------------------------
server <- function(input, output, session) {
  
  # Filter data
  data_filt <- reactive({
    d <- buildings_sf
    if(input$lga != "All"){
      d <- d %>% filter(lganame == input$lga)
    }
    d
  })
  
  # Safe color palette
  pal <- reactive({
    vals <- data_filt()[[input$metric]]
    vals <- vals[!is.na(vals)]
    if(length(vals) == 0) vals <- 0:1
    colorNumeric("YlOrRd", domain = vals)
  })
  
  # KPI boxes
  output$avg <- renderValueBox({
    valueBox(
      round(mean(data_filt()[[input$metric]], na.rm = TRUE),2),
      paste("Average", input$metric),
      icon = icon("chart-line"),
      color = "blue"
    )
  })
  
  output$total <- renderValueBox({
    valueBox(
      round(sum(data_filt()[[input$metric]], na.rm = TRUE),0),
      paste("Total", input$metric),
      icon = icon("calculator"),
      color = "green"
    )
  })
  
  output$count <- renderValueBox({
    valueBox(
      length(unique(data_filt()$lganame)),
      "Number of LGAs",
      icon = icon("map"),
      color = "yellow"
    )
  })
  
  # Indicator description below chart
  output$indicator_desc <- renderText({
    desc <- var_descs[which(var_names == input$metric)]
    if(is.na(desc)) desc <- input$metric
    desc
  })
  
  # Render initial map
  output$map <- renderLeaflet({
    centroid <- st_centroid(st_union(buildings_sf)) %>% st_coordinates()
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = centroid[1], lat = centroid[2], zoom = 9) %>%
      addControl("Data source: Gambian Census, 2024", position = "bottomleft")
  })
  
  # Update map
  observe({
    d <- data_filt()
    if(nrow(d)==0) return(NULL)
    
    p <- pal()
    base <- switch(input$basemap,
                   "Light" = providers$CartoDB.Positron,
                   "Dark" = providers$CartoDB.DarkMatter,
                   "Satellite" = providers$Esri.WorldImagery)
    
    centroids <- st_centroid(d)
    
    # Proportional hotspot scaling
    minRadius <- 50
    maxRadius <- 300
    if(input$dynamic_hotspots){
      vals <- pmax(replace_na(d[[input$metric]],0),0)
      hotspot_radius <- if(max(vals)>0) minRadius + (vals / max(vals)) * (maxRadius-minRadius) else rep(minRadius, nrow(d))
    } else {
      hotspot_radius <- rep(100, nrow(d))
    }
    
    # Hotspot size legend
    addSizeLegend <- function(map, values, minR, maxR, title="Hotspot Size"){
      steps <- pretty(values[!is.na(values)], n=5)
      html <- paste0("<div style='background:#fff;padding:5px;border-radius:5px;'><b>", title,"</b><br>")
      for(s in steps){
        r <- minR + (s / max(values, na.rm=TRUE))*(maxR-minR)
        html <- paste0(html,
                       "<svg height='30' width='30'><circle cx='15' cy='15' r='", r/10,
                       "' fill='red' fill-opacity='0.3' stroke='none'></circle></svg>", s,"<br>")
      }
      html <- paste0(html,"</div>")
      map %>% addControl(html, position="topright")
    }
    
    leafletProxy("map", data=d) %>%
      clearShapes() %>% clearMarkers() %>% clearControls() %>%
      addProviderTiles(base) %>%
      addPolygons(
        fillColor = ~p(get(input$metric)),
        fillOpacity = input$opacity,
        color = "black",
        weight = 1,
        label = ~paste0("<b>LGA:</b> ", lganame,
                        "<br><b>", input$metric, ":</b> ", get(input$metric)),
        labelOptions = labelOptions(direction = "auto")
      ) %>%
      {if(input$centroids) addCircleMarkers(., data=centroids, radius=4, color="blue") else .} %>%
      {if(input$hotspots) addCircles(., data=centroids, radius=hotspot_radius,
                                     fillColor="red", fillOpacity=0.3, stroke=FALSE) else .} %>%
      addLegend("bottomright", pal=p, values=d[[input$metric]], title=input$metric) %>%
      addSizeLegend(values=d[[input$metric]], minR=minRadius, maxR=maxRadius) %>%
      addControl("Map credit: Gambia Bureau of Statistics & OpenStreetMap contributors Produced By: Ansu Singhateh, GIS Analyst, ", position="bottomleft")
  })
  
  # BAR chart
  output$bar <- renderPlotly({
    df <- data_filt() %>% st_drop_geometry() %>% filter(!is.na(.data[[input$metric]]))
    validate(need(nrow(df) > 0, "No data available"))
    plot_ly(df, x = ~lganame, y = as.formula(paste0("~`", input$metric, "`")), type = "bar")
  })
  
  # PIE chart
  output$pie <- renderPlotly({
    df <- data_filt() %>% st_drop_geometry() %>% filter(!is.na(.data[[input$metric]]))
    validate(need(nrow(df) > 0, "No data available"))
    plot_ly(df, labels = ~lganame, values = as.formula(paste0("~`", input$metric, "`")), type = "pie")
  })
}

# ------------------------------------------
# 4️⃣ RUN APP
# ------------------------------------------
shinyApp(ui, server)
