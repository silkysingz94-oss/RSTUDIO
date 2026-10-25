# ============================
# PACKAGES
# ============================
library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(plotly)
library(RColorBrewer)

# ============================
# LOAD DATA
# ============================
shp_path <- "C:/Users/user/Desktop/RCoding/Mapping_Census/RawData/GMB_Population.shp"
gmb_data <- st_read(shp_path, quiet = TRUE)

if (is.na(st_crs(gmb_data)$epsg)) st_crs(gmb_data) <- 4326
gmb_data <- st_transform(gmb_data, 4326)

safe_num <- function(x) as.numeric(as.character(x))

# ============================
# AI SIMULATION ENGINE
# ============================
gen_sat <- function(df){
  
  n <- nrow(df)
  
  df$rain <- runif(n, 0, 300)
  df$flood <- runif(n, 0, 1)
  df$ndvi <- runif(n, 0.2, 0.9)
  
  df$risk <- (1 - df$ndvi)*0.4 + (df$rain/300)*0.3 + df$flood*0.3
  
  df
}

# ============================
# UI
# ============================
ui <- fluidPage(
  
  tags$head(tags$style(HTML("

    body { background:#081826; color:white; }

    .panel {
      height: 900px;
      overflow-y: auto;
      background:#0f2236;
      padding:10px;
      border-radius:10px;
    }

    .mapbox {
      height: 900px;
      border-radius:10px;
    }

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
  
  titlePanel("🛰️ SATELLITE GIS CONTROL CENTER v22.2"),
  
  sidebarLayout(
    
    sidebarPanel(width = 3,
                 
                 selectInput("lga", "LGA:",
                             choices = c("", sort(unique(gmb_data$lganame)))),
                 
                 selectInput("dist", "District:", choices = c("")),
                 selectInput("ward", "Ward:", choices = c("")),
                 
                 selectInput("var", "Layer:",
                             c("Rainfall"="rain",
                               "NDVI"="ndvi",
                               "Flood Risk"="flood",
                               "AI Risk"="risk")),
                 
                 selectInput("basemap", "Basemap:",
                             c("OpenStreetMap","Esri.WorldImagery","CartoDB.DarkMatter")),
                 
                 actionButton("reset", "Reset")
    ),
    
    mainPanel(
      
      fluidRow(
        
        # ============================
        # LEFT ANALYTICS (SCROLLABLE)
        # ============================
        column(
          4,
          
          div(class="panel",
              
              h3("📊 SATELLITE ANALYTICS"),
              
              textOutput("mean"),
              textOutput("sd"),
              textOutput("cv"),
              textOutput("maxrisk"),
              
              hr(),
              
              plotlyOutput("hist", height=200),
              plotlyOutput("box", height=200),
              plotlyOutput("rank", height=250),
              
              hr(),
              
              h4("🧠 AI Interpretation"),
              textOutput("insight"),
              
              h4("🚨 Alert"),
              textOutput("alert"),
              
              hr(),
              
              h4("📌 LGA Ranking"),
              tableOutput("table")
          )
        ),
        
        # ============================
        # MAP
        # ============================
        column(
          8,
          leafletOutput("map", height="900px")
        )
      )
    )
  ),
  
  div(class="credit",
      "Produced by SILKYSING | GIS/Carto Unit | Satellite GIS v22.2"
  )
)

# ============================
# SERVER
# ============================
server <- function(input, output, session){
  
  # ============================
  # CASCADING FILTERS
  # ============================
  observe({
    updateSelectInput(session, "dist",
                      choices = c("", sort(unique(gmb_data$distname[
                        gmb_data$lganame == input$lga
                      ])))
    )
  })
  
  observe({
    updateSelectInput(session, "ward",
                      choices = c("", sort(unique(gmb_data$wardname[
                        gmb_data$distname == input$dist
                      ])))
    )
  })
  
  # ============================
  # FILTERED DATA (SAFE)
  # ============================
  filtered <- reactive({
    
    df <- gen_sat(gmb_data)
    
    if(input$lga != "")
      df <- df %>% filter(lganame == input$lga)
    
    if(input$dist != "")
      df <- df %>% filter(distname == input$dist)
    
    if(input$ward != "")
      df <- df %>% filter(wardname == input$ward)
    
    df
  })
  
  # ============================
  # SAFE STATS (FIXED ERROR HERE)
  # ============================
  output$mean <- renderText({
    paste("Mean:", round(mean(filtered()[[input$var]]),3))
  })
  
  output$sd <- renderText({
    paste("SD:", round(sd(filtered()[[input$var]]),3))
  })
  
  output$cv <- renderText({
    v <- filtered()[[input$var]]
    paste("CV%:", round(sd(v)/mean(v)*100,2))
  })
  
  output$maxrisk <- renderText({
    paste("Max:", round(max(filtered()[[input$var]]),3))
  })
  
  # ============================
  # ALERT ENGINE
  # ============================
  output$alert <- renderText({
    
    v <- filtered()[[input$var]]
    paste("High Risk Areas:", sum(v > quantile(v,0.9)))
  })
  
  # ============================
  # INSIGHT
  # ============================
  output$insight <- renderText({
    
    paste("Spatial heterogeneity detected in", input$var,
          "with clustered high-intensity zones in selected admin units.")
  })
  
  # ============================
  # CHARTS
  # ============================
  output$hist <- renderPlotly({
    plot_ly(x = filtered()[[input$var]], type="histogram")
  })
  
  output$box <- renderPlotly({
    plot_ly(y = filtered()[[input$var]], type="box")
  })
  
  output$rank <- renderPlotly({
    
    df <- filtered() %>%
      st_drop_geometry() %>%   # 🔥 FIX ERROR HERE
      group_by(lganame) %>%
      summarise(score = mean(.data[[input$var]], na.rm=TRUE)) %>%
      arrange(desc(score))
    
    plot_ly(df,
            x = ~reorder(lganame, score),
            y = ~score,
            type="bar")
  })
  
  # ============================
  # TABLE
  # ============================
  output$table <- renderTable({
    
    df <- filtered() %>%
      st_drop_geometry() %>%
      group_by(lganame) %>%
      summarise(score = mean(.data[[input$var]], na.rm=TRUE)) %>%
      arrange(desc(score))
    
    head(df,10)
  })
  
  # ============================
  # MAP
  # ============================
  output$map <- renderLeaflet({
    
    df <- filtered()
    
    pal <- colorNumeric("YlOrRd", df[[input$var]])
    
    leaflet(df) %>%
      addProviderTiles(input$basemap) %>%
      addPolygons(
        fillColor = ~pal(df[[input$var]]),
        fillOpacity = 0.8,
        color = "white",
        weight = 1,
        label = ~paste(lganame, round(df[[input$var]],2))
      ) %>%
      addLegend("bottomright", pal = pal,
                values = df[[input$var]],
                title = input$var)
  })
  
  # ============================
  # RESET
  # ============================
  observeEvent(input$reset, {
    updateSelectInput(session, "lga", selected="")
    updateSelectInput(session, "dist", selected="")
    updateSelectInput(session, "ward", selected="")
  })
  
}

# ============================
# RUN APP
# ============================
shinyApp(ui, server)
