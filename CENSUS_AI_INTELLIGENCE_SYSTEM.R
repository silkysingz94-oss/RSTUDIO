
# ============================
# PACKAGES
# ============================
library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(plotly)
library(RColorBrewer)
library(ggplot2)

# ============================
# LOAD DATA
# ============================
shp_path <- "C:/Users/ansu.s/Desktop/RCoding/Mapping_Census/RawData/GMB_Population.shp"
gmb_data <- st_read(shp_path, quiet = TRUE)

if (is.na(st_crs(gmb_data)$epsg)) st_crs(gmb_data) <- 4326
gmb_data <- st_transform(gmb_data, 4326)

safe_num <- function(x) as.numeric(as.character(x))

# ============================
# UI
# ============================
ui <- fluidPage(
  
  tags$head(tags$style(HTML("

    body { background:#eef2f7; }

    .header {
      background:#0b2e4a;
      color:white;
      padding:12px;
      border-radius:10px;
      margin-bottom:10px;
    }

    /* LEFT ANALYTICS PANEL (SCROLL ONLY HERE) */
    .left-panel {
      height: 900px;
      overflow-y: auto;
      background: white;
      padding: 10px;
      border-radius: 10px;
    }

    /* RIGHT MAP (STATIC) */
    .map-panel {
      height: 900px;
      border-radius: 10px;
      overflow: hidden;
    }

    .credit {
      position: fixed;
      bottom: 0;
      width: 100%;
      background: #111;
      color: white;
      text-align: center;
      font-size: 12px;
      padding: 6px;
    }

    .box {
      background: #f9fafc;
      padding: 10px;
      border-radius: 10px;
      margin-bottom: 10px;
      border: 1px solid #ddd;
    }

  "))),
  
  div(class="header",
      h2("🧠 SMART GIS VERSION 15 — CENSUS AI INTELLIGENCE SYSTEM"),
      p("National Statistical Dashboard | AI Analytics | Gambia Census 2023–2024")
  ),
  
  sidebarLayout(
    
    sidebarPanel(width = 3,
                 
                 selectInput("var", "Variable:",
                             c("Population"="map_pop",
                               "Households"="enum_hhs",
                               "Enumeration"="enum_pop")
                 ),
                 
                 selectInput("lga", "LGA:", c("")),
                 selectInput("dist", "District:", c("")),
                 
                 actionButton("run", "🚀 Run Analysis"),
                 actionButton("reset", "Reset")
    ),
    
    mainPanel(
      
      fluidRow(
        
        # ============================
        # LEFT ANALYTICS (SCROLLABLE)
        # ============================
        column(
          4,
          
          div(class="left-panel",
              
              h3("📊 AI STATISTICAL ENGINE"),
              
              div(class="box",
                  h4("📌 KPIs"),
                  textOutput("kpi_total"),
                  textOutput("kpi_mean"),
                  textOutput("kpi_max"),
                  textOutput("kpi_min"),
                  textOutput("kpi_sd"),
                  textOutput("kpi_cv")
              ),
              
              div(class="box",
                  h4("📈 Distribution"),
                  plotlyOutput("hist", height=250)
              ),
              
              div(class="box",
                  h4("📉 Boxplot Analysis"),
                  plotlyOutput("box", height=250)
              ),
              
              div(class="box",
                  h4("🏆 LGA Ranking"),
                  plotlyOutput("rank", height=300)
              ),
              
              div(class="box",
                  h4("📊 Inequality Insight"),
                  textOutput("gini")
              ),
              
              div(class="box",
                  h4("🧠 Interpretation"),
                  textOutput("insight")
              )
          )
        ),
        
        # ============================
        # RIGHT MAP (STATIC)
        # ============================
        column(
          8,
          div(class="map-panel",
              leafletOutput("map", height="900px")
          )
        )
      )
    )
  ),
  
  div(class="credit",
      "Produced by Ansu Singhateh | GIS Analyst | GBoS © 2026 | AI GIS v15 (Scrollable Analytics Engine)"
  )
)

# ============================
# SERVER
# ============================
server <- function(input, output, session){
  
  # ============================
  # INIT FILTERS
  # ============================
  observe({
    updateSelectInput(session, "lga",
                      choices = c("", sort(unique(gmb_data$lganame)))
    )
  })
  
  observeEvent(input$lga, {
    
    req(input$lga)
    
    d <- gmb_data %>%
      filter(lganame == input$lga) %>%
      pull(distname) %>%
      unique() %>%
      sort()
    
    updateSelectInput(session, "dist",
                      choices = c("", d)
    )
  })
  
  # ============================
  # FILTERED DATA
  # ============================
  data_ai <- eventReactive(input$run, {
    
    df <- gmb_data
    
    if (input$lga != "")
      df <- df %>% filter(lganame == input$lga)
    
    if (input$dist != "")
      df <- df %>% filter(distname == input$dist)
    
    df$val <- safe_num(df[[input$var]])
    
    df
  }, ignoreNULL = FALSE)
  
  # ============================
  # KPIs
  # ============================
  output$kpi_total <- renderText({
    paste("Total:", sum(data_ai()$val, na.rm=TRUE))
  })
  
  output$kpi_mean <- renderText({
    paste("Mean:", mean(data_ai()$val, na.rm=TRUE))
  })
  
  output$kpi_max <- renderText({
    paste("Max:", max(data_ai()$val, na.rm=TRUE))
  })
  
  output$kpi_min <- renderText({
    paste("Min:", min(data_ai()$val, na.rm=TRUE))
  })
  
  output$kpi_sd <- renderText({
    paste("SD:", sd(data_ai()$val, na.rm=TRUE))
  })
  
  output$kpi_cv <- renderText({
    v <- data_ai()$val
    cv <- sd(v)/mean(v)*100
    paste("CV%:", round(cv,2))
  })
  
  # ============================
  # GINI INDEX
  # ============================
  output$gini <- renderText({
    v <- data_ai()$val
    g <- sum(abs(outer(v, v, "-")), na.rm=TRUE) /
      (2 * length(v)^2 * mean(v, na.rm=TRUE))
    paste("Inequality (Gini):", round(g,3))
  })
  
  # ============================
  # INSIGHT ENGINE
  # ============================
  output$insight <- renderText({
    
    paste("Areas show spatial variation driven by",
          input$var,
          "with clustering in high-density EAs.")
  })
  
  # ============================
  # CHARTS
  # ============================
  output$hist <- renderPlotly({
    plot_ly(x = data_ai()$val, type="histogram")
  })
  
  output$box <- renderPlotly({
    plot_ly(y = data_ai()$val, type="box")
  })
  
  output$rank <- renderPlotly({
    
    df <- data_ai() %>%
      st_drop_geometry() %>%
      group_by(lganame) %>%
      summarise(score = mean(val, na.rm=TRUE)) %>%
      arrange(desc(score))
    
    plot_ly(df,
            x = ~reorder(lganame, score),
            y = ~score,
            type="bar")
  })
  
  # ============================
  # MAP (STATIC)
  # ============================
  output$map <- renderLeaflet({
    
    df <- data_ai()
    
    pal <- colorNumeric("YlOrRd", df$val)
    
    leaflet(df) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(val),
        fillOpacity = 0.8,
        weight = 1,
        color = "white",
        label = ~paste(eaname, val)
      ) %>%
      addLegend("bottomright", pal = pal, values = df$val)
  })
  
  # ============================
  # RESET
  # ============================
  observeEvent(input$reset, {
    
    updateSelectInput(session, "lga", selected = "")
    updateSelectInput(session, "dist", selected = "")
  })
  
}

# ============================
# RUN
# ============================
shinyApp(ui, server)

