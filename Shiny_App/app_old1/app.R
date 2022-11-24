# SHINY APP UI & SERVER SCRIPT ----
# **** ----

# Set Working Dir ----
# setwd(here::here("Shiny_App", "app"))

# Libraries ----

# * Core ----
library(tidyverse)
library(janitor)

# * Shiny ----
library(shiny)
library(shinyjs)
library(rintrojs)
library(shinyWidgets)
library(shinydashboard)
library(shinyBS)

# * Visualization ----
library(plotly)

# Source ----
source("../Functions/app_functions.R")
source("../Functions/forecast_functions.R")


# Data ----
dataset <- get_raw_data() %>% get_product_sales_data()

future_dataset <- read_rds("../Artifacts/artifacts_list.rds")$data$future_forecast_tbl


# Static Params ----
country_list <- c("Belgium", "Framce", "Germany", "Italy", "Poland", "Spain")

store_list <- c("KaggleMart", "KaggleRama")

products_list <- c("Kaggle Advanced Techniques", "Kaggle Getting Started",
                   "Kaggle Recipe Book", "Kaggle for Kids: One Smart Goose")

# **************************************************************
# UI ----
# **************************************************************
ui <- navbarPage(
  useShinydashboard(),
  useShinyjs(),
  
  # App Title ----
  title = "Kaggle Sales App",
  
  # Sales Trend Tab ----
  tabPanel(
    "Sales", icon = icon("stats", lib = "glyphicon"),
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          width = 2,
          
          # * Date Range Picker ----
          dateRangeInput(
            inputId   = "date_range",
            label     = h4("Date Range"),
            start     = as.Date("2018-01-01"),
            end       = as.Date("2020-12-31"),
            min       = as.Date("2018-01-01"),
            max       = as.Date("2020-12-31"),
            startview = "year"
          ),
          
          br(),
          
          # * Country Picker ----
          pickerInput(
            inputId  = "country_picker",
            label    = h4("Country"),
            choices  = country_list,
            selected = country_list,
            multiple = TRUE,
            options  = list(
              `actions-box` = TRUE,
              size          = 10,
              `selected-text-format` = "count > 3"
            )),
          
          # * Store Picker ----
          pickerInput(
            inputId  = "store_picker",
            label    = h4("Store"),
            choices  = store_list,
            selected = store_list,
            multiple = TRUE,
            options  = list(
              `actions-box` = TRUE,
              size          = 10,
              `selected-text-format` = "count > 1"
            )),
          
          # * Product Picker
          pickerInput(
            inputId  = "product_picker",
            label    = h4("Product Name"),
            choices  = products_list,
            selected = products_list,
            multiple = TRUE,
            options  = list(
              `actions-box` = TRUE,
              size          = 10,
              `selected-text-format` = "count > 3"
            )),
          
          br(),
          hr(),
          br(),
          
          # * Apply Button ----
          actionButton(
            inputId = "apply", 
            label   = "Apply", 
            icon    = icon(name = "play", lib = "font-awesome")
          ),
          
          # * Reset Button ----
          actionButton(
            inputId = "reset", 
            label   = "Reset", 
            icon    = icon("sync")
          ),
          
          br(),
          hr(),
          br(),
          
          
          # * Time Unit Picker ----
          radioGroupButtons(
            inputId  = "time_unit", 
            label    = "Time Unit",
            choices  = c("D" = "day", "W" = "week", "M" = "month"),  
            selected = "day"
          ),
          
          br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), 
          br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
          
   
        ),
        
        # Sales Tab Main Panel ----
        mainPanel(
          fluidRow(
            # * Value Box 1 ----
            valueBoxOutput(outputId = "num_sold_box", width = 4),
            
            # * Value Box 2 ----
            valueBoxOutput(outputId = "sales_box", width = 4),
            
            # * Value Box 3 ----
            valueBoxOutput(outputId = "profit_box", width = 4),
            
            # * Value Box 4 ----
          #   valueBoxOutput(outputId = "profit_margin_box", width = 3),
          ),
          
          tags$hr(),
          br(),
          
          fluidRow(
            
            column(
              width = 6,
              tags$fieldset(
                tags$legend("Sales Map", tags$span(id = "info1", icon("info-circle"))),
                plotlyOutput("sales_map", height = "400px")
              )
            ),
            
            bsPopover(
              "info1",
              title = "Sales Map",
              content = "This plot shows number of books sold and sales dollars by country.",
              placement = "left"
            ),
            
            column(
              width = 6, 
              
              # * Sales Trend $ ----
              tags$fieldset(
                tags$legend("Sales Trend", tags$span(id = "info2", icon("info-circle"))),
                plotlyOutput("sales_trend_dollars", height = "400px")
              ),
              
              br(),
              
              tags$fieldset(
                tags$legend("Product Trend", tags$span(id = "info3", icon("info-circle"))),
                plotlyOutput("product_trend_count", height = "400px")
              )
            ),
            
            bsPopover(
              "info2",
              title = "Sales Trend",
              content = "This plot shows sales dollars for books sold. Use Time Unit input to see trend by day, week or month.",
              placement = "left"
            ),
            
            bsPopover(
              "info3",
              title = "Products Trend",
              content = "This plot shows sales by product. Use Time Unit input to see trend by day, week or month.",
              placement = "left"
            ),
            
          )
        )
      )
    )
  ), # End tabPanel - Sales Tab
  
  # Forecast Tab ----
  tabPanel(
    "Forecast", icon("trending", lib = "glyphicon"),
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          width = 2,
          
          # * Country Selector ----
          pickerInput(inputId  = "country_picker_forecast",
                      label    = h4("Country"),
                      choices  = country_list,
                      selected = country_list,
                      multiple = TRUE,
                      options  = list(
                        `actions-box` = TRUE,
                        size          = 10,
                        `selected-text-format` = "count > 3"
                      )),
          
          # * Store Selector ----
          pickerInput(inputId  = "store_picker_forecast",
                      label    = h4("Store"),
                      choices  = store_list,
                      selected = store_list,
                      multiple = TRUE,
                      options  = list(
                        `actions-box` = TRUE,
                        size          = 10,
                        `selected-text-format` = "count > 1"
                      )),
          
          br(),
          hr(),
          br(),
          
          # * Apply Button ---
          actionButton(
            inputId = "apply_forecast_tab", 
            label   = "Apply", 
            icon    = icon(name = "play", lib = "font-awesome")
          ),
          
          # Reset Button ---
          actionButton(
            inputId = "reset_forecast_tab", 
            label   = "Reset", 
            icon    = icon("sync")
          ),
          
          br(),
          hr(),
          br(),
          
          # * Download Data Button ----
          downloadButton(
            outputId = "download_forecast",
            label    = "Download Forecast Data"
          )
        ),
        
        mainPanel(
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Forecast",
              fluidRow(
                column(
                  width = 12,
                  tags$fieldset(
                    tags$legend("Future Forecast", tags$span(id = "info1", icon("info-circle"))),
                    plotlyOutput("future_forecast_plot", height = "400px")
                  )
                  
                )
              )
            ),
            tabPanel("Test Forecast")
          )
        )
      )
    )
  )
)

# tabPanel(
#   "Sales", icon = icon("stats", lib = "glyphicon"),
#   fluidPage(
#     sidebarLayout(
#       sidebarPanel(
#         width = 2,

# fluidRow(
#   
#   column(
#     width = 6,
#     tags$fieldset(
#       tags$legend("Sales Map", tags$span(id = "info1", icon("info-circle"))),
#       plotlyOutput("sales_map", height = "400px")
#     )
#   ),
#   
#   bsPopover(
#     "info1",
#     title = "Sales Map",
#     content = "This plot shows number of books sold and sales dollars by country.",
#     placement = "left"
#   ),

# **************************************************************
# SERVER ----
# **************************************************************
server <- function(input, output) {
  
  # Data Setup
  data_filtered_tbl <- eventReactive(eventExpr = input$apply, valueExpr = {
    
    dataset %>% 
      filter(between(date, input$date_range[1], input$date_range[2])) %>% 
      filter(country %in% input$country_picker) %>% 
      filter(store %in% input$store_picker) %>% 
      filter(product %in% input$product_picker)
    
  }, ignoreNULL = FALSE
  )
  
  # Future Data Setup ----
  data_filtered_future_tbl <- eventReactive(eventExpr = input$apply_forecast_tab, valueExpr = {
    
    future_dataset %>% 
      filter(country %in% input$country_picker_forecast) %>% 
      filter(store %in% input$store_picker_forecast) 
    
  }, ignoreNULL = FALSE
  )
  
  
  # Value Box 1 ----
  output$num_sold_box <- shinydashboard::renderValueBox({
    valueBox(
      get_value_box(data = data_filtered_tbl())$total_sold_txt, 
      "Books Sold", 
      icon = icon(name = "boxes", lib = "font-awesome")
    )
  })
  
  # Value Box 2 ----
  output$sales_box <- renderValueBox({
    valueBox(
      get_value_box(data = data_filtered_tbl())$total_sales_txt, 
      "Sales", 
      icon = icon(name = "money-bill", lib = "font-awesome")
    )
  })
  
  # Value Box 3 ----
  output$profit_box <- renderValueBox({
    valueBox(
      get_value_box(data = data_filtered_tbl())$total_profit_txt, 
      "Profit", 
      icon = icon(name = "piggy_bank", lib = "font-awesome")
    )
  })
  
   # Sales Map ----
  output$sales_map <- renderPlotly({
    get_sales_map_data(data_filtered_tbl()) %>%
      get_sales_map_plot_plotly()
  })
  
  # Sales Trend ----
  output$sales_trend_dollars <- renderPlotly({
    get_trend_plot_data(data_filtered_tbl(), period = input$time_unit) %>%
      get_sales_trend_plot_area()
  })
  
  
  # Count Sold by Product Plot ----
  output$product_trend_count <- renderPlotly({
    get_sold_count_by_product_data(data_filtered_tbl(), period = input$time_unit) %>%
      get_sold_count_by_product_plot()
  })
  

  # Apply / Reset Reactive Filters (Sales Tab) ----
  observeEvent(eventExpr = input$reset, handlerExpr = {
    
    # * Reset Date Range Input ----
    updateDateRangeInput(
      session   = getDefaultReactiveDomain(),
      inputId   = "date_range",
      start     = as.Date("2018-01-01"),
      end       = as.Date("2020-12-31")
    )
    
    # * Reset Country Input ----
    updatePickerInput(
      session = getDefaultReactiveDomain(),
      inputId = "country_picker",
      choices  = country_list,
      selected = country_list
    )
    
    # * Store Input ----
    updatePickerInput(
      session = getDefaultReactiveDomain(),
      inputId = "store_picker",
      choices  = store_list,
      selected = store_list
    )
    
    # * Product Country Input ----
    updatePickerInput(
      session = getDefaultReactiveDomain(),
      inputId = "product_picker",
      choices  = products_list,
      selected = products_list
    )
    
    # * Apply Button ----
    shinyjs::delay(ms = 300, expr = {
      shinyjs::click(id = "apply")
    })
    
  })
  
  # Future Forecast ----
  output$future_forecast_plot <- renderPlotly({
    get_future_forecast(data_filtered_future_tbl()) 
  })
  
  # Apply / Reset Reactive Filters (Forecast Tab) ----
  observeEvent(eventExpr = input$reset_forecast_tab, handlerExpr = {
    
    # * Reset Country Input ----
    updatePickerInput(
      session = getDefaultReactiveDomain(),
      inputId = "country_picker_forecast",
      choices  = country_list,
      selected = country_list
    )
    
    # * Store Input ----
    updatePickerInput(
      session = getDefaultReactiveDomain(),
      inputId = "store_picker_forecast",
      choices  = store_list,
      selected = store_list
    )
  
    
    # * Apply Button ----
    shinyjs::delay(ms = 300, expr = {
      shinyjs::click(id = "apply_forecast_tab")
    })
    
  })
  
}

# Run the application ----
shinyApp(ui = ui, server = server)

