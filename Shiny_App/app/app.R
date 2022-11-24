# SHINY APP UI & SERVER SCRIPT ----
# **** ----

# Libraries ----

# * Core ----
library(tidyverse)
library(janitor)
library(timetk)

# * Shiny ----
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinyBS)
library(shinyjs)

# Visualization ----
library(plotly)
library(DT)

# * Source ----
source("../Functions/forecast_functions.R")

# Static Params ----
country_list <- c("Belgium", "France", "Germany", "Italy", "Poland", "Spain")

store_list <- c("KaggleMart", "KaggleRama")

products_list <- c("Kaggle Advanced Techniques", "Kaggle Getting Started",
                   "Kaggle Recipe Book", "Kaggle for Kids: One Smart Goose")

# Data ----
future_forecast_tbl <- load_data(ret = "future_forecast")

ui <- 
    dashboardPage(
    dashboardHeader(title = "Forecasting App"),
 
    dashboardSidebar(
        
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
        
        br(),
        hr(),
        br(),
        
        # * Apply Button ---
        actionButton(
            inputId = "apply", 
            label   = "Apply", 
            icon    = icon(name = "play", lib = "font-awesome")
        ),
        
        # * Reset Button ----
        actionButton(
            inputId = "reset",
            label = "Reset",
            icon = icon("sync")
        )
    ),
    dashboardBody(
        dataTableOutput("test_table")
    )
)

server <- function(input, output) { 
    
    # Data Setup
    future_forecast_filtered_tbl <- eventReactive(eventExpr = input$apply, valueExpr = {
        
        future_forecast_tbl %>% 
            filter(country %in% input$country_picker) 
        
    }, ignoreNULL = FALSE
    )
    
    output$test_table <- renderDataTable(({future_forecast_filtered_tbl()}))
    
    # Apply / Reset Reactive Filters ----
    observeEvent(eventExpr = input$reset, handlerExpr = {
        
        # * Reset Date Range Input ----
        updateDateRangeInput(
            session   = getDefaultReactiveDomain(),
            inputId   = "date_range",
            start     = as.Date("2018-01-01"),
            end       = as.Date("2020-12-31")
        )
    })
    
    
}



shinyApp(ui, server)