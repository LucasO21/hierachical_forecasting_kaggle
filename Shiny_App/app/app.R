# SHINY APP UI & SERVER SCRIPT ----
# **** ----

# Libraries ----

# * Core ----
# library(tidyverse)
# library(janitor)
# library(timetk)

# * Shiny ----
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinyBS)
library(shinyjs)

# * Visualization ----
library(plotly)
library(DT)

# * Modeling ----
library(xgboost)

# * Source ----
source("app_functions/forecast_functions.R")

# Static Params ----
country_list <- c("Belgium", "France", "Germany", "Italy", "Poland", "Spain")

store_list <- c("KaggleMart", "KaggleRama")

products_list <- c("Kaggle Advanced Techniques", "Kaggle Getting Started",
                   "Kaggle Recipe Book", "Kaggle for Kids: One Smart Goose")

# Data ----
future_forecast_tbl <- load_data(ret = "future_forecast")
test_forecast_tbl   <- load_data(ret = "test_forecast")

ui <-
    dashboardPage(
    dashboardHeader(title = "Forecasting App"),
    
    # * Sidebar Panel ----
    dashboardSidebar(
        
        br(),
        br(),

        # ** Country Picker ----
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

        # ** Store Picker ----
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
        
        # ** Product Picker
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

        # ** Apply Button ---
        actionButton(
            inputId = "apply",
            label   = "Apply",
            icon    = icon(name = "play", lib = "font-awesome")
        ),

        # ** Reset Button ----
        actionButton(
            inputId = "reset",
            label   = "Reset",
            icon    = icon(name = "sync")
        ),
        
        br(),
        hr(),
        br(),
        
        # ** Download Button ----
        downloadButton(
            outputId = "download_forecast_data",
            label    = "Download Forecast Data"
        )
    ),
    
    # Dashboard Body ----
    dashboardBody(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom-css.css")
        ),
        useShinyjs(),
        
        # * Column 1 ----
        column(
            width = 6,
            
            # Future Forecast Plot ----
            box(
                width = 12,
                tags$fieldset(
                    tags$legend("Future Forecast", tags$span(id = "ff_plot_info", icon("info-circle"))),
                    plotlyOutput("future_forecast_plot", height = "400px")
                )
            ), # end future forecast box
            
            # * Future Forecast Data ----
            box(
                width = 12,
                tags$fieldset(
                    tags$legend("Future Forecast Data", tags$span(id = "ff_data_info", icon("info-circle"))),
                    dataTableOutput("future_forecast_dt", height = "300px")
                )
            ),
        ), # end column
        
        # * Column 2 ----
        column(
            width = 6,
            
            # ** Test Data Plot ----
            box(
                width = 12,
                tags$fieldset(
                    tags$legend("Test Forecast Plot", tags$span(id = "tf_plot_info", icon("info-circle"))),
                    plotlyOutput("test_forecast_plot", height = "400px")
                ) 
            ), # end test plot box
            
            # ** Test Metrics Datatable ----
            box(
                width = 12,
                tags$fieldset(
                    tags$legend("Test Data Metrics", tags$span(id = "tm_data_info", icon("info-circle"))),
                    dataTableOutput("test_forecast_metrics_dt", height = "300px")
                ) 
            ), # end test metrics datatable
            
        ), # end column 2
        
        bsPopover(
            "ff_plot_info", title = "Future Forecast",
            content = "This plot shows 90 day future forecast (in red).",
            placement = "left"
        ),
        
        bsPopover(
            "ff_data_info", title = "Future Forecast Data",
            content = "This plot shows 90 day future forecast values.",
            placement = "left"
        ),
        
        bsPopover(
            "tf_plot_info", title = "Test Forecast Plot",
            content = "This plot shows prediction vs actual for the test data.",
            placement = "left"
        ),
        
        bsPopover(
            "tm_data_info", title = "Test Data Metrics",
            content = "This table shows the metrics model performance on the test data.",
            placement = "left"
        ),
        
        
    ) # end dashboardBody
)

# ******************************************************************************
# SERVIER ----
# ******************************************************************************v

server <- function(input, output) {

    # * Future Forecast Data ----
    future_forecast_filtered_tbl <- eventReactive(eventExpr = input$apply, valueExpr = {

        future_forecast_tbl %>%
            filter(country %in% input$country_picker) %>% 
            filter(store %in% input$store_picker) %>% 
            filter(product %in% input$product_picker)

    }, ignoreNULL = FALSE
    )

    # * Test Forecast Data ----
    test_forecast_filtered_tbl <- eventReactive(eventExpr = input$apply, valueExpr = {
        
        test_forecast_tbl %>%
            filter(country %in% input$country_picker) %>% 
            filter(store %in% input$store_picker) %>% 
            filter(product %in% input$product_picker)
        
    }, ignoreNULL = FALSE
    )
    
    # * Future Forecast Plot ----
    output$future_forecast_plot <- renderPlotly({
        future_forecast_filtered_tbl() %>% 
            get_future_forecast_data_prepared() %>% 
            get_forecast_plot()
    })
    
    # * Test Forecast Plot ----
    output$test_forecast_plot <- renderPlotly({
        test_forecast_filtered_tbl() %>% 
            get_test_forecast_data_prepared() %>% 
            get_forecast_plot()
    })
    
    # * Future Forecast Data ----
    output$future_forecast_dt <- DT::renderDataTable(
        future_forecast_filtered_tbl() %>% 
            get_future_forecast_data_prepared() %>% 
            get_future_forecast_data_prepared_dt(),
        options = list(pageLength = 5)
    )
    
    # * Test Forecast Metrics ----
    output$test_forecast_metrics_dt <- DT::renderDataTable(
        test_forecast_filtered_tbl() %>% 
            get_test_forecast_metrics_dt()
    )
    
    # * Download Handler (Forecast Data) ----
    output$download_forecast_data <- downloadHandler(
        
        filename = function(){
            paste("future_forecast", "csv", sep = ".")
        },
        
        content = function(file){
            write.csv(
                future_forecast_filtered_tbl() %>% 
                    get_future_forecast_data_prepared() %>% 
                    get_future_forecast_data_prepared_dt(),
                file
            )
        }
    )

    # * Apply / Reset Reactive Filters (Sales Tab) ----
    shiny::observeEvent(eventExpr = input$reset, handlerExpr = {
        
        # ** Reset Country Input ----
        updatePickerInput(
            session = getDefaultReactiveDomain(),
            inputId = "country_picker",
            choices  = country_list,
            selected = country_list
        )

        # ** Store Input ----
        updatePickerInput(
            session = getDefaultReactiveDomain(),
            inputId = "store_picker",
            choices  = store_list,
            selected = store_list
        )

        # ** Product Country Input ----
        updatePickerInput(
            session = getDefaultReactiveDomain(),
            inputId = "product_picker",
            choices  = products_list,
            selected = products_list
        )

        # ** Apply Button ----
        shinyjs::delay(ms = 300, expr = {
            shinyjs::click(id = "apply")
        })

    })


}



shinyApp(ui, server)


