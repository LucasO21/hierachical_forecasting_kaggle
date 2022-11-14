#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(janitor)
library(shiny)
library(shinyjs)
source("../Functions/app_functions.R")

# Data
dataset <- vroom::vroom("../Data/train.csv") %>% 
    clean_names() %>% 
    as_tibble() %>% 
    filter(date >= as.Date("2018-01-01")) %>% 
    mutate(cost = num_sold * 1.99) %>% 
    mutate(sales = num_sold * 5.99) %>% 
    mutate(profit = sales - cost)



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    # Sales Data
    dataset_filtered_tbl <- eventReactive(eventExpr = input$apply, valueExpr = {
        
        dataset %>% 
            filter(between(date, input$date_range[1], input$date_range[2])) %>% 
            filter(country %in% input$country_picker) %>% 
            filter(store %in% input$store_picker) %>% 
            filter(product %in% input$product_picker)
        
    }, ignoreNULL = FALSE
    )
    
    # Value Box
    # value_box_tbl <- reactive({
    #     get_value_box(data = dataset_filtered_tbl())
    # })
        
    # Num Sold Value Box
    output$num_sold_box <- renderValueBox({
        valueBox(
            get_value_box(data = dataset_filtered_tbl())$num_sold, 
            "Products Sold", 
            icon = icon(name = "boxes", lib = "font-awesome"),
            color = "green"
        )
    })
    
   # Sales Box
    output$sales_box <- renderValueBox({
        valueBox(
            get_value_box(data = dataset_filtered_tbl())$sales, 
            "Sales", 
            icon = icon(name = "money-bill", lib = "font-awesome"),
            color = "green"
        )
    })
    
    # Profit Box
    output$profit_box <- renderValueBox({
        valueBox(
            get_value_box(data = dataset_filtered_tbl())$profit, 
            "Profit", 
            icon = icon(name = "piggy_bank", lib = "font-awesome"),
            color = "green"
        )
    })
    
    # Profit Margin Box
    output$profit_margin_box <- renderValueBox({
        valueBox(
            get_value_box(data = dataset_filtered_tbl())$profit_margin, 
            "Profit %", 
            icon = icon(name = "percent", lib = "font-awesome"),
            color = "green"
        )
    })
    
    # Apply / Reset Reactive Filters
    observeEvent(eventExpr = input$reset, handlerExpr = {
        
        # Reset Date Range Input
        updateDateRangeInput(
            session   = getDefaultReactiveDomain(),
            inputId   = "date_range",
            start     = min(dataset$date),
            end       = max(dataset$date)
        )
        
        # Reset Country Input
        updatePickerInput(
            session = getDefaultReactiveDomain(),
            inputId = "country_picker",
            choices  = unique(dataset$country),
            selected = unique(dataset$country)
        )
        
        # Store Input
        updatePickerInput(
            session = getDefaultReactiveDomain(),
            inputId = "store_picker",
            choices  = unique(dataset$store),
            selected = unique(dataset$store)
        )
        
        # Product Country Input
        updatePickerInput(
            session = getDefaultReactiveDomain(),
            inputId = "product_picker",
            choices  = unique(dataset$product),
            selected = unique(dataset$product)
        )
        
        # Apply Button
        delay(ms = 300, expr = {click(id = "apply")})
        
    })
    
    
 

 

})
