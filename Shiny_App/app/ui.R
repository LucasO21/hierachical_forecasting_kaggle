#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinyWidgets)

# Data
dataset <- vroom::vroom("../Data/train.csv") %>% 
    clean_names() %>% 
    as_tibble() %>% 
    filter(date >= as.Date("2018-01-01")) %>% 
    mutate(cost = num_sold * 1.99) %>% 
    mutate(sales = num_sold * 5.99) %>% 
    mutate(profit = sales - cost)

dashboardPage(
    dashboardHeader(title = "Kaggle Products Dashboard"),
    dashboardSidebar(
        sidebarMenu(
            
            # Date Range Input
            dateRangeInput(
                inputId   = "date_range",
                label     = h4("Date Range"),
                start     = min(dataset$date),
                end       = max(dataset$date),
                min       = min(dataset$date),
                max       = max(dataset$date),
                startview = "year"
            ),
            
            br(),
            
            # Country Selector
            pickerInput(inputId  = "country_picker",
                        label    = h4("Country"),
                        choices  = unique(dataset$country),
                        selected = unique(dataset$country),
                        multiple = TRUE,
                        options  = list(
                            `actions-box` = TRUE,
                            size          = 10,
                            `selected-text-format` = "count > 3"
                        )),
            
            # Store
            pickerInput(inputId  = "store_picker",
                        label    = h4("Store"),
                        choices  = unique(dataset$store),
                        selected = unique(dataset$store),
                        multiple = TRUE,
                        options  = list(
                            `actions-box` = TRUE,
                            size          = 10,
                            `selected-text-format` = "count > 1"
                        )),
            
            # Product
            pickerInput(inputId  = "product_picker",
                        label    = h4("Product Name"),
                        choices  = unique(dataset$product),
                        selected = unique(dataset$product),
                        multiple = TRUE,
                        options  = list(
                            `actions-box` = TRUE,
                            size          = 10,
                            `selected-text-format` = "count > 3"
                        )),
            
            
            br(),
            hr(),
            br(),
            
            # Apply Button
            actionButton(inputId = "apply", label = "Apply", icon = icon(name = "play", lib = "font-awesome")),
            
            # Reset Button
            actionButton(inputId = "reset", label = "Reset", icon = icon("sync"))
            
            
        )
    ),
    dashboardBody(
        fluidRow(
            
            # Value Box 1
            valueBoxOutput(outputId = "num_sold_box", width = 3),
            
            # Value Box 2
            valueBoxOutput(outputId = "sales_box", width = 3),
            
            # Value Box 3
            valueBoxOutput(outputId = "profit_box", width = 3),
            
            # Value Box 4
            valueBoxOutput(outputId = "profit_margin_box", width = 3)
        )
    )
)

