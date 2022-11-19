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
library(shinyjs)

# Data
dataset <- vroom::vroom("../Data/train.csv") %>%
    clean_names() %>%
    as_tibble() %>%
    filter(date >= as.Date("2018-01-01")) %>%
    mutate(cost = num_sold * 1.99) %>%
    mutate(sales = num_sold * 5.99) %>%
    mutate(profit = sales - cost)

min_date <- min(dataset$date)
max_date <- max(dataset$date)

country_list <- c("Belgium", "Framce", "Germany", "Italy", "Poland", "Spain")

store_list <- c("KaggleMart", "KaggleRama")

products_list <- c("Kaggle Advanced Techniques", "Kaggle Getting Started",
                   "Kaggle Recipe Book", "Kaggle for Kids: One Smart Goose")

min_date <- min(dataset$date)
max_date <- max(dataset$date)

dashboardPage(
    dashboardHeader(title = "Kaggle Products Dashboard"),
    dashboardSidebar(
        sidebarMenu(
            
            # Date Range Input
            dateRangeInput(
                inputId   = "date_range",
                label     = h4("Date Range"),
                start     = min_date,
                end       = max_date,
                min       = min_date,
                max       = max_date,
                startview = "year"
            ),
            
            br(),
            
            # Country Selector
            pickerInput(inputId  = "country_picker",
                        label    = h4("Country"),
                        choices  = country_list,
                        selected = country_list,
                        multiple = TRUE,
                        options  = list(
                            `actions-box` = TRUE,
                            size          = 10,
                            `selected-text-format` = "count > 3"
                        )),
            
            # Store
            pickerInput(inputId  = "store_picker",
                        label    = h4("Store"),
                        choices  = store_list,
                        selected = store_list,
                        multiple = TRUE,
                        options  = list(
                            `actions-box` = TRUE,
                            size          = 10,
                            `selected-text-format` = "count > 1"
                        )),
            
            # Product
            pickerInput(inputId  = "product_picker",
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
            
            # Apply Button
            actionButton(inputId = "apply", label = "Apply", icon = icon(name = "play", lib = "font-awesome")),
            
            # Reset Button
            actionButton(inputId = "reset", label = "Reset", icon = icon("sync"))
            
            
        )
    ),
    dashboardBody(
        useShinyjs(),
        fluidRow(
            
            
            
            # Value Box 1
            valueBoxOutput(outputId = "num_sold_box", width = 3),
            
            # Value Box 2
            valueBoxOutput(outputId = "sales_box", width = 3),
            
            # Value Box 3
            valueBoxOutput(outputId = "profit_box", width = 3),
            
            # Value Box 4
            valueBoxOutput(outputId = "profit_margin_box", width = 3),
            
            # Map Box
            box(actionButton(inputId = "map_info", label = "",
                             icon = icon("question-circle")),
                
             
                
            
                
                title = "Sales Map", width = 6, solidHeader = T, status = "primary",
                plotlyOutput("sales_map", height = 700)
            ),
            
            # Sales Trend Box
            box(
                div(style = "display:inline-block;", radioGroupButtons(inputId = "time_unit", label = "Time Unit",
                                  choices = c("Day" = "day", "Week" = "week"), selected = "day",
                                  status = "primary")),
                
                div(style = "display:inline-block;", numericInput(inputId = "lookback", label = "Look Back",
                            min = 90, max = 360, value = 90, step = 30)),
                
                
                title = "Sales Trend", width = 6, solidHeader = T, status = "primary",
                plotlyOutput("sales_trend", height = 250)
            )
        )
    )
)

