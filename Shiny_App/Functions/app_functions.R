

# SETUP ----

# * Set Working Dir
setwd(here::here("Shiny_App", "Functions"))

# * Libraries ----
library(tidyverse)
library(janitor)
library(lubridate)
library(timetk)


# * Load Data ----
# products_tbl <- vroom::vroom("../Data/train.csv") %>% 
#     clean_names() %>% 
#     as_tibble() %>% 
#     filter(date >= as.Date("2018-01-01")) %>% 
#     mutate(cost = num_sold * 1.99) %>% 
#     mutate(sales = num_sold * 5.99) %>% 
#     mutate(profit = sales - cost) %>% 

# products_tbl %>% glimpse()


# * Params ----
# min_date <- as.Date("2020-01-01")
# max_date <- as.Date("2020-03-31")
# country  <- "Belgium"
# store    <- "KaggleMart"
# product  <- "Kaggle Advanced Techniques"


# * Data Filtered ----
# data_filtered <- products_tbl %>% 
#     filter(between(date, .min_date, .max_date)) %>% 
#     filter(country %in% .country) %>% 
#     filter(store %in% .store) %>% 
#     filter(product %in% .product)
# 
# data <- data_filtered


# FUNCTIONS ----

# * Value Boxes ----
get_value_box <- function(data){
    
    data %>% 
        # filter(between(date, .min_date, .max_date)) %>% 
        # filter(country %in% .country) %>% 
        # filter(store %in% .store) %>% 
        # filter(product %in% .product) %>% 
        summarise(
            num_sold = sum(num_sold),
            cogs     = sum(cost),
            sales    = sum(sales),
            profit   = sum(profit)
        ) %>% 
        mutate(profit_margin = profit / sales) %>% 
        mutate(
            num_sold = num_sold %>% scales::comma(),
            sales    = sales %>% scales::dollar(accuracy = 1),
            cogs     = cogs %>% scales::dollar(accuracy = 1),
            profit   = profit %>% scales::dollar(accuracy = 1),
            profit_margin = profit_margin %>% scales::percent(accuracy = 1)
        )
        
   
}

# value_box_tbl <- get_value_box(data = data_filtered)
# 
# value_box_tbl$num_sold
