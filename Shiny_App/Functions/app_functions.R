# FUNCTIONS FOR APP ----
# **** ----

# **********************************************************************
# SETUP ----
# **********************************************************************


# * Set Working Dir
setwd(here::here("Shiny_App", "Functions"))

# * Libraries ----
library(tidyverse)
library(janitor)
library(lubridate)
library(timetk)
library(plotly)


# * Load Data ----
products_tbl <- vroom::vroom("../Data/train.csv") %>%
    clean_names() %>%
    as_tibble() %>%
    filter(date >= as.Date("2018-01-01")) %>%
    mutate(cost = num_sold * 1.99) %>%
    mutate(sales = num_sold * 5.99) %>%
    mutate(profit = sales - cost)

products_tbl %>% glimpse()


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

# **********************************************************************
# FUNCTIONS ----
# **********************************************************************

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

# Country Map of Sales ----
get_sales_map_plot <- function(data){
    
    sales_by_country_tbl <- products_tbl %>% 
        group_by(country) %>% 
        summarise(
            total_sold = sum(num_sold),
            total_sales = sum(sales)
        ) %>% 
        mutate(
            total_sold_text = total_sold %>% scales::dollar(accuracy = 1),
            total_sales_text = total_sales %>% scales::dollar(accuracy = 1)
        ) %>% 
        ungroup() %>% 
        mutate(country_code = case_when(
            country == "Belgium" ~ "BEL",
            country == "France"  ~ "FRA", 
            country == "Germany" ~ "DEU",
            country == "Italy"   ~ "ITA",
            country == "Poland"  ~ "POL",
            country == "Spain"   ~ "ESP"
        )) %>% 
        mutate(tool_tip_label = str_glue(
            "Country: {country}
        Total Sold: {total_sold_text}
        Total Sales: {total_sales_text}"
        
        ))
    
    p <- sales_by_country_tbl %>% 
        plot_geo(locations = sales_by_country_tbl$country_code) %>% 
        add_trace(
            z         = ~total_sold, 
            locations = ~country_code, 
            color     = ~total_sold,
            colors    = "Blues",
            text      = ~tool_tip_label
        ) %>% 
        layout(geo = list(
            lonaxis = list(range = c(-15, 23)),
            lataxis = list(range = c(35, 60))
        ))
    
    return(p)
    
}

products_tbl %>% get_sales_map_plot()


    



# Data
countries <- c("Germany", "Belgium", "Framce", "Italy", "Spain", "Poland")
codes     <- c("DEU", "BEL", "FRA", "ITA", "ESP", "POL")
values    <- c(100, 200, 300, 400, 500, 600)

df <- tibble(countries, codes, values)

# Maps
plot_geo(locations = df$codes) %>% 
    add_trace(
        z = ~values,
        locations = ~codes,
        color = ~values
    )

