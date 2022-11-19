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
library(sp)
library(raster)
library(leaflet)


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
    
    sales_by_country_tbl <- data %>% 
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
        layout(
            geo = list(
                lonaxis = list(range = c(-15, 23)),
                lataxis = list(range = c(35, 60)),
                showlakes = TRUE,
                lakecolor = toRGB("white")
            )
        )
    
    return(p)
    
}

products_tbl %>% get_sales_map_plot()


period   <- "day"
lookback <- 365/2

data <- products_tbl
by   <- "day"

get_trend_plot_data <- function(data, by, lookback_days = 500){
    
    # Look Back Setup
    # if(by == "day") lookback_days  <- lookback_days
    # if(by == "week") lookback_days <- lookback_days/7
    
    if(by == "day" & lookback_days > 90) lookback_days = 90
    if(by == "week" & lookback_days > 360) lookback_days = 360/7
    
    data_prep <- data %>% 
        summarise_by_time(date, by, num_sold = sum(num_sold)) %>% 
        bind_cols(
            data %>% 
                summarise_by_time(date, by, sales = sum(sales)) %>% 
                select(sales)
        ) %>% 
        tail(lookback_days) %>% 
        mutate(label_text = str_glue("
                            Products Sold: {num_sold %>% scales::comma(accuracy = 1)}
                            Sales: {sales %>% scales::dollar(accuracy = 1)}
                            
                            "))
    
    return(data_prep)
    
}

get_trend_plot_data(products_tbl, "week", lookback_days = 500)


get_trend_plot <- function(data){
    
    p <-  data %>% 
        ggplot(aes(date, sales))+
        geom_line(size = 0.5)+
        geom_point(aes(text = label_text), size = 0.8)+
        expand_limits(y = 0)+
        geom_smooth(method = "loess", span = 0.15)+
        scale_y_continuous(labels = scales::dollar_format())+
        scale_x_date(date_breaks = "months", date_labels = "%b-%y")+
        theme_minimal()+
        theme(axis.text = element_text(size = 10))+
        labs(y = NULL, x = NULL)
    
    p <- ggplotly(p, tooltip = "text")
    
    return(p)
    
}

get_trend_plot_data(products_tbl, "week", lookback_days = 360) %>% 
    get_trend_plot()




sales_by_country_tbl

eu_iso <- c(BEL = "BEL", FRA = "FRA", DEU = "DEU", ITA = "ITA", POL = "POL", ESP = "ESP")

eu <- list()

for(country_code in eu_iso){
    eu[[country_code]] <- getData("GADM", country = eu_iso[country_code], level = 1)
}

EU <- rbind(eu$BEL, eu$FRA, eu$DEU, eu$ITA, eu$POL, eu$ESP, makeUniqueIDs = TRUE)

col_pal <- colorQuantile(palette = "BuPu", domain = sales_by_country_tbl$total_sales)

leaflet() %>% 
    addTiles() %>% 
    addPolygons(data = EU, 
                stroke = FALSE,
                fillColor = ~col_pal(sales_by_country_tbl$total_sales),
                popup = paste("Text", sales_by_country_tbl$total_sales_text, "<br>"),
                fillOpacity = 0.9,
                color = "white",
                weight = 0.3) %>% 
    addLegend(position = "bottomleft", pal = col_pal,
              values = sales_by_country_tbl$total_sales, title = "Total Sales")

col_pal <- colorNumeric(
    palette = "Blues",
    domain = sales_by_country_tbl$total_sales
)


?getData

