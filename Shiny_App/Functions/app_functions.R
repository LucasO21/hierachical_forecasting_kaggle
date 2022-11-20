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


# **********************************************************************
# FUNCTIONS ----
# **********************************************************************

# Load Raw Data ----
get_raw_data <- function(){
    
    output <- vroom::vroom("../Data/train.csv") %>%
        clean_names() %>%
        as_tibble() %>%
        filter(date >= as.Date("2018-01-01"))
    
    return(output)
}


# Prep Product Sales Data ----
get_product_sales_data <- function(data){
    
    # Pricing Dataframe
    product <- unique(data$product)
    cost    <- c(1.99, 1.89, 1.29, 1.59)
    price   <- c(5.99, 4.99, 3.99, 4.99) 
    
    pricing_tbl <- tibble(product, cost, price)
    
    output <- data %>% 
        mutate(country_code = case_when(
            country == "Belgium" ~ "BEL",
            country == "France"  ~ "FRA", 
            country == "Germany" ~ "DEU",
            country == "Italy"   ~ "ITA",
            country == "Poland"  ~ "POL",
            country == "Spain"   ~ "ESP"
        )) %>% 
        left_join(pricing_tbl) %>% 
        mutate(total_cost = num_sold * cost) %>%
        mutate(total_sales = num_sold * price) %>%
        mutate(total_profit = total_sales - total_cost)
    
    return(output)
    
}

# sales_data <- get_raw_data() %>% get_product_sales_data()


# Value Boxes ----
get_value_box <- function(data){
    
    output <- data %>% 
        # filter(between(date, .min_date, .max_date)) %>% 
        # filter(country %in% .country) %>% 
        # filter(store %in% .store) %>% 
        # filter(product %in% .product) %>% 
        summarise(
            total_sold   = sum(num_sold),
            total_cogs   = sum(total_cost),
            total_sales  = sum(total_sales),
            total_profit = sum(total_profit)
        ) %>% 
        mutate(profit_margin = total_profit / total_sales) %>% 
        mutate(
            total_sold_txt    = total_sold %>% scales::comma(),
            total_sales_txt   = total_sales %>% scales::dollar(accuracy = 1),
            total_cogs_txt    = total_cogs %>% scales::dollar(accuracy = 1),
            total_profit_txt  = total_profit %>% scales::dollar(accuracy = 1),
            profit_margin_txt = profit_margin %>% scales::percent(accuracy = 1)
        )
    
    return(output)
        
   
}

# value_box_data <-  sales_data %>% get_value_box()


# Data Prep For Sales Map ----
get_sales_map_data <- function(data){
    
    data %>% 
        group_by(country, country_code) %>% 
        summarise(
            total_sold = sum(num_sold),
            total_sales = sum(total_sales)
        ) %>% 
        mutate(
            total_sold_text = total_sold %>% scales::dollar(accuracy = 1),
            total_sales_text = total_sales %>% scales::dollar(accuracy = 1)
        ) %>% 
        ungroup() %>% 
        mutate(tool_tip_label = str_glue(
            "Country: {country}
        Books Sold: {total_sold_text}
        Total Sales: {total_sales_text}"
        
        ))
    
}
#---

# map_data <- sales_data %>% get_sales_map_data()

# Country Map of Sales ----
get_sales_map_plot_plotly <- function(data){
    
    p <- data %>% 
        plot_geo(locations = data$country_code) %>% 
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
                lakecolor = toRGB("white"),
            )
        ) %>% 
        layout(legend = list(orientation = 'h'))
    
    return(p)
    
}

# sales_data %>% get_sales_map_data() %>% get_sales_map_plot_plotly()


# Data Prep - Sales Trend Plot ----
get_trend_plot_data <- function(data, period){
    
   data_prep <- data %>% 
        summarise_by_time(date, period, total_sold = sum(num_sold)) %>% 
        bind_cols(
            data %>% 
                summarise_by_time(date, period, total_sales = sum(total_sales)) %>% 
                dplyr::select(total_sales)
        ) %>% 
        arrange(date)
   
   if(period == "day" & n_distinct(data_prep$date) > 90) lookback = 90
   if(period == "week" & n_distinct(data_prep$date) > 52) lookback = 52
    
    data_prep <- data_prep %>% 
        tail(lookback) %>% 
        mutate(label_text = str_glue("
                            Date: {date}
                            Books Sold: {total_sold %>% scales::comma(accuracy = 1)}
                            Sales: {total_sales %>% scales::dollar(accuracy = 1)}
                            
                            "))
    
    return(data_prep)
    
}

# sales_data %>% get_trend_plot_data("week")


# Sales Trend Plot ----
get_sales_trend_plot_area <- function(data){
    
    diff_days <- as.numeric(str_extract(data$date[2] - data$date[1], "(\\d+)"))
    
    if(diff_days > 7){
        .date_breaks = "3 months"
    } else if (diff_days < 6){
        .date_breaks = "14 day"
    } else {
        .date_breaks = "2 months"
    }
    

    p <- data %>%
        ggplot(aes(date, total_sales))+
        geom_area(fill = "lightblue", alpha = 0.6)+
        geom_line(size = 1, color = "lightblue")+
        geom_point(aes(text = label_text), size = 2, color = "lightblue")+
        scale_y_continuous(labels = scales::dollar_format())+
        theme_minimal()+
        scale_x_date(date_breaks = .date_breaks, date_labels = "%b-%y")+
        theme_minimal()+
        theme(axis.text = element_text(size = 8))+
        labs(y = NULL, x = NULL)

    p <- ggplotly(p, tooltip = "text")

    return(p)


}

# sales_data %>% get_trend_plot_data("day") %>% get_sales_trend_plot_area()


# Data Prep - Sales by Product Barplot ----
get_sold_count_by_product_data <- function(data, period){
    
   data_prep <- data %>%
       group_by(product) %>%
       summarise_by_time(date, period, total_sold = sum(num_sold)) %>%
       ungroup()
    
    if(period == "day" & n_distinct(data_prep$date) > 90) lookback = 90
    if(period == "week" & n_distinct(data_prep$date) > 52) lookback = 52
    
    data_prep <- data_prep %>% 
        pivot_wider(names_from = product, values_from = total_sold) %>% 
        arrange(date) %>% 
        tail(lookback) %>% 
        pivot_longer(cols = `Kaggle Advanced Techniques`:`Kaggle Recipe Book`,
                     names_to = "product", values_to = "total_sold")
    
    return(data_prep)

}

# sales_data %>% get_sold_count_by_product_data("day")


# Sales by Product Barplot  ----
get_sold_count_by_product_plot <- function(data){
    
    diff_days <- as.numeric(str_extract(data$date[5] - data$date[1], "(\\d+)"))
    
    if(diff_days > 7){
        .date_breaks = "3 months"
    } else {
        .date_breaks = "1 months"
    }

    p <- data %>%
        mutate(label_text = str_glue("Date: {date}
                                     Product: {product}
                                     Total Sold: {total_sold %>% scales::comma(accuracy = 1)}")) %>%
        ggplot(aes(date, total_sold, fill = factor(product)))+
        geom_col(aes(text = label_text))+
        theme_minimal()+
        scale_x_date(date_breaks = .date_breaks, date_labels = "%b-%y")+
        tidyquant::scale_fill_tq()+
        scale_y_continuous(labels = scales::comma_format())+
        theme(axis.text = element_text(size = 8))+
        theme(legend.position = "none")+
        labs(x = NULL, y = NULL)

    ggplotly(p, tooltip = "text")
}

# data <- 
#     sales_data %>% 
#     get_sold_count_by_product_data("week") %>% 
#     # distinct(product) %>% 
#     get_sold_count_by_product_plot()







# Sales Map with Leaflet ----
# eu_iso <- c(BEL = "BEL", FRA = "FRA", DEU = "DEU", ITA = "ITA", POL = "POL", ESP = "ESP")
# 
# eu <- list()
# 
# for(country_code in eu_iso){
#     eu[[country_code]] <- getData("GADM", country = eu_iso[country_code], level = 0)
# }
# 
# EU <- rbind(eu$BEL, eu$FRA, eu$DEU, eu$ITA, eu$POL, eu$ESP, makeUniqueIDs = TRUE)
# 
# col_pal <- colorQuantile(palette = "BuPu", domain = sales_data$total_sales)
# 
# leaflet() %>%
#     addTiles() %>%
#     addPolygons(data = EU,
#                 stroke = FALSE,
#                 fillColor = ~col_pal(sales_data$total_sales),
#                 popup = paste("Text", sales_data$total_sales_text, "<br>"),
#                 fillOpacity = 0.9,
#                 color = "white",
#                 weight = 0.3) %>%
#     addLegend(position = "bottomleft", pal = col_pal,
#               values = sales_data$total_sales, title = "Total Sales")
# 
# col_pal <- colorNumeric(
#     palette = "Blues",
#     domain = sales_by_country_tbl$total_sales
# )
# ----




