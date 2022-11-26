# FORECAST FUNCTIONS FOR SHINY APP ----
# **** ----

# Set Working Dir ----
# setwd(here::here("Shiny_App", "Functions"))

# Libraries ----
library(tidyverse)
library(janitor)
library(lubridate)
library(timetk)
library(data.table)
library(dtplyr)
library(modeltime)


# ******************************************************************************
# LOAD DATA ----
# ******************************************************************************
load_data <- function(ret = "future_forecast"){
    
    # Artifacts
    artifacts_list <- read_rds("../app_artifacts/artifacts_list_version2.rds")
    
    # Data
    if(ret == "future_forecast") data_raw <- artifacts_list$data$future_forecast_tbl 
    if(ret == "test_forecast")   data_raw <- artifacts_list$data$test_data_forecast_tbl

    data_raw <- data_raw %>% 
        mutate(across(.cols = c(.value, total_sold), .fns = exp))
    
    # Codes
    country_code_tbl <- artifacts_list$data$country_code_tbl
    store_code_tbl   <- artifacts_list$data$store_code_tbl
    product_code_tbl <- artifacts_list$data$product_code_tbl
    
    # Final Output
    output <- data_raw %>% 
        mutate(country_code = substr(id, 1, 3)) %>% 
        mutate(store_code = substr(id, 4, 6)) %>% 
        mutate(product_code = substr(id, 7, 9)) %>% 
        left_join(country_code_tbl) %>% 
        left_join(store_code_tbl) %>% 
        left_join(product_code_tbl) %>% 
        dplyr::select(-ends_with("_code"), -ends_with("K1"), -contains("lag90"))
    
    return(output)
    

}

# sample_future_forecast_tbl <- load_data(ret = "future_forecast")
# 
# sample_test_forecast_tbl <- load_data(ret = "test_forecast")
# 
# sample_future_forecast_tbl %>% glimpse()
# 
# sample_future_forecast_tbl %>% View()


# ******************************************************************************
# FUTURE FORECAST DATA PREP ----
# ******************************************************************************

get_future_forecast_data_prepared <- function(.data){
    
    output <- .data %>% 
        group_by(.model_id, .model_desc, .key, .index) %>% 
        summarise_by_time(
            .date_var   = .index, 
            .by         = "day", 
            .value      = sum(.value), 
            total_sold  = sum(total_sold)
        ) %>% 
        ungroup() 
    
    return(output)
    
}


# ******************************************************************************
# TEST FORECAST DATA PREP ----
# ******************************************************************************

get_test_forecast_data_prepared <- function(.data){
    
    output <- .data %>% 
        group_by(.model_id, .model_desc, .key, .index) %>% 
        summarise_by_time(
            .date.var  = date,
            .by        = "day",
            .value     = sum(.value),
            total_sold = sum(total_sold)
        ) %>% 
        ungroup() %>% 
        filter(.index >= as.Date("2020-07-01"))
    
    return(output)
    
    
    
}

# sample_test_forecast_tbl %>% get_test_forecast_data_prepared()

# ******************************************************************************
# FORECAST PLOT ----
# ******************************************************************************

# .data <- sample_future_forecast_tbl %>% get_future_forecast_data_prepared() %>% 
#     filter(.key == "prediction") 

get_forecast_plot <- function(.data){
    
    p <- .data %>% 
        plot_modeltime_forecast(.title = "")
    
    return(p)
    
    
}

# sample_test_forecast_tbl %>% 
#     get_test_forecast_data_prepared() %>% 
#     get_forecast_plot()


# ******************************************************************************
# FORECAST DATA (DT TABLE) ----
# ******************************************************************************
get_future_forecast_data_prepared_dt <- function(.data){
    
    .data %>% 
        get_future_forecast_data_prepared() %>% 
        filter(.key == "prediction") %>% 
        dplyr::select(.index, .key, .model_desc, .value) %>% 
        rename(date = .index, key = .key, forecast_model = .model_desc,
               forecast_value = .value) %>% 
        mutate(forecast_value = forecast_value %>% scales::comma(accuracy = 1))
    
    
}


# ******************************************************************************
# TEST FORECAST ACCURACY DATA (DT TABLE) ----
# ******************************************************************************
get_test_forecast_metrics_dt <- function(.data){
    
    .data %>% 
        group_by(.model_desc) %>% 
        summarise_by_time(
            .date_var = date,
            .by       = "day",
            .value    = sum(.value, na.rm = TRUE),
            total_sold = sum(total_sold, na.rm = TRUE)
        ) %>% 
        ungroup() %>% 
        pivot_wider(names_from = .model_desc, values_from = .value) %>% 
        dplyr::select(-total_sold) %>% 
        filter(!is.na(XGBOOST_Tuned)) %>% 
        pivot_longer(cols = XGBOOST_Tuned) %>% 
        summarize_accuracy_metrics(
            truth = ACTUAL, 
            estimate = value,
            metric_set = default_forecast_accuracy_metric_set()
        ) %>% 
        ungroup() %>% 
        mutate(across(.cols = mae:rsq, round, 3)) %>% 
        setNames(names(.) %>% str_to_upper())
}

   
        

