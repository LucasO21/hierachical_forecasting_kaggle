# KAGGLE TPS SEPT 2022 ----
# HIERACHICAL FORECASTING ----
# EDA & EXPERIMENT SCRIPT ----
#  **** ----

# ******************************************************************************
# SETUP ----
# ******************************************************************************

# * Working Dir ----
setwd(here::here("R"))


# * Libraries ----

# ** Core ----
library(tidyverse)
library(janitor)
library(lubridate)
library(timetk)

# ** Modeling ----
library(xgboost)
library(treesnip)
library(lightgbm)
library(tidymodels)
library(modeltime)
library(modeltime.resample)
library(rules)

# ** Parallel Processing ----
library(tictoc)
library(doFuture)
library(parallel)


# ******************************************************************************
# LOAD DATA ----
# ******************************************************************************

# * Train Data ----
train_raw_tbl <- vroom::vroom("../Data/train.csv") %>% 
    clean_names() %>% 
    as_tibble() %>% 
    filter(date >= as.Date("2018-01-01"))
    

train_raw_tbl %>% glimpse()
train_raw_tbl %>% distinct(country)
train_raw_tbl %>% distinct(store)
train_raw_tbl %>% distinct(date) %>% View()
min(train_raw_tbl$date)
max(train_raw_tbl$date)
train_raw_tbl %>% sapply(function(x) sum(is.na(x)))


# ******************************************************************************
# EXPLORATORY DATA ANALYSIS ----
# ******************************************************************************

# * Summary Stats ----
summary(train_raw_tbl$num_sold)

# Total Sales by Categories ----

# * Total Sales by Country ----
train_raw_tbl %>%
    group_by(country) %>%
    summarise(total_num_sold = sum(num_sold)) %>%
    ungroup() %>%
    arrange(desc(total_num_sold))

# * Total Sales by Store ----
train_raw_tbl %>% 
    group_by(store) %>% 
    summarise(total_num_sold = sum(num_sold)) %>% 
    ungroup() %>% 
    arrange(desc(total_num_sold))

# * Total Sales by Weekday ----
train_raw_tbl %>%
    mutate(wday = wday(date, label = TRUE)) %>%
    mutate_at(.vars = c("country", "wday"), .funs = as.factor) %>%
    group_by(wday) %>%
    summarise(total_num_sold = sum(num_sold)) %>%
    ungroup() %>% 
    arrange(desc(total_num_sold))

# * Total Sales by Product ----
train_raw_tbl %>% 
    group_by(product) %>% 
    summarise(total_num_sold = sum(num_sold)) %>% 
    ungroup() %>% 
    arrange(desc(total_num_sold))


# Sales Trends by Categories ----

# Sales Trend by Country ----
train_raw_tbl %>% 
    group_by(country) %>% 
    summarize_by_time(date, "day", num_sold = sum(num_sold)) %>% 
    plot_time_series(date, num_sold, .facet_ncol = 2, .title = "Total Sold (Daily)")

# Sales Trend by Store ----
train_raw_tbl %>% 
    #filter(country == "Poland") %>% 
    group_by(store) %>% 
    summarize_by_time(date, "day", num_sold = sum(num_sold)) %>% 
    plot_time_series(date, num_sold, .facet_ncol = 1, .title = "Total Sold (Daily)")


# Trend Diagnostics ----

# * Anomaly Diagnostics ----
train_raw_tbl %>% 
    group_by(store) %>% 
    summarize_by_time(date, "day", num_sold = sum(num_sold)) %>% 
    plot_anomaly_diagnostics(date, num_sold, .facet_ncol = 1)


# ACF & PACF ----
train_raw_tbl %>% 
    #filter(country == "Germany") %>% 
    summarize_by_time(date, "day", num_sold = sum(num_sold)) %>% 
    plot_acf_diagnostics(date, num_sold)










