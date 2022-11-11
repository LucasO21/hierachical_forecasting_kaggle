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



# ******************************************************************************
# DATA PREP FOR MODELING ----
# ******************************************************************************

# Summarize by Time (All Hierarchy) ----
daily_hierachical_tbl <- train_raw_tbl %>% 
    group_by(country, store, product) %>% 
    summarize_by_time(date, "day", num_sold = sum(num_sold)) %>% 
    ungroup() %>% 
    select(date, everything(.))

# Params ----
FORECAST_TIMEFRAME <- "day"

FORECAST_HORIZON  <- 90

# Full Data ----
full_data_tbl <- train_raw_tbl %>% 
    
    # fix data types
    mutate_if(is.character, as.factor) %>% 
    
    # add hierarchical aggregations
    select(-row_id) %>% 
    add_column(all_countries_id = "all_countries", .before = 1) %>% 
    pivot_longer(
        cols      = c(all_countries_id, country, store, product),
        names_to  = "hierachy",
        values_to = "identifier"
    ) %>% 
    group_by(hierachy, identifier, date) %>% 
    summarise(num_sold = sum(num_sold, na.rm = TRUE)) %>% 
    ungroup() %>% 
    
    # transformations
    mutate(num_sold = log(num_sold)) %>% 
    
    # Apply Time Series Feature Engineering
    group_by(hierachy, identifier) %>% 
    pad_by_time(date, .by = FORECAST_TIMEFRAME, .pad_value = 0) %>% 
    
    # Extend Into Future
    future_frame(date, .length_out = FORECAST_HORIZON, .bind_data = TRUE) %>% 
    
    # Add Time Series Features
    tk_augment_fourier(date, .periods = c(7, 14, 21, 28)) %>% 
    tk_augment_lags(num_sold, .lags = FORECAST_HORIZON) %>% 
    tk_augment_slidify(
        .value   = num_sold_lag90,
        .f       = ~ mean(.x, na.rm = TRUE),
        .period  = c(7, 14, 21, 28),
        .partial = TRUE,
        .align   = "center"
    ) %>% 
    ungroup() %>% 
    rowid_to_column(var = "row_id")

full_data_tbl %>% glimpse()

skimr::skim(full_data_tbl)

# * Data Prepared ----
data_prepared_tbl <- full_data_tbl %>% 
    filter(! is.na(num_sold)) %>% 
    drop_na()

# * Future Data ----
future_data_tbl <- full_data_tbl %>% 
    filter(is.na(num_sold))


# ******************************************************************************
# TIME SPLIT ----
# ******************************************************************************
splits <- data_prepared_tbl %>% 
    time_series_split(date, assess = FORECAST_HORIZON, cumulative = TRUE)

train_tbl <- training(splits)
test_tbl  <- testing(splits)

splits %>% 
    tk_time_series_cv_plan() %>% 
    plot_time_series_cv_plan(date, num_sold)


# ******************************************************************************
# CLEAN DATA (REMOVE OUTLIERS) ----
# ******************************************************************************

train_cleaned_tbl <- train_tbl %>% 
    group_by(hierachy, identifier) %>% 
    mutate(num_sold = ts_clean_vec(num_sold, period = 3)) %>% 
    ungroup() 

train_cleaned_tbl %>% 
    filter(hierachy == "country" & identifier == "France") %>% 
    plot_anomaly_diagnostics(date, num_sold)



# ******************************************************************************
# RECIPE ----
# ******************************************************************************

recipe_spec <- recipe(num_sold ~., data = train_cleaned_tbl) %>% 
    step_rm(row_id) %>% 
    step_timeseries_signature(date) %>% 
    step_rm(matches("(.iso)|(.xts)|(hour)|(minute)(second)(am.pm)")) %>% 
    step_dummy(all_nominal(), one_hot = TRUE) %>% 
    step_normalize(date_index.num, date_year)

recipe_spec %>% prep() %>% juice() %>% glimpse()

# ******************************************************************************
# MODELING ----
# ******************************************************************************

# * Xgboost ----
wflw_fit_xgboost <- workflow() %>% 
    add_model(spec = boost_tree() %>% set_mode("regression") %>% set_engine("xgboost")) %>% 
    add_recipe(recipe_spec %>% step_rm(date)) %>% 
    fit(train_cleaned_tbl)

# * Random Forest ----
wflw_fit_ranger <- workflow() %>% 
    add_model(rand_forest() %>% set_mode("regression") %>% set_engine("ranger")) %>% 
    add_recipe(recipe_spec %>% step_rm(date)) %>% 
    fit(train_cleaned_tbl)

# * MARS ----
wflw_fit_mars <- workflow() %>% 
    add_model(mars() %>% set_mode("regression") %>% set_engine("earth")) %>% 
    add_recipe(recipe_spec %>% step_rm(date)) %>% 
    fit(train_cleaned_tbl)

# * Cubist Model ----
wflw_fit_cubsist <- workflow() %>% 
    add_model(cubist_rules("regression") %>% set_engine("Cubist")) %>% 
    add_recipe(recipe_spec %>% step_rm(date)) %>% 
    fit(train_cleaned_tbl)


# ******************************************************************************
# MODELTIME TABLE ----
# ******************************************************************************
modeltime_table_fit <- modeltime_table(
    wflw_fit_xgboost,
    wflw_fit_ranger,
    wflw_fit_mars,
    wflw_fit_cubsist
)

calibration_fit_tbl <- modeltime_table_fit %>% 
    modeltime_calibrate(test_tbl) %>% 
    modeltime_accuracy() %>% 
    arrange((rmse))

test_tbl %>% 
    summarise(
        mean   = mean(num_sold),
        median = median(num_sold),
        sd     = sd(num_sold)
    )










