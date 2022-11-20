# KAGGLE TPS SEPT 2022 ----
# HIERACHICAL FORECASTING VERSION 3 ----
# EXPERIMENT SCRIPT ----
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
# library(treesnip)
library(lightgbm)
library(tidymodels)
library(modeltime)
library(modeltime.resample)
# library(rules)

# ** Parallel Processing ----
library(tictoc)
library(doFuture)
library(parallel)


# ******************************************************************************
# LOAD DATA ----
# ******************************************************************************

# * Data ----
train_raw_tbl <- vroom::vroom("../Data/train.csv") %>% 
    clean_names() %>% 
    as_tibble() %>% 
    filter(date >= as.Date("2019-01-01"))

# ******************************************************************************
# DATA FORMATTING ----
# ******************************************************************************

# * Country Code ----
country_code_tbl <- train_raw_tbl %>% 
    distinct(country) %>% 
    mutate(country_code = c("BEL", "FRA", "GER", "ITA", "POL", "ESP"))

# * Store Code ----
store_code_tbl <- train_raw_tbl %>% 
    distinct(store) %>% 
    mutate(store_code = c("KGM", "KGR"))

# * Product Code ----
product_code_tbl <- train_raw_tbl %>% 
    distinct(product) %>% 
    mutate(product_code = c("KAT", "KGS", "KRB", "KFK"))

train_prep_tbl <- train_raw_tbl %>%
    left_join(country_code_tbl) %>% 
    left_join(store_code_tbl) %>% 
    left_join(product_code_tbl) %>% 
    select(-country, -store, -product) %>% 
    mutate(total_sold = num_sold) %>% 
    select(row_id, date, country_code, store_code, product_code, total_sold)

# ******************************************************************************
# DATA PREP FOR MODELING ----
# ******************************************************************************

# * Summarize by Time ----
daily_hierachical_tbl <- train_prep_tbl %>% 
    group_by(country_code, store_code, product_code) %>% 
    summarize_by_time(date, "day", total_sold = sum(total_sold)) %>% 
    ungroup()

# * Params ----
FORECAST_TIMEFRAME <- "day"
FORECAST_HORIZON   <- 90

# * Full Data ----
full_data_tbl <- daily_hierachical_tbl %>% 
    mutate_if(is.character, as.factor) %>% 
    
    # Transformations
    mutate(total_sold = log(total_sold)) %>% 
    
    # Apply Time Series Engineering
    group_by(country_code, store_code, product_code) %>% 
    pad_by_time(date, .by = FORECAST_TIMEFRAME, .pad_value = 0) %>% 
    
    # Extend Into Future
    future_frame(date, .length_out = FORECAST_HORIZON, .bind_data = TRUE) %>% 
    
    # Add Time Series Features
    tk_augment_fourier(date, .periods = c(7, 14, 21, 28)) %>% 
    tk_augment_lags(total_sold, .lags = FORECAST_HORIZON) %>% 
    tk_augment_slidify(
        .value   = total_sold_lag90,
        .f       = ~ mean(.x, na.rm = TRUE),
        .period  = c(7, 14, 21, 28),
        .partial = TRUE,
        .align   = "center"
    ) %>% 
    ungroup() %>% 
    rowid_to_column(var = "row_id")


# * Data Prepared ----
data_prepared_tbl <- full_data_tbl %>% 
    filter(! is.na(total_sold)) %>% 
    drop_na()

# * Future Data ----
future_data_tbl <- full_data_tbl %>% 
    filter(is.na(total_sold))

# ******************************************************************************
# TIME SPLIT ----
# ******************************************************************************
splits <- data_prepared_tbl %>% 
    time_series_split(date, assess = FORECAST_HORIZON, cumulative = TRUE)

train_tbl <- training(splits)
test_tbl  <- testing(splits)

splits %>% 
    tk_time_series_cv_plan() %>% 
    plot_time_series_cv_plan(date, total_sold)


# ******************************************************************************
# CLEAN DATA (REMOVE OUTLIERS) ----
# ******************************************************************************

train_cleaned_tbl <- train_tbl %>% 
    group_by(country_code, store_code, product_code) %>% 
    mutate(total_sold = ts_clean_vec(total_sold, period = 3)) %>% 
    ungroup() 

train_cleaned_tbl %>% 
    filter(country_code == "BEL" & store_code == "KGM" & product_code == "KAT") %>% 
    plot_anomaly_diagnostics(date, total_sold)


# ******************************************************************************
# RECIPE ----
# ******************************************************************************

recipe_spec <- recipe(total_sold ~., data = train_cleaned_tbl) %>% 
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
