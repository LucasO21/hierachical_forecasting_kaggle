# KAGGLE TPS SEPT 2022 ----
# HIERACHICAL FORECASTING VERSION 2 ----
# FORECASTING AT THE PRODUCT LEVEL ----
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
train_raw_tbl <- vroom::vroom("../data/train.csv") %>% 
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
    mutate(id = paste0(country_code, store_code, product_code))


# ******************************************************************************
# DATA PREP FOR MODELING ----
# ******************************************************************************

# * Summarize by Time ----
daily_hierachical_tbl <- train_prep_tbl %>% 
    group_by(id) %>% 
    summarize_by_time(date, "day", total_sold = sum(num_sold)) %>% 
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
    group_by(id) %>% 
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
    group_by(id) %>% 
    mutate(total_sold = ts_clean_vec(total_sold, period = 3)) %>% 
    ungroup() 

train_cleaned_tbl %>% 
    filter(str_detect(id, "BELKGRKAT")) %>% 
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


# ******************************************************************************
# MODELTIME TABLE ----
# ******************************************************************************
modeltime_table_fit <- modeltime_table(
    wflw_fit_xgboost,
    wflw_fit_ranger
)

calibration_fit_tbl <- modeltime_table_fit %>% 
    modeltime_calibrate(test_tbl) %>% 
    modeltime_accuracy() %>% 
    arrange((rmse))


# ******************************************************************************
# HYPER PARAMETER TUNING ----
# ******************************************************************************

# * K FOLD Resamples ----
set.seed(123)
resamples_kfold <- train_cleaned_tbl %>% vfold_cv(v = 5)

resamples_kfold %>%
    tk_time_series_cv_plan() %>% 
    plot_time_series_cv_plan(date, total_sold, .facet_ncol = 2)

# * Parallel Processing ----
registerDoFuture()
n_cores <- 4
plan(strategy = cluster, workers = makeCluster(n_cores))


# * Xgboost Tune ----

# ** Spec ----
model_spec_xgboost_tune <- boost_tree(
    mode           = "regression",
    mtry           = tune(),
    trees          = tune(),
    min_n          = tune(),
    tree_depth     = tune(),
    learn_rate     = tune(),
    loss_reduction = tune()
) %>% 
    set_engine("xgboost")

# ** Workflow ----
wflw_spec_xgboost_tune <- workflow() %>% 
    add_model(model_spec_xgboost_tune) %>% 
    add_recipe(recipe_spec %>% step_rm(date))

# ** Tuning ----
tic()
set.seed(123)
tune_results_xgboost <- wflw_spec_xgboost_tune %>% 
    tune_grid(
        resamples  = resamples_kfold,
        # param_info = parameters(wflw_spec_xgboost_tune) %>% 
        #   update(learn_rate = learn_rate(c(0.001, 0.400), trans = NULL)),
        grid       = 10,
        control    = control_grid(verbose = TRUE, allow_par = TRUE)
        
    )
toc()

# ** Results ----
tune_results_xgboost %>% show_best("rmse", n = 5)

# ** Finalize ----
wflw_fit_xgboost_tuned <- wflw_spec_xgboost_tune %>% 
    finalize_workflow(select_best(tune_results_xgboost, "rmse")) %>% 
    fit(train_cleaned_tbl)
    

# ******************************************************************************
# ACCURACY CHECK (TUNED MODELS) ----
# ******************************************************************************

# * Modeltime Table ----
modeltime_table_tuned_tbl <- modeltime_table(wflw_fit_xgboost_tuned)%>% 
    mutate(.model_desc = paste0(.model_desc, "_Tuned"))

# * Calibrate ----
calibration_tuned_tbl <- modeltime_table_tuned_tbl %>% 
    modeltime_calibrate(test_tbl)

# * Accuracy ----
accuracy_tuned_tbl <- calibration_tuned_tbl %>% 
    modeltime_accuracy(test_tbl) %>% 
    arrange(rmse)

# * Visualize Test Forecast ----
test_forecast_tuned_tbl <- calibration_tuned_tbl %>% 
    modeltime_forecast(
        new_data    = test_tbl,
        actual_data = data_prepared_tbl,
        keep_data   = TRUE
    ) 
    
# ** Test Forecast by Country ----
test_tbl %>% distinct(id)

test_forecast_tuned_tbl %>% 
    filter(id %in% c("BELKGMKAT", "BELKGMKFK", "BELKGMKGS", "BELKGMKRB")) %>% 
    group_by(id) %>% 
    plot_modeltime_forecast(
        .facet_ncol          = 2,
        .conf_interval_alpha = 0.1,
        .interactive         = TRUE
    )


# ******************************************************************************
# REFIT MODELS TO FULL DATA ----
# ******************************************************************************

model_refit_xgboost_tbl <- modeltime_table_tuned_tbl %>% 
    modeltime_refit(
        data_prepared_tbl %>% 
            mutate(total_sold = ts_clean_vec(total_sold, period = 3)) %>% 
            ungroup()    
        )


# ******************************************************************************
# CODE FOR FUTURE FORECAST ----
# ******************************************************************************
future_forecast_tbl <- model_refit_xgboost_tbl %>% 
    modeltime_forecast(
        new_data    = future_data_tbl,
        actual_data = test_tbl,
        keep_data   = TRUE 
    )
    
future_forecast_tbl %>% glimpse()

future_data_tbl %>% glimpse()

# ******************************************************************************
# SAVE ARTIFACTS ----
# ******************************************************************************
artifacts_list_version2 <- list(
    
    # Data
    data = list(
        data_prepared_tbl      = data_prepared_tbl,
        future_data_tbl        = future_data_tbl,
        train_tbl              = train_tbl,
        test_tbl               = test_tbl,
        test_data_forecast_tbl = test_forecast_tuned_tbl,
        future_forecast_tbl    = future_forecast_tbl,
        accuracy_tuned_tbl     = accuracy_tuned_tbl,
        country_code_tbl       = country_code_tbl,
        store_code_tbl         = store_code_tbl,
        product_code_tbl       = product_code_tbl
    ),
    
    # Recipes
    recipe = list(recipe = recipe_spec),
    
    # Tuned Models
    models = list(
        xgboost = model_refit_xgboost_tbl
    )
)

artifacts_list_version2 %>% write_rds("../artifacts//artifacts_list_version2.rds")
