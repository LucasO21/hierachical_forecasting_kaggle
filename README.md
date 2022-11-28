# Hierarchical Forecasting - Proof of Concept Shiny App
================================================================================

### Project Goal
Create an end to end hierarchical forecasting system for a hypothetical Kaggle book store. 
Forecasts need to be created for four books, sold in 2 stores, in 6 countries.

### Data
Data used in this project comes from [Kaggle's TPS series for September 2022](https://www.kaggle.com/competitions/tabular-playground-series-sep-2022/overview). 

### Analysis Approach
Using the [Modeltime](https://business-science.github.io/modeltime/) framework, an xgboost model was used
to create a 90 day forecast at the product level. Due to computational time, only one model was explored. 
Additionally, a typical hierarchical forecast in this case will have a separate forecasts at the product, store and country level. Once again to ease computational time, a forecast was created at the product level, then aggregated at the store and country level. See reproducible code for building hierarchical forecast [here](https://github.com/LucasO21/hierachical_forecasting_kaggle/blob/master/R/hierachical_forecast_v2.R).

### Deployment
This area was the main focus of the project. A simple shiny was was created that lets a user explore forecasts by product, store, country etc. The user is also able to see how the model performed on test data, as well as download the 90 day future forecast to a csv file. You can interact with the app [here](https://lucas-okwudishu.shinyapps.io/kaggle_product_forecasting_app/?_ga=2.226453978.1546251635.1669672788-1940270335.1668539253). 


![Screen Shot 2022-11-26 at 8 42 14 AM](https://user-images.githubusercontent.com/62886078/204091923-6dbe31b5-0eab-4996-abf0-34518622da10.png)
