## Vermont County-Level Poverty Forecasting (STAT 4840)

This project analyzes and forecasts county-level poverty trends in Vermont using data from the
U.S. Census Bureau’s Small Area Income and Poverty Estimates (SAIPE), county SNAP participation,
and IRS poverty exemption data.

County-level datasets were cleaned, reshaped, and merged using FIPS codes to create a
longitudinal time series spanning over 25 years. Multiple modeling approaches were evaluated,
including log-linear regression, exponential smoothing (ETS), and ARIMA models.

Models were compared using rolling-origin cross-validation and RMSE to identify a robust
statewide forecasting approach. Five-year forecasts were generated for each county, and counties
with the highest projected percentage increases in poverty were identified and visualized using
geospatial maps.

Main code: code/vermont_poverty_forecasting.R
Report: output/luke_liles_stat4840_final_project.pdf

**Tools:** R, tidyverse, time series analysis, ARIMA, ETS, ggplot2  
**Course:** STAT 4840 – Time Series 
