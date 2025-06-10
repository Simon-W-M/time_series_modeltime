library(tidyverse)

download.file("https://files.digital.nhs.uk/A0/A7EED3/Appointments_GP_Daily_CSV_Feb_25.zip",
              destfile = "data.zip")

zipped_csv_names <- grep('\\.csv$',
                         unzip('data.zip',
                               list = TRUE)$Name,
                         ignore.case = TRUE,
                         value=TRUE)

zipped_csv_names <- zipped_csv_names[startsWith(zipped_csv_names, 'SUB')]

unzip('data.zip', files = zipped_csv_names)

df <- zipped_csv_names |>
  lapply(read_csv) |>
  bind_rows()

view(head(df,100))

library(janitor)

df <- clean_names(df)

# create a daily total of appts
df <- df |>
  mutate(appointment_date = as.Date(appointment_date, '%d%b%Y')) |>
  summarise(tot_appt = sum(count_of_appointments),
            .by = appointment_date)|>
  arrange(appointment_date)

# quick visual of the data
df |> ggplot() +
  aes(x = appointment_date, y = tot_appt) +
  geom_point()

# what is with those weird low numbers?
df |>
  filter(tot_appt < 10000) |>
  arrange (tot_appt) 


library(modeltime)
library(tidymodels)
library(timetk)
library(lubridate)


# modeltime also has a plotting function
# this does the same but much quicker and prettier

df |>
  plot_time_series(appointment_date, 
                   tot_appt)

# create a test train split
# we hold back the last 3 months in order to evaluate 
# the models' predictive powers
splits <- time_series_split(
  df,
  assess     = "3 months",
  cumulative = TRUE
)

# we can also do a quick lot that shows our
# test train splits
splits |>
  tk_time_series_cv_plan() |>
  plot_time_series_cv_plan(appointment_date, 
                           tot_appt)

# create some forecast models 

# method 1
# auto arima

# ARIMA, or AutoRegressive Integrated Moving Average, is a set of models that 
# explains a time series using its own previous values given by the lags 
# (AutoRegressive) and lagged errors (Moving Average) while considering 
# stationarity corrected by differencing (oppossite of Integration.) 
#
# In other words, ARIMA assumes that the time series is described by 
# autocorrelations in the data rather than trends and seasonality. 
# In these context, we define trends and seasonality as the following:
#   
# Trend: A time series has a trend if there is a overlying long term increase 
#        or decrease in the data, which is not necessarily linear.
# 
# Seasonality: A time series data has seasonality when it is affected by 
#              seasonal factors such as the time of the year or the day of 
#              the week. The seasonality of data is apparent as there is a 
#              fixed frequency of pattern occurence.
# 
# 

model_arima <- arima_reg() |>
  set_engine("auto_arima") |>
  fit(tot_appt ~ appointment_date, training(splits))

# as we run the model it quickly picks up the data follws a weekly seasonality

model_arima

# model 2

# prophet (facebook prophet)

# Prophet is a procedure for forecasting time series data based on an additive
# model where non-linear trends are fit with yearly, weekly, and daily 
# seasonality, plus holiday effects. It works best with time series that 
# have strong seasonal effects and several seasons of historical data. 
# Prophet is robust to missing data and shifts in the trend, and typically 
# handles outliers well.


model_prophet <- prophet_reg(
  seasonality_weekly = TRUE
) |>
  set_engine("prophet") |>
  fit(tot_appt ~ appointment_date, training(splits))

model_prophet

# model 3

# GLM (basic machine learning)

# A classical linear models assume that the regression variables should 
# have an additive relationship with each other, converts the date
# into a number and identifies the more direct and linear relationship

model_glmnet <- linear_reg(penalty = 0.01) |>
  set_engine("glmnet") |>
  fit(
    tot_appt ~ wday(appointment_date, label = TRUE)
    + month(appointment_date, label = TRUE)
    + as.numeric(appointment_date),
    training(splits)
  )

model_glmnet

# evaluate models 

# modeltime - create a table to compare models
model_tbl <- modeltime_table(
  model_arima,
  model_prophet,
  model_glmnet
)

# modeltime claibration 
calib_tbl <- model_tbl |>
  modeltime_calibrate(testing(splits))

# compare accuracy - get stats on accuracy
calib_tbl |> 
  modeltime_accuracy()

# You want all of these to be as low as possible
#
# MAE - Mean absolute error
# MAPE - Mean absolute percentage error
# MASE - Mean absolute scaled error
# SMAPE - Symmetric mean absolute percentage error
# RMSE - Root mean squared error
#
# you want this one to be high to show good fit of data
# 
# RSQ - R-squared, rsq()


# plop accuracy into table 
calib_tbl |> 
  modeltime_accuracy() |>
  table_modeltime_accuracy()

# plot test data
calib_tbl |>
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = df
  ) %>%
  plot_modeltime_forecast()

# forecast the future
future_forecast_tbl <- calib_tbl |>
  modeltime_refit(df) |>
  modeltime_forecast(
    h           = "12 months",
    actual_data = df
  )

# plot the forecast
future_forecast_tbl |>
  plot_modeltime_forecast()



# maybe look at forecasting the weekly data?

# load in the data again
df <- zipped_csv_names |>
  lapply(read_csv) |>
  bind_rows()

view(head(df,100))


df <- clean_names(df)

df <- df |>
  mutate(appointment_date = as.Date(appointment_date, '%d%b%Y'),
         appointment_date = floor_date(appointment_date, 'week')) |>
  summarise(tot_appt = sum(count_of_appointments),
            .by = appointment_date)|>
  arrange(appointment_date)


df |>
  plot_time_series(appointment_date, 
                   tot_appt)

# create a test train split
# we hold back the last 3 months in order to evaluate 
# the models' predictive powers
splits <- time_series_split(
  df,
  assess     = "3 months",
  cumulative = TRUE
)

# we can also do a quick lot that shows our
# test train splits
splits |>
  tk_time_series_cv_plan() |>
  plot_time_series_cv_plan(appointment_date, 
                           tot_appt)

# create some forecast models 

# method 1
# auto arima

model_arima <- arima_reg() %>%
  set_engine("auto_arima") %>%
  fit(tot_appt ~ appointment_date, training(splits))

# as we run the model it quickly picks up the data follws a weekly seasonality

model_arima

# model 2

# prophet (facebook prophet)

model_prophet <- prophet_reg(
  seasonality_weekly = TRUE
) |>
  set_engine("prophet") %>%
  fit(tot_appt ~ appointment_date, training(splits))

model_prophet

# model 3

# GLM (basic machine learning)

model_glmnet <- linear_reg(penalty = 0.01) |>
  set_engine("glmnet") |>
  fit(
    tot_appt ~ wday(appointment_date, label = TRUE)
    + month(appointment_date, label = TRUE)
    + as.numeric(appointment_date),
    training(splits)
  )

model_glmnet

# evalutate models 

# modeltime - create a table to compare models
model_tbl <- modeltime_table(
  model_arima,
  model_prophet,
  model_glmnet
)

# modeltime claibration 
calib_tbl <- model_tbl |>
  modeltime_calibrate(testing(splits))

# compare accuracy - get stats on accuracy
calib_tbl |> 
  modeltime_accuracy()

# plop accuracy into table 
calib_tbl |> 
  modeltime_accuracy() |>
  table_modeltime_accuracy()

# plot test data
calib_tbl |>
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = df
  ) %>%
  plot_modeltime_forecast()

# forecast the future
future_forecast_tbl <- calib_tbl |>
  modeltime_refit(df) |>
  modeltime_forecast(
    h           = "12 months",
    actual_data = df
  )

# plot the forecast
future_forecast_tbl |>
  plot_modeltime_forecast()
