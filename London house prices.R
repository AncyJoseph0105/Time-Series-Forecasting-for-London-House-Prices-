library(readxl)
library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(knitr)
library(kableExtra)
library(forecast)
library(urca)
library(zoo)  
library(ggpubr) 



# Read the data from the first sheet
file_path <- "C:/Users/Ancy/Time series/code/TIme Series analysis  of London house Price.xlsx"
data <- read_excel(file_path)
head(data)

# 1. Data Preparation and Descriptive Analysis

## a. cleaning the data ##

# Convert Date column to Date type
data$Date <- ym(data$Date)

# Check the structure to ensure Date conversion is correct
str(data)

# Data Cleaning: Remove rows with missing values
data <- na.omit(data)

# Check for complex numbers in each column
is_complex <- sapply(data, function(x) any(is.complex(x)))
print(is_complex)

## b. summary statistics ##
summary(data)
head(data)

## c. Plotting the data ##
# Create ggplot object
p <- ggplot(data, aes(x = Date)) +
  geom_line(aes(y = Detached, color = "Detached")) +
  geom_line(aes(y = `Semi Detached`, color = "Semi Detached")) +
  geom_line(aes(y = Terraced, color = "Terraced")) +
  geom_line(aes(y = Flat, color = "Flat")) +
  labs(
    title = "London House Prices Over Time",
    x = "Date",
    y = "Price",
    color = "House Type"
  ) +
  theme_minimal()+
  scale_y_continuous(labels = scales::comma)

# Convert ggplot to plotly for interactivity
p <- ggplotly(p)

# Save the interactive plot as an HTML file
html_file <- "London_House_Prices_Plotly.html"
htmlwidgets::saveWidget(p, file = html_file, selfcontained = TRUE)

p


# 2. Stationarity Testing

## Unit Root Tests ##
#Augmented Dickey-Fuller (ADF) : to check for stationarity
adf_detached <- ur.df(data$Detached, type = "trend", lags = 4)
adf_semi_detached <- ur.df(data$`Semi Detached`, type = "trend", lags = 4)
adf_terraced <- ur.df(data$Terraced, type = "trend", lags = 4)
adf_flat <- ur.df(data$Flat, type = "trend", lags = 4)

# Print the ADF test results
print("ADF Test for Detached:")
summary(adf_detached)

print("ADF Test for Semi Detached:")
summary(adf_semi_detached)

print("ADF Test for Terraced:")
summary(adf_terraced)

print("ADF Test for Flat:")
summary(adf_flat)

# Performing differencing on Flats because it was the only house type that was non stationary 

# Perform differencing on the Flat series
flat_diff <- diff(data$Flat, differences = 1)

# Create a new data frame that excludes the first row
data_diff <- data[-1, ]

# Assign the differenced series to the new data frame
data_diff$Flat_diff <- flat_diff

# Re-run the ADF test on the differenced Flat series to check for stationarity
adf_flat_diff <- ur.df(data_diff$Flat_diff, type = "trend", lags = 4)

# Print the ADF test results for the differenced Flat series
print("ADF Test for Differenced Flat:")
summary(adf_flat_diff)

# 3. Trend Analysis

p_original <- ggplot(data, aes(x = Date)) +
  geom_line(aes(y = Detached, color = "Detached")) +
  geom_line(aes(y = `Semi Detached`, color = "Semi Detached")) +
  geom_line(aes(y = Terraced, color = "Terraced")) +
  geom_line(aes(y = Flat, color = "Flat")) +
  labs(
    title = "Original London House Prices Over Time",
    x = "Date",
    y = "Price",
    color = "House Type"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)

# Convert to plotly object for interactivity
p_original <- ggplotly(p_original)

# Display the interactive plot
p_original

# Define the window size for the moving average
window_size <- 12

# Compute the moving averages
data_ma <- data %>%
  mutate(
    Detached_MA = rollmean(Detached, k = window_size, fill = NA),
    Semi_Detached_MA = rollmean(`Semi Detached`, k = window_size, fill = NA),
    Terraced_MA = rollmean(Terraced, k = window_size, fill = NA),
    Flat_MA = rollmean(Flat, k = window_size, fill = NA)
  )
p_ma <- ggplot(data_ma, aes(x = Date)) +
  geom_line(aes(y = Detached_MA, color = "Detached")) +
  geom_line(aes(y = Semi_Detached_MA, color = "Semi Detached")) +
  geom_line(aes(y = Terraced_MA, color = "Terraced")) +
  geom_line(aes(y = Flat_MA, color = "Flat")) +
  labs(
    title = "Moving Average (12 months) of London House Prices Over Time",
    x = "Date",
    y = "Price",
    color = "House Type"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)

# Convert to plotly object for interactivity
p_ma <- ggplotly(p_ma)

# Display the interactive plot
p_ma

# Print the plot
print(p_ma)

# detrend  
# Detrend each type of house prices using moving average
data$Detached_detrended <- data$Detached - rollmean(data$Detached, k = window_size, fill = NA)
data$Semi_Detached_detrended <- data$'Semi Detached' - rollmean(data$'Semi Detached', k = window_size, fill = NA)
data$Terraced_detrended <- data$Terraced - rollmean(data$Terraced, k = window_size, fill = NA)
data$Flat_detrended <- data$Flat - rollmean(data$Flat, k = window_size, fill = NA)

p_detrended <- ggplot() +
  geom_line(data = data, aes(x = Date, y = Detached_detrended, color = "Detached")) +
  geom_line(data = data, aes(x = Date, y = Semi_Detached_detrended, color = "Semi Detached")) +
  geom_line(data = data, aes(x = Date, y = Terraced_detrended, color = "Terraced")) +
  geom_line(data = data, aes(x = Date, y = Flat_detrended, color = "Flat")) +
  labs(
    title = "Detrended House Prices Over Time",
    x = "Date",
    y = "Detrended Price",
    color = "House Type"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)

# Convert ggplot object to plotly for interactivity
p_detrended <- ggplotly(p_detrended)

# Display the interactive plot
p_detrended

# 4. Seasonal Decomposition

decompose_and_plot <- function(series, series_name) {
  ts_data <- ts(series, frequency = 12)  # Convert the series to a time series object with monthly frequency
  decomposition <- stl(ts_data, s.window = "periodic")  # Perform STL decomposition
  
  # Plot the decomposition components
  p_decomp <- autoplot(decomposition) +
    labs(
      title = paste("STL Decomposition of", series_name),
      x = "Date",
      y = "Price"
    ) +
    theme_minimal()+
    scale_y_continuous(labels = scales::comma)
  return(p_decomp)
}

# Decompose and plot for each house type
p_decomp_detached <- decompose_and_plot(data$Detached, "Detached")
p_decomp_semi_detached <- decompose_and_plot(data$`Semi Detached`, "Semi Detached")
p_decomp_terraced <- decompose_and_plot(data$Terraced, "Terraced")
p_decomp_flat <- decompose_and_plot(data$Flat, "Flat")

# Print the decomposition plots
print(p_decomp_detached)
print(p_decomp_semi_detached)
print(p_decomp_terraced)
print(p_decomp_flat)

# Convert the ggplot objects to plotly for interactivity
p_decomp_detached_plotly <- ggplotly(p_decomp_detached)
p_decomp_semi_detached_plotly <- ggplotly(p_decomp_semi_detached)
p_decomp_terraced_plotly <- ggplotly(p_decomp_terraced)
p_decomp_flat_plotly <- ggplotly(p_decomp_flat)

# Display the interactive plots
p_decomp_detached_plotly
p_decomp_semi_detached_plotly
p_decomp_terraced_plotly
p_decomp_flat_plotly

# 5. Correlation Analysis
library(forecast)
library(ggplot2)
library(gridExtra)

# Function to plot ACF and PACF
plot_acf_pacf <- function(series, series_name) {
  # Compute ACF and PACF
  acf_series <- Acf(series, plot = FALSE)
  pacf_series <- Pacf(series, plot = FALSE)
  
  # Create ACF plot
  p_acf <- autoplot(acf_series) +
    labs(
      title = paste("ACF of", series_name),
      x = "Lag",
      y = "Autocorrelation"
    ) +
    theme_minimal()
  
  # Create PACF plot
  p_pacf <- autoplot(pacf_series) +
    labs(
      title = paste("PACF of", series_name),
      x = "Lag",
      y = "Partial Autocorrelation"
    ) +
    theme_minimal()
  
  return(list(acf = p_acf, pacf = p_pacf))
}

# Plot ACF and PACF for each house type
plots_detached <- plot_acf_pacf(data$Detached, "Detached")
plots_semi_detached <- plot_acf_pacf(data$`Semi Detached`, "Semi Detached")
plots_terraced <- plot_acf_pacf(data$Terraced, "Terraced")
plots_flat <- plot_acf_pacf(data$Flat, "Flat")

# Display the plots
grid.arrange(plots_detached$acf, plots_detached$pacf, ncol = 2)
grid.arrange(plots_semi_detached$acf, plots_semi_detached$pacf, ncol = 2)
grid.arrange(plots_terraced$acf, plots_terraced$pacf, ncol = 2)
grid.arrange(plots_flat$acf, plots_flat$pacf, ncol = 2)


#6. Anomaly detection 
# outlier detection 
# Create boxplots for each house type
p_boxplot_detached <- ggplot(data, aes(x = "", y = Detached)) +
  geom_boxplot(fill = "skyblue", color = "blue") +
  labs(
    title = "Boxplot of Detached House Prices",
    y = "Price"
  ) +
  theme_minimal()+
  scale_y_continuous(labels = scales::comma)

p_boxplot_semi_detached <- ggplot(data, aes(x = "", y = `Semi Detached`)) +
  geom_boxplot(fill = "skyblue", color = "blue") +
  labs(
    title = "Boxplot of Semi Detached House Prices",
    y = "Price"
  ) +
  theme_minimal()+
  scale_y_continuous(labels = scales::comma)

p_boxplot_terraced <- ggplot(data, aes(x = "", y = Terraced)) +
  geom_boxplot(fill = "skyblue", color = "blue") +
  labs(
    title = "Boxplot of Terraced House Prices",
    y = "Price"
  ) +
  theme_minimal()+
  scale_y_continuous(labels = scales::comma)

p_boxplot_flat <- ggplot(data, aes(x = "", y = Flat)) +
  geom_boxplot(fill = "skyblue", color = "blue") +
  labs(
    title = "Boxplot of Flat House Prices",
    y = "Price"
  ) +
  theme_minimal()+
  scale_y_continuous(labels = scales::comma)

# Arrange boxplots in a grid
grid.arrange(p_boxplot_detached, p_boxplot_semi_detached, p_boxplot_terraced, p_boxplot_flat, ncol = 2)

library(changepoint)

# Function to detect change points using PELT (Pruned Exact Linear Time) method
detect_change_points <- function(series, series_name) {
  ts_data <- ts(series, frequency = 12)  # Convert the series to a time series object with monthly frequency
  
  # Perform change point detection using PELT method
  cpt <- cpt.mean(ts_data, method = "PELT")
  
  # Plot change points
  plot(cpt, type = "l", ylab = "Series", main = paste("Change Points in", series_name))
  
  # Add y-axis formatting
  scale_y_continuous(labels = scales::comma)
  
  # Return the changepoint object
  return(cpt)
}

# Detect change points for each house type
cpt_detached <- detect_change_points(data$Detached, "Detached")
cpt_semi_detached <- detect_change_points(data$`Semi Detached`, "Semi Detached")
cpt_terraced <- detect_change_points(data$Terraced, "Terraced")
cpt_flat <- detect_change_points(data$Flat, "Flat")

# 7. Forecasting
# Fit ARIMA model for the differenced Flat series using auto.arima to find the best model
arima_flat <- auto.arima(data_diff$Flat_diff, seasonal = FALSE)

# Print the summary of the ARIMA model
summary(arima_flat)

# Forecasting the next 12 months
forecast_flat <- forecast(arima_flat, h = 12)

# Plot the forecast
plot(forecast_flat, main = "Forecast for Differenced Flat Prices", xlab = "Time", ylab = "Differenced Price")

# Get the last observed value of the Flat series
last_flat_value <- tail(data$Flat, n = 1)

# Convert the differenced forecast to the original scale
original_forecast_flat <- forecast_flat$mean + last_flat_value

# Create a data frame for the forecasted values
forecast_dates <- seq(tail(data$Date, n = 1) + months(1), by = "month", length.out = 12)
forecast_df <- data.frame(Date = forecast_dates, Forecast = original_forecast_flat)

# Plot the original data along with the forecast
p_forecast <- ggplot(data, aes(x = Date)) +
  geom_line(aes(y = Flat, color = "Observed")) +
  geom_line(data = forecast_df, aes(y = Forecast, color = "Forecast")) +
  labs(
    title = "London House Prices (Flat) Forecast",
    x = "Date",
    y = "Price",
    color = "Legend"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)

# Convert ggplot to plotly for interactivity
p_forecast <- ggplotly(p_forecast)

# Save the interactive forecast plot as an HTML file
forecast_html_file <- "London_House_Prices_Flat_Forecast_Plotly.html"
htmlwidgets::saveWidget(p_forecast, file = forecast_html_file, selfcontained = TRUE)

p_forecast

# other house types that are already stationary 
# Fit ARIMA model and forecast for Detached houses
arima_detached <- auto.arima(data$Detached, seasonal = FALSE)
summary(arima_detached)
forecast_detached <- forecast(arima_detached, h = 12)

# Fit ARIMA model and forecast for Semi Detached houses
arima_semi_detached <- auto.arima(data$`Semi Detached`, seasonal = FALSE)
summary(arima_semi_detached)
forecast_semi_detached <- forecast(arima_semi_detached, h = 12)

# Fit ARIMA model and forecast for Terraced houses
arima_terraced <- auto.arima(data$Terraced, seasonal = FALSE)
summary(arima_terraced)
forecast_terraced <- forecast(arima_terraced, h = 12)

# Create data frames for the forecasted values
forecast_dates <- seq(tail(data$Date, n = 1) + months(1), by = "month", length.out = 12)

forecast_df_detached <- data.frame(Date = forecast_dates, Forecast = forecast_detached$mean)
forecast_df_semi_detached <- data.frame(Date = forecast_dates, Forecast = forecast_semi_detached$mean)
forecast_df_terraced <- data.frame(Date = forecast_dates, Forecast = forecast_terraced$mean)

# Plotting the original data along with the forecast for Detached houses
p_forecast_detached <- ggplot(data, aes(x = Date)) +
  geom_line(aes(y = Detached, color = "Observed")) +
  geom_line(data = forecast_df_detached, aes(y = Forecast, color = "Forecast")) +
  labs(
    title = "London House Prices (Detached) Forecast",
    x = "Date",
    y = "Price",
    color = "Legend"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)

p_forecast_detached <- ggplotly(p_forecast_detached)
forecast_html_file_detached <- "London_House_Prices_Detached_Forecast_Plotly.html"
htmlwidgets::saveWidget(p_forecast_detached, file = forecast_html_file_detached, selfcontained = TRUE)

# Plotting the original data along with the forecast for Semi Detached houses
p_forecast_semi_detached <- ggplot(data, aes(x = Date)) +
  geom_line(aes(y = `Semi Detached`, color = "Observed")) +
  geom_line(data = forecast_df_semi_detached, aes(y = Forecast, color = "Forecast")) +
  labs(
    title = "London House Prices (Semi Detached) Forecast",
    x = "Date",
    y = "Price",
    color = "Legend"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)

p_forecast_semi_detached <- ggplotly(p_forecast_semi_detached)
forecast_html_file_semi_detached <- "London_House_Prices_Semi_Detached_Forecast_Plotly.html"
htmlwidgets::saveWidget(p_forecast_semi_detached, file = forecast_html_file_semi_detached, selfcontained = TRUE)

# Plotting the original data along with the forecast for Terraced houses
p_forecast_terraced <- ggplot(data, aes(x = Date)) +
  geom_line(aes(y = Terraced, color = "Observed")) +
  geom_line(data = forecast_df_terraced, aes(y = Forecast, color = "Forecast")) +
  labs(
    title = "London House Prices (Terraced) Forecast",
    x = "Date",
    y = "Price",
    color = "Legend"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)

p_forecast_terraced <- ggplotly(p_forecast_terraced)
forecast_html_file_terraced <- "London_House_Prices_Terraced_Forecast_Plotly.html"
htmlwidgets::saveWidget(p_forecast_terraced, file = forecast_html_file_terraced, selfcontained = TRUE)

# Display the plots
p_forecast_detached
p_forecast_semi_detached
p_forecast_terraced

# Print the forecasted values for the next 12 months
print("Detached")
print(forecast_df_detached)

print("Semi Detached")
print(forecast_df_semi_detached)

print("Terraced")
print(forecast_df_terraced)

print("Flat")
print(forecast_df)
# Combine the Date column from each forecast data frame with their Forecast values
forecast_combined <- data.frame(
  Date = forecast_df_detached$Date,
  Detached = forecast_df_detached$Forecast,
  Semi_Detached = forecast_df_semi_detached$Forecast,
  Terraced = forecast_df_terraced$Forecast,
  Flat = forecast_df$Forecast
)

# Print the combined forecast in the desired format
print(forecast_combined)

##############################################################################
#8. The Ljung-Box test
# Residual Diagnostics
checkresiduals(arima_detached)
checkresiduals(arima_semi_detached)
checkresiduals(arima_terraced)
checkresiduals(arima_flat)
############################################################################
# SARIMA
# Fit SARIMA model for differenced Flat houses
sarima_flat_diff <- auto.arima(data_diff$Flat_diff, seasonal = TRUE)
summary(sarima_flat_diff)

# Forecasting the next 12 months
forecast_flat_sarima_diff <- forecast(sarima_flat_diff, h = 12)

# Get the last observed value of the Flat series
last_flat_value <- tail(data$Flat, n = 1)

# Convert the SARIMA forecast to the original scale
original_forecast_flat_sarima_diff <- forecast_flat_sarima_diff$mean + last_flat_value

# Create a data frame for the forecasted values
forecast_dates <- seq(tail(data$Date, n = 1) + months(1), by = "month", length.out = 12)
forecast_df_flat_sarima_diff <- data.frame(Date = forecast_dates, Forecast = original_forecast_flat_sarima_diff)

# Plotting the original data along with the SARIMA forecast for differenced Flat houses
p_forecast_flat_sarima_diff <- ggplot(data, aes(x = Date)) +
  geom_line(aes(y = Flat, color = "Observed")) +
  geom_line(data = forecast_df_flat_sarima_diff, aes(y = Forecast, color = "Forecast")) +
  labs(
    title = "London House Prices (Flat) Forecast (SARIMA)",
    x = "Date",
    y = "Price",
    color = "Legend"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)

# Convert ggplot to plotly for interactivity
p_forecast_flat_sarima_diff <- ggplotly(p_forecast_flat_sarima_diff)

# Save the interactive forecast plot as an HTML file
forecast_html_file_flat_sarima_diff <- "London_House_Prices_Flat_Forecast_SARIMA_Differenced_Plotly.html"
htmlwidgets::saveWidget(p_forecast_flat_sarima_diff, file = forecast_html_file_flat_sarima_diff, selfcontained = TRUE)

# Display the plot
p_forecast_flat_sarima_diff
######################THE ABOVE MODEL WAS SAME AS ARIMA ########################################

# Fit SARIMA model for Terraced houses
sarima_terraced <- auto.arima(data$Terraced, seasonal = TRUE)
summary(sarima_terraced)

# Forecasting the next 12 months
forecast_terraced_sarima <- forecast(sarima_terraced, h = 12)

# Create data frame for the forecasted values
forecast_df_terraced_sarima <- data.frame(Date = forecast_dates, Forecast = forecast_terraced_sarima$mean)

# Plotting the original data along with the SARIMA forecast for Terraced houses
p_forecast_terraced_sarima <- ggplot(data, aes(x = Date)) +
  geom_line(aes(y = Terraced, color = "Observed")) +
  geom_line(data = forecast_df_terraced_sarima, aes(y = Forecast, color = "Forecast")) +
  labs(
    title = "London House Prices (Terraced) Forecast (SARIMA)",
    x = "Date",
    y = "Price",
    color = "Legend"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)

# Convert ggplot to plotly for interactivity
p_forecast_terraced_sarima <- ggplotly(p_forecast_terraced_sarima)

# Save the interactive forecast plot as an HTML file
forecast_html_file_terraced_sarima <- "London_House_Prices_Terraced_Forecast_SARIMA_Plotly.html"
htmlwidgets::saveWidget(p_forecast_terraced_sarima, file = forecast_html_file_terraced_sarima, selfcontained = TRUE)

# Display the plot
p_forecast_terraced_sarima
######################THE ABOVE MODEL WAS SAME AS ARIMA ########################################

# Fit SARIMA model for Semi Detached houses
sarima_semi_detached <- auto.arima(data$`Semi Detached`, seasonal = TRUE)
summary(sarima_semi_detached)

# Forecasting the next 12 months
forecast_semi_detached_sarima <- forecast(sarima_semi_detached, h = 12)

# Create data frame for the forecasted values
forecast_df_semi_detached_sarima <- data.frame(Date = forecast_dates, Forecast = forecast_semi_detached_sarima$mean)

# Plotting the original data along with the SARIMA forecast for Semi Detached houses
p_forecast_semi_detached_sarima <- ggplot(data, aes(x = Date)) +
  geom_line(aes(y = `Semi Detached`, color = "Observed")) +
  geom_line(data = forecast_df_semi_detached_sarima, aes(y = Forecast, color = "Forecast")) +
  labs(
    title = "London House Prices (Semi Detached) Forecast (SARIMA)",
    x = "Date",
    y = "Price",
    color = "Legend"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)

# Convert ggplot to plotly for interactivity
p_forecast_semi_detached_sarima <- ggplotly(p_forecast_semi_detached_sarima)

# Save the interactive forecast plot as an HTML file
forecast_html_file_semi_detached_sarima <- "London_House_Prices_Semi_Detached_Forecast_SARIMA_Plotly.html"
htmlwidgets::saveWidget(p_forecast_semi_detached_sarima, file = forecast_html_file_semi_detached_sarima, selfcontained = TRUE)

# Display the plot
p_forecast_semi_detached_sarima
######################THE ABOVE MODEL WAS SAME AS ARIMA ########################################

# Fit SARIMA model for Detached houses
sarima_detached <- auto.arima(data$Detached, seasonal = TRUE)
summary(sarima_detached)

# Forecasting the next 12 months
forecast_detached_sarima <- forecast(sarima_detached, h = 12)

# Create data frame for the forecasted values
forecast_df_detached_sarima <- data.frame(Date = forecast_dates, Forecast = forecast_detached_sarima$mean)

# Plotting the original data along with the SARIMA forecast for Detached houses
p_forecast_detached_sarima <- ggplot(data, aes(x = Date)) +
  geom_line(aes(y = Detached, color = "Observed")) +
  geom_line(data = forecast_df_detached_sarima, aes(y = Forecast, color = "Forecast")) +
  labs(
    title = "London House Prices (Detached) Forecast (SARIMA)",
    x = "Date",
    y = "Price",
    color = "Legend"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)

# Convert ggplot to plotly for interactivity
p_forecast_detached_sarima <- ggplotly(p_forecast_detached_sarima)

# Save the interactive forecast plot as an HTML file
forecast_html_file_detached_sarima <- "London_House_Prices_Detached_Forecast_SARIMA_Plotly.html"
htmlwidgets::saveWidget(p_forecast_detached_sarima, file = forecast_html_file_detached_sarima, selfcontained = TRUE)

# Display the plot
p_forecast_detached_sarima
######################THE ABOVE MODEL WAS SAME AS ARIMA ########################################

