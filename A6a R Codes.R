# Set a CRAN mirror
options(repos = c(CRAN = "https://cran.rstudio.com/"))

# Install necessary packages if not already installed
required_packages <- c("quantmod", "ggplot2", "forecast", "tseries", "dplyr", "tibble", "keras", "rpart", "randomForest", "reticulate")
installed_packages <- installed.packages()
for (pkg in required_packages) {
  if (!pkg %in% installed_packages[, "Package"]) {
    install.packages(pkg)
  }
}

install.packages("reticulate")
# Load reticulate
library(reticulate)

# Load necessary libraries
library(quantmod)
library(ggplot2)
library(forecast)
library(tseries)
library(dplyr)
library(tibble)
library(keras)
library(rpart)
library(randomForest)
library(reticulate)

# 1. Data Fetching and Preprocessing
ticker <- "MARICO.NS"
data <- getSymbols(ticker, src = "yahoo", from = "2021-04-01", to = "2024-03-31", auto.assign = FALSE)
data <- as.data.frame(data)
data <- rownames_to_column(data, "Date")

# Select the target variable and clean the data
df <- data %>% select(Date, MARICO.NS.Adjusted) %>% rename(Adj_Close = MARICO.NS.Adjusted)

# Check for missing values
print(sum(is.na(df$Adj_Close)))

# Plot the data
ggplot(df, aes(x = as.Date(Date), y = Adj_Close)) +
  geom_line() +
  labs(title = "MARICO.NS Adj Close Price", x = "Date", y = "Adj Close Price")

# 2. Time Series Decomposition
decomposed <- stl(ts(df$Adj_Close, frequency = 12), s.window = "periodic")

# Plot the decomposed components
plot(decomposed)

# 3. Holt-Winters Model
# Convert to time series
df_ts <- ts(df$Adj_Close, frequency = 12, start = c(2021, as.numeric(format(as.Date(df$Date[1]), "%j")) / 365))

# Train-test split for monthly data
train_data <- head(df, floor(0.8 * nrow(df)))
test_data <- tail(df, floor(0.2 * nrow(df)))

# Convert the train_data to a time series object
train_ts <- ts(train_data$Adj_Close, frequency = 12, start = c(2021, as.numeric(format(as.Date(train_data$Date[1]), "%j")) / 365))

# Fit the Holt-Winters model on the training data
holt_winters_model <- HoltWinters(train_ts)

# Forecast
holt_winters_forecast <- forecast(holt_winters_model, h = 12)

# Plot the forecast along with the observed values
autoplot(holt_winters_forecast) +
  autolayer(train_ts, series = "Observed") +
  labs(title = "Holt-Winters Forecast", x = "Date", y = "Close Price")


# 4. ARIMA Model
# ARIMA model
arima_model <- auto.arima(train_ts)

# Forecast
forecast <- forecast(arima_model, h = length(test_data$Adj_Close))

# Plot the forecast
autoplot(forecast) +
  autolayer(train_ts, series = "Original Data") +
  labs(title = "Auto ARIMA Forecasting", x = "Date", y = "Value")

# Metrics
y_pred <- forecast$mean
test_data_vals <- ts(test_data$Adj_Close, frequency = 12, start = end(train_ts))

rmse <- sqrt(mean((y_pred - test_data_vals)^2))
mae <- mean(abs(y_pred - test_data_vals))
r2 <- 1 - sum((test_data_vals - y_pred)^2) / sum((test_data_vals - mean(test_data_vals))^2)

cat("RMSE:", rmse, "\n")
cat("MAE:", mae, "\n")
cat("R-squared:", r2, "\n")

#Couldnt do the LSTM Model

#6
# Fetch data from Yahoo Finance
ticker <- "MARICO.NS"
data <- getSymbols(ticker, src = "yahoo", from = "2021-04-01", to = "2024-03-31", auto.assign = FALSE)
data <- as.data.frame(data)
data <- rownames_to_column(data, "Date")

# Preprocess data
data <- data %>% select(-MARICO.NS.Adjusted, -Date)  # Exclude Date and Adjusted columns
features <- data %>% select(-MARICO.NS.Close)  # Exclude Close column
target <- data$MARICO.NS.Close

# Train-test split
set.seed(42)
train_indices <- sample(seq_len(nrow(data)), size = 0.8 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Decision Tree
dt_model <- rpart(MARICO.NS.Close ~ ., data = train_data)
y_pred_dt <- predict(dt_model, test_data)

# Random Forest
rf_model <- randomForest(MARICO.NS.Close ~ ., data = train_data)
y_pred_rf <- predict(rf_model, test_data)

# Metrics for Decision Tree
y_test <- test_data$MARICO.NS.Close
rmse_dt <- sqrt(mean((y_pred_dt - y_test)^2))
mae_dt <- mean(abs(y_pred_dt - y_test))
r2_dt <- 1 - sum((y_test - y_pred_dt)^2) / sum((y_test - mean(y_test))^2)

# Metrics for Random Forest
rmse_rf <- sqrt(mean((y_pred_rf - y_test)^2))
mae_rf <- mean(abs(y_pred_rf - y_test))
r2_rf <- 1 - sum((y_test - y_pred_rf)^2) / sum((y_test - mean(y_test))^2)

cat("Decision Tree - RMSE:", rmse_dt, "\n")
cat("Decision Tree - MAE:", mae_dt, "\n")
cat("Decision Tree - R-squared:", r2_dt, "\n")

cat("Random Forest - RMSE:", rmse_rf, "\n")
cat("Random Forest - MAE:", mae_rf, "\n")
cat("Random Forest - R-squared:", r2_rf, "\n")

# Plot the predictions vs true values for Decision Tree
ggplot() +
  geom_line(aes(x = 1:length(y_test), y = y_test, color = "True Values")) +
  geom_line(aes(x = 1:length(y_pred_dt), y = y_pred_dt, color = "Decision Tree Predictions")) +
  labs(title = "Decision Tree: Predictions vs True Values", x = "Index", y = "Close Price") +
  scale_color_manual(values = c("True Values" = "blue", "Decision Tree Predictions" = "red"))

# Plot the predictions vs true values for Random Forest
ggplot() +
  geom_line(aes(x = 1:length(y_test), y = y_test, color = "True Values")) +
  geom_line(aes(x = 1:length(y_pred_rf), y = y_pred_rf, color = "Random Forest Predictions")) +
  labs(title = "Random Forest: Predictions vs True Values", x = "Index", y = "Close Price") +
  scale_color_manual(values = c("True Values" = "blue", "Random Forest Predictions" = "green"))

# Plot both Decision Tree and Random Forest predictions together
ggplot() +
  geom_line(aes(x = 1:length(y_test), y = y_test, color = "True Values")) +
  geom_line(aes(x = 1:length(y_pred_dt), y = y_pred_dt, color = "Decision Tree Predictions")) +
  geom_line(aes(x = 1:length(y_pred_rf), y = y_pred_rf, color = "Random Forest Predictions")) +
  labs(title = "Decision Tree & Random Forest: Predictions vs True Values", x = "Index", y = "Close Price") +
  scale_color_manual(values = c("True Values" = "blue", "Decision Tree Predictions" = "red", "Random Forest Predictions" = "green"))