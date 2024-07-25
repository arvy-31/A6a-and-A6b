# Set CRAN mirror
options(repos = c(CRAN = "https://cran.r-project.org"))


install.packages("readxl")
install.packages("rugarch")
install.packages("FinTS")
install.packages("ggplot2")
install.packages("gridExtra")

# Load the necessary packages
library(readxl)
library(rugarch)
library(FinTS)
library(ggplot2)
library(gridExtra)

# Set the path to the file
file_path <- "D:\\SCMA632__FIRE632\\Stats\\Assignment\\A6\\A6b\\Marico_Financial_Data.xlsx"

# Read the data
data <- read_excel(file_path, sheet = 1)

# Display the first few rows to ensure data is loaded correctly
print(head(data))

# Use the 'Close' column for the time series data
price_data <- data$Close

# Check if price_data contains any NA values
if (any(is.na(price_data))) {
  stop("Price data contains NA values")
}

# Ensure there are no missing values
price_data <- na.omit(price_data)

# Convert to time series object if needed
price_ts <- ts(price_data)

# Ensure the time series object has observations
if (length(price_ts) <= 1) {
  stop("'ts' object must have one or more observations")
}

# Perform ARCH test
arch_test <- FinTS::ArchTest(price_ts, lags=12)
print(arch_test)

# Specify the GARCH(1,1) model
spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                   mean.model = list(armaOrder = c(0, 0)))

# Fit the model
fit <- ugarchfit(spec = spec, data = price_ts)

# Display the fit summary
summary(fit)

# Forecast the volatility
forecast <- ugarchforecast(fit, n.ahead = 90) # Assuming 30 days in a month

# Extract the volatility forecast
vol_forecast <- forecast@forecast$sigmaFor

# Print the forecasted volatility
print(vol_forecast)

# Convert the 'Date' column to Date format if it isn't already
data$Date <- as.Date(data$Date)

# 1. Plot the original time series data
p1 <- ggplot(data, aes(x = Date, y = Close)) +
  geom_line(color = "blue") +
  labs(title = "Original Time Series Data", x = "Date", y = "Close Price")

# 2. Plot the fitted GARCH model's volatility
sigma_fitted <- fit@fit$sigma

# Debug print to check data alignment
print(length(sigma_fitted))
print(length(data$Date))

# Ensure the data alignment
data_fitted <- data.frame(Date = data$Date[1:length(sigma_fitted)], Volatility = sigma_fitted)
print(head(data_fitted)) # Debug print to ensure data_fitted is correct

p2 <- ggplot(data_fitted, aes(x = Date, y = Volatility)) +
  geom_line(color = "red") +
  labs(title = "Fitted GARCH Model's Volatility", x = "Date", y = "Volatility")

# 3. Plot the forecasted volatility
forecast_dates <- seq.Date(from = max(data$Date), by = "days", length.out = 90)
forecast_data <- data.frame(Date = forecast_dates, Volatility = vol_forecast)
colnames(forecast_data) <- c("Date", "Volatility")
print(head(forecast_data)) # Debug print to ensure forecast_data is correct

p3 <- ggplot(forecast_data, aes(x = Date, y = Volatility)) +
  geom_line(color = "green") +
  labs(title = "Forecasted Volatility", x = "Date", y = "Volatility")

# Arrange the plots in a grid
grid.arrange(p1, p2, p3, nrow = 3)
