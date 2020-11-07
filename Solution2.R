# Import required library
library(package = data.table)
library(package = forecast)
library(package = ggplot2)
library(package = zoo)
library(package = tidyquant)

# Read in csv file
big5 <- fread(file = "big_five_stocks.csv", stringsAsFactors = T)

# Create new column to store average of open, close, high and low
big5$average = (big5$open + big5$close + big5$high + big5$low) / 4

# Check data
summary(object = big5)

# Plot multiple time series data
ggplot(data = big5, mapping = aes(x = V1, y = open, color = name)) + geom_line() + theme_grey(base_size = 15) + 
  ggtitle(label = "Open price against time") + xlab(label = "Year") + ylab(label = "Open Price")
ggplot(data = big5, mapping = aes(x = V1, y = close, color = name)) + geom_line() + theme_grey(base_size = 15) + 
  ggtitle(label = "Close price against time") + xlab(label = "Year") + ylab(label = "Close Price")
ggplot(data = big5, mapping = aes(x = V1, y = high, color = name)) + geom_line() + theme_grey(base_size = 15) + 
  ggtitle(label = "High price against time") + xlab(label = "Year") + ylab(label = "High Price")
ggplot(data = big5, mapping = aes(x = V1, y = low, color = name)) + geom_line() + theme_grey(base_size = 15) + 
  ggtitle(label = "Low price against time") + xlab(label = "Year") + ylab(label = "Low Price")
ggplot(data = big5, mapping = aes(x = V1, y = volume, color = name)) + geom_line() + theme_grey(base_size = 15) + 
  ggtitle(label = "Volume against time") + xlab(label = "Year") + ylab(label = "Volume")
ggplot(data = big5, mapping = aes(x = V1, y = average, color = name)) + geom_line() + theme_grey(base_size = 15) + 
  ggtitle(label = "Average against time") + xlab(label = "Year") + ylab(label = "Volume")

# Open, close, high, low in a single graph
ggplot(data = big5, mapping = aes(x = V1, y = average, color = name)) + geom_barchart(mapping = aes(open = open, high = high, low = low, close = close)) + theme_grey(base_size = 15) + 
  ggtitle(label = "Financial Barchart") + xlab(label = "Year") + ylab(label = "Value")

# Create new column to store moving average
big5$mov_avg = 0

# Get moving average and store it
big5[name == "AAPL"]$mov_avg = coredata(rollmean(zoo(x = big5[name == "AAPL"]$average, order.by = big5[name == "AAPL"]$V1), 121, fill = list(NA, NULL, NA)))
big5[name == "AMZN"]$mov_avg = coredata(rollmean(zoo(x = big5[name == "AMZN"]$average, order.by = big5[name == "AMZN"]$V1), 121, fill = list(NA, NULL, NA)))
big5[name == "FB"]$mov_avg = coredata(rollmean(zoo(x = big5[name == "FB"]$average, order.by = big5[name == "FB"]$V1), 121, fill = list(NA, NULL, NA)))
big5[name == "GOOGL"]$mov_avg = coredata(rollmean(zoo(x = big5[name == "GOOGL"]$average, order.by = big5[name == "GOOGL"]$V1), 121, fill = list(NA, NULL, NA)))
big5[name == "MSFT"]$mov_avg = coredata(rollmean(zoo(x = big5[name == "MSFT"]$average, order.by = big5[name == "MSFT"]$V1), 121, fill = list(NA, NULL, NA)))
big5[name == "^IXIC"]$mov_avg = coredata(rollmean(zoo(x = big5[name == "^IXIC"]$average, order.by = big5[name == "^IXIC"]$V1), 121, fill = list(NA, NULL, NA)))

# Plot average against moving average
ggplot(data = big5[name == "AAPL"], mapping = aes(x = V1)) + geom_line(mapping = aes(y = average), color = "red") + geom_line(mapping = aes(y = mov_avg), color = "blue") + theme_grey(base_size = 15) + 
  ggtitle(label = "Apple roll mean") + xlab(label = "Year") + ylab(label = "Value")
ggplot(data = big5[name == "AMZN"], mapping = aes(x = V1)) + geom_line(mapping = aes(y = average), color = "red") + geom_line(mapping = aes(y = mov_avg), color = "blue") + theme_grey(base_size = 15) + 
  ggtitle(label = "Amazon roll mean") + xlab(label = "Year") + ylab(label = "Value")
ggplot(data = big5[name == "FB"], mapping = aes(x = V1)) + geom_line(mapping = aes(y = average), color = "red") + geom_line(mapping = aes(y = mov_avg), color = "blue") + theme_grey(base_size = 15) + 
  ggtitle(label = "Facebook roll mean") + xlab(label = "Year") + ylab(label = "Value")
ggplot(data = big5[name == "GOOGL"], mapping = aes(x = V1)) + geom_line(mapping = aes(y = average), color = "red") + geom_line(mapping = aes(y = mov_avg), color = "blue") + theme_grey(base_size = 15) + 
  ggtitle(label = "Google roll mean") + xlab(label = "Year") + ylab(label = "Value")
ggplot(data = big5[name == "MSFT"], mapping = aes(x = V1)) + geom_line(mapping = aes(y = average), color = "red") + geom_line(mapping = aes(y = mov_avg), color = "blue") + theme_grey(base_size = 15) + 
  ggtitle(label = "Microsoft roll mean") + xlab(label = "Year") + ylab(label = "Value")
ggplot(data = big5[name == "^IXIC"], mapping = aes(x = V1)) + geom_line(mapping = aes(y = average), color = "red") + geom_line(mapping = aes(y = mov_avg), color = "blue") + theme_grey(base_size = 15) + 
  ggtitle(label = "NASDAQ roll mean") + xlab(label = "Year") + ylab(label = "Value")

# Plot average against volume, and found no correlation. As such, it was not used as a variable
linreg <- lm(average ~ volume, data = big5[name == "AAPL"])
plot(x = big5[name == "AAPL"]$volume, y = big5[name == "AAPL"]$average, main = "Apple's average against volume", xlab = "Volume", ylab = "Average")
abline(linreg, col = "red")
linreg <- lm(average ~ volume, data = big5[name == "AMZN"])
plot(x = big5[name == "AMZN"]$volume, y = big5[name == "AMZN"]$average, main = "Amazon's average against volume", xlab = "Volume", ylab = "Average")
abline(linreg, col = "red")
linreg <- lm(average ~ volume, data = big5[name == "FB"])
plot(x = big5[name == "FB"]$volume, y = big5[name == "FB"]$average, main = "Facebook's average against volume", xlab = "Volume", ylab = "Average")
abline(linreg, col = "red")
linreg <- lm(average ~ volume, data = big5[name == "GOOGL"])
plot(x = big5[name == "GOOGL"]$volume, y = big5[name == "GOOGL"]$average, main = "Google's average against volume", xlab = "Volume", ylab = "Average")
abline(linreg, col = "red")
linreg <- lm(average ~ volume, data = big5[name == "MSFT"])
plot(x = big5[name == "MSFT"]$volume, y = big5[name == "MSFT"]$average, main = "Microsoft's average against volume", xlab = "Volume", ylab = "Average")
abline(linreg, col = "red")

# Do ARIMA forecast for Apple
ts_apple <- ts(big5[name == "AAPL"])[, 9]
train_apple <- window(x = ts_apple, end = 0.95 * length(x = ts_apple))
test_apple <- window(x = ts_apple, start = 0.95 * length(x = ts_apple) + 1)
optimal_apple = auto.arima(y = train_apple)
forecast_apple <- forecast(object = optimal_apple, h = length(test_apple))
autoplot(object = ts_apple) + autolayer(object = forecast_apple, alpha = 0.3) + theme_grey(base_size = 15) + 
  ggtitle(label = "Apple actual vs predicted values") + ylab(label = "Value")

# Repeat for the rest
# Amazon
ts_amazon <- ts(big5[name == "AMZN"])[, 9]
train_amazon <- window(x = ts_amazon, end = 0.91 * length(x = ts_amazon))
test_amazon <- window(x = ts_amazon, start = 0.91 * length(x = ts_amazon) + 1)
optimal_amazon = auto.arima(y = train_amazon)
forecast_amazon <- forecast(object = optimal_amazon, h = length(test_amazon))
autoplot(object = ts_amazon) + autolayer(object = forecast_amazon, alpha = 0.3) + theme_grey(base_size = 15) + 
  ggtitle(label = "Amazon actual vs predicted values") + ylab(label = "Value")

# Facebook
ts_facebook <- ts(big5[name == "FB"])[, 9]
train_facebook <- window(x = ts_facebook, end = 0.75 * length(x = ts_facebook))
test_facebook <- window(x = ts_facebook, start = 0.75 * length(x = ts_facebook) + 1)
optimal_facebook = auto.arima(y = train_facebook)
forecast_facebook <- forecast(object = optimal_facebook, h = length(test_facebook))
autoplot(object = ts_facebook) + autolayer(object = forecast_facebook, alpha = 0.3) + theme_grey(base_size = 15) + 
  ggtitle(label = "Facebook actual vs predicted values") + ylab(label = "Value")

# Google
ts_google <- ts(big5[name == "GOOGL"])[, 9]
train_google <- window(x = ts_google, end = 0.875 * length(x = ts_google))
test_google <- window(x = ts_google, start = 0.875 * length(x = ts_google) + 1)
optimal_google = auto.arima(y = train_google)
forecast_google <- forecast(object = optimal_google, h = length(test_google))
autoplot(object = ts_google) + autolayer(object = forecast_google, alpha = 0.3) + theme_grey(base_size = 15) + 
  ggtitle(label = "Google actual vs predicted values") + ylab(label = "Value")


# Microsoft
ts_microsoft <- ts(big5[name == "MSFT"])[, 9]
train_microsoft <- window(x = ts_microsoft, end = 0.94 * length(x = ts_microsoft))
test_microsoft <- window(x = ts_microsoft, start = 0.94 * length(x = ts_microsoft) + 1)
optimal_microsoft = auto.arima(y = train_microsoft)
forecast_microsoft <- forecast(object = optimal_microsoft, h = length(test_microsoft))
autoplot(object = ts_microsoft) + autolayer(object = forecast_microsoft, alpha = 0.3) + theme_grey(base_size = 15) + 
  ggtitle(label = "Microsoft actual vs predicted values") + ylab(label = "Value")

# NASDAQ
ts_nasdaq <- ts(big5[name == "^IXIC"])[, 9]
train_nasdaq <- window(x = ts_nasdaq, end = 0.96 * length(x = ts_nasdaq))
test_nasdaq <- window(x = ts_nasdaq, start = 0.96 * length(x = ts_nasdaq) + 1)
optimal_nasdaq = auto.arima(y = train_nasdaq)
forecast_nasdaq <- forecast(object = optimal_nasdaq, h = length(test_nasdaq))
autoplot(object = ts_nasdaq) + autolayer(object = forecast_nasdaq, alpha = 0.3) + theme_grey(base_size = 15) + 
  ggtitle(label = "NASDAQ actual vs predicted values") + ylab(label = "Value")
