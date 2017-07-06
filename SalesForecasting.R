#########################################################################
#                                                                       #
# Purpose:                                                              #
#	Try out a variety of forecasting methods in order to forecast sales #
#                                                                       #
#########################################################################




library(dplyr)
library(ggvis)
library(forecast)
library(zoo)
salesData <- read.csv("SalesData.csv")
salesData.ts <- ts(salesData$Sales, start = c(2010,28), end = c(2015, 38), freq = 52)


############# Useful exploratory information and plots ##############
summary(salesData.ts)

par(mfrow = c(3, 1))
plot(salesData.ts, xlab = "Time", ylab = "Sales", bty = "l")
seasonplot(salesData.ts, ylab="Sales", xlab="Period", main="Seasonal Plot", year.labels=TRUE)
monthplot(salesData.ts, ylab="Sales", xlab="Period", main="Seasonal Deviation Plot")


par(mfrow = c(2, 1))
lag.plot(salesData.ts, lags=16)
tsdisplay(salesData.ts)


############### Differencing Models ###################

par(mfrow = c(2, 2))
plot(salesData.ts, ylab = "Sales", xlab = "Time", bty = "l", main = "Sales Time Series", lty = 1)


diff.once1.ts <- diff(salesData.ts, lag = 1)
plot(diff.once1.ts, ylab = "Sales (Lag 1 Difference)", xlab = "Time", bty = "l", main = "Difference Plot with Lag 1", lty = 1)

diff.once12.ts <- diff(salesData.ts, lag = 26)
plot(diff.once12.ts, ylab = "Sales (Lag 12 Difference)", xlab = "Time", bty = "l", main = "Difference Plot with Lag 26", lty = 1)


diff.twice.ts <- diff(diff(salesData.ts, lag = 26), lag = 1)

plot(diff.twice.ts, ylab = "Sales (Twice-Differenced)", xlab = "Time", bty = "l", main = "Remainder - No Seasonality or Trend", lty = 1)

par(mfrow = c(1, 1))

### Using residuals to find a pattern:

nValid <- 12
nTrain <- length(diff.twice.ts) - nValid
train.ts <- window(salesData.ts, start = c(2010, 28), end = c(2010, nTrain + 1))
valid.ts <- window(salesData.ts, start = c(2010, nTrain + 2), end = c(2010, nTrain + 1 + nValid))
ses <- auto.arima(train.ts)
ses.pred <- forecast(ses, h = nValid)
plot(ses.pred, ylab = "Sales (Twice-Differenced)", xlab = "Time", bty = "l", main = "AutoArima Forecast", flty = 2)
lines(ses.pred$fitted, lwd = 2, col = "blue")
lines(valid.ts)




##################### MA Models ##############################


par(mfrow = c(1, 1))

# Identify TREND

par(mfrow = c(2,1))
ma.trailing <- rollmean(salesData.ts, k = 26, align = "right")
ma.centered <- ma(salesData.ts, order = 26)
plot(salesData.ts, ylab = "Sales", xlab = "Time", bty = "l", main = "")
lines(ma.centered, lwd = 2) # Centered Moving Average (For Exploration)
lines(ma.trailing, lwd = 2, lty = 2) # Trailing Moving Average (For Forecasting)


ma.trailing <- rollmean(train.ts, k = 26, align = "right")
last.ma <- tail(ma.trailing, 1)
ma.trailing.pred <- ts(rep(last.ma, nValid), start = c(2010, nTrain + 1), end = c(2010, nTrain + nValid), freq = 52)
plot(train.ts, ylab = "Sales", xlab = "Time", bty = "l", main = "")
lines(ma.trailing, lwd = 2, col = "blue")
lines(ma.trailing.pred, lwd = 2, col = "red", lty = 2)
lines(valid.ts)


######################### STL ################################

par(mfrow = c(1,1))
fit <- stl(salesData.ts, t.window = 12, s.window=12, robust=TRUE)
plot(fit)

forecast <- forecast(fit, h=52) # Full Year Forecast
plot(forecast)


#################### ESOpt #####################################

ESOpt <- stlf(train.ts)
plot(ESOpt)
ESOpt

par(mfrow = c(1, 1))


ESOpt.pred <- forecast(ESOpt, h = nValid, level = 0)
plot(ESOpt.pred, ylab = "Residuals", xlab = "Time", bty = "l", main = "", flty = 2)
lines(ESOpt.pred$fitted, lwd = 2, col = "blue")
lines(valid.ts)

################  Basic Fitting #######################

par(mfrow = c(3, 1))

plot(salesData.ts, xlab = "Time", ylab = "Sales", bty = "l")
sales.lm <- tslm(salesData.ts ~ poly(trend, 1))
lines(sales.lm$fitted, lwd = 2)

plot(salesData.ts, xlab = "Time", ylab = "Sales", bty = "l")
sales.lm <- tslm(salesData.ts ~ poly(trend, 2))
lines(sales.lm$fitted, lwd = 2)

plot(salesData.ts, xlab = "Time", ylab = "Sales", bty = "l")
sales.lm <- tslm(salesData.ts ~ poly(trend, 3))
lines(sales.lm$fitted, lwd = 2)

par(mfrow = c(1,1))

################### ARIMA Models ######################

plot(salesData.ts)

tsdisplay(train.ts)

fitARIMA <- arima(train.ts, order = c(1,0,0))
summary(fitARIMA)
Box.test(residuals(fitARIMA), lag=26, fitdf=1, type="Ljung-Box")


residualARIMA <- arima.errors(fitARIMA)
tsdisplay(residualARIMA)

forecastARIMA <- forecast(fitARIMA, level=c(80,95), h=nValid)
plot(forecastARIMA)

diff.train.ts <- diff(train.ts, lag = 1) # Detrending differencing (lag order 1)

tsdisplay(diff.train.ts)

fitSARIMA <- arima(train.ts, order = c(0,1,0), seasonal=c(1,0,0))
summary(fitSARIMA)
Box.test(residuals(fitSARIMA), lag=26, fitdf=1, type="Ljung-Box")

residualSARIMA <- arima.errors(fitSARIMA)
tsdisplay(residualSARIMA)

forecastSARIMA <- forecast(fitSARIMA, level=c(80,95), h=nValid)
plot(forecastSARIMA)

par(mfrow = c(2, 1))
hist(forecastSARIMA$residuals, ylab = "Frequency", xlab = "Fit Error", bty = "l", main = "")
hist(valid.ts - forecastSARIMA$mean, ylab = "Frequency", xlab = "Forecast Error", bty = "l", main = "")

fitArima111 <- arima(train.ts, order = c(1,1,1))
forecastArima111 <- forecast(fitArima111, level=c(80,95), h=nValid)

fitArima101 <- arima(train.ts, order = c(1,0,1))
forecastArima101 <- forecast(fitArima101, level=c(80,95), h=nValid)

fitArima011 <- arima(train.ts, order = c(0,1,1))
forecastArima011 <- forecast(fitArima011, level=c(80,95), h=nValid)

fitArima001 <- arima(train.ts, order = c(0,0,1))
forecastArima001 <- forecast(fitArima001, level=c(80,95), h=nValid)

fitArima110 <- arima(train.ts, order = c(1,1,0))
forecastArima110 <- forecast(fitArima110, level=c(80,95), h=nValid)

fitArima010 <- arima(train.ts, order = c(0,1,0))
forecastArima010 <- forecast(fitArima010, level=c(80,95), h=nValid)

fitSARIMA111 <- arima(train.ts, order = c(1,1,1), seasonal=c(1,0,0))
forecastSARIMA111 <- forecast(fitSARIMA111, level=c(80,95), h=nValid)

fitSARIMA110 <- arima(train.ts, order = c(1,1,0), seasonal=c(1,0,0))
forecastSARIMA110 <- forecast(fitSARIMA110, level=c(80,95), h=nValid)

fitSARIMA100 <- arima(train.ts, order = c(1,0,0), seasonal=c(1,0,0))
forecastSARIMA100 <- forecast(fitSARIMA100, level=c(80,95), h=nValid)

fitSARIMA011 <- arima(train.ts, order = c(0,1,1), seasonal=c(1,0,0))
forecastSARIMA011 <- forecast(fitSARIMA011, level=c(80,95), h=nValid)

fitSARIMA001 <- arima(train.ts, order = c(0,0,1), seasonal=c(1,0,0))
forecastSARIMA001 <- forecast(fitSARIMA001, level=c(80,95), h=nValid)

fitSARIMA010 <- arima(train.ts, order = c(0,1,0), seasonal=c(1,0,0))
forecastSARIMA010 <- forecast(fitSARIMA010, level=c(80,95), h=nValid)

naivePred <- naive(train.ts, h = nValid)
naiveP <- forecast(naivePred, level = c(80,95), h=nvalid)


linearModel <-  tslm(train.ts ~ poly(trend, 2))
lmPred <- forecast(linearModel, h = nValid, level = c(80,95))

accuracy(forecastSARIMA$mean, valid.ts)
accuracy(forecastARIMA$mean, valid.ts)
accuracy(forecastArima111$mean, valid.ts)
accuracy(forecastArima011$mean, valid.ts)
accuracy(forecastArima001$mean, valid.ts)
accuracy(forecastArima110$mean, valid.ts)
accuracy(forecastArima010$mean, valid.ts)
accuracy(forecastSARIMA111$mean, valid.ts)
accuracy(forecastSARIMA110$mean, valid.ts)
accuracy(forecastSARIMA100$mean, valid.ts)
accuracy(forecastSARIMA011$mean, valid.ts)
accuracy(forecastSARIMA001$mean, valid.ts)
accuracy(naivePred$mean, valid.ts)
accuracy(lmPred$mean, valid.ts)

################# Model Comparison ##################

# Create Results Matrix #
ETS <- as.data.frame(accuracy(ESOpt.pred$mean, valid.ts))
Arima100 <- as.data.frame(accuracy(forecastARIMA$mean, valid.ts))
Sarima010 <- as.data.frame(accuracy(forecastSARIMA$mean, valid.ts))
Arima111 <- as.data.frame(accuracy(forecastArima111$mean, valid.ts))
Arima011 <- as.data.frame(accuracy(forecastArima011$mean, valid.ts))
Arima001 <- as.data.frame(accuracy(forecastArima001$mean, valid.ts))
Arima110 <- as.data.frame(accuracy(forecastArima110$mean, valid.ts))
Arima010 <- as.data.frame(accuracy(forecastArima010$mean, valid.ts))
Sarima111 <- as.data.frame(accuracy(forecastSARIMA111$mean, valid.ts))
Sarima110 <- as.data.frame(accuracy(forecastSARIMA110$mean, valid.ts))
Sarima100 <- as.data.frame(accuracy(forecastSARIMA100$mean, valid.ts))
Sarima011 <- as.data.frame(accuracy(forecastSARIMA011$mean, valid.ts))
Sarima001 <-as.data.frame(accuracy(forecastSARIMA001$mean, valid.ts))
naive <- as.data.frame(accuracy(naivePred$mean, valid.ts))
lmodel <- as.data.frame(accuracy(lmPred$mean, valid.ts))


# Accuracy Results
finalResults <- as.data.frame(matrix(0,15,8))
names(finalResults) <- c("Model Name", "ME","RMSE","MAE","MPE","MAPE","ACF1","Theil's U")
finalResults[2,] <- c("ETSModel", ETS$ME, ETS$RMSE, ETS$MAE, ETS$MPE, ETS$MAPE, ETS$ACF1, ETS$`Theil's U`)
finalResults[3,] <- c("ARIMA100", Arima100$ME, Arima100$RMSE, Arima100$MAE, Arima100$MPE, Arima100$MAPE, Arima100$ACF1, Arima100$`Theil's U`)
finalResults[4,] <- c("ARIMA010", Arima010$ME, Arima010$RMSE, Arima010$MAE, Arima010$MPE, Arima010$MAPE, Arima010$ACF1, Arima010$`Theil's U`)
finalResults[5,] <- c("ARIMA111", Arima111$ME, Arima111$RMSE, Arima111$MAE, Arima111$MPE, Arima111$MAPE, Arima111$ACF1, Arima111$`Theil's U`)
finalResults[7,] <- c("ARIMA011", Arima011$ME, Arima011$RMSE, Arima011$MAE, Arima011$MPE, Arima011$MAPE, Arima011$ACF1, Arima011$`Theil's U`)
finalResults[8,] <- c("ARIMA001", Arima001$ME, Arima001$RMSE, Arima001$MAE, Arima001$MPE, Arima001$MAPE, Arima001$ACF1, Arima001$`Theil's U`)
finalResults[9,] <- c("ARIMA110", Arima110$ME, Arima110$RMSE, Arima110$MAE, Arima110$MPE, Arima110$MAPE, Arima110$ACF1, Arima110$`Theil's U`)
finalResults[10,] <- c("SARIMA010", Sarima010$ME, Sarima010$RMSE, Sarima010$MAE, Sarima010$MPE, Sarima010$MAPE, Sarima010$ACF1, Sarima010$`Theil's U`)
finalResults[11,] <- c("SARIMA111", Sarima111$ME, Sarima111$RMSE, Sarima111$MAE, Sarima111$MPE, Sarima111$MAPE, Sarima111$ACF1, Sarima111$`Theil's U`)
finalResults[12,] <- c("SARIMA110", Sarima110$ME, Sarima110$RMSE, Sarima110$MAE, Sarima110$MPE, Sarima110$MAPE, Sarima110$ACF1, Sarima110$`Theil's U`)
finalResults[13,] <- c("SARIMA100", Sarima100$ME, Sarima100$RMSE, Sarima100$MAE, Sarima100$MPE, Sarima100$MAPE, Sarima100$ACF1, Sarima100$`Theil's U`)
finalResults[14,] <- c("SARIMA011", Sarima011$ME, Sarima011$RMSE, Sarima011$MAE, Sarima011$MPE, Sarima011$MAPE, Sarima011$ACF1, Sarima011$`Theil's U`)
finalResults[1,] <- c("SARIMA001", Sarima001$ME, Sarima001$RMSE, Sarima001$MAE, Sarima001$MPE, Sarima001$MAPE, Sarima001$ACF1, Sarima001$`Theil's U`)
finalResults[6,] <- c("Naive", naive$ME, naive$RMSE, naive$MAE, naive$MPE, naive$MAPE, naive$ACF1, naive$`Theil's U`)
finalResults[15,] <- c("Linear Model", lmodel$ME, lmodel$RMSE, lmodel$MAE, lmodel$MPE, lmodel$MAPE, lmodel$ACF1, lmodel$`Theil's U`)

finalResults # Forecasting Results Grid

write.csv(finalResults, file = "Results.csv", row.names=FALSE)

#SARIMA Models
par(mfrow=c(3,2))
plot(forecastSARIMA010)
plot(forecastSARIMA111)
plot(forecastSARIMA110)
plot(forecastSARIMA100)
plot(forecastSARIMA011)
plot(forecastSARIMA001)

# Arima Models
par(mfrow=c(3,2))
plot(forecastArima010)
plot(forecastArima111)
plot(forecastArima110)
plot(forecastARIMA)
plot(forecastArima011)
plot(forecastArima001)


# Other Models
par(mfrow=c(3,1))
plot(lmPred)
plot(naiveP)
plot(ESOpt.pred, ylab = "Residuals", xlab = "Time", bty = "l", main = "Forecast from ETS Method", flty = 2)


###### Ensemble ##########

nValid <- 12
nTest <- 12
nTrain <- length(salesData.ts) - nValid-nTest
train.ts <- window(salesData.ts, start = c(2010, 28), end = c(2010, nTrain + 1))
valid.ts <- window(salesData.ts, start = c(2010, nTrain + 2), end = c(2010, nTrain + 1 + nValid))
test.ts <- window(salesData.ts, start = c(2010, nTrain+2+nValid), end = c(2010, nTrain+1+nValid+nTest))

fitSARIMA110 <- arima(train.ts, order = c(1,1,0), seasonal=c(1,0,0))
forecastSARIMA110 <- forecast(fitSARIMA110, level=c(80,95), h=nValid)


fitSARIMA011 <- arima(train.ts, order = c(0,1,1), seasonal=c(1,0,0))
forecastSARIMA011 <- forecast(fitSARIMA011, level=c(80,95), h=nValid)


fitSARIMA010 <- arima(train.ts, order = c(0,1,0), seasonal=c(1,0,0))
forecastSARIMA010 <- forecast(fitSARIMA010, level=c(80,95), h=nValid)

# Setting weights of varying forecasts
weight_110 <- 0
weight_011 <- 0
weight_010 <- 1

# Creating weighted average ensemble forecast MEAN, LOWER, UPPER

ensembleForecast <- forecast(fitSARIMA110, level=c(80,95),h = nValid)
ensembleForecast$mean <- weight_110 * forecastSARIMA110$mean + weight_011 * forecastSARIMA011$mean + weight_010 * forecastSARIMA010$mean
ensembleForecast$lower <- weight_110 * forecastSARIMA110$lower + weight_011 * forecastSARIMA011$lower + weight_010 * forecastSARIMA010$lower
ensembleForecast$upper <- weight_110 * forecastSARIMA110$upper + weight_011 * forecastSARIMA011$upper + weight_010 * forecastSARIMA010$upper


par(mfrow=c(1,1))
plot(ensembleForecast, ylab = "Sales", xlab = "Time", bty = "l", main = "Ensemble Forecast", flty = 2)
lines(ensembleForecast$fitted, lwd = 2, col = "blue")
lines(valid.ts)

accuracy(forecastSARIMA110$mean, valid.ts)
accuracy(forecastSARIMA011$mean, valid.ts)
accuracy(forecastSARIMA010$mean, valid.ts)
accuracy(ensembleForecast$mean, valid.ts)


fitSARIMA110 <- arima(train.ts, order = c(1,1,0), seasonal=c(1,0,0))
forecastSARIMA110 <- forecast(fitSARIMA110, level=c(80,95), h=nTest)


fitSARIMA011 <- arima(train.ts, order = c(0,1,1), seasonal=c(1,0,0))
forecastSARIMA011 <- forecast(fitSARIMA011, level=c(80,95), h=nTest)


fitSARIMA010 <- arima(train.ts, order = c(0,1,0), seasonal=c(1,0,0))
forecastSARIMA010 <- forecast(fitSARIMA010, level=c(80,95), h=nTest)



accuracy(forecastSARIMA110$mean, as.numeric(test.ts))
accuracy(forecastSARIMA011$mean, as.numeric(test.ts))
accuracy(forecastSARIMA010$mean, as.numeric(test.ts))
accuracy(ensembleForecast$mean, as.numeric(test.ts))