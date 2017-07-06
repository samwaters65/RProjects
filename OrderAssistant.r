##############################################
#                                            #
# Purpose:                                   #
#	Use ARIMA forecasting to forecast demand #
#	to aid in order generation               #
#                                            #
##############################################


library(dplyr)
library(forecast)
library(zoo)
library(rowr)

####################################################
# Load the data and tranform it into a time series #
####################################################

orderData <- read.csv("TimeSeriesTemplate.csv")


orderDataSmall <- subset(orderData, select = -c(WK, ScanU ))
orderDataUnique <- unique(orderDataSmall)

x<-by(orderData,orderData$Ident,function(dat)with(dat, ts(ScanU)))


df <- data.frame(matrix(ncol = 2, nrow = len(x)))
colnames(df) <- c("Ident", "Pred")

for(i in 1:len(x)) {
  train.ts <- x[[i]]
  ses <- auto.arima(train.ts)
  ses.pred <- forecast(ses, h = 1)
  df$Ident[i] <- names(x[i])
  df$Pred[i] <- ses.pred$mean[1]
  print(i)
}


df$Pred <- round(df$Pred,0)

write.csv(df,"TimeSeriesOutput.csv")
