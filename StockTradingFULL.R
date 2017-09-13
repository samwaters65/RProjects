#######################################
# Purpose:                            #
# Use Quandl data for stock selection #
#######################################



# install and load packages
if (!require("Quandl"))
  install.packages("Quandl")
if (!require("RMySQL"))
  install.packages("RMySQL")
library(Quandl)
library(RMySQL)
library(PerformanceAnalytics)
library(quantmod)
library(plyr)
library(doParallel)
library(parallel)


no_cores <- detectCores() - 1



# Initiate cluster

cl <- makeCluster(no_cores)
registerDoParallel(cl)

##############
# Setup Code #
##############

startDate <- '2004-01-02'
useDate <- startDate
qryDate <- as.character(useDate)

Quandl.api_key("APIKey")

con <-
  dbConnect(
    RMySQL::MySQL(),
    dbname = "invest_sma",
    username = "user",
    password = "password"
  )


accountCash <-
  as.data.frame(dbGetQuery(con, "select * from accountbalance limit 1"))


symbolsPull <-
  as.data.frame(dbGetQuery(con, "select distinct ticker, free_code from tickerlist"))

tickers <- symbolsPull[, 1]

symbols <- symbolsPull[1:nrow(symbolsPull), 2]

i <- 1

num <- 1

smaMultiplier <- 1.1


portfolio <- list()

xtsList <- list()

symbolList <- list()

initDate <- '2003-01-01' # Date for begin date of Quandl data pulls

smaN <- 50

smaNLong <- 200


#######################################################
# Creating list of Quandl Pulls - one for each ticker #
#######################################################

for (i in 1:length(symbols)) {
  symbolName <- as.character(symbols[i])
  xtsList[[i]] <-
    Quandl(symbols[i], type = 'xts', start_date = initDate)
  symbolList[i] <- symbolName
}

names(xtsList) <- symbolList

# spyFull <- Quandl('YAHOO/INDEX_W5000', type = 'xts', start_date = initDate)


##############################################
# Begin While Loop - for each day to current #
##############################################


while (useDate <= Sys.Date()) {
  startTime <- Sys.time()
  
  
  # tickerListSelector <- as.data.frame(dbGetQuery(con, paste("Select trim(z.ticker) as ticker, t.free_code, (z.ROI*.3 + p.ROI*.7) as WeightedROI
  #                                                           from (
  #                                                           select ticker, round(sum(profit)/sum(TTLPurchases)*100,2) as ROI
  #                                                           from output
  #                                                           where saledate < date_sub('",qryDate,"', INTERVAL 365 day)
  #                                                           group by ticker) z
  #                                                           inner join (
  #                                                           select ticker, round(sum(profit)/sum(TTLPurchases)*100,2) as ROI
  #                                                           from output
  #                                                           where saledate between date_sub('",qryDate,"', INTERVAL 60 day) and '",qryDate,"'
  #                                                           group by ticker) p on p.ticker = z.ticker
  #                                                           inner join tickerlist t on trim(t.ticker) = trim(p.ticker)
  #                                                           group by z.ticker, t.free_code
  #                                                           having WeightedROI > 0.1
  #                                                           order by (z.ROI*.3 + p.ROI*.7) desc;", sep = "")))
  
  tickerListSelector <-
    as.data.frame(dbGetQuery(con, "select distinct ticker, free_code from tickerlist"))
  symbolListSelector <- tickerListSelector[, 2]
  tickerListSelector <- tickerListSelector[, 1]
  
  
  ##############################################
  # Start for loop to loop through the tickers #
  ##############################################
  
  for (i in 1:length(symbolListSelector)) {
    ticker <- tickerListSelector[i]
    symbol <- symbolListSelector[i]
    
    stockDataFull <- xtsList[[symbol]]
    
    dateRange <- paste(initDate, '/', useDate)
    dateRange <- gsub(" ", "", dateRange, fixed = TRUE)
    
    stockData <- stockDataFull[dateRange]
    stockData <- na.exclude(stockData)
    stockDataDF <- stockData$Close
    stockDataDF <-
      data.frame(value = coredata(stockDataDF),
                 timestamp = index(stockDataDF))
    stockDataDF <- subset(stockDataDF, timestamp == useDate)
    
    
    if (is.null(nrow(stockData)) || nrow(stockData) < 200) {
      #useDate <- as.Date(useDate) + 1
      #qryDate <- as.Date(qryDate) + 1
      i <- i + 1
      next
    }
    else if (as.Date(index(tail(stockData, n = 1))) < (as.Date(useDate) - 5)) {
      i <- i + 1
      #useDate <- as.Date(useDate) + 1
      #qryDate <- as.Date(qryDate) + 1
      next
    }
    
    else {
      stockDataClose <- stockData$Close
      stockDataClose <- na.exclude(stockDataClose)
      
      sma <- SMA(stockDataClose, n = smaN)
      
      smaToUseInter <-
        data.frame(value = coredata(sma), timestamp = index(sma))
      smaToUse <- tail(smaToUseInter, n = 1)
      smaToUse <- smaToUse$SMA
      
      
      smaMinus1 <- tail(smaToUseInter, n = 2)
      smaMinus1 <- smaMinus1[1, 1]
      
      
      
      smaLong <- SMA(stockDataClose, n = smaNLong)
      
      smaToUseInterLong <-
        data.frame(value = coredata(smaLong),
                   timestamp = index(smaLong))
      smaToUseLong <- tail(smaToUseInterLong, n = 1)
      smaToUseLong <- smaToUseLong$SMA
      
      
      smaMinus1Long <- tail(smaToUseInterLong, n = 2)
      smaMinus1Long <- smaMinus1Long[1, 1]
      
      
      ##########
      # Output #
      ##########
      
      
      price <- tail(stockDataClose, n = 1)
      price <-
        data.frame(value = coredata(price), timestamp = index(price))
      price <- price[, 1]
      
      
      PurchQuery <- dbGetQuery(con, paste("select case when (select count(a.calendardate) from metrics a where trim(a.ticker) = '", ticker ,"' and todo = ' sell ') = 0 then (select ifnull(count(a.CalendarDate),0) from metrics a where trim(a.ticker) = '", ticker ,"')
            else (select count(a.CalendarDate) from metrics a
            inner join (
            select ticker, max(CalendarDate) as MaxSellDate
            from metrics
            where todo = ' sell '
            group by ticker) z on z.ticker = a.ticker
            where a.todo = ' buy ' and a.CalendarDate > z.MaxSellDate and trim(a.ticker) = '", ticker ,"'
            group by a.ticker)
            END,
            case when (select count(a.calendardate) from metrics a where trim(a.ticker) = '", ticker ,"' and todo = ' sell ') = 0 then (select ifnull(sum(a.PriorPrice),0) from metrics a where trim(a.ticker) = '", ticker ,"')
            else (select sum(a.priorprice) from metrics a
            inner join (
            select ticker, max(CalendarDate) as MaxSellDate
            from metrics
            where todo = ' sell '
            group by ticker) z on z.ticker = a.ticker
            where a.todo = ' buy ' and a.CalendarDate > z.MaxSellDate and trim(a.ticker) = '", ticker ,"'
            group by a.ticker)
            END;", sep = ""))
      
      numberOfShares <- PurchQuery[,1]
      TTLPurchases <- PurchQuery[,2]
      
      
      if (is.na(smaToUse) ||
          is.na(smaToUseLong) || is.na(smaMinus1) || is.na(smaMinus1Long)) {
        status <- 'Error'
      }
      else if (smaToUse >= smaToUseLong &&
               smaMinus1 < smaMinus1Long) {
        #&& accountCash > price
        status <- 'Buy'
        #accountCash <- accountCash - price
        if (is.element(ticker, portfolio)) {
          q <- 0
        } else {
          portfolio[[length(portfolio) + 1]] <- ticker
        }
      } else if (smaToUse <= smaToUseLong &&
                 smaMinus1 > smaMinus1Long && is.element(ticker, portfolio) &&
                 price*numberOfShares > TTLPurchases) {
        status <- 'Sell'
        #numShares <- dbGetQuery(con, paste("select case when (select count(a.calendardate) from transactions a where trim(a.ticker) = '", ticker ,"' and todo = ' sell ') = 0 then (select count(a.CalendarDate) from transactions a where trim(a.ticker) = '", ticker ,"')
        #                                   else (select count(a.calendardate) from transactions a
        #                                   inner join (
        #                                   select ticker, max(CalendarDate) as MaxSellDate
        #                                   from transactions
        #                                   where todo = ' sell '
        #                                   group by ticker) z on z.ticker = a.ticker
        #                                   where a.todo = ' buy ' and a.CalendarDate > z.MaxSellDate and trim(a.ticker) = '", ticker ,"'
        #                                   group by a.ticker)
        #                                   END;", sep = ""))
        #numShares <- numShares[,1]
        #accountCash <- accountCash + (price * numShares)
        toRemove <- match(ticker, portfolio)
        portfolio[toRemove] <- NULL
      } else {
        status <- 'Nothing'
      }
    }
    
    
    
    #   if (smaMinus2 < smaMinus3 && smaMinus1 > smaAvg*smaMultiplier) { #&& accountCash > price
    #     status <- 'Buy'
    #     #accountCash <- accountCash - price
    #     if (is.element(ticker, portfolio)) {q <- 0} else {portfolio[[length(portfolio)+1]] <- ticker}
    #   } else if (smaMinus2 > smaMinus3 && smaMinus1*smaMultiplier < smaAvg && is.element(ticker, portfolio)) {
    #     status <- 'Sell'
    #     #numShares <- dbGetQuery(con, paste("select case when (select count(a.calendardate) from transactions a where trim(a.ticker) = '", ticker ,"' and todo = ' sell ') = 0 then (select count(a.CalendarDate) from transactions a where trim(a.ticker) = '", ticker ,"')
    #     #                                   else (select count(a.calendardate) from transactions a
    #     #                                   inner join (
    #     #                                   select ticker, max(CalendarDate) as MaxSellDate
    #     #                                   from transactions
    #     #                                   where todo = ' sell '
    #     #                                   group by ticker) z on z.ticker = a.ticker
    #     #                                   where a.todo = ' buy ' and a.CalendarDate > z.MaxSellDate and trim(a.ticker) = '", ticker ,"'
    #     #                                   group by a.ticker)
    #     #                                   END;", sep = ""))
    #     #numShares <- numShares[,1]
    #     #accountCash <- accountCash + (price * numShares)
    #     toRemove <- match(ticker, portfolio)
    #     portfolio[toRemove] <- NULL
    #   } else {
    #     status <- 'Nothing'
    #   }
    # }
    
    
    date <- as.character(useDate)
    
    
    
    record <-
      data.frame(ticker,
                 symbol,
                 price,
                 date,
                 smaToUse,
                 status,
                 stringsAsFactors = FALSE)
    
    
    query <-
      paste(
        "INSERT INTO metrics VALUES('",
        record$ticker,
        "','" ,
        record$symbol,
        "'," ,
        record$price,
        ",'" ,
        record$date,
        "'," ,
        record$smaToUse,
        ",'" ,
        record$status,
        "'",
        ")"
      )
    #query2 <- paste("INSERT INTO transactions VALUES('",record$ticker, "','" , record$symbol, "'," ,record$price, ",'" ,record$date, "'," ,record$smaMean, "," ,record$trendStrength, "," ,record$aroonOscillator, "," ,record$alphaScore, "," ,record$betaScore, "," ,record$ARScore, "," ,record$knowSureThing, ",'" ,record$status,"'", ")")
    
    if (record$status == 'Buy') {
      dbGetQuery(con, query)
    } else if (record$status == 'Sell') {
      dbGetQuery(con, query)
      #print(paste("Sale: ", "ticker: ", ticker, " Date: ", date, " Price: ", price, " NumShares: ", numberOfShares, " TTLPurch: ",TTLPurchases, sep = ""))
    }
    
    # if (record$status == 'Buy') {
    #   dbGetQuery(con, query2)
    # } else if (record$status == 'Sell') {
    #   dbGetQuery(con, query2)
    # }
    i = i + 1
  } # this one closes for loop
  
  
  endTime <- Sys.time()
  processTime <- endTime - startTime
  print(
    paste(
      "#",
      num,
      " ",
      useDate,
      " is completed...",
      round(processTime, 2),
      " seconds with accountCash of ",
      accountCash,
      sep = ""
    )
  )
  useDate <- as.Date(useDate) + 1
  qryDate <- as.Date(qryDate) + 1
  num <- num + 1
  
} # this one closes while loop


dbGetQuery(con, "truncate table accountbalance")

dbGetQuery(con,
           paste("insert into accountbalance values ('", accountCash, "');", sep = ""))

print(accountCash)

remainingPortfolio <-
  dbGetQuery(
    con,
    "select trim(a.ticker) as ticker, count(a.calendardate) as NumShares
    from transactions a
    inner join (
    select ticker, max(CalendarDate) as MaxSellDate
    from transactions
    where todo = ' sell '
    group by ticker) z on z.ticker = a.ticker
    where a.todo = ' buy ' and a.CalendarDate > z.MaxSellDate
    group by a.ticker;"
  )

remainingPortfolio

on.exit(dbDisconnect(con))


stopCluster(cl)
