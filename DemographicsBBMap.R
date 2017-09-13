######################################
# Purpose:                           #
# Pull store level demographic data  #
# from Broadband Map API and persist #
# in a table                         #
######################################



# install.packages(c("httr", "jsonlite", "lubridate"))

library(httr)
library(jsonlite)
library(lubridate)
library(RODBC)


options(stringsAsFactors = FALSE)

con <- odbcDriverConnect(connection="Driver={SQL Server Native Client 10.0}; Server=ServerName; Database=DatabaseName;Trusted_Connection=yes;")



p <- sqlQuery(con, "
              select a.storeid, a.Latitude, a.Longitude
              from database.dbo.Stores a
              left outer join database.dbo.Demographics b on a.StoreID = b.storeid
              where b.storeid is null and CustomerID in (1, 2, 3);")


y <- as.data.frame(p)
baseData <- na.exclude(y)
ttlct <- nrow(baseData)
counter <- 1

options(stringsAsFactors = FALSE)


for (i in 1:nrow(baseData)) {

storeid <- baseData[i,1] 
latitude <- baseData[i,2]
longitude <- baseData[i,3]

url <- 'https://www.broadbandmap.gov/'
path <- paste('broadbandmap/demographic/2014/coordinates?latitude=',latitude,'&longitude=',longitude,'&format=json', sep="")

raw.result <- GET(url = url, path = path)


this.raw.content <- rawToChar(raw.result$content)

this.content <- fromJSON(this.raw.content)

incomeBelowPoverty <- round(this.content$Results$incomeBelowPoverty, 4)
medianIncome <- round(this.content$Results$medianIncome, 4)
incomeLessThan25 <- round(this.content$Results$incomeLessThan25, 4)
incomeBetween25To50 <- round(this.content$Results$incomeBetween25to50, 4)
incomeBetween50To100 <- round(this.content$Results$incomeBetween50to100, 4)
incomeBetween100To200 <- round(this.content$Results$incomeBetween100to200, 4)
incomeGreater200 <- round(this.content$Results$incomeGreater200, 4)
percHighSchoolGrad <- round(this.content$Results$educationHighSchoolGraduate, 4)
percBachelorOrGreater <- round(this.content$Results$educationBachelorOrGreater, 4)

path2 <- paste('broadbandmap/census/county?latitude=',latitude,'&longitude=',longitude,'&format=json', sep = "")

raw.result2 <- GET(url = url, path = path2)

this.raw.content2 <- rawToChar(raw.result2$content)

this.content2 <- fromJSON(this.raw.content2)

fips <- this.content2$Results$county$fips

path3 <- paste('broadbandmap/demographic/jun2014/county/ids/',fips,'?format=json', sep = "")

raw.result3 <- GET(url = url, path = path3)

this.raw.content3 <- rawToChar(raw.result3$content)

this.content3 <- fromJSON(this.raw.content3)

countyName <- this.content3$Results$geographyName
countySqMiles <- round(this.content3$Results$landArea, 4)
population <- round(this.content3$Results$population, 4)
households <- round(this.content3$Results$households, 4)
raceWhite <- round(this.content3$Results$raceWhite, 4)
raceBlack <- round(this.content3$Results$raceBlack, 4)
raceHispanic <- round(this.content3$Results$raceHispanic, 4)
raceAsian <- round(this.content3$Results$raceAsian, 4)
raceNativeAmerican <- round(this.content3$Results$raceNativeAmerican, 4)
ageUnder5 <- round(this.content3$Results$ageUnder5, 4)
ageBetween5to19 <- round(this.content3$Results$ageBetween5to19, 4)
ageBetween20to34 <- round(this.content3$Results$ageBetween20to34, 4)
ageBetween35to59 <- round(this.content3$Results$ageBetween35to59, 4)
ageGreaterThan60 <- round(this.content3$Results$ageGreaterThan60, 4)


sqlQuery(con, paste("Insert into database.dbo.Demographics VALUES (",storeid,",",latitude,",",longitude,",",incomeBelowPoverty,",",incomeLessThan25,",",incomeBetween25To50, ",",incomeBetween50To100,",",incomeBetween100To200,",",incomeGreater200, ",", medianIncome, ",", percHighSchoolGrad, ",", percBachelorOrGreater, ",'", countyName, "',", countySqMiles, ",", population, ",", households, ",", raceNativeAmerican, ",", raceAsian, ",", raceHispanic, ",", raceBlack, ",", raceWhite, ",", ageUnder5, ",", ageBetween5to19, ",", ageBetween20to34, ",", ageBetween35to59, ",", ageGreaterThan60,")",sep = ""))

print(paste("Store ", counter, " out of ", ttlct, " Completed", sep = ""))

counter <- counter + 1

Sys.sleep(1)      
} # Close the for loop


odbcCloseAll()
