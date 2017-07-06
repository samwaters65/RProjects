#######################################################
#                                                     #
# Purpose:                                            #
#	Identify locations without Lat/Long coordinates,  #
#	then use the Google Maps API to geolocate and     #
#	update the database                               #
#                                                     #
#######################################################




library(httr)
library(jsonlite)
library(lubridate)
library(RODBC)


options(stringsAsFactors = FALSE)

con <- odbcDriverConnect(connection="Driver={SQL Server Native Client 10.0}; Server=ServerName; Database=DBName;Trusted_Connection=yes;")



missing <- sqlQuery(con, "
                    select a.storeid, b.AddressLine1,b.city, b.PostalStateCode, b.ZipPostalCode
                    from dbo.Stores a
                    left outer join dbo.addresses b on a.storeid = b.storeid
					where b.storeid is null;")




df <- as.data.frame(missing)

ttlct <- nrow(df)
counter <- 1

options(stringsAsFactors = FALSE)

for (i in 1:nrow(df)) {
  
  storeid <- df[i,1]
  street <- df[i,2]
  city <- df[i,3]
  state <- df[i,4]

street <- gsub(" ","+",street)
city <- gsub(" ","+",city)
apiKey <- 'apikey'

geoURL <- 'https://maps.googleapis.com/'

geoPath <- paste('maps/api/geocode/json?address=',street,',+',city,',+',state,'&key=',apiKey,sep = "")


rawGeo <- GET(url = geoURL, path = geoPath)


geoRawContent <- rawToChar(rawGeo$content)

sub <- substr(geoRawContent,1,2)

if (sub == '<!') {
  next
}

geoContent <- fromJSON(geoRawContent)

geoLat <- geoContent$results$geometry$location$lat
geoLong <- geoContent$results$geometry$location$lng



sqlQuery(con,paste('update dbo.Stores set latitude =', geoLat,' where storeid = ',storeid,sep=""))

sqlQuery(con,paste('update dbo.Stores set longitude =', geoLong,' where storeid = ',storeid,sep=""))

print(paste(street," ", city, " ", state, " ", geoLat, " ", geoLong, sep = ""))

counter <- counter + 1

}


odbcCloseAll()