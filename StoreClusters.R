###################################
# Purpose:                        #
# Use demographic data from SQL   #
# Server db and perform variable  #
# selection and KMeans clustering #
###################################




library(RODBC)
library(dplyr)



options(stringsAsFactors = FALSE)

con <- odbcDriverConnect(connection="Driver={SQL Server Native Client 10.0}; Server=Server; Database=Database;Trusted_Connection=yes;")


qry <- 'select * from dbo.TableName'

rawData <- sqlQuery(con, qry)

#######################
# Basic Preprocessing #
#######################

colDrops <- c("StoreID","Latitude", "Longitude", "countyName", "Cluster")
useData <- rawData[ , !(names(rawData) %in% colDrops)]


useData <- scale(useData)

######################
# Feature Importance #
######################


# ensure the results are repeatable
set.seed(7)
# load the library
library(mlbench)
library(caret)
# calculate correlation matrix
correlationMatrix <- cor(useData[,1:ncol(useData)])
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)



deleteCols <- colnames(useData)[highlyCorrelated]
useData <- useData[ , !(colnames(useData) %in% deleteCols)]

# Select number of clusters (Elbow Method)
wss <- (nrow(useData)-1)*sum(apply(useData,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(useData,
                                     centers=i, iter.max = 30)$withinss)
plot(1:15, wss, type="b", xlab="# Clusters",
     ylab="Within groups sum of squares")


fit <- kmeans(useData, centers = 4, iter.max=30)
# get cluster means 
aggregate(useData,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata <- data.frame(rawData, fit$cluster)
names(mydata)[28] <- 'ClusterNum'

a <- 1
b <- nrow(mydata)
for (i in 1:nrow(mydata)) {
  cluster <- mydata[i,28]
  storeid <- mydata[i,1]
  qry2 <- paste('update database.dbo.TableName set cluster =', cluster,' where storeid = ', storeid, sep="")
  sqlQuery(con, qry2)
  print(paste('Store # ', a, ' out of ', b, ' total stores'))
  a <- a + 1
}


odbcCloseAll()
