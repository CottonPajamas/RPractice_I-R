
#!/usr/bin/env Rscript
rm(list=ls())
#sink()

# 1. Set directory
tempwd <- getwd()
setwd(tempwd)
tempwd


# 2. Install packages
#Retrieve list of all installed packages
listPack <- installed.packages()[,"Package"]

#If package not found, it will be installed accordingly
if (!"RODBC" %in% listPack) {
  install.packages("RODBC", repos='http://cran.us.r-project.org', 
                   destdir=(paste(tempwd,"/RPackages", sep = "")))
  print("Installing package: RODBC")
}
if (!"R2HTML" %in% listPack) {
  install.packages("R2HTML", repos='http://cran.us.r-project.org', 
                   destdir=(paste(tempwd,"/RPackages", sep = "")))
  print("Installing package: R2HTML")
}
if (!"forecast" %in% listPack) {
  install.packages("forecast", repos='http://cran.us.r-project.org', 
                   destdir=(paste(tempwd,"/RPackages", sep = "")))
  print("Installing package: forecast")
}




# 3. Initialize all the required libraries
library(RODBC)
library(R2HTML)
library(forecast)


odbc
# 4. Connecting to the DB
#Open connection
dbconnection <- odbcDriverConnect("Driver=ODBC Driver 11 for SQL Server;
                                  Server=127.0.0.1; Database=Bookshop; Uid=<<user>>; Pwd=<<password>>; trusted_connection=yes")

#Retrieving our primary data
sData <- sqlQuery(dbconnection, "SELECT Quantity FROM [Bookshop].[dbo].[Sheet1]")

#Retrieving earliest season from the earliest period
#ePeriod <- sqlQuery(dbconnection, "EXEC [Bookshop].[dbo].[Earliest_MnY]")

#Retrieving latest season from the latest period
#lPeriod <- sqlQuery(dbconnection, "EXEC [Bookshop].[dbo].[Latest_MnY]")


#Close connection
odbcClose(dbconnection)

plot(sData)















