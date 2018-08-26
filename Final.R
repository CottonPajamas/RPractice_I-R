
#!/usr/bin/env Rscript
rm(list=ls())
#sink()

# 1. Set directory
tempwd <- getwd()
setwd(tempwd)




# 2. Install packages
#Retrieve list of all installed packages
listPack <- c(2, 3, 5)
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
  install.packages("forecast", repos = c("http://rstudio.org/_packages", "http://cran.rstudio.com"), 
                   destdir=(paste(tempwd,"/RPackages", sep = "")))
  print("Installing package: forecast")
}

install.packages("forecast")
remove.packages("forecast")
library("forecast", lib.loc="~/Documents/R/win-library/3.4")


# 3. Initialize all the required libraries
library(RODBC)
library(R2HTML)
library(forecast)



# 4. Connecting to the DB
#Open connection
dbconnection <- odbcDriverConnect("Driver=ODBC Driver 11 for SQL Server;
                                  Server=127.0.0.1; Database=Bookshop; Uid=<<user>>; Pwd=<<password>>; trusted_connection=yes")

#Retrieving our primary data
sData <- sqlQuery(dbconnection, "SELECT Quantity FROM [Bookshop].[dbo].[Sheet1]")

#Retrieving earliest season from the earliest period
ePeriod <- sqlQuery(dbconnection, "EXEC [Bookshop].[dbo].[Earliest_MnY]")

#Retrieving latest season from the latest period
lPeriod <- sqlQuery(dbconnection, "EXEC [Bookshop].[dbo].[Latest_MnY]")


#Close connection
odbcClose(dbconnection)





# 5. Formatting data into ts for easier manipulation
tData <- c(sData$Quantity)
tsData <- ts(tData, start=c(c(ePeriod$Year), c(ePeriod$Month)), frequency=12)   #Change freq to 52 for weeks
plot(tsData)



# 6. Performing our forecast using Automated ARIMA forecasting
print("Starting forecasting algorithm...")
fitData <- auto.arima(tsData)
#auto.arima means letting the com decide on the best model for you. It does everything for you.

#fitData    #Can see that R will use arima 2,1,2 to perform the focus

#Determining the forecasted value
resultF <- forecast(fitData, 5)

#The accuracy() function will calculate the errors for the dataset. You can then analyse it. 
#accuracy(fitData)

#Plot data
#The range gets wider over time cos its getting harder to predict the future the further down the road.
plot(resultF, xlab="Year", ylab="Quantity", main="Forecasted Demand", type="h")






# 7. Populating the forecasted values back to the database

tempY <- c(lPeriod$Year)
tempM <- c(lPeriod$Month)
#If its Dec (12th season), it will reset it back to zero and increase the year by 1. Change to 52 if using weeks.*
if (tempM == 12) {
  tempM <- 0
  tempY <- tempY + 1
}


#Open connection
dbconnection <- odbcDriverConnect("Driver=ODBC Driver 11 for SQL Server;
                                  Server=127.0.0.1; Database=Bookshop; Uid=root; Pwd=password; trusted_connection=yes")
for (x in 1:5) {
  tM <- as.numeric(resultF$mean)
  tL <- as.numeric(resultF$lower)
  tH <- as.numeric(resultF$upper)
  
  #Add the month
  tempM <- tempM + 1
  
  #Querying the DB
  stmt <- sprintf('EXEC [Bookshop].[dbo].[Insert_Forecast] @varYear=%d, @varMonth=%d, @varDd=%f, @varLo80=%f, 
                  @varHi80=%f, @varLo95=%f, @varHi95=%f', tempY, tempM, tM[x], tL[x], tH[x], tL[x + 5], tH[x + 5])
  sqlQuery(dbconnection, stmt)
  #sqlSave(dbconnection, tempF, tablename = "[PizzaRUs].[dbo].[Forecast001]")
}
odbcClose(dbconnection)  #Close connection

print("Item xx successful.")











