
#Set directory
tempwd <- getwd()
setwd(tempwd)


# 1. Install packages
install.packages(c ("RODBC","R2HTML"))
install.packages("forecast")

library(RODBC)
library(R2HTML)
library(forecast)
library(zoo)





# 2. Connecting to the DB
#Open connection
dbconnection <- odbcDriverConnect("Driver=ODBC Driver 11 for SQL Server;
                Server=127.0.0.1; Database=Bookshop; Uid=<<user>>; Pwd=<<password>>; trusted_connection=yes")

#Retrieving our primary data
sData <- sqlQuery(dbconnection, "SELECT Quantity FROM [PizzaRUs].[dbo].[Sheet1]")

#Retrieving earliest season from the earliest period
ePeriod <- sqlQuery(dbconnection, "EXEC [PizzaRUs].[dbo].[Earliest_MnY]")

#Retrieving latest season from the latest period
lPeriod <- sqlQuery(dbconnection, "EXEC [PizzaRUs].[dbo].[Latest_MnY]")


#Close connection
odbcClose(dbconnection)





# 3. Formatting data into ts for easier manipulation
tData <- c(sData$Quantity)
tsData <- ts(tData, start=c(c(ePeriod$Year), c(ePeriod$Month)), frequency=12)   #Change freq to 52 for weeks
plot(tsData)

#Last data is dec 2014
tsData





# 4. Performing our forecast using Automated ARIMA forecasting
fitData <- auto.arima(tsData)
#auto.arima means letting the com decide on the best model for you. It does everything for you.

fitData    #Can see that R will use arima 2,1,2 to perform the focus

#Determining the forecasted value
resultF <- forecast(fitData, 5)

#The accuracy() function will calculate the errors for the dataset. You can then analyse it. 
accuracy(fitData)

#Plot data
plot(resultF, xlab="Year", ylab="Quantity", main="Forecasted Demand")
#The range gets wider over time cos its getting harder to predict the future the further down the road.






#5. Populating the forecasted values back to the database

tempY <- c(lPeriod$Year)
tempM <- c(lPeriod$Month)
#If its Dec (12th season), it will reset it back to zero and increase the year by 1. Change to 52 if using weeks.*
if (tempM == 12) {
  tempM <- 0
  tempY <- tempY + 1
}


#Open connection
dbconnection <- odbcDriverConnect("Driver=ODBC Driver 11 for SQL Server;
                                  Server=127.0.0.1; Database=Bookshop; Uid=<<user>>; Pwd=<<password>>; trusted_connection=yes")
for (x in 1:5) {
  tM <- as.numeric(resultF$mean)
  tL <- as.numeric(resultF$lower)
  tH <- as.numeric(resultF$upper)
  
  #Add the month
  tempM <- tempM + 1
  
  #Querying the DB
  stmt <- sprintf('EXEC [PizzaRUs].[dbo].[Insert_Forecast] @varYear=%d, @varMonth=%d, @varDd=%f, @varLo80=%f, 
                  @varHi80=%f, @varLo95=%f, @varHi95=%f', tempY, tempM, tM[x], tL[x], tH[x], tL[x + 5], tH[x + 5])
  sqlQuery(dbconnection, stmt)
  #sqlSave(dbconnection, tempF, tablename = "[PizzaRUs].[dbo].[Forecast001]")
}
odbcClose(dbconnection)  #Close connection











# BONUS
resultF[10]
resultF
tempF <- as.numeric(resultF$mean)  #Retrieve only the forecasted figures and converting it to a Vector (R's arrays)

#For 5 months predictions, x is 1 - 5
tempF[4]
tempF
length(tempF)

#Finding the month using library(zoo)
vv <- as.yearmon(time(resultF$mean))
vv[2]

#Month and week
month.abb[1]
strftime(c("2014-03-16", "2014-03-17","2014-03-18", "2014-01-01"), format = "%V")

#for loops, var x from 1 to 5
for (x in 1:10) {
  print(tempF[x])
}



cat('<set name="',55, '" value="', 55, '"></set>\n', sep='')

cat(sprintf('EXEC [PizzaRUs].[dbo].[Insert_Forecast] @varYear=%d, @varMonth=%d, @varDd=%f, @varLo80=%f, @varHi80=%f, 
            @varLo95=%f, @varHi95=%f', tempY, tempM, tM[x], tL[x], tH[x], tL[x + 5], tH[x + 5]))


sqlSave(dbconnection, tempF, tablename = "[PizzaRUs].[dbo].[Forecast001]")


cat('EXEC [PizzaRUs].[dbo].[Insert_Forecast] @varYear =',tempY,', @varMonth =',tempM,', @varDd =',tM[x],', 
    @varLo80 =',tL[x],', @varHi80 =',tH[x],', @varLo95 =',tL[x + 5],', @varHi95 =',tH[x + 5],'')

tM[1]
tL[1]
tH[1]



#Maybe no need
install.packages('dplyr')
remove.packages('forecast')
library('dplyr')
rm(list=ls())



install.packages('quantmod')
install.packages('xlsx')



#Specify ticker symbol
getSymbols("MSFT")
head(AAPL)
AAPL
TSLA
MSFT


data()
head(EuStockMarkets)
EuStockMarkets[4,]


tData <- c(EuStockMarkets$FTSE)
tsData <- ts(tData, start=c(c(ePeriod$Year), c(ePeriod$Month)), frequency=12)   #Change freq to 52 for weeks
plot(tsData)


fitData <- auto.arima(tsData)

fitData 
resultF <- forecast(fitData, 5)








