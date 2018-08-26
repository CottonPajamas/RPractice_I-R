

#!/usr/bin/env Rscript
rm(list=ls())
#sink()

# 1. Set directory
tempwd <- getwd()
setwd(tempwd)



# 2. Install packages
#Retrieve list of all installed packages
listPack <- installed.packages()[,"Package"]


if (!"quantmod" %in% listPack) {
  install.packages("quantmod", repos='http://cran.us.r-project.org', 
                   destdir=(paste(tempwd,"/RPackages", sep = "")))
  print("Installing package: quantmod")
}
if (!"xlsx" %in% listPack) {
  install.packages("xlsx", repos='http://cran.us.r-project.org', 
                   destdir=(paste(tempwd,"/RPackages", sep = "")))
  print("Installing package: xlsx")
}
if (!"xts" %in% listPack) {
  install.packages("xts", repos='http://cran.us.r-project.org', 
                   destdir=(paste(tempwd,"/RPackages", sep = "")))
  print("Installing package: xts")
}
if (!"forecast" %in% listPack) {
  install.packages("forecast", repos='http://cran.us.r-project.org', 
                   destdir=(paste(tempwd,"/RPackages", sep = "")))
  print("Installing package: forecast")
}




# 3. Load libraries
library(quantmod)
library(xlsx)
library(xts)
library(forecast)



# 4. Retrieving data
#Specify ticker symbol
#To get all in one line: getSymbols(c("MSFT","AAPL","TSLA"))

#But specify diff dates to get the 'seasonality' shape that I want :P
getSymbols("MSFT", from="2009-01-01", to="2013-02-4")
getSymbols("S51.SI", from="2006-01-01", to="2010-02-4")
getSymbols("BRK-B", from="2009-01-01", to="2013-02-4")
S51 <- `S51.SI`
BRKB <- `BRK-B`   #Must convert for ease in using in R
BRKB.Close


head(S51)
tail(AAPL)
MSFT
AAPL
BRKB
MSFT$MSFT.Close


msftWeekly
indexFormat(msftWeekly) <- ""
today <- as.POSIXlt(Sys.Date())
today$mweek
msftWeekly$MSFT.Close





# 5. Changing daily data to data in weeks
msftWeekly <- MSFT[endpoints(MSFT, "weeks")]
s51Weekly <- S51[endpoints(S51, "weeks")]
brkbWeekly <- BRKB[endpoints(BRKB, "weeks")]



#Testing to see if it can be 'seasonlized'
#MSFT
tData <- c(msftWeekly$MSFT.Close)
tsData <- ts(tData, start=c(2014, 1), frequency=52)   #Change freq to 52 for weeks
plot(tsData)
fitData <- forecast::auto.arima(tsData)
resultF <- forecast(fitData, 5)
plot(resultF, xlab="Year", ylab="Quantity", main="Forecasted Demand Item I")

#S51.SI
tData <- c(s51Weekly$`S51.SI.Close`)
tsData <- ts(tData, start=c(2014, 1), frequency=52)   #Change freq to 52 for weeks
plot(tsData)
fitData <- forecast::auto.arima(tsData)
resultF <- forecast(fitData, 5)
plot(resultF, xlab="Year", ylab="Quantity", main="Forecasted Demand Item II")

#BRK-B
tData <- c(brkbWeekly$`BRK-B.Close`)
tsData <- ts(tData, start=c(2014, 1), frequency=52)   #Change freq to 52 for weeks
plot(tsData)
fitData <- forecast::auto.arima(tsData)
resultF <- forecast(fitData, 5)
plot(resultF, xlab="Year", ylab="Quantity", main="Forecasted Demand Item III")




# 6. Changing the index from date to just its year
indexFormat(msftWeekly) <- "%Y"
indexFormat(s51Weekly) <- "%Y"
indexFormat(brkbWeekly) <- "%Y"





# 7. Populating to excel to be further 

#Ref: write.xlsx2(x, file, sheetName="Sheet1", col.names=TRUE, row.names=TRUE, append=FALSE, ...)
write.xlsx(msftWeekly$MSFT.Close, paste(tempwd,"/HarvestedData/Data1.xlsx", sep = ""),
           sheetName = "Sheet1", append=TRUE) 
write.xlsx(s51Weekly$`S51.SI.Close`, paste(tempwd,"/HarvestedData/Data1.xlsx", sep = ""),
           sheetName = "Sheet2", append=TRUE) 
write.xlsx(brkbWeekly$`BRK-B.Close`, paste(tempwd,"/HarvestedData/Data1.xlsx", sep = ""),
           sheetName = "Sheet3", append=TRUE) 




#Experimenting with chartseries
chartSeries(AAPL, multi.col=TRUE, theme="black")
AAPL
