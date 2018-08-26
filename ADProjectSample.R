
#Set directory
tempwd <- getwd()
setwd(tempwd)

# 1. Install packages
install.packages(c ("RODBC","R2HTML"))
install.packages("forecast")

library(RODBC)
library(R2HTML)
library(forecast)
library(xts)



# Dont use
?odbcConnect
cn<-odbcConnect('dsn', uid='',pwd='')
coreKPI<-sqlFetch(cn,'Ch1_Core_KPIs_vw')
odbcClose(cn)

# MEHHHHHHH - Dont use
install.packages("odbc")
library(odbc)
?dbConnect
con <- dbConnect(odbc(),
                 Driver = "SQLServer",
                 Server = "Microsoft SQL Server",
                 Database = "NorthWiindBaby",
                 UID = "",
                 PWD = "",
                 Port = 0000)




# 2. Connecting to the DB
dbconnection <- odbcDriverConnect("Driver=ODBC Driver 11 for SQL Server;
                Server=127.0.0.1; Database=Bookshop; Uid=<<user>>; Pwd=<<password>>; trusted_connection=yes")
#Does not work well: coreKPI<-sqlGetResults(dbconnection, "SELECT * FROM [NorthWiindBabyy~].[dbo].[Sales by Category]")
coreKPI<-sqlQuery(dbconnection, "SELECT Quantity FROM [Bookshop].[dbo].[Sheet1]")
odbcClose(dbconnection)

#Check data if successfully retrieved
coreKPI

#As you can see its not properly formatted into ts or xts to be plotted
plot(coreKPI)

#Gotta format properly to ts datatype
data(sunspots)
cKPI_xts <- as.xts(sunspots)
plot(sunspots)
plot(cKPI_xts)


coreKPI
coreKPI[144,]
coreKPI$Quantity

abc <- c(coreKPI$Quantity)
abc
tabc <- ts(abc, start=c(2003), frequency=12)
tabc
class(tabc)
plot(tabc)
monthdays(tabc)
month.name(tabc)

library(zoo)
as.yearmon(time(tabc))




# Use this to produce a html file with the data populated

tmp <- 100*(coreKPI$TYtd-coreKPI$LYtd)/coreKPI$LYtd
tmp <- round(tmp, 2)
tmp <- paste(tmp,'% ',sep="")
coreKPI$Growth <- paste(tmp, coreKPI$Growth)

#Specify your column names
colnames(coreKPI)<-c('Category ID','Category Name','Product Name','Product Sales', '')
coreKPI

#Finding a dir to paste the html file
htmldir <- paste(Sys.getenv("HOME"), "\\R Analytics\\R-2-ASP\\Reporting\\Content\\html\\", sep="")
htmldir <- paste(tempwd, "/html", sep="")
htmldir

#This method below does not crash when there is an existing dir. It just shows a warning [Good exception handler]
dir.create(file.path(htmldir), showWarnings = FALSE)
setwd(file.path(htmldir))
setwd(htmldir)
getwd()

#Specifying file name
pageFile <- 'plotChart.html'
imageFile <- 'chart.png'

#Checking if file with specified name exist in the working directory, if it does, remove it
if(file.exists(pageFile)) file.remove(pageFile)
if(file.exists(imageFile)) file.remove(imageFile)


#Create your image with your plot inside it
png(filename=paste(tempwd, "/html/", imageFile, sep=""))   #Specify where the file is
plot(tabc)
dev.off()
chartSeries(sunspots, multi.col=TRUE, theme="white")
library("quantmod")

##Actually, just use the image can liao


#Creating your HTML file using retrieved data (coreKPI), name specified (filename)
#The other four parameters are merely formating commands
HTML(tabc, file = pageFile, nsmall=2, decimal.mark='.', row.names=FALSE, Border='0')


HTMLInsertGraph(imageFile, file=pageFile, caption="Sample discrete distribution plot")
source('paste(tempwd, \'/Whut.R\')')
print(tempwd+1)
paste(tempwd, '/Whut.R', sep='')
source('paste(tempwd, '/Whut.R/', sep=/'/')')

HTMLplot(Caption = "Sup", file = pageFile, append = TRUE,
         GraphDirectory = paste(tempwd, "/html/", sep=""), GraphFileName = imageFile, 
         GraphSaveAs = "png", GraphBorder = 1,
         Align = "center", Width = 500, Height = 500, WidthHTML = NULL, HeightHTML = NULL,
         GraphPointSize = 12, GraphBackGround = "white", GraphRes = 72)



