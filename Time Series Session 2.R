## Economics & Finance Lab Series 
## Session 4: Time Series 
##Best to go thru MM notes on index, forecasting and timeseries first

getwd()
setwd()

# 1. Creating a time-series object in R

?ts  #Using ts makes your data more readable
sales <- c(18, 33, 41, 7, 34, 35, 24, 25, 24, 21, 25, 20,
           22, 31, 40, 29, 25, 21, 22, 54, 31, 25, 26, 35)
tsales <- ts(sales, start=c(2003, 1), frequency=12)
sales
tsales

#Compare the two
plot(sales)
plot(tsales)


start(tsales)
end(tsales)
frequency(tsales)
tsales.subset <- window(tsales, start=c(2003, 5), end=c(2004, 6))
tsales.subset

# 2. Smoothing and Seasonal Decomposition
install.packages("forecast")
library(forecast)
opar <- par(no.readonly=TRUE)
par(mfrow=c(2,2))
ylim <- c(min(Nile), max(Nile))
plot(Nile, main="Raw time series")
plot(ma(Nile, 3), main="Simple Moving Averages (k=3)", ylim=ylim)
plot(ma(Nile, 7), main="Simple Moving Averages (k=7)", ylim=ylim)
plot(ma(Nile, 15), main="Simple Moving Averages (k=15)", ylim=ylim)
par(opar)

# 2.1 Seasonal Decomposition 



#Custom
plot(AirPassengers)
plot(AirPassengers, main="Stapler No. 28")
Demand <- AirPassengers
remove(mAirPassengers)

plot(Demand, main="Stapler No. 28", ylab="Demand")
Demand

row.names(Demand)
colnames(Demand)

Demand[,1]
#




lAirPassengers <- log(AirPassengers)
plot(lAirPassengers, ylab="log(AirPassengers)")
fit <- stl(lAirPassengers, s.window="period")
#Here, we use the 'stl' function.

plot(fit)
fit$time.series             #here we log it
exp(fit$time.series)           #then we apply exp. recall math1
# $ = means targeting. so here we are targeting the time.series. fitting it to that.


par(mfrow=c(2,1))
monthplot(AirPassengers, xlab="", ylab="")
#we get the range for each month. The line is the average (mean)

seasonplot(AirPassengers, year.labels="TRUE", main="")
#Messy but easier to visualise except when it crossover fml
#as we can see both cases, july and aug its the highest


# 3 Exponential Forecasting Models 
?xts
# 3.1 Simple Exponential Smoothin 
install.packages("forecast")
library(forecast)
fit <- ets(nhtemp, model="ANN")
fit
forecast(fit, 1)
#Forecast is a very impt function here, cos its your predicting function
#Specify the number to forecast the no of years. 1 = one year.

plot(forecast(fit, 1), xlab="Year",
       ylab=expression(paste("Temperature (", degree*F,")",)),
       main="New Haven Annual Mean Temperature")
#The dark blue is the area the program is predicting it to have the highest chance of being attained.
#So if you put more periods like 12, it will predict for 12 periods.
#Each shade denotes different chances low 80 to high 80 and low 95 and high 96. Recall confidence level


accuracy(fit)  
#The accuracy() function will calculate the errors for the dataset. You can then analyse it. RECALL MM notes.


# 3.2	Holt and Holt-Winters exponential smoothing
plot(AirPassengers)  #original

#ver1
fit <- ets(log(AirPassengers), model="AAA")
fit
accuracy(fit)
pred <- forecast(fit, 5)      #So we predicting for five periods.
pred
plot(pred, main="Forecast for Air Travel", ylab="Log(AirPassengers)", xlab="Time")
#You get a very nice prediction. Small range and going upwards

#ver2
fit <- ets(log(AirPassengers), model="AAN")
fit
accuracy(fit)
pred <- forecast(fit, 5)      #So we predicting for five periods.
pred
plot(pred, main="Forecast for Air Travel", ylab="Log(AirPassengers)", xlab="Time")
##The diff btw the two is the model used. You can see here that using a diff model will
#greatly affect the outcome of our prediction.


pred$mean <- exp(pred$mean)
pred$lower <- exp(pred$lower)
pred$upper <- exp(pred$upper)
p <- cbind(pred$mean, pred$lower, pred$upper)
dimnames(p)[[2]] <- c("mean", "Lo 80", "Lo 95", "Hi 80", "Hi 95")
p
##We just put it like this for easy viewing in the console.



# 3.3	The ets() function and automated forecasting

fit <- ets(JohnsonJohnson)
fit

#Here cos we never spcify, we are just using the default forecast periods.
plot(forecast(fit), main="Johnson & Johnson Forecasts",
       ylab="Quarterly Earnings (Dollars)", xlab="Time", flty = 2)
?forecast
# 4.	ARIMA forecasting models
save.image()
# 4.1	Prerequisite concepts
# 4.2	ARMA and ARIMA models

# Ensuring that the time series is stationary 
library(forecast)
library(tseries)
plot(Nile)
#Just a regular plot of nile

ndiffs(Nile)
dNile <- diff(Nile)
plot(dNile)
#The nile minus the nile-1

adf.test(dNile)

# Identify one or more reasonable models
Acf(dNile)    #Considered as 'Zero after lag'
Pacf(dNile)   #Considered as 'Trails off to zero'
##Based on this refer to pdf pg 20, and you can decide where to put your 1 and 0.
#For here, 0 will be first.

# Fit the Models
fit <- arima(Nile, order=c(0,1,1))
fit
accuracy(fit)

# Evaluating Model Fit
qqnorm(fit$residuals)
qqline(fit$residuals)
Box.test(fit$residuals, type="Ljung-Box")


# Making Forecasts 
forecast(fit, 3)
plot(forecast(fit, 3), xlab="Year", ylab="Annual Flow")
##Useful cos from something that you can't read or infer, comparing the normal
#and ARIMA model forecast we can get some rough forecast. Better than nothing.



# 4.3	Automated ARIMA forecasting

library(forecast)
fit <- auto.arima(sunspots)
#auto.arima means letting the com decide on the best model for you. It does everything for you.

fit    #Can see that R will use arima 2,1,2 to perform the focus
forecast(fit, 3)
accuracy(fit)

?sunspots

#Attempting to plot
plot(forecast(fit, 3), xlab="Year", ylab="Monthly sunspot numbers")


#Guy's plot
plot(forecast(fit, 3), xlab="Time", ylab="Sunspots")
plot(forecast(fit, 50), xlab="Time", ylab="Sunspots")
#The range gets wider over time cos its getting harder to predict the future the further down the road.


## END OF SESSION ## 

