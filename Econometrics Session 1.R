## ECONOMICS AND FINANCE LAB SERIES ##
## ECONOMETRICS ##
##Recap regression first before proceeding

getwd()
setwd()

# Install Packages
install.packages('car')
install.packages('MASS')
install.packages('effects')
install.packages('gvlma')
install.packages('leaps')

# Load Packages
library(car)
library(MASS)
library(effects)
library(gvlma)
library(leaps)
View(women)            #To check if its working.


# Simple Linear Regression Analysis
fit <- lm(weight ~ height, data=women)   #we want to see the relation between height of women and their weight.
summary(fit)    #This will print. Height is your gradient. Recall y=mx+c! 
#If women increase weight by 1kg, height will increase by 3.45.


women$weight
fitted(fit)
#will realiise that its close to their actual weight

residuals(fit)
plot(women$height,women$weight,
       xlab="Height (in inches)",
       ylab="Weight (in pounds)")
abline(fit)  #This will draw a straight line so you can see the data trend
#Can clearly see there is a relationship.


# Polynomial Regression - To get a better result. A more straight line. Better trend.
fit2 <- lm(weight ~ height + I(height^2), data=women)
summary(fit2)
#Recall this formula: ax^2+bx+c
#Its also better cos the rsquared is closer to 1

plot(women$height,women$weight,
       xlab="Height (in inches)",
       ylab="Weight (in lbs)")
lines(women$height,fitted(fit2))


#Can also use scatterplot to display the resutls
scatterplot(weight ~ height, data=women,
            spread=FALSE, smoother.args=list(lty=2), pch=19,
            main="Women Age 30-39",
            xlab="Height (inches)",
            ylab="Weight (lbs.)")


# Multi-Linear Regression   -   We are now looking at multiple variables and find correalation
states <- as.data.frame(state.x77[,c("Murder", "Population", "Illiteracy", "Income", "Frost")])
cor(states)        
#This here will show the relationship between each of the variables.
#If look at illiteracy and murder, its very close to 1. SO theres a strong correlation.

library(car)
#using scatterplot to view the correlation 
scatterplotMatrix(states, spread=FALSE, smoother.args=list(lty=2),
                    main="Scatter Plot Matrix")
#If the image is too small and you want to see it begger, export it and save it on pdf.


#This here will find std.error, t-value etc..
fit3 <- lm(Murder ~ Population + Illiteracy + Income + Frost,
            data=states)
summary(fit3)
#If you look at illiteracy, its very big. Theres three star, so theres a strong relationship.


# Multi-Linear Regression with Interactions(Meaning two variables combine will give a new value)
fit4 <- lm(mpg ~ hp + wt + hp:wt, data=mtcars)
mtcars
summary(fit4)
#recall: ax+by+cxy
#To see mileage, we will see the horsepower and weight.

library(effects)
plot(effect("hp:wt", fit4,, list(wt=c(2.2,3.2,4.2))), multiline=TRUE)
#This will plot the relationship. Can see that as you increase your weight,
#there will be lesser effect on horsepower.


# Regression Diagnostics

fit5 <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)
confint(fit5)
#If there is alot of zero infront, then its insignificant
#Those that have numbers instead like illiteracy and murder, means that there is high significance.


fit6 <- lm(weight ~ height, data=women)
par(mfrow=c(2,2))
plot(fit6)

fit7 <- lm(weight ~ height + I(height^2), data=women)
par(mfrow=c(2,2))   #This par function will show your regression in four of these way then you decide if you need outliers or not
plot(fit7)


fit8 <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)
par(mfrow=c(2,2))
plot(fit8)

  # Normality
library(car)
fit9 <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)
qqPlot(fit9, labels=row.names(states), id.method="identify",
       simulate=TRUE, main="Q-Q Plot")
#qqplot is useful if you want to track your plot and see the values by moving the mouse.


states["Nevada",]
fitted(fit9) ["Nevada"]


#This is the whole command for your histogram
residplot <- function(fit9, nbreaks=10) {
  z <- rstudent(fit9)
  hist(z, breaks=nbreaks, freq=FALSE,
       xlab="Studentized Residual",
       main="Distribution of Errors")
  rug(jitter(z), col="brown")
  curve(dnorm(x, mean=mean(z), sd=sd(z)),
        add=TRUE, col="blue", lwd=2)
  lines(density(z)$x, density(z)$y,
        col="red", lwd=2, lty=2)
  legend("topright",
         legend = c( "Normal Curve", "Kernel Density Curve"),
         lty=1:2, col=c("blue","red"), cex=.7)
}
residplot(fit9)

  # Durbin Watson Test 
durbinWatsonTest(fit9)
#lag means youre comparing with the one next to it.
#But here there is no significance thats why p-value is only 0.236


  # Homoscedasticity Test aka non-constant variance (ncv) test
ncvTest(fit9)
spreadLevelPlot(fit9)  #will tell you if you need to change the power
#if power transformation is zero, you need to up your log


# Global Validation of Linear Model Assumption
library(gvlma)
gvmodel <- gvlma(fit9)
summary(gvmodel)
# This is useful to check if the regression is good or not - a summary basically
#But you cannot rely on it solely cos you need to investigate further
#best to do this at the end just to compare.



# Multicollinearity Test - Checking if any problem with multicollinearity
#It will help you test if the variables are a direct example
#aka Variance inflexion factor (vif)
vif(fit9)
sqrt(vif(fit9)) > 2 


#all these are very subjective cos it really depends on what you want
#eg: how you define an outlier.


# Outliers Test
library(car)
outlierTest(fit9)



# Influential Observations 

  # Cook's Distance - Playing around with terms and seeing how we wanna set our outliers
cutoff <- 4/(nrow(states)-length(fit9$coefficients)-2)   #Change values to change coefficcient etc
plot(fit9, which=4, cook.levels=cutoff)
abline(h=cutoff, lty=2, col="red")

?abline #If you wanna find out more about it.
#lty = linetype



  # Added-Variable Plot
#This will show you which of the datasets you need to look out for
#by right can find a way to put names so can easily identify the diff states
avPlots(fit9, ask=FALSE, id.method="identify")


  # Influence Plot
influencePlot(fit, id.method="identify", main="Influence Plot",
              sub="Circle size is proportional to Cook's distance")

# Comparing Models

  # ANOVA function
fit10 <- lm(Murder ~ Population + Illiteracy + Income + Frost,
             data=states)
fit11 <- lm(Murder ~ Population + Illiteracy, data=states)
anova(fit11, fit10)
#For ANNOVE, look at the F-Value. Here its close to 1, so its very important.


  # using AIC - Arhcaic Information Criteria
fit13 <- lm(Murder ~ Population + Illiteracy + Income + Frost,
              data=states)
fit14 <- lm(Murder ~ Population + Illiteracy, data=states)
AIC(fit13,fit14)
#Look at the smaller AIC, as it will indicate the bigger regression btw the two after perf ANNOVA.



# Variable Selection

  # Stepwise Regression
#Need both MASS and leaps package to calculate stepwise regression
library(MASS)

fit15 <- lm(Murder ~ Population + Illiteracy + Income + Frost,
              data=states)
stepAIC(fit15, direction="backward")
#To see if changing the variable will affect the regression.
#We see yet again popln and illiteracy as the highest.
#Here they have the higher AIC.


library(leaps)
leaps <-regsubsets(Murder ~ Population + Illiteracy + Income +
                     Frost, data=states, nbest=4)
plot(leaps, scale="adjr2")
#Thos with the highest adjr are the best to look at and analyse further.


library(car)
subsets(leaps, statistic="cp",
        main="Cp Plot for All Subsets Regression")
abline(1,1,lty=2,col="red")
#Dotted red line is your gradient from 1.0 to 1.0 (its actually 45 degrees - Just draw that way so abit hard to see)
#Frost and income are close to the red dotted line.
#Help with finding out which variables you wanna use to analyse further.


## End of Session ##










