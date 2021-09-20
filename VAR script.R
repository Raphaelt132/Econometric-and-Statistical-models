"3 Equation Vector Auto Regression"
#Loading relevant R packages
library(vars)
library(mFilter)
library(tseries)
library(TSstudio)
library(forecast)
library(tidyverse)
library(urca)

#Declaring variables as time series
#IR - Level
#Tenyr - Ten year bond yields % chg
#CPI - %chg in CPI
IR <- ts(FEDFUNDS$IR, start = c(2000,1,1), frequency = 12)
TENYR <- ts(FEDFUNDS$TENYR, start = c(2000,1,1), frequency = 12)
CPI <- ts(FEDFUNDS$CPI, start = c(2000,1,1), frequency = 12)

#Plotting the data 
#This gives us an idea of whether or not the VAR model is the correct specification. Can also help us visualize how the data appears
autoplot(IR)
autoplot(TENYR)
autoplot(CPI)


#Augmented dickey fuller for stationarity
#Econometric theory for VAR models suggest that data needs to be stationary (no spurious correlation) due to the inherent time dependency calculated in the model
adf.test(IR)
adf.test(TENYR)
adf.test(CPI)

#Building the VAR model

#Start by column binding the variables
#We build the VAR by arranging the three variables as a 3 x 3 vector
V2 <- cbind(IR, TENYR, CPI)
colnames(V2) <- cbind("IR", "TENYR", "CPI")

#Using AIC for optimal lag selection
#The AIC is an equation that helps us determine the optimal time series lags we should ascribe to the model
lagselect <- VARselect(V2, lag.max = 9, type="const")
lagselect$selection

#acf and pacf
#Stands for auto correlation function and partial auto correlation function
#Lets us see how persistent time dependency is within the variables and allows us to appropriately model for it
acf(IR, main = "ACF for Interest rates")
acf(TENYR, main = "ACF for Bond yields")
acf(CPI, main = "ACF for Iinflation")
pacf(IR, main = "PACF for Interest rates")
pacf(TENYR, main = "PACF for Bond yields")
pacf(CPI, main = "PACF for Inflation")

#VAR model: p is the lag length defined by the AIC
#Here I select the specifications for the VAR model. This line of code states that there are 2 lags, no seasonality and no exogenous variables
V1 <- VAR(V2, p=2, type="const", season=NULL, exog=NULL)
summary(V1)

#Test for stationarity
serial.test(V1, type="BG")

#Test for ARCH effects 
#Here I test for heteroskedasticity (non constant variance) within the model
arch.test(V1, lags.multi = 9, multivariate.only = TRUE)

#Test for normality of residuals
#Sees if the residuals assume a normal / gaussian distribution
normality.test(V1, multivariate.only = TRUE)

#Test for structural breaks
stab <- stability(V1, type = "OLS-CUSUM")
plot(stab)

#Granger causality
#Shows us if variables 'granger cause' each other
#Granger causality determines if past values of variable X can determine future values of variable Y
causality(V1, cause="IR")
causality(V1, cause="TENYR")
causality(V1, cause="CPI")

#Impulse response functions
#Shows us what happens to Y when variable X is shocked positively by one standard deviation
IRirf <- irf(V1, impulse="IR", response = "IR", n.ahead=12, boot=TRUE)
plot(IRirf, ylab = "IR", main="IR's shock to Interest Rates")

IRCPI <- irf(V1, impulse="IR", response = "CPI", n.ahead=12, boot=TRUE)
plot(IRCPI, ylab = "CPI", main="IR's shock to CPI")

IRTENYR <- irf(V1, impulse="IR", response = "TENYR", n.ahead=12, boot=TRUE)
plot(IRTENYR, ylab = "TENYR", main="IR's shock to Bond Yield")

CPIirf <- irf(V1, impulse="CPI", response = "CPI", n.ahead=12, boot=TRUE)
plot(CPIirf, ylab = "CPI", main="CPI's shock to CPI")

CPIIR <- irf(V1, impulse="CPI", response = "IR", n.ahead=12, boot=TRUE)
plot(CPIIR, ylab = "IR", main="CPI's shock to Interest Rates")

CPITENYR <- irf(V1, impulse="CPI", response = "TENYR", n.ahead=12, boot=TRUE)
plot(CPITENYR, ylab = "TENYR", main="CPI's shock to Bond Yield")

TENYRirf <- irf(V1, impulse="TENYR", response = "TENYR", n.ahead=12, boot=TRUE)
plot(TENYRirf, ylab = "TENYR", main="Bond Yield's shock to Bond Yield")

TENYRIR <- irf(V1, impulse="TENYR", response = "IR", n.ahead=12, boot=TRUE)
plot(TENYRIR, ylab = "IR", main="Bond Yield's shock to IR")

TENYRCPI <- irf(V1, impulse="TENYR", response = "CPI", n.ahead=12, boot=TRUE)
plot(TENYRCPI, ylab = "CPI", main="Bond Yield's shock to CPI")

#Variance Decomposition
#Shows us how much of the variation in Y is explinable by X1, X2, ..... Xn
FEVD1 <- fevd(V1, n.ahead = 10)
FEVD1
plot(FEVD1)

#Forecasting using the VAR
#Produces a time dynamic forecast based on previous fitted values of the model
forecast <- predict(V1, n.ahead = 12, ci = 0.95)

fanchart(forecast, names = "IR", main = "Fanchart for IR", 
xlab = "Horizon", ylab = "Interest Rate")

fanchart(forecast, names = "TENYR", main = "Fanchart for Ten year bond yields", 
xlab = "Horizon", ylab = "Ten Year Bond Yields")

fanchart(forecast, names = "CPI", main = "Fanchart for CPI", 
xlab = "Horizon", ylab = "CPI")

forecast
