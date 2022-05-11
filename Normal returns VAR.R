"3 Equation VAR"

library(vars)
library(mFilter)
library(tseries)
library(TSstudio)
library(forecast)
library(tidyverse)
library(urca)

#Declaring variables as time series
#R - Portfolio returns
#Tyield - Ten year Treasury Yields % chg
#Oil - %chg in Oil

R <- ts(dailydata$Total, start = c(2000,1,4), frequency = 365)
Tyield <- ts(dailydata$`Treasury Yield`, start = c(2000,1,4), frequency = 365)
Oil <- ts(dailydata$`Oil Prices`, start = c(2000,1,4), frequency = 365)

#Plotting the data 
plot(R, main = "Returns")
plot(Tyield, main = "Treasury Yields")
plot(Oil, main = "Oil Prices (WTICO)")

#Augmented dickey fuller for stationarity
adf.test(R)
adf.test(Tyield)
adf.test(Oil)

#Building the VAR model

#Start by column binding the variables
V2 <- cbind(R, Tyield, Oil)
colnames(V2) <- cbind("R", "Tyield", "Oil")

#Using AIC for optimal lag selection
lagselect <- VARselect(V2, lag.max = 9, type="const")
lagselect$selection

#acf and pacf
acf(R, main = "ACF for Returns")
acf(Tyield, main = "ACF for Treasury Yields")
acf(Oil, main = "ACF for Iinflation")

pacf(R, main = "PACF for Returnss")
pacf(Tyield, main = "PACF for Treasury Yields")
pacf(Oil, main = "PACF for Inflation")

#VAR model: p is the lag length defined by the lag selection
V1 <- VAR(V2, p=2, type="const", season=NULL, exog=NULL)
summary(V1)

#Test for stationarity
serial.test(V1, type="BG")

#Test for ARCH effects 
arch.test(V1, lags.multi = 9, multivariate.only = TRUE)

#Test for normality of residuals
normality.test(V1, multivariate.only = TRUE)

#Test for structural breaks
stab <- stability(V1, type = "OLS-CUSUM")
plot(stab)

#Johansen Test for cointegration
jotest = ca.jo(data.frame(R,Tyield,Oil), type="eigen", K=2,ecdet="none",spec="longrun")
summary(jotest)

#Granger causality
causality(V1, cause="R")
causality(V1, cause="Tyield")
causality(V1, cause="Oil")

#Individual causality
grangertest(R,Tyield,order=2)
grangertest(R,Oil,order=2)
grangertest(Oil,R,order=2)
grangertest(Tyield, Oil, order=2)
grangertest(Oil,Tyield,order=2)

#Impulse response functions
RRf <- irf(V1, impulse="R", response = "R", n.ahead=5, ortho=TRUE, boot=TRUE)
plot(RRf, ylab = "R", main="Return's shock to Returns")

ROil <- irf(V1, impulse="R", response = "Oil", n.ahead=5,ortho=TRUE, boot=TRUE)
plot(ROil, ylab = "Oil", main="Return's shock to Oil")

RTyield <- irf(V1, impulse="R", response = "Tyield", n.ahead=5,ortho=TRUE, boot=TRUE)
plot(RTyield, ylab = "Tyield", main="R's shock to Treasury Yield")

OilRf <- irf(V1, impulse="Oil", response = "Oil", n.ahead=5, ortho=TRUE, boot=TRUE)
plot(OilRf, ylab = "Oil", main="Oil's shock to Oil")

OilR <- irf(V1, impulse="Oil", response = "R", n.ahead=5,ortho=TRUE, boot=TRUE)
plot(OilR, ylab = "R", main="Oil's shock to Returns")

OilTyield <- irf(V1, impulse="Oil", response = "Tyield", n.ahead=5,ortho=TRUE, boot=TRUE)
plot(OilTyield, ylab = "Tyield", main="Oil's shock to Treasury Yield")

TyieldRf <- irf(V1, impulse="Tyield", response = "Tyield", n.ahead=5,ortho=TRUE, boot=TRUE)
plot(TyieldRf, ylab = "Tyield", main="Treasury Yield's shock to Treasury Yields")

TyieldR <- irf(V1, impulse="Tyield", response = "R", n.ahead=5,ortho=TRUE, boot=TRUE)
plot(TyieldR, ylab = "R", main="Treasury Yield's shock to Returns")

TyieldOil <- irf(V1, impulse="Tyield", response = "Oil", n.ahead=5,ortho=TRUE, boot=TRUE)
plot(TyieldOil, ylab = "Oil", main="Treasury Yield's shock to Oil")

#Variance Decomposition
FEVD1 <- fevd(V1, n.ahead = 10)
FEVD1
plot(FEVD1)

#Forecasting using the VAR
forecast <- predict(V1, n.ahead = 5, ci = 0.95)

fanchart(forecast, names = "R", main = "Fanchart for R", 
         xlab = "Horizon", ylab = "Returns")

fanchart(forecast, names = "Tyield", main = "Fanchart for Ten year Treasury Yields", 
         xlab = "Horizon", ylab = "Ten Year Treasury Yields")

fanchart(forecast, names = "Oil", main = "Fanchart for Oil", 
         xlab = "Horizon", ylab = "Oil")

forecast
