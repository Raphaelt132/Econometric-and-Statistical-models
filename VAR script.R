"3 Equation US VAR (Interest,Inflation,Bond Yield)"

library(vars)
library(mFilter)
library(tseries)
library(TSstudio)
library(forecast)
library(tidyverse)
library(urca)

ggsave("graph.png", width=15.10, height=9.5, units="in")

#Declaring variables as time series
IR <- ts(FEDFUNDS$IR, start = c(2000,1,1), frequency = 12)
TENYR <- ts(FEDFUNDS$TENYR, start = c(2000,1,1), frequency = 12)
CPI <- ts(FEDFUNDS$CPI, start = c(2000,1,1), frequency = 12)

#Plotting the data 
autoplot(IR)
autoplot(TENYR)
autoplot(CPI)

#Augmented dickey fuller for stationarity
adf.test(IR)
adf.test(TENYR)
adf.test(CPI)

#Building the VAR model

#Start by column binding the variables
V2 <- cbind(IR, TENYR, CPI)
colnames(V2) <- cbind("IR", "TENYR", "CPI")

#Using AIC for optimal lag selection
lagselect <- VARselect(V2, lag.max = 9, type="const")
lagselect$selection

#VAR model: p is the lag length defined by the AIC
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

#Granger causality
causality(V1, cause="IR")
causality(V1, cause="TENYR")
causality(V1, cause="CPI")

#Impulse response functions
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
FEVD1 <- fevd(V1, n.ahead = 10)
FEVD1
plot(FEVD1)

#Forecasting using the VAR
forecast <- predict(V1, n.ahead = 12, ci = 0.95)

fanchart(forecast, names = "IR", main = "Fanchart for IR", 
xlab = "Horizon", ylab = "Interest Rate")

fanchart(forecast, names = "TENYR", main = "Fanchart for Ten year bond yields", 
xlab = "Horizon", ylab = "Ten Year Bond Yields")

fanchart(forecast, names = "CPI", main = "Fanchart for CPI", 
xlab = "Horizon", ylab = "CPI")

forecast
