"6 Equation VAR"

library(vars)
library(mFilter)
library(tseries)
library(TSstudio)
library(forecast)
library(tidyverse)
library(urca)

#Declaring variables as time series
INDPRO <- ts(Thesis_Data_set$INDPRO, start = c(2003,4,1), frequency = 4)
UNEM <- ts(Thesis_Data_set$UNEM, start = c(2003,4,1), frequency = 4)
CPI <- ts(Thesis_Data_set$CPI, start = c(2003,4,1), frequency = 4)
CONS <- ts(Thesis_Data_set$CONS, start = c(2003,4,1), frequency = 4)
GAP = ts(Thesis_Data_set$GAP, start = c(2003,4,1), frequency = 4)
EXPEC = ts(Thesis_Data_set$EXPEC, start = c(2003,4,1), frequency = 4)

#Plotting the data 
autoplot(INDPRO)
autoplot(UNEM)
autoplot(CPI)
autoplot(CONS)
autoplot(GAP)
autoplot(EXPEC)

#Augmented dickey fuller for stationarity
adf.test(INDPRO)
adf.test(UNEM)
adf.test(CPI)
adf.test(CONS)
adf.test(GAP)
adf.test(EXPEC)

#Building the VAR model

#Start by column binding the variables
V2 <- cbind(INDPRO, UNEM, CPI, CONS, GAP, EXPEC)
colnames(V2) <- cbind("INDPRO", "UNEM", "CPI", "CONS", "GAP", "EXPEC")

#Using AIC for optimal lag selection
lagselect <- VARselect(V2, lag.max = 9, type="const")
lagselect$selection

#acf and pacf
acf(INDPRO, main = "ACF for Industrial Production")
acf(UNEM, main = "ACF for Unemployment")
acf(CPI, main = "ACF for Inflation")
acf(CONS, main = "ACF for Consumption")
acf(GAP, main = "ACF for GAP")
acf(EXPEC, main = "ACF for EXPEC")

pacf(INDPRO, main = "PACF for Industrial Production")
pacf(UNEM, main = "PACF for Unemployment")
pacf(CPI, main = "PACF for Inflation")
pacf(CONS, main = "PACF for Consumption")
pacf(GAP, main = "PACF for GAP")
pacf(EXPEC, main = "PACF for EXPEC")

#VAR model: p is the lag length defined by the AIC
V1 <- VAR(V2, p=9, type="const", season=NULL, exog=NULL)
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
jotest = ca.jo(data.frame(INDPRO,UNEM,CPI,CONS,GAP,EXPEC), 
          type="eigen", K=2,ecdet="none",spec="longrun")
summary(jotest)

#Granger causality
causality(V1, cause="INDPRO")
causality(V1, cause="UNEM")
causality(V1, cause="CPI")
causality(V1, cause="CONS")
causality(V1, cause="GAP")
causality(V1, cause="EXPEC")

INDPRO<- irf(V1, impulse="INDPRO", response = "CPI", n.ahead=4, ortho=TRUE, boot=TRUE, runs=1000)
plot(INDPRO, ylab = "CPI", main="INDPRO's shock to CPI")

UNEM <- irf(V1, impulse="UNEM", response = "CPI", n.ahead=4, ortho=TRUE, boot=TRUE, runs=1000)
plot(UNEM, ylab = "CPI", main="Unemployment's shock to CPI")

CPI <- irf(V1, impulse="CPI", response = "UNEM", n.ahead=4, ortho=TRUE, boot=TRUE, runs=1000)
plot(CPI, ylab = "UNEM", main="CPI's shock to Unemployment")

CONS <- irf(V1, impulse="CONS", response = "CPI", n.ahead=4, ortho=TRUE, boot=TRUE, runs=1000)
plot(CONS, ylab = "CPI", main="Consumption's shock to CPI")

GAP <- irf(V1, impulse="GAP", response = "CPI", n.ahead=4, ortho=TRUE, boot=TRUE, runs=1000)
plot(GAP, ylab = "CPI", main="Output Gap's shock to CPI")

EXPEC <- irf(V1, impulse="EXPEC", response = "CPI", n.ahead=4, ortho=TRUE, boot=TRUE, runs=1000)
plot(EXPEC, ylab = "CPI", main="Inflation expectation's shock to CPI")

EXPEC2 <- irf(V1, impulse="CPI", response = "EXPEC", n.ahead=4, ortho=TRUE, boot=TRUE, runs=1000)
plot(EXPEC2, ylab = "EXPEC", main="CPI's shock to Inflation expectations")

#Variance Decomposition
FEVD1 <- fevd(V1, n.ahead = 10)
FEVD1
plot(FEVD1)

#Forecasting using the VAR
forecast <- predict(V1, n.ahead = 12, ci = 0.95)

fanchart(forecast, names = "INDPRO", main = "Fanchart for INDPRO", 
         xlab = "Horizon", ylab = "Interest Rate")

fanchart(forecast, names = "UNEM", main = "Fanchart for Ten year bond yields", 
         xlab = "Horizon", ylab = "Ten Year Bond Yields")

fanchart(forecast, names = "CPI", main = "CPI forecast", 
         xlab = "Horizon", ylab = "CPI")

forecast
