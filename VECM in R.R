#VECM in R
#Justin S. Eloriaga

install.packages("tsDyn")
library(tsDyn)
library(vars)

########################################
#JOHANSEN COINTEGRATION in R
########################################

#Calling the packages for use

library(urca)
library(forecast)
library(tidyverse)



#Loading the Dataset

data <- read_csv(file.choose())
head(data)

#Declare the Time Series Objects

GDP <- ts(data$lnGDP, start = c(2003,1,31), frequency = 4)
CPI <- ts(data$lnCPI, start = c(2003,1,31), frequency = 4)
M3 <- ts(data$lnM3, start = c(2003,1,31), frequency = 4)

#Creating our System

dset <- cbind(GDP,CPI,M3)

#Selecting the Optimal Number of Lags (Recall, this is p - 1)

lagselect <- VARselect(dset, lag.max = 7, type = "const")
lagselect$selection
lagselect$criteria
#Since 5 came up the most, we use (5-1) or 4 lags

ctest1t <- ca.jo(dset, type = "trace", ecdet = "const", K = 4)
summary(ctest1t)

ctest1e <- ca.jo(dset, type = "eigen", ecdet = "const", K = 4)
summary(ctest1e)

#Hence, we have one cointegrating relationship in this model

######################################################################

#Build the VECM Model

Model1 <- VECM(dset, 4, r = 1, estim =("2OLS"))
summary(Model1)

#Diagnostic Tests

#Need to Transform VECM to VAR

Model1VAR <- vec2var(ctest1t, r = 1)

#Serial Correlation

Serial1 <- serial.test(Model1VAR, lags.pt = 5, type = "PT.asymptotic")
Serial1

#ARCH Effects

Arch1 <- arch.test(Model1VAR, lags.multi = 15, multivariate.only = TRUE)
Arch1

#Normality of Residuals

Norm1 <- normality.test(Model1VAR, multivariate.only = TRUE)
Norm1

#Impulse Response Functions

M3irf <- irf(Model1VAR, impulse = "GDP", response = "M3", n.ahead = 20, boot = TRUE)
plot(M3irf, ylab = "M3", main = "GDP's shock to M3")

CPIirf <- irf(Model1VAR, impulse = "GDP", response = "CPI", n.ahead = 20, boot = TRUE)
plot(CPIirf, ylab = "CPI", main = "GDP's shock to CPI")

GDPirf <- irf(Model1VAR, impulse = "GDP", response = "GDP", n.ahead = 20, boot = TRUE)
plot(GDPirf, ylab = "GDP", main = "GDP's shock to GDP")

#Variance Decomposition

FEVD1 <- fevd(Model1VAR, n.ahead = 10)
plot(FEVD1)



