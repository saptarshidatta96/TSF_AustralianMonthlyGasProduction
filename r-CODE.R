install.packages("forecast")
library(forecast)
plot(gas)
str(gas)
gas1 <- ts(gas, start=c(1956),frequency =1)
plot.ts(gas1)

#create train and test data
gas.train <- window(gas1, end = c(1993))
gas.test<- window(gas1, start = c(1994))

#simple moving average
library(TTR)
Gas.SMA <- SMA(gas1,n=13)
ts.plot(Gas.SMA, gas1, gpars = list(col = c("red", "black")))

#simple exponential smoothing #no trend no seasonality
#constant forecast
ses.gas = ses(gas.train, alpha = 0.2, h=3)
autoplot(ses.gas)
accuracy(ses.gas, gas.test)
ses.gas$model
ses.gas$fitted

#lag 1 difference #ses on diff sales data
gas.dif.train <- diff(gas.train)
gas.dif.test <- diff(gas.test)

autoplot(gas.dif.train)
ses.msales.dif <- ses(msales.dif.train, h = 2)
ses.msales.dif$model

accuracy(ses.msales.dif, msales.dif.test)
autoplot(ses.msales.dif)

#holt's model with level and trend but no seasonality
#double exponential smoothing
holt.gas <- holt(gas.train, h = 3)
autoplot(holt.gas)
autoplot(forecast(holt.gas))

holt.gas$model
holt.gas$mean
holt.gas$fitted
accuracy(holt.gas, gas.test)
