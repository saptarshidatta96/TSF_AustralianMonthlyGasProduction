install.packages("forecast")
library(forecast)
write.csv(gas, file = "AusGas.csv")
plot(gas)
str(gas)
gas1 <- ts(gas, start=c(1956),frequency =12)
plot.ts(gas1)

#create train and test data
gas.train <- window(gas1, end = c(1993))
str(gas.train)
gas.test<- window(gas1, start = c(1994))
str(gas.test)

#simple moving average
library(TTR)
Gas.SMA <- SMA(gas1,n=13)
ts.plot(Gas.SMA, gas1, gpars = list(col = c("red", "black")))

#Decomposition
gasComp = decompose(gas1) #Additive
plot(gasComp)

gasComp = decompose(gas1,"multiplicative") #multiplicative
plot(gasComp)


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
ses.gas.dif <- ses(gas.dif.train, h = 3)
ses.gas.dif$model
ses.gas.dif$fitted
accuracy(ses.gas.dif, gas.dif.test)
autoplot(ses.gas.dif)

#holt's model with level and trend but no seasonality
#double exponential smoothing
holt.gas <- holt(gas.train, h = 3)
autoplot(holt.gas)
autoplot(forecast(holt.gas))
holt.gas$model
holt.gas$mean
holt.gas$fitted
accuracy(holt.gas, gas.test)

#holt winter's model
HoltWinters.gas = HoltWinters(gas.train)
HW.forecast=forecast(HoltWinters.gas)
HW.forecast
HW.forecast$model
HW.forecast$mean
HW.forecast$fitted
accuracy(HW.forecast, gas.test)
#ARIMA Forecasting
