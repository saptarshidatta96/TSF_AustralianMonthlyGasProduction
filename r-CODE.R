#install.packages("forecast")
library(forecast)
#write.csv(gas, file = "AusGas.csv")
plot(gas)
str(gas)
frequency(gas)
gas1 <- ts(gas, start=c(1956,1),frequency =12)
str(gas1)
plot.ts(gas1)

#create train and test data
gas.train <- window(gas1, end = c(1994,12))
str(gas.train)
gas.test<- window(gas1, start = c(1995,1))
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
ses.gas = ses(gas.train, alpha = NULL,
              h = 24,level = c(80, 95))
autoplot(ses.gas)
accuracy(ses.gas, gas.test)
ses.gas$model
ses.gas$fitted
ses.forecast=forecast(ses.gas)
ses.forecast


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
holt.gas <- holt(gas.train, h=24,
                 level = c(80, 95),alpha = NULL, beta= NULL)
Holt.forecast=forecast(holt.gas)
Holt.forecast
autoplot(holt.gas)
Holt.forecast$model
holt.gas$mean
holt.gas$fitted
accuracy(holt.gas, gas.test)

#holt winter's model
HoltWinters.gas = HoltWinters(gas.train, alpha = NULL, beta = NULL, gamma = NULL,
                              seasonal = c("additive", "multiplicative"))
HW.forecast=forecast(HoltWinters.gas)
HW.forecast
HW.forecast$model
HW.forecast$mean
HW.forecast$fitted
accuracy(HW.forecast, gas.test)
autoplot(HW.forecast)

#Checking for Stationary series
library(tseries)
autoplot(gas1)
acf(gas1, lag.max = 50)
adf.test(gas1)

#Differencing the series
diff.TS=diff(diff(gas1,lag =12),lag =1)
autoplot(diff.TS)
adf.test(diff.TS)
acf(diff.TS, lag.max=50)
acf(diff.TS, lag.max=50, plot = FALSE)
pacf(diff.TS, lag.max=50)
pacf(diff.TS, lag.max=50, plot = FALSE)

#ARIMA Model
gas.arima.fit <- arima(diff.TS, c(3, 0, 0))
gas.arima.fit
Box.test(gas.arima.fit$residuals, lag=30, type="Ljung-Box")
autoplot(forecast(gas.arima.fit, h=3))

