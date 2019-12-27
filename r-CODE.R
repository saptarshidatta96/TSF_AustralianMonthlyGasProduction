install.packages("forecast")
library(forecast)
plot(gas)
str(gas)
gas1 <- ts(gas)
plot.ts(gas1)
library(TTR)
Gas.SMA <- SMA(gas1,n=13)
plot.ts(Gas.SMA)

# double exponential - models level and trend
fit <- HoltWinters(gas1, gamma=FALSE)

forecast(fit,3)
plot(forecast(fit,3))
