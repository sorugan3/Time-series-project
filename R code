#read the timeseries data which is in csv format
cpidata <- read.csv(file="Medical care data.csv", header = T)

#select only relevant rows/columns
cpidata <- cpidata[1:10,1:13]

#stack the columns within data
library(data.table)
cpidata <- cbind(cpidata[,1], stack(transpose(cpidata[2:13])))

#create the monthly timeseries
cpidata.ts <- ts(cpidata$values , frequency = 12 , start=c(2005,1))

#check the structure of the data generated above
str(cpidata.ts)

##data splitting for building and then validating the model##
#8 years of data for building the model
cpidata.ts.8yrs <- ts(cpidata.ts[1:96] , frequency = 12 , start = c(2005,1))
#2 years of data for validating the model
cpidata.ts.2yrs <- ts(cpidata.ts[97:120] , frequency = 12 , start = c(2013,1))

#plot the time series for 8 years
plot(cpidata.ts.8yrs , xlab="2005 Jan to 2012 Dec", ylab="CPI-All Urban Consumers(Medical Care)")

#generate linearly detrended data of 8 years
library(pracma)
detrended.data <- pracma::detrend(cpidata.ts[1:96] , "linear")

#convert detrended data to ts
detrended.data.ts <- ts(detrended.data , frequency = 12 , start = c(2005,1))

#plot the detrended data
plot(detrended.data.ts)

#look at the Acf and Pacf plots for 8 years of data
library(forecast)
Acf(cpidata.ts.8yrs)
Pacf(cpidata.ts.8yrs)

#look at the Acf and Pacf plots of non-seasonal diff at lag1
cpidata.8.diff <- diff(cpidata.ts.8yrs , differences = 1)
Acf(cpidata.8.diff)
Pacf(cpidata.8.diff)

#look at Acf and Pacf plots for first diff applied together with seasonal diff
cpidata.8.seasonaldiff <- diff(cpidata.8.diff , 12)
tsdisplay(cpidata.8.seasonaldiff)

#Fitting appropriate seasonal arima models based on the previous results 
model1 <- arima(cpidata.ts.8yrs, order= c(1,1,1), seasonal = list(order=c(0,1,1), period=12))
model2 <- arima(cpidata.ts.8yrs, order= c(0,1,1), seasonal = list(order=c(0,1,1), period=12))

#summary of model1 and model2
summary(model1)
summary(model2)

#scatterplot of residuals of both models
plot.default(model1$residuals)
plot.default(model2$residuals)

#Acf and pacf plots of both models
Acf(model1$residuals)
Pacf(model1$residuals)
Acf(model2$residuals)
Pacf(model2$residuals)

#forecast for two years using both models
forecast1 <- forecast.Arima(model1, h=24)
forecast2 <- forecast.Arima(model2, h=24)

#plot the forecasts
plot.forecast(forecast1)
plot.forecast(forecast2)

#perform the Ljung-Box test for forecast1 and forecast2
Box.test(forecast1$residuals, lag=24, type="Ljung-Box")
Box.test(forecast2$residuals, lag=24, type="Ljung-Box")

#Acf and Pacf plots for residuals of forecast1 and forecast2
Acf(forecast1$residuals)
Pacf(forecast1$residuals)
Acf(forecast2$residuals)
Pacf(forecast2$residuals)

#Using auto.arima since the Acf and Pacf plots of residuals of forecasts are not satisfactory
auto.arima(cpidata.ts.8yrs, stepwise = F, approximation = F)
model3 <- arima(cpidata.ts.8yrs, order= c(1,1,1), seasonal = list(order=c(2,0,0), period=12))

#forecast for two years using the new model
forecast3 <- forecast.Arima(model3, h=24)
plot.forecast(forecast3)

#Acf and Pacf plots for new model
Acf(model3$residuals)
Pacf(model3$residuals)

#Acf and Pacf plots for residuals of forecast3
Acf(forecast3$residuals)
Pacf(forecast3$residuals)

#perform the Ljung-Box test for forecast3
Box.test(forecast3$residuals, lag=24, type="Ljung-Box")

##............VALIDATION............##

#combine the original data for 2 years with forecasts from model3 to create a dataframe
forecast.data <- cbind.data.frame(cpidata.ts.2yrs,forecast3$mean)
forecast.data <- ts(forecast.data, frequency = 12 , start = c(2013,1))
forecast.data <- melt(forecast.data)
colnames(forecast.data) <- c("Time", "Series", "Values")

#plot the original data for 2 years versus the forecast values
ggplot(forecast.data, aes(Time,Values)) + geom_line(aes(colour = Series))

#look at the q-q plot for forecast3 using model3
qqnorm(forecast3$mean)
qqline(forecast3$mean)
