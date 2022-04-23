#loading necessary packages which includes ggplot,forecast
library("ggplot2")
library("forecast")
library("fpp2")
#reading the csv
Baton_data <- read.csv("C:\\Users\\$$shan$$\\Downloads\\Louisiana prep\\Baton Rouge.csv")
FD <- read.csv("C:\\Users\\Shantanu\\PycharmProjects\\Flooding_classification\\archive\\database.csv\\database.csv")
head(Baton_data)
#converting into time series
BDT <- ts(Baton_data[,2], frequency=1, start=c(1895,1))
#auto_ploting 
autoplot(BDT,facets=FALSE)
#printing the frequency of the given dataset
frequency(BDT)
#seasonal_plot
#for seasonal data we use the following code, but since we have yearly data, we cant come up with seasonal graph
#ggseasonplot(BDT)
#for seasonal data in polar coordinates
#ggseasonplot(a10, polar = TRUE)
#making lagplot
ggAcf(BDT)
#using Ljung-box test to check for white noise in the data
Box.test(BDT, lag = 10, type = "Ljung")
#hence p value is less than 5%, which indicates that the trend is not a white noise

#lets fit different forecasting models
df = diff(BDT)
autoplot(df)
##snaive model
fit = snaive(df)#residual sd : 27.69
print(summary(fit))
checkresiduals(fit)


##arima
fit_arima <- auto.arima(BDT,d=1,D=1,stepwise = FALSE,approximation = FALSE,trace=TRUE)#residual = 11.43
print(summary(fit_arima))
checkresiduals(fit_arima)


#forecasting using arima
fcst <- forecast(fit_arima,20,level=c(95))
autoplot(fcst)

#forecasting using snaive
fcst <- forecast(fit_arima,20)
autoplot(fcst)


