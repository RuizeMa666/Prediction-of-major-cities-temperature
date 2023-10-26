library(lubridate)
library(zoo)

#Tokyo

tky <- subset(city_temperature, city_temperature$City == "Tokyo")
tky <- subset(tky, tky$AvgTemperature != -99)
tky <- subset(tky, tky$Year <= 2020 & tky$Year >= 1995)

avgtemp <- data.frame()
date <- data.frame()
a <- seq(0,25,1)
for(i in a) {
  year = 1995+i
  tool <- subset(tky, tky$Year == year)
  temp <- data.frame(temp = tapply(tool$AvgTemperature, tool$Month, mean))
  date.tool = c(seq(year+.01, year+.12, .01))
  date = rbind(date, date.tool)
  avgtemp <- rbind(avgtemp, temp)
}
tky.ts <- ts(avgtemp$temp, start = c(1995, 1), end = c(2020, 5), frequency = 12)
plot(tky.ts)
#Exploratory Analysis
x <- tky.ts
par(mfrow = c(2,1))
plot(x)
plot(diff(x))
ddx <- diff(x, 12)
plot.ts(ddx)
acf(ddx, lag.max = 50)
pacf(ddx,lag.max = 50)
arima(x, order=c(1, 0,1), seasonal=list(order=c(0, 1,1), period=12))
arima(x, order=c(0, 0,1), seasonal=list(order=c(0, 1,1), period=12))
arima(x, order=c(1, 0,0), seasonal=list(order=c(0, 1,1), period=12))
arima(x, order=c(1, 0,1), seasonal=list(order=c(1, 1,1), period=12))
arima(x, order=c(1, 1,1), seasonal=list(order=c(0, 1,1), period=12))
#pick ARIMA(1,1,1)*(0,1,1)_12
tky.sarima <- arima(x, order=c(1, 1,1), seasonal=list(order=c(0, 1,1), period=12))
plot(tky.sarima$residuals, ylab="residuals")
acf(tky.sarima$residuals)
qqnorm(tky.sarima$residuals)
qqline(tky.sarima$residuals)
#Test the Prediction
xx <- ts(x[1:240], start = c(1995,1), end = c(2014,12), frequency = 12)
test <- arima(xx, order=c(1, 1,1), seasonal=list(order=c(0, 1,1), 
                                                 period=12))
forecast2=predict(test, n.ahead =65)
ts.plot(cbind(x, forecast2$pred), lty=1:2, main=
          "Test For Tokyo")
#Prediction
forecast=predict(tky.sarima, n.ahead =19)
ts.plot(cbind(x, forecast$pred), lty=1:2, main=
          "Tokyo Average Temperature")

location1 <- seq(1,289, by = 12)#location of Jan in the time series
location2 <- seq(7, 295, by = 12)#location of July in the time series
Year <- seq(1995, 2019)
Jantemp <- as.numeric(x[location1])
Jultemp <- as.numeric(x[location2])
tky.tempchange <- as.data.frame(cbind(Year, Jantemp, Jultemp))
View(tky.tempchange)
matplot(x = tky.tempchange$Year, y = tky.tempchange[,2:3], type = 'l', 
        xlab = "Year", ylab = "Temperature", main = "Tokyo Jan and Jul's Temperature",
        lwd = 1, lty = 1, bty = "l", col = 3:4)
legend("center", .1, c("January temperature", "July temperature"), pch = 1, 
       col = 3:4, bty = "n")
