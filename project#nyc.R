library(lubridate)
library(zoo)

#NYC

nyc <- subset(city_temperature, city_temperature$City == "New York City")
nyc <- subset(nyc, nyc$AvgTemperature != -99)
nyc <- subset(nyc, nyc$Day != 0)
nyc <- subset(nyc, nyc$Year <= 2020 & nyc$Year >= 1995)


avgtemp <- data.frame()
date <- data.frame()
a <- seq(0,25,1)
for(i in a) {
  year = 1995+i
  tool <- subset(nyc, nyc$Year == year)
  temp <- data.frame(temp = tapply(tool$AvgTemperature, tool$Month, mean))
  avgtemp <- rbind(avgtemp, temp)
}

nyc.ts <- ts(avgtemp$temp, start = c(1995, 1), end = c(2020, 5), frequency = 12)
plot(nyc.ts)

#Exploratory Analysis
x <- nyc.ts
par(mfrow =c(2,1))
plot(x)
plot(diff(x))
ddx <- diff(x, 12)
plot.ts(ddx)
acf(ddx)
pacf(ddx)
arima(x, order=c(1, 0,1), seasonal=list(order=c(0, 1,1), period=12))
arima(x, order=c(0, 0,1), seasonal=list(order=c(0, 1,1), period=12))
arima(x, order=c(1, 0,0), seasonal=list(order=c(0, 1,1), period=12))
arima(x, order=c(1, 0,1), seasonal=list(order=c(1, 1,1), period=12))
#pick ARIMA(1,0,1)*(0,1,1)_12
nyc.sarima <- arima(x, order=c(1, 0,1), seasonal=list(order=c(0, 1,1), period=12))
plot(nyc.sarima$residuals, ylab="residuals")
acf(nyc.sarima$residuals)
qqnorm(nyc.sarima$residuals)
qqline(nyc.sarima$residuals)
#Test the prediction
xx <- ts(x[1:240], start = c(1995,1), end = c(2014,12), frequency = 12)
test <- arima(xx, order=c(1, 0,1), seasonal=list(order=c(0, 1,1), 
                                                 period=12))
forecast2=predict(test, n.ahead =65)
ts.plot(cbind(x, forecast2$pred), lty=1:2, main=
          "Test For New York City")
#Prediction
forecast=predict(nyc.sarima, n.ahead =19)
ts.plot(cbind(x, forecast$pred), lty=1:2, main=
          "New York City Average Temperature")



location1 <- seq(1,289, by = 12)#location of Jan in the time series
location2 <- seq(7, 295, by = 12)#location of July in the time series
Year <- seq(1995, 2019)
Jantemp <- as.numeric(x[location1])
Jultemp <- as.numeric(x[location2])
nyc.tempchange <- as.data.frame(cbind(Year, Jantemp, Jultemp))
View(nyc.tempchange)
matplot(x = nyc.tempchange$Year, y = nyc.tempchange[,2:3], type = 'l', 
        xlab = "Year", ylab = "Temperature", main = "NYC Jan and Jul's Temperature",
        lwd = 1, lty = 1, bty = "l", col = 3:4)
legend("center", .1, c("January temperature", "July temperature"), pch = 1, 
       col = 3:4, bty = "n")
