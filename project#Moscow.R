library(lubridate)
library(zoo)

#Moscow

msc <- subset(city_temperature, city_temperature$City == "Moscow")
msc <- subset(msc, msc$AvgTemperature != -99)
msc <- subset(msc, msc$Year <= 2020 & msc$Year >= 1995)


avgtemp <- data.frame()
date <- data.frame()
a <- seq(0,25,1)
for(i in a) {
  year = 1995+i
  tool <- subset(msc, msc$Year == year)
  temp <- data.frame(temp = tapply(tool$AvgTemperature, tool$Month, mean))
  date.tool = c(seq(year+.01, year+.12, .01))
  date = rbind(date, date.tool)
  avgtemp <- rbind(avgtemp, temp)
}
msc.ts <- ts(avgtemp$temp, start = c(1995, 1), end = c(2020, 5), frequency = 12)
plot(msc.ts)

#Exploratory Analysis
x <- msc.ts
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
#pick ARIMA(1,0,0)*(0,1,1)_12
msc.sarima <- arima(x, order=c(1, 0,0), seasonal=list(order=c(0, 1,1), period=12))
plot(msc.sarima$residuals, ylab="residuals")
acf(msc.sarima$residuals)
qqnorm(msc.sarima$residuals)
qqline(msc.sarima$residuals)
#Test the Prediction
xx <- ts(x[1:240], start = c(1995,1), end = c(2014,12), frequency = 12)
test <- arima(xx, order=c(1, 0,0), seasonal=list(order=c(0, 1,1), 
                                                 period=12))
forecast2=predict(test, n.ahead =65)
ts.plot(cbind(x, forecast2$pred), lty=1:2, main=
          "Test For Moscow")
#Prediction
forecast=predict(msc.sarima, n.ahead =19)
ts.plot(cbind(x, forecast$pred), lty=1:2, main=
          "Moscow Average Temperature")



location1 <- seq(1,289, by = 12)#location of Jan in the time series
location2 <- seq(7, 295, by = 12)#location of July in the time series
Year <- seq(1995, 2019)
Jantemp <- as.numeric(x[location1])
Jultemp <- as.numeric(x[location2])
msc.tempchange <- as.data.frame(cbind(Year, Jantemp, Jultemp))
View(msc.tempchange)
matplot(x = msc.tempchange$Year, y = msc.tempchange[,2:3], type = 'l', 
        xlab = "Year", ylab = "Temperature", main = "Moscow Jan and Jul's Temperature",
        lwd = 1, lty = 1, bty = "l", col = 3:4)
legend("center", .1, c("January temperature", "July temperature"), pch = 1, 
       col = 3:4, bty = "n")