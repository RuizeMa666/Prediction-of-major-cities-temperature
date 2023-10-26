library(lubridate)
library(zoo)
#London

ld <- subset(city_temperature, city_temperature$City == "London")
ld <- subset(ld, ld$AvgTemperature != -99)
ld <- subset(ld, ld$Year <= 2020 & ld$Year >= 2000)


avgtemp <- data.frame()
date <- data.frame()
a <- seq(0,20,1)
for(i in a) {
  year = 2000+i
  tool <- subset(ld, ld$Year == year)
  temp <- data.frame(temp = tapply(tool$AvgTemperature, tool$Month, mean))
  date.tool = c(seq(year+.01, year+.12, .01))
  date = rbind(date, date.tool)
  avgtemp <- rbind(avgtemp, temp)
}
ld.ts <- ts(avgtemp$temp, start = c(2000, 1), end = c(2020, 5), frequency = 12)
plot(ld.ts)

#Exploratory Analysis
x <- ld.ts
plot(x)
ddx <- diff(x, 12)
plot.ts(ddx)
acf(ddx)
pacf(ddx)
arima(x, order=c(1, 0,1), seasonal=list(order=c(0, 1,1), period=12))
arima(x, order=c(0, 0,1), seasonal=list(order=c(0, 1,1), period=12))
arima(x, order=c(1, 0,0), seasonal=list(order=c(0, 1,1), period=12))
arima(x, order=c(1, 0,1), seasonal=list(order=c(1, 1,1), period=12))
#pick ARIMA(0,0,1)*(0,1,1)_12
ld.sarima <- arima(x, order=c(0, 0,1), seasonal=list(order=c(0, 1,1), period=12))
plot(ld.sarima$residuals, ylab="residuals")
acf(ld.sarima$residuals)
qqnorm(ld.sarima$residuals)
qqline(ld.sarima$residuals)
#Test the Prediction
xx <- ts(x[1:240], start = c(2000,1), end = c(2014,12), frequency = 12)
test <- arima(xx, order=c(0, 0,1), seasonal=list(order=c(0, 1,1), 
                                                 period=12))
forecast2=predict(test, n.ahead =65)
ts.plot(cbind(x, forecast2$pred), lty=1:2, main=
          "Test for London")
#Prediction
forecast=predict(ld.sarima, n.ahead =19)
ts.plot(cbind(x, forecast$pred), lty=1:2, main=
          "London Average Temperature")



location1 <- seq(1,289, by = 12)#location of Jan in the time series
location2 <- seq(7, 295, by = 12)#location of July in the time series
Year <- seq(1995, 2019)
Jantemp <- as.numeric(x[location1])
Jultemp <- as.numeric(x[location2])
ld.tempchange <- as.data.frame(cbind(Year, Jantemp, Jultemp))
View(ld.tempchange)
matplot(x = ld.tempchange$Year, y = ld.tempchange[,2:3], type = 'l', 
        xlab = "Year", ylab = "Temperature", main = "London Jan and Jul's Temperature",
        lwd = 1, lty = 1, bty = "l", col = 3:4)
legend("center", .1, c("January temperature", "July temperature"), pch = 1, 
       col = 3:4, bty = "n")