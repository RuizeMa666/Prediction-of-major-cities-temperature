library(lubridate)

#polis
indiana <- subset(city_temperature, city_temperature$State == "Indiana")
polis <- subset(indiana, indiana$City == "Indianapolis")
polis <- subset(polis, polis$AvgTemperature != -99)
polis <- subset(polis, polis$Day != 0)
polis <- subset(polis, polis$Year <= 2020 & polis$Year >= 1995)


avgtemp <- data.frame()
a <- seq(0,25,1)
for(i in a) {
  year = 1995+i
  tool <- subset(polis, polis$Year == year)
  temp <- data.frame(temp = tapply(tool$AvgTemperature, tool$Month, mean))
  avgtemp <- rbind(avgtemp, temp)
}
polis.ts <- ts(avgtemp$temp, start = c(1995, 1), end = c(2020, 5), frequency = 12)
plot(polis.ts)

#Exploratory Analysis
x <- polis.ts
#par(mfrow = c(2,1))
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

#pick ARIMA(1,0,0)*(0,1,1)_12
polis.sarima <- arima(x, order=c(1, 0,0), seasonal=list(order=c(0, 1,1), period=12))
plot(polis.sarima$residuals, ylab="residuals")
acf(polis.sarima$residuals)
qqnorm(polis.sarima$residuals)
qqline(polis.sarima$residuals)
#Test the Prediction
xx <- ts(x[1:240], start = c(1995,1), end = c(2014,12), frequency = 12)
test <- arima(xx, order=c(1, 0,0), seasonal=list(order=c(0, 1,1), 
                                                 period=12))
forecast2=predict(test, n.ahead =65)
ts.plot(cbind(x, forecast2$pred), lty=1:2, main=
          "Test For Indianapolis")
#Prediction
forecast=predict(polis.sarima, n.ahead =19)
ts.plot(cbind(x, forecast$pred), lty=1:2, main=
          "Indianapolis Average Temperature")


location1 <- seq(1,289, by = 12)#location of Jan in the time series
location2 <- seq(7, 295, by = 12)#location of July in the time series
Year <- seq(1995, 2019)
Jantemp <- as.numeric(x[location1])
Jultemp <- as.numeric(x[location2])
polis.tempchange <- as.data.frame(cbind(Year, Jantemp, Jultemp))
View(polis.tempchange)
matplot(x = polis.tempchange$Year, y = polis.tempchange[,2:3], type = 'l', 
        xlab = "Year", ylab = "Temperature", main = "Indianapolis Jan and Jul's Temperature",
        lwd = 1, lty = 1, bty = "l", col = 3:4)
legend("center", .1, c("January temperature", "July temperature"), pch = 1, 
       col = 3:4, bty = "n")

