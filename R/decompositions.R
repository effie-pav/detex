#'
#'@title Trends and Patterns in Time series
#'@description Time series smoothing, Holt-Winters fitting and forecasting. Required packages: xts, forecast.
#'@param input The input time series as a single-column dataframe
#'@param
#'@examples
#
#'@author Effie Pavlidou
#'@export
#'
decompositions<-function(input){
input<-as.numeric(input$x)
#set timestamp series
calender<-seq(ISOdate(2011,1,1, hour=0), ISOdate(2011, 12, 31, hour=23), by="hour")
input.x<-xts(x=input, order.by=calender)

#smoothing using averaging
dailyr<-rollapply(input.x, 24, mean, fill = NA, align = "right")
daily<-apply.daily(input.x, mean, align = "right", na.rm=TRUE)
monthly<-apply.monthly(input.x, mean, align = "right", na.rm=TRUE)
#plot
plot(as.zoo(input.x), xlab = "Time", ylab = "Surface Temperature", main = "Series Smoothing", ylim = c(260, 340), col = "gray")
par(new=T)
plot(as.zoo(daily), ylim=c(260, 340), xlab="", ylab="", col="red", lwd=2, axes=FALSE)
par(new=T)
plot(as.zoo(monthly), ylim=c(260, 340), xlab="", ylab="", col="blue", lwd=3, axes=FALSE)
legend(x = 'topleft', legend = c("original", "daily average",  "monthly"), lty = 1, col = c("gray", "red", "blue"))

#Holt's exponential smoothing forecast based on the daily series
# one month forecast
# 80% prediction intervals dark shaded area,  95% prediction intervals  light shaded area
library(forecast)
daily.ts<-ts(as.numeric(daily))
daily.ts <- na.interp(daily.ts)
fordaily <- HoltWinters(daily.ts, gamma=FALSE) #remove the gamma=FALSE if there is seasonality
plot(fordaily)
forsmooth <- forecast(fordaily, h=50)
plot(forsmooth)

}

