library(timeSeries)
library(openxlsx)
require(XLConnectJars)
getwd()
wb <- loadWorkbook("Blood Bank Data Nashik.xlsx")
rb <- readWorkbook(wb,sheet = "Collection Whole Blood")
#View(rb)
df <- data.frame(rb[, c(-1,-4,-5,-6,-7,-8,-9,-10,-11,-12,-13,-14)])
#View(df)
df1 <- df[,c(-1,-2)]
#View(df1)
test_data <- ts(df1,start=c(2015,8),end=c(2016,11),frequency = 12)
plot(test_data,main = "Blood Samples issued for Aug-2015 - Nov-2016",
     xlab="Time-Frame in Years", ylab="Units of Blood samples issued",
     type = "l")

accuracy(meanf(test_data,h=5,fan = F,lambda = NULL))
accuracy(naive(test_data,h=5,fan = F,lambda = NULL))
accuracy(rwf(test_data,h=5,fan = F,lambda = NULL))
#getwd()


library(tseries)

adf.test(test_data,alternative = "stationary")
#since for adftest p value is 0.7087 which is greater than 0.05 thus differencing is required.

#Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test. This reverses the hypotheses, 
#so the null-hypothesis is that the data are stationary. In this case, 
#small p-values (e.g., less than 0.05) suggest that differencing is required.
#reverse of adftest
kpss.test(test_data)

#Differencing the data
diff_data <- diff(test_data)
diff_data

#determining the appropriate number of differences required for non-seasonal time series.
nsdiffs(test_data)

#check for seasonality in data
Stl = stl(x =test_data,s.window = "periodic")
auto.arima(diff_data)

fit <- Arima(diff_data, order=c(0,1,2), seasonal=c(0,0,0), 
      xreg=NULL, include.mean=F, include.drift=F, 
      include.constant = F, lambda=model$lambda, method="CSS-ML",model=NULL)
summary(fit)
plot(forecast(fit,h=5),main = "Forecast for Aug-2015 - Nov-2016",
     xlab="Time-Frame in Years", ylab="Units of Blood samples issued",
     type = "l")

#Box.test(test_data,lag=1,type="Ljung")

plot(pacf(diff_data,lag.max = NULL,na.action = na.pass,plot = F),main = "Partial ACF")
#as the acf plots drops quickly ,the series is stationary.

plot(acf(diff_data,lag.max = NULL,na.action = na.pass,plot = F,type = "correlation"),main = "Auto Correlation")
acf(fit$residuals)
forecast(auto.arima(test_data))

#n.ahead species the number of years the prediction is required.
predicted  <- predict(fit, n.ahead = 10) 
plot(predicted$pred)
predicted

#prediction in the month of January till 2015
plot <- ts.plot(diff_data,2.718^predicted$pred, log = "y", lty = c(1,3), 
                          xlab="Time-Frame in Years", ylab="Units of Blood samples issued",
                          main = "Prediction for January month till 2025",type ="l")


#smoothening the plot
test_smooth <- HoltWinters(test_data,beta = F,gamma = F)

plot(test_smooth)
test_smooth$seasonal
plot(HoltWinters(test_data,beta = F,gamma = F,
                 seasonal = "additive",l.start = 618.4467))
forecast_data <- forecast.HoltWinters(test_smooth,h=5)
plot.forecast(forecast_data)
acf(forecast_data$residuals,lag.max = 20,na.action = na.pass)



