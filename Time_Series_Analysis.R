## Read the dataset and transform into Time Series 

setwd("G:/Career in Data/Time Series Analysis")

website <- read.csv("daily-website-visitors.csv")
## Pull data in 2019 and 2020 
website$Date2 <- as.Date(website$Date, format="%Y-%m-%d")
website$year <- format(website$Date2, format = "%Y")
website <- website[website$year>=2019, ]
## Convert to time Series 
visits <- ts(website$Unique.Visits)
plot.ts(visits)
## Check the ACF of the original time series without differencing 
acf(visits)
## Conduct differencing to transform the data into stationary status
## Let's conduct weekly differencing first, as there is clearly weekly seasonality 
visits.diff <- diff(visits,7)
## Let's plot the differenced time series
plot(visits.diff, main='Time Series Plot With Seasonal Differencing of Lag 7')
## The trend clearly gets smoothed out a bit after the differencing, but there are still some monthly seasonalities 
## Let's do another differencing in annual period.
visits.diff2 <- diff(visits.diff)

plot.ts(visits.diff2, main='Time Series plot with one seasonal and non-seasonal differencing')
## After the second differencing, we can say it's closer to White Noise for now 

## In general, one seaonal differencing + one non-seasonal differencing seems ok to achieve stationarity 
## D = 1, d = 1 with seasonality S = 7 

## Let's use Ljung-Box test to test if there is significant autocorrelation in the differenced data 
Box.test(visits.diff2, lag = log(length(visits.diff2)))
## p-value is very low, suggesting strong auto correlation 

acf(visits.diff2, main='ACF of seasonally and non-seasonally differenced time sereies')
## ACF is suggesting an Seaonsal MA order of possibly 1,2,3 
## ACF also suggests non-seasonal MA order of possibly 1,2

pacf(visits.diff2, main ='PACF of seasonally and non-seasonally differenced time sereies')
## PACF is suggesting an non-Seaonal AR order of possibly 1,2
## PACF also suggests a seasonal AR order of possibly 1,2,3 


## Summary: We will try SARIMA Model with S=7, D = 1, d = 1, p = [1,2], P=[1,2,3], q=[1,2], Q=[1,2,3]
## let's write a for-loop to check which model is better 
d = 1
DD = 1 
per = 7 
for(p in 1:2){
  for(q in 1:2){
   for(p_seaonsal in 1:3){
     for(q_seasonal in 1:3){
       if(p+d+q+p_seaonsal+DD+q_seasonal <= 10){
         model <- arima(x=visits, order=c(p, d, q), seasonal = list(order=c(p_seaonsal, DD, q_seasonal), period=7))
         pval <- Box.test(model$residuals, lag=log(length(model$residuals)))
         sse <- sum(model$residuals^2)
         cat('p=', p, 'd=', d, 'q=', q, 'P=', p_seaonsal, 'D=', DD, 'Q=', q_seasonal, 'Sesonal Factor=', per, 'AIC = ', model$aic, 'SSE = ', sse, 'p-Value = ', pval$p.value, '\n')
       }
     }
   } 
  }
}
## Best model is the one with 2,1,2,1,1,2 and S=7 
## run the final best model 
final_arima = arima(x=visits, order=c(2,1,2), seasonal=list(order=c(1,1,2), period=7))
final_arima

## Let's check the ACF of the residuals for final arima 
acf(final_arima$residuals)
## although there are still some seasonality in ACF, there doesn't seem to be any statistically significant ACF 
library(forecast)
final_forecast = forecast(final_arima,30)

plot(final_forecast)

# when setting the time series to feed HoltWinters 
visits.ts <- ts(visits, frequency=7)
plot(visits.ts)
visits.ts
hw <- HoltWinters(visits.ts, seasonal='additive')
forecast <- forecast:::forecast.HoltWinters(hw, h=30)
plot(forecast)
Box.test(na.omit(forecast$residuals), lag=log(length(forecast$residuals)))
acf(na.omit(forecast$residuals))


