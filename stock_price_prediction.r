library(quantmod)
library(forecast)
library(tseries)
library(timeSeries)
library("xlsx")

getSymbols("AMZN", src="yahoo", from="2015-01-01")
chartSeries(AMZN)

# Select the relevant close price series
close_price = AMZN[,4]

plot(close_price)

# Conduct ADF test for dataset
print(adf.test(close_price))

acf(close_price,main='ACF Plot', lag.max=100)
pacf(close_price,main='PACF Plot', lag.max=100)

#We apply auto arima to the dataset 
modelfit <- auto.arima(close_price, lambda = "auto")
print(modelfit)

#check residuals of model with arima parameters selected
plot(residuals(modelfit))

g =residuals(modelfit)
m<-mean(g)
std<-sqrt(var(g))
hist(g, density=10, breaks=10, prob=TRUE, 
     xlab="x-variable", ylim=c(0, 100), 
     main="normal curve over histogram")
curve(dnorm(x, mean=m, sd=std), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")


tsdiag(modelfit)

#Box test for lag=2
Box.test(modelfit$residuals, type="Ljung-Box")



#Dataset forecasting plot for the next 30 days
price_forecast <- forecast(modelfit, h=30)
plot(price_forecast)

#Dataset forecast mean first 5 values
head(price_forecast$mean)

#Dataset forecast lower first 5 values
head(price_forecast$lower)

#Dataset forecast upper first 5 values
head(price_forecast$upper)

#Dividing the data into train and test, applying the model
N = length(close_price)
n = 0.7*N
train = close_price[1:n, ]
test  = close_price[(n+1):N,  ]
trainarimafit <- auto.arima(train, lambda = "auto")
predlen=length(test)
trainarimafit <- forecast(trainarimafit, h=predlen)
plot(trainarimafit)

#Plotting mean predicted values vs real data
meanvalues <- as.vector(trainarimafit$mean)
precios <- as.vector(test$AMZN.Close)
plot(meanvalues, type= "l", col= "red")
lines(precios, type = "l")