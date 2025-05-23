#converting data to time series
score <- c(15,34,56,34,67,34.5,98,56,78,23,10,23)

score

score.timeseries <- ts(score,start = c(2010,1),frequency = 12)

score.timeseries

plot(score.timeseries)

#How to plot multiple time series
score1 <- c(15,34,56,34,67,34.5,98,56,78,23,10,23)
score1

score2 <- c(25,56,78,65,90,100,132,200,213,78,65,34)
score2

combined.score <-  matrix(c(score1,score2),nrow = 12)
score.timeseries <- ts(combined.score,start = c(2012,1),
                       frequency = 12)

combined.score

score.timeseries

plot(score.timeseries)

install.packages("zoo")
library (zoo)

A.Date <- as.Date(paste(2003, 02, c(1,3,7,9,14), sep = "-"))

A.Date
#breaking the code for understating 
paste(2003, 02, c(1, 3, 7, 9, 14), sep = "-")

#c(1, 3, 7, 9, 14) 
#as.Date()

#construct a zoo object with univerate plotting

A <- zoo(rnorm(5), A.Date)
A
A2 <-zoo(rnorm(5, sd = 0.2), A.Date)
A2
plot(A)
lines(A2, col = 2)

#construct a zoo object with multivatiate plotting
z <- cbind(A, A2, zoo(rnorm(5, sd = 0.5), A.Date))
colnames(z) <- LETTERS[1:3]

plot(z, plot.type = "single", col = list(B = 2))
 pch =plot(z, type = "b", pch = 1:3, col = 1:3)
     plot(z, type = "b", pch = list(A = 1:5, B = 3), col = list(C = 4, 2))

plot(z, type = "b", screen = c(1,2,1), col = 1:3) 
     
#breaking the codes
cbind(A, A2)
z

#Bar plot using zoo
A.Date
A <- zoo(cbind(rpois(5, 2), rpois(5, 3)), A.Date)
barplot(A, beside = TRUE)

#breaking the codes
rpois(5,2)
rpois(5,3)
A
cbind(rpois(5, 2), rpois(5, 3))

#construct xts object
#install.packa("xts)
library(xts)

data<- rnorm (5)

#create date with lenght 5 byb days
data
dates <- seq(as.Date("2017-01-01"), length = 5, by = "days")
dates

data1 <- xts(x = data, order.by = dates)
data1

data2 <- as.POSIXct("1899-05-08")
data2

data3 <- xts(x = data, order.by = dates, born = data2)
data3

#Assignment 
#get this data (financial/sales data suitable for time series analysis)(search)
#Data<- read.zoo("Datalf.csv",header = TRUE, sep = ",",format="%m/%d/%Y") 

#17th April 25
##Linear Filters is the left for another day

getwd()
setwd("C:\\Users\\HP\\Desktop\\DA TERM 2")
#Autoregressive Models
salesdata <- read.csv("Truck_sales.csv")
salesdata

#salesdata1 <- ts(salesdata$Number_Trucks_Sold, frequency = 5)
salesdata1 <- ts(salesdata$Number_Trucks_Sold, frequency = 1)
str(salesdata)
plot(salesdata1)

salesdataDiff <- diff(salesdata1, differences = 1)
plot(salesdataDiff)

#To identify the ACF & PCF 
#plotting ACF
acf(salesdataDiff, lag.max = 5)

pacf(salesdataDiff, lag.max = 5)

#plot the acf and pacf to determine p & q
salesdataDiff <- arima(salesdataDiff, order = c(1,1,0))
salesdataDiff 

sales_data_price.Arima <- Arima(sales_data_diff, order =c(1,1,0))
sales_data_price.Arima

          
#23/4/2025
  
#forecasting wit the data
#install package forecast
library(forecast)
sales_data_future_forecast <- forecast(sales_data_price.Arima, h = 20)
sales_data_future_forecast
plot(sales_data_future_forecast)
Box.test(sales_data_future_forecast$residuals, lag = 5, 
         type = 'ljung-Box')

#GARCH EXAMPLE
installed.packages("rugarch")
library (zoo)
library(rugarch)

gspec <- read.csv(file.choose(), header = TRUE)
gspec
colnames(gspec) <- c("Interval", "Price")
head(gspec)

garchspec.rup <-ugarchspec(mean.model = list(armaOrder = c(0,0)), 
                           distribution ="std"))

garchspec.rup

garchspec.rup <- ugarchfit(garchspec.rup, gspec[, 1])
coef(garchfit.rup)


plot(garchfit.rup@fit$sigma, type = '1')          
FutureForcast <-ugarchforcast(garchfit.rup,
                              n.ahead = 5)

FutureForecast

#Know the basic thinh like what is garch
#know the r package that is used for egarch

          
          
          
          
          
          
          
          