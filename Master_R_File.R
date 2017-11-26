### Jet Rail ####

# Importing Libraries
library(dplyr)
library(plyr)
library(fpp)
library(MLmetrics)

# Importing Dataset
setwd('C:/Users/vino/Desktop/JetRail')
master_data <- read.csv('JetRail_TrainSet.csv',header=TRUE)
head(master_data)
str(master_data)

# sorting data chronologically
master_data$Datetime <- strptime(master_data$Datetime,'%d-%m-%Y %H:%M')
master_data <- arrange(master_data,Datetime)
head(master_data)
str(master_data)

# subsetting to only 2014 data
master_data <- subset(master_data,strftime(Datetime,'%Y') == '2014' & as.numeric(strftime(Datetime,'%m'))>2)
row.names(master_data) <- 1:nrow(master_data)

# Creating timeseries object
ts_data <- ts(master_data$Count,start=1,frequency=24)
start(ts_data)
end(ts_data)

# plotting timeseries object
ts.plot(ts_data,
        xlab='Time (days)',
        ylab='Count of Passengers',
        main='Jet Rail Timeseries')
points(time(ts_data),cummean(ts_data),col='red',type='l',lwd=2)
legend(10,900,bty='n',legend='Mean Line',lwd=2,col='red')


#### Checking Stationarity in Timeseries ####

# ACF for timeseries
Acf(ts_data,lag.max=24,type='correlation',plot=TRUE)
## ACF plot implies that timeseries object is not Stationary


### Converting non-stationary series to stationary ###

# Box-Cox transformation to make the variance contanst
# stationary_ts <- BoxCox(ts_data,
#                         lambda = BoxCox.lambda(ts_data))
stationary_ts <- log(ts_data)
ts.plot(stationary_ts,
        xlab='Time (days)',
        ylab='log(count of passengers)',
        main='BoxCox Transformed Time Series')
points(time(stationary_ts),cummean(stationary_ts),
       col='red',type='l',lwd=2)
legend(0,7,bty='n',lwd=2,col='red',legend='Mean Line')


# differencing timeseries object to make mean constant (only to visualize)
# stationary_ts <- diff(stationary_ts)
ts.plot(diff(stationary_ts),
        xlab='Time (days)',
        ylab='Transformed count of passengers',
        main='Stationary Time Series')
points(time(diff(stationary_ts)),cummean(diff(stationary_ts)),
       col='red',type='l',lwd=2)
legend(50,1,bty='n',lwd=2,col='red',legend='Mean Line')

## Mean and Variance are made constant and the timeseries is stationary now.

# Ljung-Box Test 
Box.test(stationary_ts,type='Ljung-Box')
# implies true autocorrelation of the timeseries is statistically significant and is not 0

# ACF plot to determine no.of MA terms
Acf(diff(stationary_ts),lag.max = 24,plot=TRUE,type='correlation')
Acf(diff(stationary_ts),lag.max = 24,plot=FALSE,type='correlation')


# PACF plot to determing no. of AR Terms
Pacf(diff(stationary_ts),lag.max = 24,plot=TRUE)
Pacf(diff(stationary_ts),lag.max = 24,plot=FALSE)


# splitting Timeseries to training and testing dataset
# start(stationary_ts)
# end(stationary_ts)
# # total 760 dayss; split training = first 700 days ; testing = last 62 days
# train <- window(stationary_ts,end=c(100,24))
# test <- window(stationary_ts,start=c(101,1))
# head(train)
# head(test)



# model fit
model_fit<- arima(stationary_ts,order=c(1,1,2),seasonal=list(order=c(1,1,2)))
summary(model_fit)

# Predicting values for next 7 months
pred_fit = predict(model_fit,n.ahead = 213*24)

# RMSPE(pred_fit$pred,test)

# Converting from logarthmic to linear scale
df_pred_fit = exp(data.frame(pred_fit))
View(df_pred_fit)

#RMSE(df_pred_fit2$pred,exp(test))

# Writing to csv
write.csv(df_pred_fit$pred,'result.csv')

