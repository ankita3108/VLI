## To clear the screen
rm(list=ls())

library(forecast)
library(tseries)
library(imputeTS)

year_variables=read.csv("../Data Directory/year_variables.csv",header = FALSE)

max_year=year_variables$V2[1]
min_year=year_variables$V2[2]-1
max_month=year_variables$V2[3]
min_month=year_variables$V2[4]
pred_year=year_variables$V2[5]

data_points=(max_year-min_year)*12-(min_month-1)+max_month

max_len=((pred_year-max_year)*12)+(12-max_month)

total_data_points=data_points+max_len

## Reading the data from csv files
data <- read.csv("../Data Directory/Soya Plantation Input Files/Soya_GO.csv", header = T, stringsAsFactors = FALSE)
str(data)

data$Plantation<-data$Plantation+1

log_data<-log(data[,c(2:6)])

sum(is.na(log_data))

## Function to scale data
scale_mm<-function(x)
{
  a<-(x-min(x))/(max(x)-min(x))
  return(a)
}

## Function to unscale data
unscale<-function(x,a)
{
  p<-(x*(max(a)-min(a)))+min(a)
  return(p)
}


scaled_data<-apply(log_data,2,scale_mm)

## Prediction for Future
train_pred<-scaled_data[c(1:data_points),]
test_pred<-scaled_data[c((data_points+1):total_data_points),]

## Creating the time series
ts_pred=ts(train_pred[,c(1)],start=c(min_year,1),frequency=12)

## Manual Arima Fit
#ARIMAfit <- arima(ts_pred,order=c(0,0,0),seasonal=list(order=c(2,1,0),period=12),xreg = train_pred[,c(2:5)])
ARIMAfit <- auto.arima(ts_pred,xreg = train_pred[,c(2:5)])

summary(ARIMAfit)

pred_test <- forecast(ARIMAfit,h=max_len,xreg = test_pred[,c(2:5)])
df_test=data.frame(pred_test)

## Unscaling the data
unscaled_test<-unscale(df_test$Point.Forecast,log_data)

## Unlogging the unscaled data
unlog_test<-exp(unscaled_test)
final_test=unlog_test-1

## Getting the fitted values
train_fit<-fitted(ARIMAfit)
unscaled_train<-unscale(train_fit,log_data)
unlog_train<-exp(unscaled_train)
unlog_train=unlog_train-1

## combination of training fitted and test values
final_predicted<-c(unlog_train,final_test)

## Creating time series for fitted data
pred=ts(final_predicted,start=c(min_year,1),frequency=12)

## Plotting the original and validated dataset
org_ts <- ts(data[,c(2)],start=c(min_year,1),frequency=12)
jpeg('../Data Directory/Plantation Plots/Soya_GO.jpg')
plot(org_ts,ylab="Plantation ('000 ha)", main="Plantation Forecast - Soya (GO)")
lines(pred,col="red")
dev.off()

dataframe=as.data.frame(final_predicted)
colnames(dataframe)<-c("Predicted Plantation")
Date<-data$Date
dataframe<-cbind(Date,dataframe)
dataframe$`Predicted Plantation`<-ifelse(dataframe$`Predicted Plantation`<0,0,dataframe$`Predicted Plantation`)

write.csv(dataframe,file = "../Data Directory/Soya Plantation Output/Predicted-Plantation_GO.csv",row.names=FALSE)