## To clear the screen
rm(list=ls())

library(forecast)
library(tseries)
library(imputeTS)
library(dplyr)

year_variables=read.csv("../Data Directory/year_variables.csv",header = FALSE)
agro_yearly=read.csv("../Data Directory/Monthly_Final.csv",header = TRUE)

max_year=year_variables$V2[1]
min_year=year_variables$V2[2]
max_month=year_variables$V2[3]
min_month=year_variables$V2[4]
year_pred=year_variables$V2[5]

data_points=(max_year-min_year)*12-(min_month-1)+max_month

max_len=((year_pred-max_year)*12)+(12-max_month)

total_data_points=data_points+max_len

## Yearly value for predicted years
agro_yearly<-subset(agro_yearly,Years<=year_pred)
agro_yearly_pred<-subset(agro_yearly, UF=='GO' & Produto=='Milho 2 Safra' & Type=='Producao' & Years>=max_year)
agro_yearly_pred = agro_yearly_pred %>% distinct(Yearly_value)
agro_yearly_pred<-unlist(agro_yearly_pred)
agro_yearly_pred[[1]]

## Monthly value for actual year
agro_monthly_act<-subset(agro_yearly, UF=='GO' & Produto=='Milho 2 Safra' & Type=='Producao' & Years==max_year & Month<=max_month)
agro_monthly_act<-agro_monthly_act %>% summarise(Actual_prediction=sum(Monthly_Value))
agro_monthly_act<-unlist(agro_monthly_act)
agro_monthly_act[[1]]

agro_len=length(agro_yearly_pred)

if(max_month==12)
{
  agroconsult_to_adjust=agro_yearly_pred[[2]]
}

if(max_month<12)
{
  zero_added=rep(0, agro_len-1)
  agro_monthly_act=c(agro_monthly_act,zero_added)
  agroconsult_to_adjust=agro_yearly_pred-agro_monthly_act
}

## Yearly list
year_list<-subset(agro_yearly, UF=='GO' & Produto=='Milho 2 Safra' & Type=='Producao')
year_list<-year_list$Years

## Yearly values for 2012 to 2019
agro_yearly<-subset(agro_yearly, UF=='GO' & Produto=='Milho 2 Safra' & Type=='Producao')
agro_yearly<-agro_yearly %>% distinct(Years,Yearly_value)

## Reading the data from csv files
data <- read.csv("../Data Directory/Milho2 Input Files/Milho2_GO.csv", header = T, stringsAsFactors = FALSE)
str(data)

data[is.na(data)] <- 0

## Adding 1 to entire data before taking log and scaling
data$Production<-data$Production+1
data$Plantation<-data$Plantation+1
data$Lag3_Plantation<-data$Lag3_Plantation+1
data$Exchange_Rate<-NULL
data$Soya_Price<-NULL
data$Lag3_Milho_Price<-NULL

colnames(data)

log_data<-log(data[,c(2:4)])

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

# ## Auto Arima
# 
# ARIMAfit <- auto.arima(ts_pred)
# pred_test <- forecast(ARIMAfit,h=max_len,level = c(70,75,80,85,90,95))

# Manual Arima Fit

#ARIMAfit <- arima(ts_pred,order=c(0,0,0),seasonal=list(order=c(1,1,0),period=12),xreg = train_pred[,c(2:3)])
ARIMAfit <- auto.arima(ts_pred,xreg = train_pred[,c(2:3)])

pred_test <- forecast(ARIMAfit,h=max_len,xreg = test_pred[,c(2:3)],level = c(70,75,80,85,90,95))

df_test=data.frame(pred_test)

## Unscaling the data
unscaled_test<-unscale(df_test$Point.Forecast,log_data)

#unscaled_test

## Unlogging the unscaled data
unlog_test<-exp(unscaled_test)
final_test=unlog_test-1

#final_test

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
jpeg('../Data Directory/Plots/Milho2_GO.jpg')
plot(org_ts,ylab="Production ('000 tonnes)", main="Production Forecast - Milho2 (GO)")
lines(pred,col="red")
dev.off()

dataframe=as.data.frame(final_predicted)
colnames(dataframe)<-c("Predicted Production")
Year<-data$Date
dataframe<-cbind(Year,dataframe)
dataframe$`Predicted Production`<-ifelse(dataframe$`Predicted Production`<0,0,dataframe$`Predicted Production`)

write.csv(dataframe,file = "../Data Directory/Jan2019/Milho2_Production_Model_Re-Run_Results/Milho2 Predicted Production New Results/Predicted-Production_GO.csv",row.names=FALSE)

dataframe_2<-cbind(year_list,dataframe)
colnames(dataframe_2)<-c('Year','Date','Predicted')
dataframe_3<-dataframe_2 %>% group_by(Year) %>% summarise(sum_prediction = sum(Predicted))
colnames(dataframe_3)=c('Years','Sum_Prediction')

## combining prediction and agrocunsult values together
final_dataframe=cbind(agro_yearly,dataframe_3$Sum_Prediction)
colnames(final_dataframe)<-c('Year','Agroconsult','Prediction')
final_dataframe$Ratio<-final_dataframe$Agroconsult/final_dataframe$Prediction

new_dataframe=merge(x = dataframe_2, y = final_dataframe, by = "Year", all.x = TRUE)
new_dataframe$Predicted<-new_dataframe$Predicted*new_dataframe$Ratio
new_dataframe$Year<-NULL
new_dataframe$Agroconsult<-NULL
new_dataframe$Prediction<-NULL
new_dataframe$Ratio<-NULL
colnames(new_dataframe)<-c("Year","Predicted Production")

write.csv(new_dataframe,file = "../Data Directory/Jan2019/Milho2_Production_Model_Re-Run_Results/Milho2 Predicted Production New Results/Predicted-Production_GO.csv",row.names=FALSE)

## unscaling all
unscale<-unscale(df_test,log_data)

## Unlogging all
unlog<-exp(unscale)
final<-unlog-1

confidence<-final
confidence <- cbind(Year = rownames(confidence), confidence)
Year<-as.character(confidence$Year)
confidence_values<-apply(confidence[2:14], 2, FUN = function(x){ifelse(x<0,0,x)})
confidence<-cbind(Year,confidence_values)
rownames(confidence) <- NULL

write.csv(confidence,file = "../Data Directory/Jan2019/Milho2_Production_Model_Re-Run_Results/Milho2 Confidence New Results/Confidence_GO.csv",row.names=FALSE)

Year=tail(year_list, max_len)
df2=as.data.frame(confidence_values)
str(df2)
df2=cbind(Year,df2)
predicted=df2 %>% group_by(Year) %>% summarise(Predicted_Prod=sum(Point.Forecast))
actual_pred=cbind(predicted,agroconsult_to_adjust)
actual_pred$Ratio=actual_pred$agroconsult_to_adjust/actual_pred$Predicted_Prod

confidence_new_data=merge(x = df2, y = actual_pred, by = "Year", all.x = TRUE)

confidence_new_data[2:14]<-confidence_new_data$Ratio*confidence_new_data[2:14]
confidence_new_data$Predicted_Prod<-NULL
confidence_new_data$agroconsult_to_adjust<-NULL
confidence_new_data$Ratio<-NULL
Year<-confidence[,1]
confidence_new_data$Year<-Year
rownames(confidence_new_data) <- NULL

write.csv(confidence_new_data,file = "../Data Directory/Jan2019/Milho2_Production_Model_Re-Run_Results/Milho2 Confidence New Results/Confidence_GO.csv",row.names=FALSE)
