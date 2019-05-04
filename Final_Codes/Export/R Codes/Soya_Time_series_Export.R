rm(list=ls(all=TRUE))

comexstat_data = read.csv("../Data Directory/Comex_state_soya_use.csv")

setwd("../Data Directory")

dir.create("export_results_soya")
dir.create("export_fitted_results_soya")

#TIME SERIES FORECASTING

library("forecast")
library("stats")
library(imputeTS)
library(stringr)

######################## BA ######################3


soya_export_BA = comexstat_data[,'BA'] 
soya_export_BA

setwd("../Data Directory/production_lags_soya")

covarites = read.csv("lagsBA.csv")


### Converting data to time series ############

soya_export_BA_ts <- ts(soya_export_BA, frequency = 12, start = c(2012,1))
soya_export_BA_ts
par(mfrow=c(1,1))
plot(soya_export_BA_ts,main = "Export  BAHIA",ylab = "Soya Export",
     xlab = "Year")

#Decomposition

Export_BA_components <- 
  decompose(soya_export_BA_ts, type = "multiplicative")

plot(Export_BA_components)

#ACF and PACF
par(mfrow=c(1,2))
plot.ts(soya_export_BA_ts)
acf(soya_export_BA_ts, lag.max=20)
pacf(soya_export_BA_ts, lag.max=20)

# ACF and PACF show significant lag-1, which then cutoff, requiring
# an AR(1) and an MA(1) term.  Also, the significant lag at the seasonal
# period is negative, requiring a SeasonalMA(1) term


vars_matrix_train <- cbind(covarites$lag2,covarites$lag3,covarites$lag4,covarites$lag5,covarites$lag6,
                           covarites$Real_vs_USD)


export_BA <- Arima(soya_export_BA_ts, order = c(1,1,1),
                   seasonal = c(0,1,1), include.drift = FALSE,xreg = vars_matrix_train[1:85,])
sum(export_BA$residuals)

export_BA

# Check residuals to ensure they are white noise
par(mfrow = c(1, 2))
acf(export_BA$residuals, lag.max = 20)
pacf(export_BA$residuals, lag.max = 30)
Box.test(export_BA$residuals, lag=30, type="Ljung-Box")


par(mfrow = c(1, 1))
export_BA_forecastsArima1 <- forecast(export_BA,h= 11,level = c(70,75,80,85,95),xreg = vars_matrix_train[86:96,])
plot(export_BA_forecastsArima1)
export_BA_forecastsArima1

par(mfrow = c(1, 1))
plot.ts(soya_export_BA_ts,col="red")
lines(fitted(export_BA),col="blue")

df = as.data.frame(export_BA_forecastsArima1)
df[df<0] <- 0
df$State = "BA"
df

setwd("../Data Directory/export_results_soya")

write.csv(df, file="forecast_BA.csv",row.names=FALSE)


df = as.data.frame(export_BA$fitted)
df[df<0] <- 0
df$State = "BA"

setwd("../Data Directory/export_fitted_results_soya")

write.csv(df, file="fitted_BA.csv",row.names=FALSE)


########################## GO ##############################

soya_export_GO = comexstat_data[,'GO'] 
soya_export_GO

setwd("../Data Directory/production_lags_soya")

covarites = read.csv("lagsGO.csv")


### Converting data to time series ############

soya_export_GO_ts <- ts(soya_export_GO, frequency = 12, start = c(2012,1))
soya_export_GO_ts
par(mfrow=c(1,1))
plot(soya_export_GO_ts,main = "Export  GOAIS",ylab = "Soya Export",
     xlab = "Year")

#Decomposition

Export_GO_components <- 
  decompose(soya_export_GO_ts, type = "multiplicative")

#ACF and PACF
par(mfrow=c(1,2))
plot.ts(soya_export_GO_ts)
acf(soya_export_GO_ts, lag.max=20)
pacf(soya_export_GO_ts, lag.max=20)



# ACF and PACF show significant lag-1, which then cutoff, requiring
# an AR(1) and an MA(1) term.  Also, the significant lag at the seasonal
# period is negative, requiring a SeasonalMA(1) term


vars_matrix_train <- cbind(covarites$lag1,covarites$lag4,
                           covarites$Real_vs_USD,covarites$lag2,covarites$lag5,covarites$lag13)


export_GO <- Arima(soya_export_GO_ts, order = c(1,1,1),
                   seasonal = c(0,1,1), include.drift = FALSE,xreg = vars_matrix_train[1:85,])

export_GO

# Check residuals to ensure they are white noise
par(mfrow = c(1, 2))
acf(export_GO$residuals, lag.max = 20)
pacf(export_GO$residuals, lag.max = 30)
Box.test(export_GO$residuals, lag=30, type="Ljung-Box")

par(mfrow = c(1, 1))
export_GO_forecastsArima1 <- forecast(export_GO,h= 11,level = c(70,75,80,85,95),xreg = vars_matrix_train[86:96,])
plot(export_GO_forecastsArima1)
export_GO_forecastsArima1

par(mfrow = c(1, 1))
plot.ts(soya_export_GO_ts,col="red")
lines(fitted(export_GO),col="blue")



df = as.data.frame(export_GO_forecastsArima1)
df[df<0] <- 0
df$State = "GO"
df

setwd("../Data Directory/export_results_soya")

write.csv(df, file="forecast_GO.csv",row.names=FALSE)


df = as.data.frame(export_GO$fitted)
df[df<0] <- 0
df$State = "GO"
df
setwd("../Data Directory/export_fitted_results_soya")

write.csv(df, file="fitted_GO.csv",row.names=FALSE)


########################## MA ##########################################3


soya_export_MA = comexstat_data[,'MA'] 
soya_export_MA

setwd("../Data Directory/production_lags_soya")
covarites = read.csv("lagsMA.csv")


### Converting data to time series ############

soya_export_MA_ts <- ts(soya_export_MA, frequency = 12, start = c(2012,1))
soya_export_MA_ts
par(mfrow=c(1,1))
plot(soya_export_MA_ts,main = "Export  MA",ylab = "Soya Export",
     xlab = "Year")

#Decomposition

Export_MA_components <- 
  decompose(soya_export_MA_ts, type = "multiplicative")

#ACF and PACF
par(mfrow=c(1,2))
plot.ts(soya_export_MA_ts)
acf(soya_export_MA_ts, lag.max=20)
pacf(soya_export_MA_ts, lag.max=20)



# ACF and PACF show significant lag-1, which then cutoff, requiring
# an AR(1) and an MA(1) term.  Also, the significant lag at the seasonal
# period is negative, requiring a SeasonalMA(1) term

vars_matrix_train <- cbind(covarites$lag1,covarites$lag2,covarites$lag5,covarites$lag3,covarites$lag4,covarites$CBOT)


export_MA <- Arima(soya_export_MA_ts, order = c(2,0,0),
                   seasonal = c(1,1,0), include.drift = FALSE,xreg = vars_matrix_train[1:85,] )
export_MA


# Check residuals to ensure they are white noise
par(mfrow = c(1, 2))
acf(export_MA$residuals, lag.max = 20)
pacf(export_MA$residuals, lag.max = 30)
Box.test(export_MA$residuals, lag=30, type="Ljung-Box")

par(mfrow = c(1, 1))
export_MA_forecastsArima1 <- forecast(export_MA,h= 11,level = c(70,75,80,85,95),xreg = vars_matrix_train[86:96,])
plot(export_MA_forecastsArima1)
export_MA_forecastsArima1

par(mfrow = c(1, 1))
plot.ts(soya_export_MA_ts,col="red")
lines(fitted(export_MA),col="blue")

df = as.data.frame(export_MA_forecastsArima1)
df[df<0] <- 0
df$State = "MA"
df

setwd("../Data Directory/export_results_soya")

write.csv(df, file="forecast_MA.csv",row.names=FALSE)


df = as.data.frame(export_MA$fitted)
df[df<0] <- 0
df$State = "MA"
df

setwd("../Data Directory/export_fitted_results_soya")

write.csv(df, file="fitted_MA.csv",row.names=FALSE)




############################ MT #########################333


soya_export_MT = comexstat_data[,'MT'] 
soya_export_MT


setwd("../Data Directory/production_lags_soya")

covarites = read.csv("lagsMT.csv")


### Converting data to time series ############

soya_export_MT_ts <- ts(soya_export_MT, frequency = 12, start = c(2012,1))
soya_export_MT_ts
par(mfrow=c(1,1))
plot(soya_export_MT_ts,main = "Export  MT",ylab = "Soya Export",
     xlab = "Year")

#Decomposition

Export_MT_components <- 
  decompose(soya_export_MT_ts, type = "multiplicative")

#ACF and PACF
par(mfrow=c(1,2))
plot.ts(soya_export_MT_ts)
acf(soya_export_MT_ts, lag.max=20)
pacf(soya_export_MT_ts, lag.max=20)


# ACF and PACF show significant lag-1, which then cutoff, requiring
# an AR(1) and an MA(1) term.  Also, the significant lag at the seasonal
# period is negative, requiring a SeasonalMA(1) term

vars_matrix_train <- cbind(covarites$lag1,covarites$lag2,covarites$lag3,covarites$lag4,covarites$lag13,
                           covarites$Real_vs_USD)


vars_matrix_train <- cbind(covarites$sum_lag6,covarites$sum_lag9,covarites$sum_lag12,covarites$lag13,
                           covarites$Real_vs_USD)



export_MT <- Arima(soya_export_MT_ts, order = c(1,1,1),
                   seasonal = c(0,1,1), include.drift = FALSE,xreg = vars_matrix_train[1:85,] )

export_MT

# Check residuals to ensure they are white noise
par(mfrow = c(1, 2))
acf(export_MT$residuals, lag.max = 20)
pacf(export_MT$residuals, lag.max = 30)
Box.test(export_MT$residuals, lag=30, type="Ljung-Box")

par(mfrow = c(1, 1))
export_MT_forecastsArima1 <- forecast(export_MT,h= 11,level = c(70,75,80,85,95),xreg = vars_matrix_train[86:96,])
plot(export_MT_forecastsArima1)
export_MT_forecastsArima1

par(mfrow = c(1, 1))
plot.ts(soya_export_MT_ts,col="red")
lines(fitted(export_MT),col="blue")

df = as.data.frame(export_MT_forecastsArima1)
df[df<0] <- 0
df$State = "MT"
df

setwd("../Data Directory/export_results_soya")

write.csv(df, file="forecast_MT.csv",row.names=FALSE)

df = as.data.frame(export_MT$fitted)
df[df<0] <- 0
df$State = "MT"
df

setwd("../Data Directory/export_fitted_results_soya")

write.csv(df, file="fitted_MT.csv",row.names=FALSE)

############################# MS #########################33


soya_export_MS = comexstat_data[,'MS'] 
soya_export_MS

setwd("../Data Directory/production_lags_soya")
covarites = read.csv("lagsMS.csv")


### Converting data to time series ############

soya_export_MS_ts <- ts(soya_export_MS, frequency = 12, start = c(2012,1))
soya_export_MS_ts
par(mfrow=c(1,1))
plot(soya_export_MS_ts,main = "Export  MS",ylab = "Soya Export",
     xlab = "Year")

#Decomposition

Export_MS_components <- 
  decompose(soya_export_MS_ts, type = "multiplicative")

#ACF and PACF
par(mfrow=c(1,2))
plot.ts(soya_export_MS_ts)
acf(soya_export_MS_ts, lag.max=20)
pacf(soya_export_MS_ts, lag.max=20)


# ACF and PACF show significant lag-1, which then cutoff, requiring
# an AR(1) and an MA(1) term.  Also, the significant lag at the seasonal
# period is negative, requiring a SeasonalMA(1) term

vars_matrix_train <- cbind(covarites$lag2,covarites$lag3,covarites$lag4,covarites$lag5,
                           covarites$lag13,covarites$Real_vs_USD,covarites$lag1)


export_MS <- Arima(soya_export_MS_ts, order = c(0,0,1),
                   seasonal = c(0,1,1), include.drift = FALSE,xreg = vars_matrix_train[1:85,] )

export_MS

# Check residuals to ensure they are white noise
par(mfrow = c(1, 2))
acf(export_MS$residuals, lag.max = 20)
pacf(export_MS$residuals, lag.max = 30)
Box.test(export_MS$residuals, lag=30, type="Ljung-Box")

par(mfrow = c(1, 1))
export_MS_forecastsArima1 <- forecast(export_MS,h= 11,level = c(70,75,80,85,95),xreg = vars_matrix_train[86:96,])
plot(export_MS_forecastsArima1)
export_MS_forecastsArima1

par(mfrow = c(1, 1))
plot.ts(soya_export_MS_ts,col="red")
lines(fitted(export_MS),col="blue")

df = as.data.frame(export_MS_forecastsArima1)
df[df<0] <- 0
df$State = "MS"
df

setwd("../Data Directory/export_results_soya")

write.csv(df, file="forecast_Ms.csv",row.names=FALSE)

df = as.data.frame(export_MS$fitted)
df[df<0] <- 0
df$State = "MS"
df

setwd("../Data Directory/export_fitted_results_soya")

write.csv(df, file="fitted_MS.csv",row.names=FALSE)

############################## MG #######################################


soya_export_MG = comexstat_data[,'MG'] 
soya_export_MG

setwd("../Data Directory/production_lags_soya")
covarites = read.csv("lagsMG.csv")

### Converting data to time series ############

soya_export_MG_ts <- ts(soya_export_MG, frequency = 12, start = c(2012,1))
soya_export_MG_ts
par(mfrow=c(1,1))
plot(soya_export_MG_ts,main = "Export  MG",ylab = "Soya Export",
     xlab = "Year")

#Decomposition

Export_MG_components <- 
  decompose(soya_export_MG_ts, type = "multiplicative")

#ACF and PACF
par(mfrow=c(1,2))
plot.ts(soya_export_MG_ts)
acf(soya_export_MG_ts, lag.max=20)
pacf(soya_export_MG_ts, lag.max=20)

# ACF and PACF show significant lag-1, which then cutoff, requiring
# an AR(1) and an MA(1) term.  Also, the significant lag at the seasonal
# period is negative, requiring a SeasonalMA(1) term

vars_matrix_train <- cbind(covarites$lag2,covarites$lag5,
                           covarites$lag13,covarites$lag1,covarites$lag3,covarites$lag4,covarites$CBOT_lag1)


export_MG <- Arima(soya_export_MG_ts, order = c(1,1,1),
                   seasonal = c(0,1,1), include.drift = FALSE,xreg = vars_matrix_train[1:85,] )

export_MG

# Check residuals to ensure they are white noise
par(mfrow = c(1, 2))
acf(export_MG$residuals, lag.max = 20)
pacf(export_MG$residuals, lag.max = 30)
Box.test(export_MG$residuals, lag=30, type="Ljung-Box")

par(mfrow = c(1, 1))
export_MG_forecastsArima1 <- forecast(export_MG,h= 11,level = c(70,75,80,85,95),xreg = vars_matrix_train[86:96,])
plot(export_MG_forecastsArima1)
export_MG_forecastsArima1

par(mfrow = c(1, 1))
plot.ts(soya_export_MG_ts,col="red")
lines(fitted(export_MG),col="blue")

df = as.data.frame(export_MG_forecastsArima1)
df[df<0] <- 0
df$State = "MG"
df

setwd("../Data Directory/export_results_soya")

write.csv(df, file="forecast_MG.csv",row.names=FALSE)

df = as.data.frame(export_MG$fitted)
df[df<0] <- 0
df$State = "MG"
df

setwd("../Data Directory/export_fitted_results_soya")

write.csv(df, file="fitted_MG.csv",row.names=FALSE)

########################### PR #########################33


soya_export_PR = comexstat_data[,'PR'] 
soya_export_PR

setwd("../Data Directory/production_lags_soya")
covarites = read.csv("lagsPR.csv")


### Converting data to time series ############

soya_export_PR_ts <- ts(soya_export_PR, frequency = 12, start = c(2012,1))
soya_export_PR_ts
par(mfrow=c(1,1))
plot(soya_export_PR_ts,main = "Export  PR",ylab = "Soya Export",
     xlab = "Year")

#Decomposition

Export_PR_components <- 
  decompose(soya_export_PR_ts, type = "multiplicative")

#ACF and PACF
par(mfrow=c(1,3))
plot.ts(soya_export_PR_ts)
acf(soya_export_PR_ts, lag.max=20)
pacf(soya_export_PR_ts, lag.max=20)

# ACF and PACF show significant lag-1, which then cutoff, requiring
# an AR(1) and an MA(1) term.  Also, the significant lag at the seasonal
# period is negative, requiring a SeasonalMA(1) term

vars_matrix_train <- cbind(covarites$lag2,covarites$lag5,covarites$lag4,covarites$Real_vs_USD,covarites$Yaun_vs_USD,
                           covarites$lag13,covarites$lag1,covarites$lag3)


export_PR <- Arima(soya_export_PR_ts, order = c(1,1,1),
                   seasonal = c(0,1,1), include.drift = FALSE,xreg = vars_matrix_train[1:85,])

export_PR


# Check residuals to ensure they are white noise
par(mfrow = c(1, 2))
acf(export_PR$residuals, lag.max = 20)
pacf(export_PR$residuals, lag.max = 30)
Box.test(export_PR$residuals, lag=30, type="Ljung-Box")

par(mfrow = c(1, 1))
export_PR_forecastsArima1 <- forecast(export_PR,h= 11,level = c(70,75,80,85,95),xreg = vars_matrix_train[86:96,])
plot(export_PR_forecastsArima1)
export_PR_forecastsArima1

par(mfrow = c(1, 1))
plot.ts(soya_export_PR_ts,col="red")
lines(fitted(export_PR),col="blue")

df = as.data.frame(export_PR_forecastsArima1)
df[df<0] <- 0
df$State = "PR"
df

setwd("../Data Directory/export_results_soya")

write.csv(df, file="forecast_PR.csv",row.names=FALSE)

df = as.data.frame(export_PR$fitted)
df[df<0] <- 0
df$State = "PR"
df

setwd("../Data Directory/export_fitted_results_soya")

write.csv(df, file="fitted_PR.csv",row.names=FALSE)


############################# RS ###################################3

soya_export_RS = comexstat_data[,20] 
soya_export_RS

setwd("../Data Directory/production_lags_soya")
covarites = read.csv("lagsRS.csv")


### Converting data to time series ############

soya_export_RS_ts <- ts(soya_export_RS, frequency = 12, start = c(2012,1))
soya_export_RS_ts
par(mfrow=c(1,1))
plot(soya_export_RS_ts,main = "Export  RS",ylab = "Soya Export",
     xlab = "Year")

#Decomposition

Export_RS_components <- 
  decompose(soya_export_RS_ts, type = "multiplicative")

#ACF and PACF
par(mfrow=c(1,3))
plot.ts(soya_export_RS_ts)
acf(soya_export_RS_ts, lag.max=20)
pacf(soya_export_RS_ts, lag.max=20)

# ACF and PACF show significant lag-1, which then cutoff, requiring
# an AR(1) and an MA(1) term.  Also, the significant lag at the seasonal
# period is negative, requiring a SeasonalMA(1) term


vars_matrix_train <- cbind(covarites$lag13,covarites$lag3)


export_RS <- Arima(soya_export_RS_ts, order = c(1,1,1),
                   seasonal = c(0,1,1), include.drift = FALSE,xreg = vars_matrix_train[1:85,] )

export_RS


# Check residuals to ensure they are white noise
par(mfrow = c(1, 2))
acf(export_RS$residuals, lag.max = 20)
pacf(export_RS$residuals, lag.max = 30)
Box.test(export_RS$residuals, lag=30, type="Ljung-Box")

par(mfrow = c(1, 1))
export_RS_forecastsArima1 <- forecast(export_RS,h= 11,level = c(70,75,80,85,95),xreg = vars_matrix_train[86:96,])
plot(export_RS_forecastsArima1)
export_RS_forecastsArima1

par(mfrow = c(1, 1))
plot.ts(soya_export_RS_ts,col="red")
lines(fitted(export_RS),col="blue")

df = as.data.frame(export_RS_forecastsArima1)
df[df<0] <- 0
df$State = "RS"
df

setwd("../Data Directory/export_results_soya")

write.csv(df, file="forecast_RS.csv",row.names=FALSE)

df = as.data.frame(export_RS$fitted)
df[df<0] <- 0
df$State = "RS"
df

setwd("../Data Directory/export_fitted_results_soya")

write.csv(df, file="fitted_RS.csv",row.names=FALSE)

########################################### SC ###################################33


soya_export_SC = comexstat_data[,'SC'] 
soya_export_SC

setwd("../Data Directory/production_lags_soya")
covarites = read.csv("lagsSC.csv")

setwd("C:/Users/tnigam/OneDrive - DXC Production/VLI_SPRINT_2_3")

### Converting data to time series ############

soya_export_SC_ts <- ts(soya_export_SC, frequency = 12, start = c(2012,1))
soya_export_SC_ts
par(mfrow=c(1,1))
plot(soya_export_SC_ts,main = "Export  SC",ylab = "Soya Export",
     xlab = "Year")

#Decomposition

Export_SC_components <- 
  decompose(soya_export_SC_ts, type = "multiplicative")

#ACF and PACF
par(mfrow=c(1,3))
plot.ts(soya_export_SC_ts)
acf(soya_export_SC_ts, lag.max=20)
pacf(soya_export_SC_ts, lag.max=20)


# ACF and PACF show significant lag-1, which then cutoff, requiring
# an AR(1) and an MA(1) term.  Also, the significant lag at the seasonal
# period is negative, requiring a SeasonalMA(1) term


vars_matrix_train <- cbind(covarites$lag3,covarites$lag5,
                           covarites$lag13,covarites$Yaun_vs_USD,covarites$lag1)


export_SC <- Arima(soya_export_SC_ts, order = c(1,1,0),
                   seasonal = c(0,1,1), include.drift = FALSE,xreg = vars_matrix_train[1:85,] )

export_SC


# Check residuals to ensure they are white noise
par(mfrow = c(1, 2))
acf(export_SC$residuals, lag.max = 20)
pacf(export_SC$residuals, lag.max = 30)
Box.test(export_SC$residuals, lag=30, type="Ljung-Box")



par(mfrow = c(1, 1))
export_SC_forecastsArima1 <- forecast(export_SC,h= 11,level = c(70,75,80,85,95),xreg = vars_matrix_train[86:96,])
plot(export_SC_forecastsArima1)
export_SC_forecastsArima1

par(mfrow = c(1, 1))
plot.ts(soya_export_SC_ts,col="red")
lines(fitted(export_SC),col="blue")


df = as.data.frame(export_SC_forecastsArima1)
df[df<0] <- 0
df$State = "SC"
df

setwd("../Data Directory/export_results_soya")

write.csv(df, file="forecast_SC.csv",row.names=FALSE)

df = as.data.frame(export_SC$fitted)
df[df<0] <- 0
df$State = "SC"
df

setwd("../Data Directory/export_fitted_results_soya")

write.csv(df, file="fitted_SC.csv",row.names=FALSE)

######################################## SP ############################3


soya_export_SP = comexstat_data[,'SP'] 
soya_export_SP

setwd("../Data Directory/production_lags_soya")
covarites = read.csv("lagsSP.csv")


### Converting data to time series ############

soya_export_SP_ts <- ts(soya_export_SP, frequency = 12, start = c(2012,1))
soya_export_SP_ts
par(mfrow=c(1,1))
plot(soya_export_SP_ts,main = "Export  SP",ylab = "Soya Export",
     xlab = "Year")

#Decomposition

Export_SP_components <- 
  decompose(soya_export_SP_ts, type = "multiplicative")

#ACF and PACF
par(mfrow=c(1,3))
plot.ts(soya_export_SP_ts)
acf(soya_export_SP_ts, lag.max=20)
pacf(soya_export_SP_ts, lag.max=20)


# ACF and PACF show significant lag-1, which then cutoff, requiring
# an AR(1) and an MA(1) term.  Also, the significant lag at the seasonal
# period is negative, requiring a SeasonalMA(1) term


vars_matrix_train <- cbind(covarites$lag3,covarites$lag5,covarites$lag1,covarites$lag2,covarites$Real_vs_USD,covarites$lag13)

export_SP <- Arima(soya_export_SP_ts, order = c(1,0,0),
                   seasonal = c(0,1,1), include.drift = FALSE,xreg = vars_matrix_train[1:85,] )

export_SP


# Check residuals to ensure they are white noise
par(mfrow = c(1, 2))
acf(export_SP$residuals, lag.max = 20)
pacf(export_SP$residuals, lag.max = 30)
Box.test(export_SP$residuals, lag=30, type="Ljung-Box")

par(mfrow = c(1, 1))
export_SP_forecastsArima1 <- forecast(export_SP,h= 11,level = c(70,75,80,85,95),xreg = vars_matrix_train[86:96,])
plot(export_SP_forecastsArima1)
export_SP_forecastsArima1

par(mfrow = c(1, 1))
plot.ts(soya_export_SP_ts,col="red")
lines(fitted(export_SP),col="blue")

df = as.data.frame(export_SP_forecastsArima1)
df[df<0] <- 0
df$State = "SP"
df

setwd("../Data Directory/export_results_soya")

write.csv(df, file="forecast_SP.csv",row.names=FALSE)

df = as.data.frame(export_SP$fitted)
df[df<0] <- 0
df$State = "SP"
df

setwd("../Data Directory/export_fitted_results_soya")

write.csv(df, file="fitted_SP.csv",row.names=FALSE)

############################################## RO ##################################


soya_export_RO = comexstat_data[,'RO'] 
soya_export_RO

setwd("../Data Directory/production_lags_soya")
covarites = read.csv("lagsRO.csv")

### Converting data to time series ############

soya_export_RO_ts <- ts(soya_export_RO, frequency = 12, start = c(2012,1))
soya_export_RO_ts
par(mfrow=c(1,1))
plot(soya_export_RO_ts,main = "Export  RO",ylab = "Soya Export",
     xlab = "Year")

#Decomposition

Export_RO_components <- 
  decompose(soya_export_RO_ts, type = "multiplicative")

#ACF and PACF
par(mfrow=c(1,3))
plot.ts(soya_export_RO_ts)
acf(soya_export_RO_ts, lag.max=20)
pacf(soya_export_RO_ts, lag.max=20)

# ACF and PACF show significant lag-1, which then cutoff, requiring
# an AR(1) and an MA(1) term.  Also, the significant lag at the seasonal
# period is negative, requiring a SeasonalMA(1) term

# Perform a seasonal differencing on the original time series (ARIMA(0,0,0)(0,1,0)12)

vars_matrix_train <- cbind(covarites$lag3,covarites$lag5,covarites$lag1,covarites$lag2,covarites$Real_vs_USD,covarites$lag12,
                           covarites$lag4,covarites$lag13)


export_RO <- Arima(soya_export_RO_ts, order = c(2,0,1),
                   seasonal = c(0,1,1), include.drift = FALSE,xreg = vars_matrix_train[1:85,] )

export_RO

# Check residuals to ensure they are white noise
par(mfrow = c(1, 2))
acf(export_RO$residuals, lag.max = 20)
pacf(export_RO$residuals, lag.max = 30)
Box.test(export_RO$residuals, lag=30, type="Ljung-Box")

par(mfrow = c(1, 1))
export_RO_forecastsArima1 <- forecast(export_RO,h= 11,level = c(70,75,80,85,95),xreg = vars_matrix_train[86:96,])
plot(export_RO_forecastsArima1)
export_RO_forecastsArima1


soya_export_RO_ts
par(mfrow = c(1, 1))
plot.ts(soya_export_RO_ts,col="red")
lines(fitted(export_RO),col="blue")

df = as.data.frame(export_RO_forecastsArima1)
df[df<0] <- 0
df$State = "RO"
df

setwd("../Data Directory/export_results_soya")

write.csv(df, file="forecast_RO.csv",row.names=FALSE)


df = as.data.frame(export_RO$fitted)
df[df<0] <- 0
df$State = "RO"
df

setwd("../Data Directory/export_fitted_results_soya")

write.csv(df, file="fitted_RO.csv",row.names=FALSE)

######################### TO ######################################

soya_export_TO = comexstat_data[,'TO'] 
soya_export_TO

setwd("../Data Directory/production_lags_soya")
covarites = read.csv("lagsTO.csv")

### Converting data to time series ############

soya_export_TO_ts <- ts(soya_export_TO, frequency = 12, start = c(2012,1))
soya_export_TO_ts
par(mfrow=c(1,1))
plot(soya_export_TO_ts,main = "Export  TO",ylab = "Soya Export",
     xlab = "Year")

#Decomposition

Export_TO_components <- 
  decompose(soya_export_TO_ts, type = "multiplicative")

#ACF and PACF
par(mfrow=c(1,3))
plot.ts(soya_export_TO_ts)
acf(soya_export_TO_ts, lag.max=20)
pacf(soya_export_TO_ts, lag.max=20)

# ACF and PACF show significant lag-1, which then cutoff, requiring
# an AR(1) and an MA(1) term.  Also, the significant lag at the seasonal
# period is negative, requiring a SeasonalMA(1) term

# Perform a seasonal differencing on the original time series (ARIMA(0,0,0)(0,1,0)12)

vars_matrix_train <- cbind(covarites$lag3,covarites$lag5,covarites$lag1,covarites$lag2,covarites$lag12,
                           covarites$lag4,covarites$lag13)

export_TO <- Arima(soya_export_TO_ts, order = c(0,1,0),
                   seasonal = c(0,1,1), include.drift = FALSE,xreg = vars_matrix_train[1:85,] )

export_TO


# Check residuals to ensure they are white noise
par(mfrow = c(1, 2))
acf(export_TO$residuals, lag.max = 20)
pacf(export_TO$residuals, lag.max = 30)
Box.test(export_TO$residuals, lag=30, type="Ljung-Box")

par(mfrow = c(1, 1))
export_TO_forecastsArima1 <- forecast(export_TO,h= 11,level = c(70,75,80,85,95),xreg = vars_matrix_train[86:96,])
plot(export_TO_forecastsArima1)
export_TO_forecastsArima1

par(mfrow = c(1, 1))
plot.ts(soya_export_TO_ts,col="red")
lines(fitted(export_TO),col="blue")

df = as.data.frame(export_TO_forecastsArima1)
df[df<0] <- 0
df$State = "TO"
df

setwd("../Data Directory/export_results_soya")

write.csv(df, file="forecast_TO.csv",row.names=FALSE)

df = as.data.frame(export_TO$fitted)
df[df<0] <- 0
df$State = "TO"
df

setwd("../Data Directory/export_fitted_results_soya")
write.csv(df, file="fitted_TO.csv",row.names=FALSE)

############################# PI #####################################

soya_export_PI = comexstat_data[,'PI'] 
soya_export_PI

setwd("../Data Directory/production_lags_soya")
covarites = read.csv("lagsPI.csv")

### Converting data to time series ############

soya_export_PI_ts <- ts(soya_export_PI, frequency = 12, start = c(2012,1))
soya_export_PI_ts
par(mfrow=c(1,1))
plot(soya_export_PI_ts,main = "Export  PI",ylab = "Soya Export",
     xlab = "Year")

#Decomposition

Export_PI_components <- 
  decompose(soya_export_PI_ts, type = "multiplicative")

#ACF and PACF
par(mfrow=c(1,3))
plot.ts(soya_export_PI_ts)
acf(soya_export_PI_ts, lag.max=20)
pacf(soya_export_PI_ts, lag.max=20)


# ACF and PACF show significant lag-1, which then cutoff, requiring
# an AR(1) and an MA(1) term.  Also, the significant lag at the seasonal
# period is negative, requiring a SeasonalMA(1) term

# Perform a seasonal differencing on the original time series (ARIMA(0,0,0)(0,1,0)12)

vars_matrix_train <- cbind(covarites$lag3,covarites$lag1,covarites$lag2,covarites$lag6,
                           covarites$lag4,covarites$lag13)

export_PI <- Arima(soya_export_PI_ts, order = c(0,1,1),
                   seasonal = c(0,1,1), include.drift = FALSE)

export_PI
# Check residuals to ensure they are white noise
par(mfrow = c(1, 2))
acf(export_PI$residuals, lag.max = 20)
pacf(export_PI$residuals, lag.max = 30)
Box.test(export_PI$residuals, lag=30, type="Ljung-Box")

par(mfrow = c(1, 1))
export_PI_forecastsArima1 <- forecast(export_PI,h= 11,level = c(70,75,80,85,95))
plot(export_PI_forecastsArima1)
export_PI_forecastsArima1

par(mfrow = c(1, 1))
plot.ts(soya_export_PI_ts,col="red")
lines(fitted(export_PI),col="blue")

df = as.data.frame(export_PI_forecastsArima1)
df[df<0] <- 0
df$State = "PI"
df

setwd("../Data Directory/export_results_soya")

write.csv(df, file="forecast_PI.csv",row.names=FALSE)

df = as.data.frame(export_PI$fitted)
df[df<0] <- 0
df$State = "PI"
df

setwd("../Data Directory/export_fitted_results_soya")

write.csv(df, file="fitted_PI.csv",row.names=FALSE)


######################### PA ##############


soya_export_PA = comexstat_data[,'PA'] 
soya_export_PA

setwd("../Data Directory/production_lags_soya")
covarites = read.csv("lagsPA.csv")


### Converting data to time series ############

soya_export_PA_ts <- ts(soya_export_PA, frequency = 12, start = c(2012,1))
soya_export_PA_ts
par(mfrow=c(1,1))
plot(soya_export_PA_ts,main = "Export  PA",ylab = "Soya Export",
     xlab = "Year")

#Decomposition

Export_PA_components <- 
  decompose(soya_export_PA_ts, type = "multiplicative")

#ACF and PACF
par(mfrow=c(1,3))
plot.ts(soya_export_PA_ts)
acf(soya_export_PA_ts, lag.max=20)
pacf(soya_export_PA_ts, lag.max=20)


# ACF and PACF show significant lag-1, which then cutoff, requiring
# an AR(1) and an MA(1) term.  Also, the significant lag at the seasonal
# period is negative, requiring a SeasonalMA(1) term

# Perform a seasonal differencing on the original time series (ARIMA(0,0,0)(0,1,0)12)

vars_matrix_train <- cbind(covarites$lag3,covarites$lag2,covarites$lag12,covarites$lag6,
                           covarites$lag4,covarites$lag13)



export_PA <- auto.arima(soya_export_PA_ts)
export_PA


export_PA <- Arima(soya_export_PA_ts, order = c(1,1,1),
                   seasonal = c(0,0,1), include.drift = FALSE,xreg = vars_matrix_train[1:85,])


export_PA


# Check residuals to ensure they are white noise
par(mfrow = c(1, 2))
acf(export_PA$residuals, lag.max = 20)
pacf(export_PA$residuals, lag.max = 30)
Box.test(export_PA$residuals, lag=30, type="Ljung-Box")

par(mfrow = c(1, 1))
export_PA_forecastsArima1 <- forecast(export_PA,h= 11,level = c(70,75,80,85,95),xreg = vars_matrix_train[86:96,])
plot(export_PA_forecastsArima1)
export_PA_forecastsArima1

par(mfrow = c(1, 1))
plot.ts(soya_export_PA_ts,col="red")
lines(fitted(export_PA),col="blue")

df = as.data.frame(export_PA_forecastsArima1)
df[df<0] <- 0
df$State = "PA"
df

setwd("../Data Directory/export_results_soya")
write.csv(df, file="forecast_PA.csv",row.names=FALSE)

df = as.data.frame(export_PA$fitted)
df[df<0] <- 0
df$State = "PA"
df

setwd("../Data Directory/export_fitted_results_soya")
write.csv(df, file="fitted_PA.csv",row.names=FALSE)

####################### DF ##############################


soya_export_DF = comexstat_data[,'DF'] 
soya_export_DF

setwd("../Data Directory/production_lags_soya")
covarites = read.csv("lagsDF.csv")

### Converting data to time series ############

soya_export_DF_ts <- ts(soya_export_DF, frequency = 12, start = c(2012,1))
soya_export_DF_ts
par(mfrow=c(1,1))
plot(soya_export_DF_ts,main = "Export  DF",ylab = "Soya Export",
     xlab = "Year")

#Decomposition

Export_DF_components <- 
  decompose(soya_export_DF_ts, type = "multiplicative")

#ACF and PACF
par(mfrow=c(1,3))
plot.ts(soya_export_DF_ts)
acf(soya_export_DF_ts, lag.max=20)
pacf(soya_export_DF_ts, lag.max=20)


# ACF and PACF show significant lag-1, which then cutoff, requiring
# an AR(1) and an MA(1) term.  Also, the significant lag at the seasonal
# period is negative, requiring a SeasonalMA(1) term

# Perform a seasonal differencing on the original time series (ARIMA(0,0,0)(0,1,0)12)

vars_matrix_train <- cbind(covarites$lag3,covarites$lag12,covarites$lag1,
                           covarites$lag4)

export_DF <- Arima(soya_export_DF_ts, order = c(2,1,1),
                   seasonal = c(1,0,0), include.drift = FALSE,xreg = vars_matrix_train[1:85,])

export_DF

# Check residuals to ensure they are white noise
par(mfrow = c(1, 2))
acf(export_DF$residuals, lag.max = 20)
pacf(export_DF$residuals, lag.max = 30)
Box.test(export_DF$residuals, lag=30, type="Ljung-Box")

par(mfrow = c(1, 1))
export_DF_forecastsArima1 <- forecast(export_DF,h= 11,level = c(70,75,80,85,95),xreg = vars_matrix_train[86:96,])
plot(export_DF_forecastsArima1)
export_DF_forecastsArima1

par(mfrow = c(1, 1))
plot.ts(soya_export_DF_ts,col="red")
lines(fitted(export_DF),col="blue")

df = as.data.frame(export_DF_forecastsArima1)
df[df<0] <- 0
df$State = "DF"
df

setwd("../Data Directory/export_results_soya")

write.csv(df, file="forecast_DF.csv",row.names=FALSE)

df = as.data.frame(export_DF$fitted)
df[df<0] <- 0
df$State = "DF"
df

setwd("../Data Directory/export_fitted_results_soya")

write.csv(df, file="fitted_DF.csv",row.names=FALSE)

###########################################################################






