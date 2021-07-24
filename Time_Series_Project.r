#install.packages("openxlsx")
#install.packages("readr")
library(forecast)
library(ggplot2)
library(openxlsx)
library(readr)


# Importing xlsx file from local drive using openxlsx package
setwd("C:/Users/Samer/Desktop/Time Series")
elec = read.xlsx('Elec-train.xlsx', sheet = 1)

names(elec)[2] = "pw"
names(elec)[3] = "tmp"


head(elec)
str(elec)



# Converting the first column into Datetime

elec[,1] = as.POSIXct(elec[,1], format='%m/%d/%Y %H:%M')
head(elec)


# Creating a Time Series object 
# Setting frequency = 4 to generate a horly Time Series

elec_pw = ts(elec[,2], frequency = 4)

# Removing NA values 
elec_pw = na.omit(elec_pw)

# Vlaidating that ts has been created and we are ready to go with our forecasting
is.ts(elec_pw)

#plotting time series with Frequency = 4

autoplot(elec_pw,) + 
  ggtitle ('Power (kW/hr)') +
  xlab('Time (hr)') +
  ylab('Power (kW)')



# Creating Train, Test datasets

elec_pw_train= window(elec_pw, start=c(1,2), end=c(902,4))
elec_pw_test= window(elec_pw, start=c(903,1), end=c(1127,4))

# PLotting Train, Test Datasets (kw per HOUR)
autoplot(elec_pw_train,series='Train') + 
  autolayer(elec_pw_test,series='Test')+
  ggtitle ('Power (kW/h)') +
  xlab('Time (hr)') +
  ylab('Power (kW)')



# Auto Correlation and Partial Auto Correlation

acf(elec_pw)
Pacf(elec_pw)



# Hotl Winters Additive

hw_ad=hw(elec_pw_train,seasonal="additive",damped=FALSE,h=900)

# Plotting

plot(elec_pw_train,xlim=c(1,1100),ylim=c(0,400))
lines(elec_pw_test,lty=2)
lines(hw_ad$mean,col=2)


#the data should be differnciated
#determining Lag will be depending in acf function 
acf(elec_pw, lag=150)



# Diffferenciating ts with lag 96
elec_pw %>% 
diff(lag = 96) %>% 
ggtsdisplay()

Box.test(diff(elec_pw, lag=96), type="Ljung-Box")


#changing the freaqency of our data to 96
elec_pw2 = ts(elec[,2], frequency = 96)
elec_pw2 = na.omit(elec_pw2)


# building new Train and Test Datasets
elec_pw_train2 = head(elec_pw2,3606)
elec_pw_test2 =  tail(elec_pw2,901)

autoplot(elec_pw_train2,series="Train") + 
  autolayer(elec_pw_test2,series='Test ')+
  ggtitle ('Power') +
  xlab('Time') +
  ylab('Power (kW)')

#Holt Winters - Additive

hw_add <- HoltWinters(elec_pw_train2,alpha=NULL,beta=NULL,gamma=NULL)

#Plotting
plot(elec_pw_train2,ylim=c(100,350))
hw_add_p <- forecast(hw_add, h=96)
lines(p,col=2)

#RMSE
print(paste("RMSE for HW-Additive:: ",sqrt(mean((hw_add_p$mean-elec_pw_test2)^2))))


# SARIMA

auto_arima <- auto.arima(elec_pw_train2)
auto_arima

#Plotting
autoplot(forecast(auto_arima, h=96))

#RMSE
auto_arima_p <- forecast(auto_arima, h=96)
print(paste("Auto ARIAM ",sqrt(mean((auto_arima_p$mean-elec_pw_test2)^2))))


#Checking residuals to see of there is any correlation missed by automatic function, and we will try to enhance it manually 
checkresiduals(auto_arima, lag.max=200)

# Neural Networks
ANN <- nnetar(elec_pw_train2)
print(ANN)

#Plotting
autoplot(forecast(ANN, h=96))

#RMSE
ANN_P <- forecast(ANN, h=96)
print(paste("Neural Network ",sqrt(mean((ANN_P$mean-elec_pw_test2)^2))))


#Without Tempreture Final Results
print(paste("RMSE for HW-Additive: ",sqrt(mean((hw_add_p$     mean-elec_pw_test2)^2))))
print(paste("Auto ARIMA:           ",sqrt(mean((auto_arima_p$ mean-elec_pw_test2)^2))))
print(paste("Neural Network:       ",sqrt(mean((ANN_P$        mean-elec_pw_test2)^2))))

# Forecasting using Auto Arima
Power_final   <- auto.arima(elec_pw2)
Power_final_p <- forecast(Power_final, h= 96)


# Plotting Final Model with zoom on day 17FEB

autoplot(elec_pw2,series="Power 01JAN10 - 16FEB10") + 
  autolayer(Power_final_p$mean,series='17FEB10 Prediction')+
  ggtitle ('Power (kw/hr)') +
# Zooming on forecasting to have better look
  coord_cartesian(xlim = c(40,50))+
  xlab('hr') +
  ylab('kW')

#Save Predection results to local CSV

Prediction_Power <- print(Power_final_p)

write_csv(Prediction_Power,path="C:/Users/Samer/Desktop/Time Series/Evaluation/Forecast_withOUT_Temprature.csv")

elec = read.xlsx('Elec-train.xlsx', sheet = 1)

head(elec)
str(elec)

# Converting the first column into Datetime

elec[,1] = as.POSIXct(elec[,1], format='%m/%d/%Y %H:%M')

head(elec)
str (elec)

# Creating a Time Series object 

elec_pw_c = ts(elec[,2:3], frequency = 96)

names(elec)[2] = "pw"
names(elec)[3] = "tmp"

# Suprressing NA values 
elec_pw_c = na.omit(elec_pw_c)

# Vlaidating that ts has been created and we are ready to go with our forecasting
is.ts(elec_pw_c)

# building new Train and Test Datasets (89% Train, 20% Test)
elec_pw_c_train = head(elec_pw_c,3606)
elec_pw_c_test =  tail(elec_pw_c,901)



autoplot(elec_pw_c[,2])


# Time Series Linear Regression Model

tslm=tslm(elec_pw_c_train[,1]~elec_pw_c_train[,2]+trend+season, data=elec_pw_c_train)

summary(tslm)
CV(tslm)
checkresiduals(tslm,test='LB',plot=TRUE)



#Dynamic Regression Model

dynamic_reg <- auto.arima(elec_pw_c_train[,1],xreg = elec_pw_c_train[,2])
summary(dynamic_reg)

#Plotting
autoplot(forecast(dynamic_reg,xreg=rep(mean(elec_pw_c_train[,2]),96)))

#RMSE
dynamic_reg_p <- forecast(dynamic_reg,xreg = elec_pw_c_test[,2], h=96)
print(paste("Dynamic Regression - with Temprature: ",sqrt(mean((dynamic_reg_p$mean-elec_pw_c_test[,1])^2))))


#Check if residuals are white noise

dynamic_reg %>% residuals() %>% Box.test()



# Neural Network - with Temprature

ANN_TMP <- nnetar(elec_pw_c_train[,1],xreg = elec_pw_c_train[,2])
ANN_TMP

# Plotting
autoplot(forecast(ANN_TMP,xreg=rep(mean(elec_pw_c_train[,2]),96)))

# RMSE
ANN_TMP_P <- forecast(ANN_TMP,xreg = elec_pw_c_test[,2], h=96)
print(paste("Auto Neural Network - with Temprature: ",sqrt(mean((ANN_TMP_P$mean-elec_pw_c_test[,1])^2))))



# Trying to have a general (Zoomed) overview to assess the models performance
# but as expected and Dynamic Regression model is our hero today, with the lowest RMSE!

autoplot(elec_pw_train2,           series="Train set") + 
  autolayer(elec_pw_test2,         series='Test set' )+
  autolayer(ANN_TMP_P$mean,       series='Neural Network')+
  autolayer(dynamic_reg_p$mean,   series='Dynamic Regression with Temperature')+
  coord_cartesian(xlim = c(35,50))+
  ggtitle ('Power (kW/hr)') +
  xlab('(hr)') +
  ylab('(kW)')

#With Tempreture Final Results
print(paste("Dynamic Regression - with Temprature: ",sqrt(mean((dynamic_reg_p$mean-elec_pw_c_test[,1])^2))))
print(paste("Auto Neural Network - with Temprature: ",sqrt(mean((ANN_TMP_P$mean-elec_pw_c_test[,1])^2))))


#Extract temperature of 17th Feb to make new time series object for forecast
tmp17 <- ts(elec[4508:4603,3], frequency = 4, start=c(1,2))
is.ts(tmp17)

#### ignore - data check
#tmp_chk<-elec[4508:4603,3]

#tmp_test <- print(tmp_chk)

#write_csv(as.data.frame(tmp_test),path="C:/Users/Samer/Desktop/Time Series/Evaluation/tmpcheck.csv")

# Forecasting using Auto Arima
Power_tmp_final   <-auto.arima(elec_pw_c[,1],   xreg = elec_pw_c[,2])
Power_tmp_final_p <- forecast (Power_tmp_final, xreg = tmp17, h= 96)


# Plotting Final Model with zoom on day 17FEB

autoplot(elec_pw2,series="Power 01JAN10 - 16FEB10") + 
  autolayer(Power_tmp_final_p$mean,series='17FEB10 Prediction WITH TEMPRETURE')+
  ggtitle ('Power (kw/hr)') +
# Zooming on forecasting to have better look
  coord_cartesian(xlim = c(40,50))+
  xlab('hr') +
  ylab('kW')

#Save Predection results to local CSV

Prediction_Power <- print(Power_tmp_final_p)

write_csv(Prediction_Power,path="C:/Users/Samer/Desktop/Time Series/Evaluation/Forecast_with_Temprature.csv")

d=1
DD=1
per=52
for(p in 1:4){
  for(q in 1:2){
    for(P in 0:4){
      for(Q in 0:2){
        if(p+d+q+P+DD+Q<=10){
          try({model<-arima(x=elec_pw_train2, order = c((p),d,(q)), seasonal = list(order=c((P),DD,(Q)), period=per))
               pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
               sse<-sum(model$residuals^2)
               cat(p,d,q,P,DD,Q,per, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n') })}}}}}
