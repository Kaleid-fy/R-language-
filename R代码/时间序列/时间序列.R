passenger = read.csv("C:/Users/dell/Desktop/00/R代码/时间序列/passenger.txt", 
                     header = F, sep = "", quote = "\"",dec = ".", fill = TRUE)
p<-unlist(passenger)
#把数据变成time series。  frequency=12表示以月份为单位的time series. start 表示时间开始点，可以用c(a,b,...)表示，  例如按月为单位，标准的做法是 start=c(2011,1) 表示从2011年1月开始
#如果要表示按天的，建议用 ts(p,frequency=7,start=c(1,1))  很多人喜欢用 ts(p,frequency=365,start=(2011,1))但是这样有个坏处就是没有按星期对齐
pt<-ts(p,frequency=12,start=2001) 
plot(pt)
print(pt)


library(ggplot2)
passenger = read.csv("C:/Users/dell/Desktop/00/R代码/时间序列/passenger.txt", 
                     header = F, sep = "", quote = "\"",dec = ".", fill = TRUE)
p<-unlist(passenger)
#把数据变成time series。  frequency=12表示以月份为单位的time series. start 表示时间开始点，可以用c(a,b,...)表示，  例如按月为单位，标准的做法是 start=c(2011,1) 表示从2011年1月开始
#如果要表示按天的，建议用 ts(p,frequency=7,start=c(1,1))  很多人喜欢用 ts(p,frequency=365,start=(2011,1))但是这样有个坏处就是没有按星期对齐
pt<-ts(p,frequency=12,start=2001) 
a<-data.frame(Time=c(time(pt)),pt=c(pt))
p<-ggplot(a,aes(x=Time,y=pt))
p+geom_line(colour="green")+xlab("the time series od data")+ylab("the time series of Nile")

library(forecast)
#对类型为“ts”的时间序列数据使用ggplot2展示
class(Nile)
a<-data.frame(Time=c(time(Nile)),Nile=c(Nile))
p<-ggplot(a,aes(x=Time,y=Nile))
p+geom_line(colour="green")+xlab("the time series od data")+ylab("the time series of Nile")

##差分
Nilediff1 <- diff(Nile, differences=1)
plot.ts(Nilediff1)
Box.test(Nilediff1, type="Ljung-Box",lag=6)##依旧非随机
arima(Nile,c(1,1,1))
fit<-auto.arima(Nile)
plot(forecast(fit,h=20))

##纯随机性检验
acf(Nile,lag=22)
pacf(Nile,lag=22)
Box.test(c(Nile), type="Ljung-Box",lag=6)


#########################################################
##模型预测
#用前十年的数据预测后一年的数据
#window()函数用来抽取从start到end的时间序列子集
train<-window(pt,start=2001,end=2011+11/12)
test<-window(pt,start=2012)
acf(train)
acf(diff(train,lag=1))#一阶差分后求自相关系数
acf(diff(diff(train,lag=7)))

library(forecast)
pred_meanf<-meanf(train,h=12)
accuracy(test,pred_meanf$mean)
rmse(test,pred_meanf$mean) #226.2657

pred_naive<-naive(train,h=12)
rmse(pred_naive$mean,test)#102.9765

pred_snaive<-snaive(train,h=12)
rmse(pred_snaive$mean,test)#50.70832

pred_rwf<-rwf(train,h=12, drift=T)
rmse(pred_rwf$mean,test)#92.66636

pred_ses <- ses(train,h=12,initial='simple',alpha=0.2)
rmse(pred_ses$mean,test) #89.77035

pred_holt<-holt(train,h=12,damped=F,initial="simple",beta=0.65)
rmse(pred_holt$mean,test)#76.86677  without beta=0.65 it would be 84.41239

pred_hw<-hw(train,h=12,seasonal='multiplicative')
rmse(pred_hw$mean,test)#16.36156

fit<-ets(train)
accuracy(predict(fit,12),test) #24.390252


pred_stlf<-stlf(train)
rmse(pred_stlf$mean,test)#22.07215

plot(stl(train,s.window="periodic"))  #Seasonal Decomposition of Time Series by Loess

fit<-auto.arima(train)
accuracy(forecast(fit,h=12),test) #23.538735

ma = arima(train, order = c(0, 1, 3),   seasonal=list(order=c(0,1,3), period=12))
p<-predict(ma,12)
accuracy(p$pred,test)  #18.55567
BT = Box.test(ma$residuals, lag=30, type = "Ljung-Box", fitdf=2)


## presidents contains NAs
## graphs in example(acf) suggest order 1 or 3
require(graphics)
acf(presidents)
pacf(presidents)

(fit1 <- arima(presidents, c(1, 0, 0)))
nobs(fit1)
tsdiag(fit1)
(fit3 <- arima(presidents, c(3, 0, 0)))  # smaller AIC
tsdiag(fit3)
BIC(fit1, fit3)
## compare a whole set of models; BIC() would choose the smallest
AIC(fit1, arima(presidents, c(2,0,0)),
    arima(presidents, c(2,0,1)), # <- chosen (barely) by AIC
    fit3, arima(presidents, c(3,0,1)))

## An example of ARIMA forecasting:
predict(fit3, 3)

