###prophet包
library(prophet)
library(dplyr)

##读入数据，并对日访问量y取对数处理
df <- read.csv('C:/Users/dell/Desktop/example_wp_peyton_manning.csv') %>%
  mutate(y = log(y))

##拟合模型
m <- prophet(df)

##构建带预测的数据框periods = 365
##代表除历史数据的日期外再往后推365天
future <- make_future_dataframe(m, periods = 365)
tail(future)

##预测数据集
forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
##展示预测结果
plot(m, forecast)
##预测成分分析绘图，展示预测中的趋势、周效应和年效应
prophet_plot_components(m, forecast)

###holidays
playoffs <- data_frame(
  holiday = 'playoff',
  ds = as.Date(c('2008-01-13', '2009-01-03', '2010-01-16',
                 '2010-01-24', '2010-02-07', '2011-01-08',
                 '2013-01-12', '2014-01-12', '2014-01-19',
                 '2014-02-02', '2015-01-11', '2016-01-17',
                 '2016-01-24', '2016-02-07')),
  lower_window = 0,
  upper_window = 1
)
superbowls <- data_frame(
  holiday = 'superbowl',
  ds = as.Date(c('2010-02-07', '2014-02-02', '2016-02-07')),
  lower_window = 0,
  upper_window = 1
)
holidays <- bind_rows(playoffs, superbowls)
m <- prophet(df, holidays = holidays)
forecast <- predict(m, future)
# R
forecast %>% 
  select(ds, playoff, superbowl) %>% 
  filter(abs(playoff + superbowl) > 0) %>%
  tail(10)

prophet_plot_components(m, forecast)




mydata <- read.csv("C:/Users/dell/Desktop/联想/数据/newData 703.csv",stringsAsFactors = FALSE)

##yoga,300 Series,100 Series，Celeron
mydata1<- mydata[mydata$DMU=="1213319780"&mydata$Series=="yoga"&mydata$Display.Size=="14\""&mydata$CPU.Family=="i7",]

df0<-data.frame(ds=mydata1$SO.DATE,y=mydata1$Sell.Out)
df <- df0 
# %>%mutate(y = log(y))
  

m <- prophet(df)
future <- make_future_dataframe(m, periods = 8)
tail(future)

forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
yhat<-tail(forecast$yhat,8)
compute_accuracy()



plot(m, forecast)
prophet_plot_components(m, forecast)