# 访问数据库管理系统（MYSQL）
# 通过ROBDC包访问MYSQL
# install.packages("RODBC")
# 需要实现配置好数据源
library(RODBC)
channel <- odbcConnect("fuyu","root","")
odbcGetInfo(channel)

# 将mtcars数据集写入到MYSQL中，生成新表mydata
sqlSave(channel,mtcars,"mydata",append = FALSE)
# 可以利用sqlFetch函数读取MYSQL中的表mydata
mydata <- sqlFetch(channel,"mydata")
str(mydata)
# 利用sqlQuery函数查询mydata表的前六行
sqlQuery(channel,"select * from mydata limit 6")
sqlQuery(channel,"select vs,am,avg(mpg) from mydata group by vs,am")
# 利用sqlDrop函数删除MYSQL中的表mydata
rm(list=ls())
sqlDrop(channel,"mydata")
odbcClose(channel)

# 利用RMySQL包连接MySQL数据库
# install.packages("RMySQL")
library(RMySQL)  #要用32位的R
conn <- dbConnect(MySQL(),dbname = "mysql",user = "root",password = "" )
conn <- dbConnect(MySQL(),dbname = "Samples1",user = "root",password = "" )
dbGetInfo(conn) # 查看连接信息
revenue <- dbGetQuery(conn,"select * from authors limit 2")
revenue
# 解决windows中文乱码问题
dbDisconnect(conn) # 断开连接
conn <- dbConnect(MySQL(),dbname = "Samples1",
                  user = "root",password = "",
                  client.flag = CLIENT_MULTI_STATEMENTS)
dbSendQuery(conn,"SET NAMES gbk")
revenue <- dbGetQuery(conn,"select * from authors limit 10")
revenue



channel <- odbcConnect('yu_sqlserver')
odbcGetInfo(channel)
sqlQuery(channel,'select * from [Return].[dbo].[item]')
odbcClose(channel)