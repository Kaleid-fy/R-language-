### 选定特定行或者子集
# 很多时候需要根据一定的条件来提取特定的行，主要使用函数subset来实现这个功能。
# subset(x, subset, select, ...)
# x表示原数据，subset是逻辑表达式，表示需要满足的条件，select是一个表达式，表示对那些列来进行选择。
subset(airquality, Temp > 80, select = c(Ozone, Temp)) 
subset(airquality, Day == 1, select = -Temp) 
subset(airquality, select = Ozone:Wind) 
with(airquality, subset(Ozone, Temp > 80)) 

### 另一种操作数据框的方法
library(sqldf)
a1 <- sqldf("select * from mtcars")
head(a1)
# 按照cyl求mpg的均值
(a <- sqldf("select cyl,avg(mpg) as 'mean.mpg' from mtcars group by cyl"))
# 取数据前六行
a1r <- head(warpbreaks)
a1s <- sqldf("select * from warpbreaks limit 6")
identical(a1r,a1s)