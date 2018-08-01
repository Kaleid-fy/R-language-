### 数据基本管理 ###
## 创建新变量
# 方法一:
mydata <- iris[,1:2]
mydata$square <- mydata$Sepal.Length*mydata$Sepal.Width
# 方法二:
rm(list = ls()) 
mydata <- iris[,1:2]
attach(mydata)
mydata$square <- Sepal.Length*Sepal.Width
# 方法三：
rm(list = ls())
mydata <- iris[,1:2]
mydata <- transform(mydata,
                    square = Sepal.Length*Sepal.Length)
library(DT)
datatable(mydata,rownames = F)

### 变量重新编码
rm(list = ls())
mydata <-mtcars
mydata$am <- ifelse(mydata$am==0,"automatic","manual")
x <- c("a", "b", "c")
revalue(x, c(a = "A", c = "C"))


### 变量重命名
# 	reshape包中有一个rename()函数，可用于修改变量名。rename()函数的使用格式为：
#   rename(dataframe, c(oldname="newname", oldname="newname",…)) 
w <- mtcars
library(reshape)
colnames(w)
w <- rename(w,
            c(mpg = "Miles/(US) gallon",cyl = "Number of Cylinders",
              disp = "Displacement(cu.in.)",hp = "Gross horsepower"))
colnames(w)
# 也可以直接用names函数进行重命名
names(w)[5] <- "Rear axle ratio"

### 转换函数-transform
# 一个数据框中常用的更改变量的函数是transform。
# 在调用这个函数时，首先要指定一个数据框（作为第一个参数），
# 跟着是一系列的表达式，表达式中的变量是数据框中的变量。
# transform函数会完成每个表达式中的计算，然后返回最终的数据框。
head(airquality)
head(transform(airquality,
               Ozone = -Ozone))
head(transform(airquality,
               new = -Ozone))
head(transform(airquality,
               new = -Ozone,
               Temp = (Temp-23)/1.8))