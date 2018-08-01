# 缺失值判断及处理
# 识别缺失值
# 以睡眠数据sleep为例：
# 加载sleep数据集
data(sleep,package="VIM")
# 查看数据结构
str(sleep)
# 列出没有缺失值的行
sleep[complete.cases(sleep),]
# 计算没有缺失值的样本量
nrow(sleep[complete.cases(sleep),])
# 列出有一个或多个缺失值的行
sleep[!complete.cases(sleep),]
# 计算有缺失值的样本量
nrow(sleep[!complete.cases(sleep),])
# 以变量Dream为研究对象
attach(sleep)
# 查看变量Dream数据
Dream
# is.na( )判断变量Dream各值是否为缺失值，TRUE为缺失值，FALSE为非缺失值
is.na(Dream)
table(is.na(Dream))
# complete.cases( )判断变量Dream各值是否为缺失值，FALSE为缺失值，TRUE为非缺失值
complete.cases(Dream)
table(complete.cases(Dream))
# 可用sum()和mean()函数来获取关于缺失数据的有用信息
# 查看变量Dream的缺失个数
sum(is.na(Dream)) # 等价于sum(!complete.cases(Dream)) 
# 查看变量Dream缺失值的占比
mean(is.na(Dream))
# 查看变量Dream非缺失值的占比
mean(complete.cases(Dream))
# 查看数据集sleep中样本有缺失值的占比
mean(!complete.cases(sleep)) 

# 探索缺失值模式
# 列表显示缺失值
# mice包中的md.pattern()函数
# 将函数应用到sleep数据集
library(mice)
data(sleep,package="VIM")
md.pattern(sleep)
# 将函数应用到nhanes2数据集
md.pattern(nhanes)

# 图形探究缺失数据
# aggr()函数
library(VIM)
aggr(sleep,prop=FALSE,numbers=TRUE)
# 代码aggr(sleep, prop = TRUE, numbers = TRUE)将生成相同的图形，但用比例代替了计数。
aggr(sleep,prop=TRUE,numbers=TRUE)
# 选项numbers = FALSE（默认）删去数值型标签
aggr(sleep,prop=FALSE,number=FALSE)

# 理性处理不完整数据
# 行删除
cor(na.omit(sleep))
cor(sleep,use="complete.obs")
# 回归模型插补
library(mice)
sub=which(is.na(nhanes2[,4])==TRUE)    # 返回nhanes2数据集中第4列为NA的行
dataTR=nhanes2[-sub,]                      # 将第4列不为NA的数存入数据集dataTR中
dataTE=nhanes2[sub,]                       # 将第4列为NA的数存入数据集dataTE中
dataTE                                             # dataTE数据
lm=lm(chl~age,data=dataTR)   # 利用dataTR中age为自变量，chl为因变量构建线性回归模型lm
nhanes2[sub,4]=round(predict(lm,dataTE)) # 利用dataTE中数据按照模型lm对nhanes2中chl中的缺失数据进行预测
   head(nhanes2)          # 缺失值处理后的nhanes2的前若干条
# 随机森林插补
airquality   #有缺失值NA的R自带的数据
complete.cases(airquality) #判断每行有没有缺失值
which(complete.cases(airquality)==F) #缺失值的行号
sum(complete.cases(airquality)) #完整观测值的个数
library(missForest) #用随机森林迭代弥补缺失值
z=missForest(airquality)
air.full=z$ximp  # 随机森林插补后的新数据集

library(randomForest)
m<-na.roughfix(airquality)