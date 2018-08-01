setwd( "F:/00/学习/天善/谢佳标老师十五大案例资料/第二课/第二课")

### 案例一：对活跃用户是否付费数据进行数据抽样 ###
# 类失衡问题处理
# 导入数据
user <- read.csv("活跃用户是否付费数据.csv",T)
# 查看变量名
colnames(user)
# 查看是否付费的类别占比(0:非付费，1:付费)
prop.table(table(user$是否付费))
str(user)
# 将是否付费变量转换成因子型
user$是否付费 <- as.factor(user$是否付费)
str(user)
library(DMwR)
# 对类失衡数据进行处理
user_new <- SMOTE(是否付费~.,data=user,perc.over=100,perc.under=200)
# 查看处理后的结果
table(user_new$是否付费)

# 利用createDataPartition函数按照”是否付费“等比例对数据进行分区
library(caret)
# 构建训练数据下标集
idx <-  createDataPartition(user$是否付费,p=0.7,list=FALSE)
# 构建训练数据集
train <- user[idx,]
# 构建测试数据集
test <- user[-idx,]
# 查看”是否付费“的0、1占比
prop.table(table(user$是否付费))
prop.table(table(train$是否付费))
prop.table(table(test$是否付费))

# 利用createFoldsh函数构建五折交叉验证的训练集和测试集
user$是否付费 <- as.factor(user$是否付费)
index <- createFolds(user$是否付费,k=5,list=FALSE)
prop.table(table(user[index==1,'是否付费']))
prop.table(table(user[index==2,'是否付费']))
prop.table(table(user[index==3,'是否付费']))
prop.table(table(user[index==4,'是否付费']))
prop.table(table(user[index==5,'是否付费']))

### 案例二：对问卷调研数据的缺失模式进行探索及插补 ###
# 导入玩家调研数据
questionnaire <- read.csv("问卷调研数据.csv",T)
# 查看问卷调研数据的行数和变量个数
dim(questionnaire)
# 对缺失值进行可视化展示
library(VIM)
aggr(questionnaire[,-1],prop=FALSE,numbers=TRUE)
# 把变量转换成因子型
str(questionnaire)
for(i in 2:ncol(questionnaire)){
  questionnaire[,i] <- as.factor(questionnaire[,i])
}
str(questionnaire)
# 对数据进行分区
train <- na.omit(questionnaire[,c("性别","职业" ,"学历","玩家游戏情况","游戏进入","游戏偏好")])
test <- questionnaire[is.na(questionnaire$性别),c("职业" ,"学历","玩家游戏情况","游戏进入","游戏偏好")]
# 建立logit回归模型
fit <- glm(性别~.,train,family = "binomial")
# 由于拟合结果是给每个观测值一个概率值，下面以0.5作为分类界限：
result <- predict(fit,test,type = "response")<0.5
# 把预测结果转换成原先的值(1或2)
z=rep(1,nrow(test));z[!result]=2 
# 在test集中增加预测的性别变量值
test_new <- cbind('性别'=z,test)
# 查看前六行数据
head(test_new)

# 利用随机森林迭代弥补缺失值的方法进行赋值
rm(list=ls())
# 导入数据
questionnaire <- read.csv("问卷调研数据.csv",T)
# 把变量转换成因子型
str(questionnaire)
for(i in 2:ncol(questionnaire)){
  questionnaire[,i] <- as.factor(questionnaire[,i])
}
# 利用missForest进行缺失值赋值
if(!require(missForest)) install.packages("missForest")
z <- missForest(questionnaire)
questionnaire.full <- z$ximp

library(randomForest)
m<-na.roughfix(questionnaire)