#参考链接：https://zhuanlan.zhihu.com/p/24416833

library(randomForest)

index <- sample(2,nrow(iris),replace = TRUE,prob = c(0.7,0.3))
traindata <- iris[index==1,]
testdata <- iris[index==2,]

set.seed(1234)
rf_ntree <- randomForest(Species~.,data = traindata,ntree=300)
plot(rf_ntree)

iris_rf <- randomForest(Species~.,data = traindata,ntree=100,proximity=TRUE)
iris_rf

plot(iris_rf)
importance(iris_rf)
varImpPlot(iris_rf)


# treesize：计算随机森林中每棵树的节点个数
head(treesize(iris_rf,terminal = TRUE))

count_data <- as.data.frame(plyr::count(treesize(iris_rf, terminal = TRUE)))
head(count_data,5)
library(rCharts)
rPlot(x='bin(x,1)',y='freq',data=count_data,type='bar')


#预测
iris_pred <- predict(iris_rf,newdata = testdata)
table(iris_pred,testdata$Species)
plot(margin(iris_rf, testdata$Species))
