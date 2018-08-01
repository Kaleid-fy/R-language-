###############################################################################
#
# 算法名称：k-近邻算法
# 作者：付宇
# 日期：2017.3.7
# 版本：V2.0
#
# 输入参数：
#   train:已知分类的样本集（数值型数据，每行为样本,每列为属性，最后一列为类别属性）
#    test:需要分类的样本集（数值型数据，每行为样本,每列为属性）
#      k :以附近k个样本为参考（数值型数据）
# 输出参数：
#  outcome.csv:有分类标识的原数据
#
################################################################################

##################算法部分######################
#调用R包
library(class)
#算法实现

JQ_CUFE_KNN<- function(filePath1,filePath2,k)
{
  #读取已知分类的样本集
  Sample<-read.csv(file=filePath1)
  #参数检查
  ##检查数据集是否为数值型数据框
  if((!is.data.frame(Sample)))
  {
    Sample<-as.data.frame(Sample)
  }
  train<-Sample[,1:ncol(Sample)-1]
  #读取需要分类的样本集
  test<-read.csv(file=filePath2)
  if((!is.data.frame( test)))
  {
    test<-as.data.frame( test)
  }
  
  #提取已知分类样本集中各样本分类标签
  cl<-Sample[,ncol(Sample)]
  
  #调用KNN算法
  out<-knn(train,test ,cl, k)
  
  #将分类结果原数据合并
  output<-cbind(out,test)
  #给结果输出矩阵列命名
  colnames(output)<-c("类别",colnames(test))
  #以表格形式输出结果
  write.table(output, file = "outcome.csv", sep = ",",col.names = NA,  qmethod = "double")
}

#测试
output<-JQ_CUFE_KNN("input_train.csv","input_test.csv",4)






