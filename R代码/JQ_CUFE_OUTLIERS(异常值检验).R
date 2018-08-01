###############################################################################
#
# 算法名称：outliers detection(异常值检测)算法
# 作者：付宇
# 日期：2017.3.7
# 版本：V2.0
#
# 输入参数：
#   filePath：储存样本集的路径
# 输出参数：
#  output.csv:存储“有孤立点标识”的数据
#  outliers.png：存储“表示出孤立点“的图表
################################################################################

##################算法部分######################
#调用R包
library(MVN)

JQ_CUFE_OUTLIERS<- function(filePath)
{
  #读取样本集
  testdata<-read.csv(file=filePath)
  #参数检查
  ##检查数据集是否为数值型数据框
  if((!is.data.frame(testdata)))
  {
    testdata<-as.data.frame(testdata)
  }
  #获取生成的图片,png...dev.off()
  png(file='outliers.png')
  #调用 mvOutlier算法
  result = mvOutlier(setosa, qqplot = TRUE, method = "quan", label = TRUE)
  #获取结果
  output<-result$outlier[,1:ncol(result$outlier)]
  #以表格和图片的形式输出结果
  write.table(output, file = "output.csv", sep = ",",col.names = NA , qmethod = "double")
  dev.off()
  
}
##测试
JQ_CUFE_OUTLIERS( "testdata.csv")


