# 利用createDataPartition函数对数据进行抽样
# 对iris数据进行演示
# 载入caret包，如果本地为安装就进行在线安装caret包
if(!require(caret)) install.packages("caret")
# 提取下标集
splitindex <- createDataPartition(iris$Species,times=1,p=0.1,list=FALSE)
splitindex
# 提取符合子集
sample <- iris[splitindex,]
# 查看Species变量中各类别的个数和占比
table(sample$Species);
prop.table(table(sample$Species))
# 设置list为TRUE
# 提取下标集
splitindex1 <- createDataPartition(iris$Species,times=1,p=0.1,list=TRUE)
# 查看下标集
splitindex1
# 提取子集
iris[splitindex1$Resample1,]
# 设置times=2
splitindex2 <- createDataPartition(iris$Species,times=2,p=0.1,list=TRUE)
splitindex2



