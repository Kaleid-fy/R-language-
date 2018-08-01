
library(networkD3)
##forceNetwork各参数实际含义测试##
source<-c(1,1,2,3,4)#出发节点
target<-c(0,0,0,1,2)#目标节点（source和target的取值域相同，都是0~节点个数-1）
value<-c(1,20,3,4,2)#表示每次连接权重，在图中表示为边的粗细
link<-data.frame(source,target,value)

name<-c("A","B","C","D","E")#ABCDE对于01234
group<-c(1,2,1,3,1)#节点的类属
size<-c(1,2,3,4,30)#size表示节点大小，实际应用中可以表示连接的边的数目的多少
node<-data.frame(name,group,size)

forceNetwork(Links = link, Nodes = node, Source = "source",
             Target = "target", Value = "value", NodeID = "name",
             Nodesize = "size",fontSize=18,
             radiusCalculation = "Math.sqrt(d.nodesize)+6",
             Group = "group", opacity = 2, legend = TRUE)
