vc<-read.csv("vc_network.csv")

#取上三角矩阵
vc[upper.tri(vc)]=0
len<-length(vc[,1])
##初始化##
source<-c()
target<-c()
value<-c()

name<-c()
group<-c()
size<-c()
##建立连接link数据框##
k<-1
for(i in 1:len){
  for(j in 1:len){
    if(vc[i,j]!=0)
      {
      source[k]<-i-1
      target[k]<-j-1
      value[k]<-1  #value 数值相差太大，画出来的图不美观。本来应该是value[k]<-vc[i,j]
      k<-k+1
    }
  }
}
#print(source)
#print(target)
#print(value)
link<-data.frame(source,target,value)

##建立节点node数据框#
vc_name<-vc[,1]
group<-c(rep(1,19),rep(2,50),rep(3,50))
  #c(rep(1,len))#节点的类属，不分组的话图不是彩色的，老师可以跑一下R里的前两个例子
size<-as.data.frame(table(source))[,2]#as.data.frame(table(x))可以用来统计向量中各个数值出现的频数，返回一个矩阵
node<-data.frame(vc_name,group,size)


forceNetwork(Links = link, Nodes = node, Source = "source",height=800,width=1000,
             Target = "target", Value = "value", NodeID = "vc_name",
             Nodesize = "size",fontSize=18,zoom=T,
             radiusCalculation = "Math.sqrt(d.nodesize)+6",
             Group = "group", opacity = 2, legend = TRUE)

