#2维无噪音
library(clv)
library(NbClust)
library(e1071)
#set.seed(1)
c1<-matrix(rnorm(300,1,sd=0.1),ncol=2)
c2<-cbind(rnorm(150,1.5,sd=0.1),rnorm(150,2.5,sd=0.1))
c3<-cbind(rnorm(150,3,sd=0.1),rnorm(150,1,sd=0.1))
c4<-cbind(rnorm(150,2.5,sd=0.1),rnorm(150,3,sd=0.1))
c5<-cbind(rnorm(150,2,sd=0.1),rnorm(150,4,sd=0.1))

#如果用下面的数据集，则指标不能得到正确的聚类数
# c1<-matrix(rnorm(300,1,sd=0.1),ncol=2)
# c2<-cbind(rnorm(150,1.5,sd=0.1),rnorm(150,2.0,sd=0.1))
# c3<-cbind(rnorm(150,3,sd=0.1),rnorm(150,1,sd=0.1))
# c4<-cbind(rnorm(150,2.5,sd=0.1),rnorm(150,2.5,sd=0.1))
# c5<-cbind(rnorm(150,2,sd=0.1),rnorm(150,3,sd=0.1))
# clusts =c(rep(1,150),rep(2,150),rep(3,150),rep(4,150),rep(5,150)) 

clusts =c(rep(1,150),rep(2,150),rep(3,150),rep(4,150),rep(5,150)) 
clusts<-as.integer(clusts)
x<-rbind(c1,c2,c3,c4,c5)

plot(x)

res1<-NbClust(x, distance = "euclidean", min.nc=2, max.nc=8, 
              method = "kmeans", index = "kl")
res2<-NbClust(x, distance = "euclidean", min.nc=2, max.nc=8, 
              method = "kmeans", index = "ch")
res3<-NbClust(x, distance = "euclidean", min.nc=2, max.nc=8, 
              method = "kmeans", index = "mcclain")
res4<-NbClust(x, distance = "euclidean", min.nc=2, max.nc=8, 
              method = "kmeans", index = "db")
res5<-NbClust(x, distance = "euclidean", min.nc=2, max.nc=8, 
              method = "kmeans", index = "silhouette")
res6<-NbClust(x, distance = "euclidean", min.nc=2, max.nc=8, 
              method = "kmeans", index = "dunn")
res7<-NbClust(x, distance = "euclidean", min.nc=2, max.nc=8, 
              method = "kmeans", index = "ccc")
res8<-NbClust(x, distance = "euclidean", min.nc=2, max.nc=8, 
              method = "kmeans", index = "sdindex")
res9<-NbClust(x, distance = "euclidean", min.nc=2, max.nc=8, 
              method = "kmeans", index = "sdbw")
re<-cbind(res1$Best.nc,res2$Best.nc,res3$Best.nc,
          res4$Best.nc,res5$Best.nc,res6$Best.nc,
          res7$Best.nc,res8$Best.nc,res9$Best.nc)
xb=c(rep(0,length(2:8)))
part<-matrix(0,7,nrow (x))
for(i in 2:8){
  cl<-cmeans(x,i,20,verbose=F,method="cmeans")
  part[i-1,]<-cl$cluster 
  xb[i-1] <- fclustIndex(cl,x, index="xie.beni")
}
xb
l<-which.min(xb)+1
xbb<-rbind(l,min(xb))
res10<-part[l-1,]

jaccard<-c(rep(0,10))
res1_std <- std.ext(res1$Best.partition,clusts)
jaccard[1] <- clv.Jaccard(res1_std)

res2_std <- std.ext(res2$Best.partition, clusts)
jaccard[2] <- clv.Jaccard(res2_std)

res3_std <- std.ext(res3$Best.partition, clusts)
jaccard[3] <- clv.Jaccard(res3_std)

res4_std <- std.ext(res4$Best.partition, clusts)
jaccard[4] <- clv.Jaccard(res4_std)

res5_std <- std.ext(res5$Best.partition, clusts)
jaccard[5] <- clv.Jaccard(res5_std)

res6_std <- std.ext(res6$Best.partition, clusts)
jaccard[6] <- clv.Jaccard(res6_std)

res7_std <- std.ext(res7$Best.partition, clusts)
jaccard[7] <- clv.Jaccard(res7_std)

res8_std <- std.ext(res8$Best.partition, clusts)
jaccard[8] <- clv.Jaccard(res8_std)

res9_std <- std.ext(res9$Best.partition, clusts)
jaccard[9] <- clv.Jaccard(res9_std)

res10_std <- std.ext(res10, clusts)
jaccard[10] <- clv.Jaccard(res10_std)

re<-cbind(res1$Best.nc,res2$Best.nc,res3$Best.nc,
          res4$Best.nc,res5$Best.nc,res6$Best.nc,
          res7$Best.nc,res8$Best.nc,res9$Best.nc,xbb)
colnames(re)<-c("kl", "ch", "mcclain","db","silhouette", "dunn","ccc","sdindex","sdbw","xb")


print(re)

mix<-cbind(res1$All.index,res2$All.index,res3$All.index,res4$All.index,
           res5$All.index,res6$All.index,res7$All.index,res8$All.index,res9$All.index,xb)
colnames(mix)<-c("kl", "ch", "mcclain","db","silhouette", "dunn","ccc","sdindex","sdbw","xb")

output<-rbind(mix,re,jaccard)
output
address2=paste("C:/Users/dell/Desktop/RES/","ws.csv")
write.table(output, address2, sep = ",", col.names = NA, qmethod = "double")
#plot(res9$Best.partition,clusts) 
plot(x,col=res7$Best.partition,pch=as.integer(clusts))#不同的颜色代表不同的聚类结果，不同的形状代表训练数据集的原始分类情况。
plot(x,col=res9$Best.partition,pch=as.integer(clusts))#不同的颜色代表不同的聚类结果，不同的形状代表训练数据集的原始分类情况。

length(res10)
