wine<-read.csv("C:/Users/dell/Desktop/wine.txt", header = F, sep = ",", quote = "\"",
            dec = ".", fill = TRUE)
Type<- wine[,1]

########聚类
sdv<- apply(wine[,-1],2,sd)
wine.dat<- sweep(wine[,-1],2,sdv,FUN = '/') ##以变量标准差为权重标准化
h<- hclust(dist(wine.dat),method = "ward.D")

plot(h,cex=.5,hang=-1,main = NULL)
h1<- cutree(h,3)
table(h1,Type,dnn=c("Predicted","True"))


k1<- kmeans(wine.dat,3)
table(k1$cl,Type,dnn=c("Predicted","True"))

k2<- kmeans(wine[,-1],3)
table(k2$cl,Type,dnn=c("Predicted","True"))

library(NbClust)
nb1<- NbClust(wine.dat, distance = "euclidean", min.nc=2, max.nc=12, 
        method = "kmeans", index = "sdindex")
table(nb1$Best.partition,Type,dnn=c("Predicted","True"))

nb2<- NbClust(wine[,-1], distance = "euclidean", min.nc=2, max.nc=12, 
              method = "kmeans", index = "sdindex")
table(nb2$Best.partition,Type,dnn=c("Predicted","True"))
###说明聚类的时候标准化还是有用的

####判别分析
library(MASS)
wine.lda<- lda(wine[,-1],prior=c(1,1,1)/3,Type)
plot(wine.lda,type="density",dimen = 1)
plot(wine.lda,dimen = 2)
table(predict(wine.lda)$class,Type,dnn = c("Predicted","Ture"))


wine.qda<- qda(wine[,-1],Type,CV=T)
table(wine.qda$class,Type,dnn = c("Predicted","Ture"))



