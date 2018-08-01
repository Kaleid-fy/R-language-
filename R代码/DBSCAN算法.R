library(fpc)
set.seed(2)
c1<-matrix(rnorm(200,1,sd=0.5),ncol=2)
c2<-cbind(rnorm(100,3,sd=0.5),rnorm(100,4,sd=0.5))
c3<-cbind(rnorm(300,5,sd=0.4),rnorm(300,1,sd=0.4))
clusts =c(rep(1,100),rep(2,100),rep(3,300)) 
x<-rbind(c1,c2,c3)

#注释整段代码，选中要注释的内容，然后 Ctrl+shift+C ；取消的操作相同
#DBSCAN算法#
ds <- dbscan(x, 0.5)
# run with showplot=1 to see how dbscan works.
ds
plot(ds, x)
dbs <- dbscanCBI(x, 0.2,MinPts=4)
dbs$partition
table(dbs$partition)