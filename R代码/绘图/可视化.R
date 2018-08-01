##通过设置col参数，可以改变图像、坐标轴、文字、点、线等的颜色参数
plot(women,col="red")#通过颜色名称
plot(women,col=554)#通过颜色下标
plot(women,col="#FF0000")#通过颜色下标
hex<-rgb(red=255,green = 0,blue=0,max=255)
plot(women,col=hex)#通过颜色下标

#对其他图形参数颜色进行设置
plot(women,main="身高VS体重散点图",sub="数据来源：women数据集",
     col="red",col.main="green",col.sub="blue",
     col.axis="grey",col.lab="yellow")
#箱线图
boxplot(iris[,1]~iris[,5])
boxplot(iris[,1]~iris[,5],col=c("blue","green","red"))

#查看657种颜色
colors()

##主题颜色,5种，颜色渐变不同
par(mfrow=c(3,2))
barplot(rep(1,6),col=rainbow(6),main="barplot(rep(1,6),col=rainbow(6))")
barplot(rep(1,6),col=heat.colors(6),main="barplot(rep(1,6),col=heat.colors(6))")
barplot(rep(1,6),col=terrain.colors(6),main="barplot(rep(1,6),col=terrain.colors(6))")
barplot(rep(1,6),col=topo.colors(6),main="barplot(rep(1,6),col=topo.colors(6))")
barplot(rep(1,6),col=cm.colors(6),main="barplot(rep(1,6),col=cm.colors(6))")

##RColorBrewer颜色扩展包
#RColorBrewer包提供了3套很好的配色方案。用户只需要制定配色方案的名称。
#sequential连续型：共18种颜色，每组分为9个渐变颜色展示
#Diverging极端型：生成用深色强调两端，浅色表示中部的系列颜色，可用来标识数据中的孤立点
#Qualitative离散型：生成一系列彼此差异比较明显的颜色，通常用来标记分类数据
display.brewer.all(type="seq")#查看连续型的所有方案配色
barplot(rep(1,6),col=brewer.pal(9,"YlOrRd")[3:8])#使用YLOrRd组的第3~8种颜色,9对应几种颜色划分（数一下）
display.brewer.all(type="div")#查看连续型的所有方案配色
barplot(rep(1,6),col=brewer.pal(11,"BrBG")[3:8])#使用BrBG组的第3~8种颜色
display.brewer.all(type="qual")#查看连续型的所有方案配色
barplot(rep(1,6),col=brewer.pal(12,"Set3")[3:8])#使用Set3组的第3~8种颜色
barplot(rep(1,10),col=brewer.pal(9,"Set1")[1:9])

#标题,允许自行设置坐标轴和文本选项
attach(iris)
boxplot(Sepal.Length~Species,col=heat.colors(3),
        main=list("Sepal.Length按照Species分类的箱线图",
                    font=4,col="red",cex=1.5),
        sub=list("数据来源：iris数据集",font=3,
                    col="green",cex=0.8),
        xlab="Species",ylab="Sepal.Length")
#title函数
boxplot(Sepal.Length~Species,col=heat.colors(3))
title(main=list("Sepal.Length按照Species分类的箱线图",
                font=4,col="red",cex=1.5),
      sub=list("数据来源：iris数据集",font=3,
               col="green",cex=0.8),
      xlab=list("Species",col="blue",cex=1.1,font=3),
      ylab="Sepal.Length")

#在图上添加新点/线/文字
plot(1:10)
points(2,4,col="red")#加点

plot(1:10)
text(2,2,"第二个点",pos=4,col="red")#在图上添加文字，pos=4表示位置在右边

plot(women)
fit<-lm(weight~height,data=women)
abline(fit,col="red",lty=2,lwd=2)#lty设置虚线,lwd设置粗细

#坐标轴
#高级绘图函数中一般都有用于设置坐标轴展示和范围的axes、xlim和ylim参数。
#axes=TRUE(默认)，则显示x轴和y轴；参数xaxt="n"和yaxt="n"将分别隐藏x、y轴；xlim和ylim参数设置坐标轴范围
#低级绘图函数axis()可以在上下左右4个边设置坐标轴，并设置坐标轴的范围、刻度等
attach(iris)
boxplot(Sepal.Length~Species,col=heat.colors(3),axes=FALSE,
        xlab="Species",ylab="Sepal.Length")
#设置x轴样式
axis(side=1,at=1:3,labels = unique(Species),col.axis="red",tick = FALSE)
axis(side=2,col.ticks = "gold",font=3,col="blue")

#图例
#绘制分组柱状图
barplot(VADeaths,beside = TRUE,col = cm.colors(5))
#添加图例
legend("top",legend = rownames(VADeaths),
       ncol = 5,fill=cm.colors(5),bty="n")


#散点图矩阵
plot(iris[,1:4],main="利用plot函数绘制散点图矩阵")
pairs(iris[,1:4],main="利用pairs函数绘制散点图矩阵")


#气泡图
attach(mtcars)
r<-sqrt(disp/pi)
symbols(wt,mpg,circles = r,inches = 0.30,
        fg="white",bg="lightblue",
        main="Bubble plot with point size proportional to displacement",
        xlab = "weight of cars(lbs/1000)",
        ylab = "miles per gallon")
text(wt,mpg,rownames(mtcars),cex=0.6)
detach(mtcars)


####lattice包
library(lattice)
#条形图
str(Titanic)#查看Titanic的数据结构
barchart(Titanic,layout=c(4,1),auto.key=TRUE)
barchart(Titanic)



####ggplot2包
library(ggplot2)
qplot()

ggplot()

###利用R语言进行交互可视化
#rCharts包
require(devtools)
install_github('rCharts', 'ramnathv')
library(rCharts)
hair_eye = as.data.frame(HairEyeColor)
r1 <- rPlot(Freq ~ Hair | Eye, color = 'Eye', data = hair_eye, type = 'bar')
r1
