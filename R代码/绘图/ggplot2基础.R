library("ggplot2")
library("reshape2")


mydata1<-data.frame(x=1:10,y=runif(10,0,100))
mydata2<-data.frame(x=1:10,y=runif(10,0,100))
mydata3<-data.frame(x=1:10,y=runif(10,0,100))
mydata1$class<-sample(LETTERS[1:3],10,replace=T)
mydata1$x1<-runif(10,0,100)
mydata1$y1<-runif(10,0,100)
mydata1

#全部共享：
ggplot(mydata1,aes(x,y))+
geom_point(size=5,shape=21,fill=NA,colour="black")+
geom_line()

ggplot()+
geom_point(data=mydata1,aes(x=x,y=y),size=5,shape=21,fill=NA,colour="black")+
geom_line(data=mydata1,aes(x=x,y=y))

#只共享数据源：
ggplot(mydata1)+
geom_point(aes(x=x,y=y),size=5,shape=21,fill=NA,colour="black")+
geom_line(aes(x=x1,y=y1))

ggplot()+
geom_point(data=mydata1,aes(x=x,y=y),size=5,shape=21,fill=NA,colour="black")+
geom_line(data=mydata1,aes(x=x1,y=y1))


#不共享任何成分：
ggplot()+
geom_line(data=mydata1,aes(x=x,y=y),colour="black")+
geom_line(data=mydata2,aes(x=x,y=y),colour="red")+
geom_line(data=mydata3,aes(x=x,y=y),colour="blue")


###参数写在aes函数内部与外部差别：
ggplot()+
geom_point(data=mydata1,aes(x=x,y=y),colour="black",size=5)

ggplot()+
geom_point(data=mydata1,aes(x=x,y=y,colour=x1),size=5)




ggplot(mydata1)+
geom_bar(aes(x=x,y=y,fill=class),stat="identity",position="identity",alpha=.6)


data1<-data.frame(
      Company = c("Apple","Google","Facebook","Amozon","Tencent"),
      Sale2015 = c(5000,3500,2300,2100,3100),
      Sale2016 = c(5050,3800,2900,2500,3300)
         )
mydata <- melt(data1,id.vars="Company",variable.name="Year",value.name="Sale")

#identity/dodge/stack/fill
ggplot(mydata,aes(Company,Sale,fill=Year))+
geom_bar(stat="identity",position="identity",alpha=.5)

ggplot(mydata,aes(Company,Sale,fill=Year))+
geom_bar(stat="identity",position="dodge")

ggplot(mydata,aes(Company,Sale,fill=Year))+
geom_bar(stat="identity",position="stack")

ggplot(mydata,aes(Company,Sale,fill=Year))+
geom_bar(stat="identity",position="fill")

###ggplot2所支持的数据源：
ggplot(data=NULL)+geom_point(aes(x=1:10,y=1:10,size=1:10))     
ggplot(data=NULL)+geom_point(aes(x=1:10,y=1:10,size=1:9))      
ggplot(data=NULL)+geom_point(aes(x=1:10,y=5,size=1:10))         

#####################案例分享##########################


###柱形图：

#要点：

#统计变换：
#位置参数：

ggplot()+
  geom_bar(data=mydata,aes(Company,Sale,fill=Year),stat="identity",position="stack")

ggplot()+
  geom_col(data=mydata,aes(Company,Sale,fill=Year),position="stack")

ggplot()+
  geom_bar(data=mydata,aes(Company,Sale,fill=Year),stat="identity",position="stack")+
  geom_text(data=mydata,aes(Company,Sale,label=Sale),position="stack")


###折线图：

#离散横坐标：
ggplot()+
  geom_line(data=data1,aes(x=Company,y=Sale2015))

ggplot(data=data1,aes(x=Company,y=Sale2015,group=1))+
  geom_line()+
  geom_point(size=5)

data1$group=1

ggplot(data=data1,aes(x=Company,y=Sale2015,group=group))+
  geom_line()+
  geom_point(size=5)


ggplot(data=mydata,aes(x=Company,y=Sale,group=Year))+
  geom_line()+
  geom_point(size=5)


ggplot()+
  geom_line(data=mydata,aes(x=Company,y=Sale,group=Year,colour=Year))+
  geom_point(data=mydata,aes(x=Company,y=Sale,group=Year,colour=Year),size=5)

###极坐标与饼图、玫瑰图、圆环图：

#饼图：
ggplot(diamonds,aes(x=factor(1),fill=cut))+geom_bar()                                 #单柱堆积柱形图：
ggplot(diamonds,aes(x=factor(1),fill=cut))+geom_bar(width=1)+coord_polar(theta = "y") #极坐标转化（Y圆周化）
ggplot(diamonds,aes(x=factor(1),fill=cut))+geom_bar(width=1)+coord_polar(theta = "x") 

#玫瑰图：
ggplot(diamonds,aes(cut))+geom_bar(width=1)

ggplot(diamonds,aes(cut))+
  geom_bar(width=0.95,fill="#3182BD")+
  coord_polar(theta = "x",start=0)+
  ylim(c(-3000,22500))

ggplot(diamonds,aes(cut))+
  geom_bar(width=1,fill="steelblue",colour="white")+
  coord_polar(theta = "y",start=0)


###分面案例：
data<-data.frame(Name = c("苹果","谷歌","脸书","亚马逊","腾讯"),Company = c("Apple","Google","Facebook","Amozon","Tencent"),Sale2013 = c(5000,3500,2300,2100,3100),Sale2014 = c(5050,3800,2900,2500,3300),Sale2015 = c(5050,3800,2900,2500,3300),Sale2016 = c(5050,3800,2900,2500,3300))
mydata<-melt(data,id.vars=c("Name","Company"),variable.name="Year",value.name="Sale")

ggplot(mydata,aes(reorder(Company,-Sale),Sale,fill=Year))+
  geom_bar(stat="identity",position="dodge")


###按列分面
ggplot(mydata,aes(reorder(Company,-Sale),Sale,fill=Year))+
  geom_bar(stat="identity",position="dodge")+
  facet_grid(.~Year)   

###按行分面
ggplot(mydata,aes(reorder(Company,-Sale),Sale,fill=Year))+
  geom_bar(stat="identity",position="dodge")+
  facet_grid(Year~.)   


###################数据地图专题##############


library(ggplot2)
library(plyr)
library(maptools)
china_map<-readShapePoly("F:/00/学习/天善/195_杜雨_R语言可视化在商务场景中的应用/bou2_4p.shp")
china_map1<-fortify(china_map)

province_city <- read.csv("D:/R/rstudy/Province/chinaprovincecity.csv") 
mydata <- read.csv("D:/R/rstudy/Province/geshengzhibiao.csv",stringsAsFactors = FALSE)
mydata$zhibiao<-as.numeric(mydata$zhibiao)


x<-china_map@data      
xs<-data.frame(id=row.names(x),x)  
china_map_data <- merge(china_map1, xs) 
china_data <- merge(china_map_data, mydata)
china_data <- dplyr::arrange(china_data,group,order)


ggplot(china_data,aes(long,lat))+
  geom_polygon(aes(group=group),fill="white",colour="grey60")+
  geom_point(data =province_city,aes(x = jd,y = wd),colour="red")+
  coord_map("polyconic")+ 
  theme_void()


ggplot(china_data, aes(x = long, y = lat, group = group,fill=zhibiao)) +
  geom_polygon(colour="grey40") +
  scale_fill_gradient(low="white",high="Blue") +  #指定渐变填充色，可使用RGB
  coord_map("polyconic") +       #指定投影方式为polyconic，获得常见视角中国地图
  theme_void()


ggplot()+
  geom_polygon(data=china_data,aes(x=long,y=lat,group=group,fill = zhibiao),colour="grey60")+
  geom_point(data =province_city,aes(x = jd,y = wd,size=zhibiao),colour="red")+
  coord_map("polyconic")+ 
  theme_void()



###地图映射原理（针对多边形填充）：


###字体、排版与清晰度控制：

library("showtext")
library("Cairo")
font_add("myfont","msyh.ttc")

setwd("F:/00/学习/天善/195_杜雨_R语言可视化在商务场景中的应用")

CairoPNG(file="map_9_12.png",width=1200,height=900)
showtext.begin()

ggplot()+
  geom_polygon(data=china_data,aes(x=long,y=lat,group=group,fill = zhibiao),colour="grey60")+
  geom_point(data =province_city,aes(x = jd,y = wd,size=zhibiao),colour="red")+
  coord_map("polyconic")+ 
  labs(title="ggplot2商务图表可视化应用")+
  theme_void(base_size = 20, base_family = "myfont")

showtext.end()
dev.off()





