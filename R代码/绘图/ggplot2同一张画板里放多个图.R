city_data<- read.csv("F:/00/研一/探索性数据分析/城市投资潜力.csv")


library(ggplot2)
library(grid)
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 4)))
for ( i in 2:9){
  p<-ggplot(city_data,aes(x=city_data[,i]))+xlab(c("(1)GDP","(2)人均可支配收入","(3)城市化水平","(4)住房密度","(5)户籍人口数量","(6)商品房销售均价","(7)商品房销售面积","(8)商品房建筑面积")[i-1])+ylab("")+theme(panel.background = element_blank())+
    theme(axis.line = element_line(colour = "black"))
  if(i<=5)
  {print(p+geom_histogram(bins=10,fill="lightblue",color="black"),vp = viewport(layout.pos.row = 1, layout.pos.col =  i-1))}else
  {print(p+geom_histogram(bins=10,fill="lightblue",color="black"),vp = viewport(layout.pos.row = 2, layout.pos.col =  i-5))}
}