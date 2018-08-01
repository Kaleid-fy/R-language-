##线性回归
x<-1:100
y<-rnorm(100)+2*x+x^2
hg<-lm(y~x+I(x^2))




a<-summary(hg)



xishu<-a[4]##将回归系数及其检验提取出来
##将list转换为矩阵
xishu1<-do.call(rbind,b)
rownames(xishu1)<-c("Intercept","x","I(x^2)")
bbbb<-matrix(0,3,5)
bbbb[,2:5]<-xishu1
as.table(bbbb)
bbbb[1,1]<-"Intercept"
bbbb[2,1]<-"x"
bbbb[3,1]<-"x^2"
colnames(bbbb)<-c("","Estimate","Std. Error","t.value","Pr(>|t|)")

r<-as.character(a[8]) 
adh.r<-as.character(a[9]) 
# write.csv(c,"C:/Users/dell/Desktop/联想/c.csv")
# cc<-read.csv("C:/Users/dell/Desktop/联想/c.csv",row.names= 1)

# rownames(xishu1)<-c("Intercept","lag(SO,1)","lag(SO,2)","lag(SO,3)",
#                     "IOH","lag(IOH,1)","lag(IOH,2)","lag(IOH,3)",
#                     "HasHoliday","BF")

ts_word <- ts_wordx()

a<-summary(hg)
ts_word<- addTitle( ts_word,"回归模型", level =  2)
ts_word <- addParagraph(ts_word, value =as.character(a[1]))

ts_word<- addTitle( ts_word,"回归系数及检验", level =  2)
a<-summary(hg)
xishu<-a[4]##将回归系数及其检验提取出来
##将list转换为矩阵
xishu1<-do.call(rbind,b)
##需要手动添加系数对应变量的名称

b<-matrix(0,10,5)
##将系数及检验放置到矩阵b的2:5列
b[,2:5]<-xishu1

##转化为表格
as.table(b)
##需要手动添加系数对应变量的名称至表b第一列
b[1,1]<-"Intercept"
b[2,1]<-"lag(SO,1)"
b[3,1]<-"lag(SO,2)"
b[4,1]<-"lag(SO,3)"
b[5,1]<-"IOH"
b[6,1]<-"lag(IOH,1)"
b[7,1]<-"lag(IOH,2)"
b[8,1]<-"lag(IOH,3)"
b[9,1]<-"HasHoliday"
b[10,1]<-"BF"
colnames(bbbb)<-c("**","Estimate","Std. Error","t.value","Pr(>|t|)")
ts_word = addFlexTable( ts_word, FlexTable(bbbb) )

ts_word<- addTitle( ts_word,"拟合优度", level =  2)
wz1<-paste("value",as.character(a[8]),"Adjusted R-squared",as.character(a[9]))
ts_word <- addParagraph(ts_word, value =wz1)

##回归模型整体F-statistic
c<-a[10]
d<-as.character(c[[1]])
wz2<-paste("R-squared",d[1],"numdf",d[2],"dendf",d[3])
ts_word <- addParagraph(ts_word, value =wz2)


writets_word( ts_word, "ex_write_ts_word.ts_wordx")


###loess回归
plot(cars,pch=19)
model1=loess(dist~speed,data=cars,span=0.4)
lines(cars$speed,model1$fit,col='red',lty=2,lwd=2)
model2=loess(dist~speed,data=cars,span=0.8)
lines(cars$speed,model2$fit,col='blue',lty=2,lwd=2)
x=5:25
predict(model2,data.frame(speed=x))
plot(model2$resid~model2$fit)
##lowess回归
plot(cars,pch=19)
lines(lowess(cars),lty=2,lwd=2)