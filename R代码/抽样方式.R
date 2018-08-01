#数据的整理和选择
load <- read.csv("F:/00/研一/探索性数据分析/关/贷款数据/LoanStats3c.csv",header = T,skip=1)
label = c("A","B","C","D","E","F","G")
l <- which(load[,9]%in% label) 
loa <- load[l,c(1,3,6,7,9,14)]
names(loa)
dim(loa)
loa[,5]<- factor(loa[,5],labels = as.character(1:7))
 
#缺失值与异常值处理
nap<- which(is.na(loa),arr.ind = TRUE)
plot(sort(as.numeric(loa$annual_inc)))

#####简单随机抽样
library(sampling)
N<- dim(loa)[1]#总体个数
n= 1000#需要抽样的样本个数
srsp<- srswor(n,N)#不放回简单随机抽样
srs<- getdata(loa,srsp)


#####分层随机抽样
wh<- n*table(loa$grade)/N #求出层权
O<- order(loa$grade)
srp<- strata(data = loa,stratanames = "grade",
             size = wh,method = "srswor")##用于分层抽样的函数

sr<- getdata(loa,srp)#入选样本

#####整群抽样
#按照用户信用等级组群
scp<- cluster(data = loa,clustername = 'grade',size = 3,
              method = 'srswor',description = FALSE)#用于整群抽样的函数，抽取了3个群
sc<- getdata(loa,scp)#入选样本 
unique(sc$grade) 
 
#####系统抽样
i<- rep(1,N)
pik1<- inclusionprobabilities(i,n)#采用等概率的系统抽样，共抽取n个样本
ssp<- UPsystematic(pik1,eps = 1e-6)#系统抽样的函数
ss<- getdata(loa,ssp)

#####多阶段抽样
#第一阶段抽取3个信用等级的用户，第二阶段再从中随机抽取n个单元
msp<- mstage(data=loa, stage = c("cluster",""),varnames = list("grade","id"),
             size = list(3,c(n/3,n/3,n/3)),method = c("srswor","srswor"),description = FALSE)
ms<- getdata(loa,msp)
mss<- ms[[2]]#入选样本


#####不等概率抽样
inc<- loa$annual_inc
pik<- inclusionprobabilities(inc,n)#以年收入为样本规模求出包含概率
usp<- UPbrewer(pik)#用布鲁尔方法进行不等概率抽样
us<- getdata(loa,usp)


#####二重抽样
n_=10000
srsp1<- srswor(n_,N)
srs1<- getdata(loa,srsp1)
o<- order(srs1$grade)
wh2<- n*table(srs1$grade)/n_
srp2<- strata(data= srs1[o,],stratanames = "grade",size=wh2,method = "srswor" )
sr2<- getdata(srs1,srp2)
