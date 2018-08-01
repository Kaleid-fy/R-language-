####################################dplyr包学习####################################
library(dplyr)

ds <- tbl_df(mtcars)
ds
as.data.frame(ds)


##############################筛选 filter&slice###########################
#过滤出cyl==8的行
filter(mtcars,cyl==8)
filter(mtcars,cyl<6)
#过滤出cyl<6并且vs==1的行
filter(mtcars,cyl<6&vs==1)
filter(mtcars,cyl<6,vs==1)
#过滤出cyl<6或者vs==1的行
filter(mtcars,cyl<6|vs==1)
#过滤出cyl为4或6的行
filter(mtcars,cyl%in%c(4,6))


#选取第一行数据
slice(mtcars,1L)
filter(mtcars,row_number()==1L)
#选取最后一行数据
slice(mtcars,n())
filter(mtcars,row_number()==n())
#选取第5行到最后一行所有数据
slice(mtcars,5:n())
filter(mtcars,between(row_number(),5,n()))


##############################排列: arrange##############################

#以cyl和disp联合升序排序
arrange(mtcars,cyl,disp)
#以disp降序排序
arrange(mtcars,desc(disp))

###############################选择: select###############################
# select()用列名作参数来选择子数据集。
# dplyr包中提供了些特殊功能的函数与select函数结合使用， 用于筛选变量，
# 包括starts_with，ends_with，contains，matches，one_of，num_range和everything等。
# 用于重命名时，select()只保留参数中给定的列，rename()保留所有的列，只对给定的列重新命名。
# 原数据集行名称会被过滤掉。
iris<-tbl_df(iris)
#选取变量名前缀包含Petal的列
select(iris,starts_with("Petal"))
#选取变量名前缀不包含Petal的列
select(iris,-starts_with("Petal"))
#选取变量名后缀包含Width的列
select(iris,ends_with("Width"))
#选取变量名后缀不包含Width的列
select(iris,-ends_with("Width"))
#选取变量名中包含etal的列
select(iris,contains("etal"))
#选取变量名中不包含etal的列
select(iris,-contains("etal"))
#正则表达式匹配，返回变量名中包含t的列
select(iris,matches(".t."))
#正则表达式匹配，返回变量名中不包含t的列
select(iris,-matches(".t."))
#直接选取列
select(iris,Petal.Length,Petal.Width)
#返回除Petal.Length和Petal.Width之外的所有列
select(iris,-Petal.Length,-Petal.Width)
#使用冒号连接列名，选择多个列
select(iris,Sepal.Length:Petal.Width)
#选择字符向量中的列，select中不能直接使用字符向量筛选，需要使用one_of函数
vars<-c("Petal.Length","Petal.Width")
select(iris,one_of(vars))
#返回指定字符向量之外的列
select(iris,-one_of(vars))
#返回所有列，一般调整数据集中变量顺序时使用
select(iris,everything())
#调整列顺序，把Species列放到最前面
select(iris,Species,everything())

df<-as.data.frame(matrix(runif(100),nrow=10))
df<-tbl_df(df[c(3,4,7,1,9,8,5,2,6,10)])
#选择V4，V5，V6三列
select(df,V4:V6)
select(df,num_range("V",4:6))

#重命名列Petal.Length，返回子数据集只包含重命名的列
select(iris,petal_length=Petal.Length)
#重命名所有以Petal为前缀的列，返回子数据集只包含重命名的列
select(iris,petal=starts_with("Petal"))
#重命名列Petal.Length，返回全部列
rename(iris,petal_length=Petal.Length)

###############################变形: mutate###############################
#添加新列wt_kg和wt_t,在同一语句中可以使用刚添加的列
mutate(mtcars,wt_kg=wt*453.592,wt_t=wt_kg/1000)
#计算新列wt_kg和wt_t，返回对象中只包含新列
transmute(mtcars,wt_kg=wt*453.592,wt_t=wt_kg/1000)


###############################去重: distinct###############################
# distinct()用于对输入的tbl进行去重，返回无重复的行，类似于 base::unique() 函数，
# 但是处理速度更快。原数据集行名称会被过滤掉。

df<-data.frame(
  x=sample(10,100,rep=TRUE),
  y=sample(10,100,rep=TRUE)
)
#以全部两个变量去重，返回去重后的行数
nrow(distinct(df))
nrow(distinct(df,x,y))
distinct(df,x,y)
#以变量x去重，只返回去重后的x值
distinct(df,x)
#以变量y去重，只返回去重后的y值
distinct(df,y)
#以变量x去重，返回所有变量
distinct(df,x,.keep_all=TRUE)
#以变量y去重，返回所有变量
distinct(df,y,.keep_all=TRUE)
#对变量运算后的结果去重
distinct(df,diff=abs(x-y))


###############################概括: summarise###############################
#返回数据框中变量disp的均值
summarise(mtcars,mean(disp))
#返回数据框中变量disp的标准差
summarise(mtcars,sd(disp))
#返回数据框中变量disp的最大值及最小值
summarise(mtcars,max(disp),min(disp))
#返回数据框mtcars的行数
summarise(mtcars,n())
#返回unique的gear数
summarise(mtcars,n_distinct(gear))
#返回disp的第一个值
summarise(mtcars,first(disp))
#返回disp的最后个值
summarise(mtcars,last(disp))

###############################抽样: sample###############################
#随机无重复的取10行数据
sample_n(mtcars,10)
#随机有重复的取50行数据
sample_n(mtcars,50,replace=TRUE)
#随机无重复的以mpg值做权重取10行数据
sample_n(mtcars,10,weight=mpg)

#默认size=1，相当于对全部数据无重复重新抽样
sample_frac(mtcars)
#随机无重复的取10%的数据
sample_frac(mtcars,0.1)
#随机有重复的取总行数1.5倍的数据
sample_frac(mtcars,1.5,replace=TRUE)
#随机无重复的以1/mpg值做权重取10%的数据
sample_frac(mtcars,0.1,weight=1/mpg)


###############################分组: group###############################
#使用变量cyl对mtcars分组，返回分组后数据集
by_cyl<-group_by(mtcars,cyl)
by_cyl
#返回每个分组中最大disp所在的行
filter(by_cyl,disp==max(disp))
#返回每个分组中变量名包含d的列，始终返回分组列cyl
select(by_cyl,contains("d"))
#使用mpg对每个分组排序
arrange(by_cyl,mpg)
#对每个分组无重复的取2行记录
sample_n(by_cyl,2)

#使用变量cyl对mtcars分组，然后对分组后数据集使用聚合函数
by_cyl<-group_by(mtcars,cyl)
#返回每个分组的记录数
summarise(by_cyl,n())
#求每个分组中disp和hp的均值
summarise(by_cyl,mean(disp),mean(hp))
#返回每个分组中唯一的gear的值
summarise(by_cyl,n_distinct(gear))
#返回每个分组第一个和最后一个disp值
summarise(by_cyl,first(disp))
summarise(by_cyl,last(disp))
#返回每个分组中最小的disp值
summarise(by_cyl,min(disp))
summarise(arrange(by_cyl,disp),min(disp))
#返回每个分组中最大的disp值
summarise(by_cyl,max(disp))
summarise(arrange(by_cyl,disp),max(disp))
#返回每个分组中disp第二个值
summarise(by_cyl,nth(disp,2))

#使用cyl对数据框分组
grouped<-group_by(mtcars,cyl)
#获取分组数据集所使用的分组变量
groups(grouped)
#ungroup从数据框中移除组合信息，因此返回的分组变量为NULL
groups(ungroup(grouped))

#返回每条记录所在分组id组成的向量
group_indices(mtcars,cyl)

#group_size用于返回每个分组的记录数，n_groups返回分成的组数
by_cyl<-group_by(mtcars,cyl)
#返回每个分组记录数组成的向量
group_size(by_cyl)
summarise(by_cyl,n())
table(mtcars$cyl)
#返回所分的组数
n_groups(by_cyl)
length(group_size(by_cyl))


###############################数据关联：join###############################
df1=data.frame(CustomerId=c(1:6),sex=c("f","m","f","f","m","m"),Product=c(rep("Toaster",3),rep("Radio",3)))
df2=data.frame(CustomerId=c(2,4,6,7),sex=c("m","f","m","f"),State=c(rep("Alabama",3),rep("Ohio",1)))
#内连接，默认使用"CustomerId"和"sex"连接
inner_join(df1,df2)
#左连接，默认使用"CustomerId"和"sex"连接
left_join(df1,df2)
#右连接，默认使用"CustomerId"和"sex"连接
right_join(df1,df2)
#全连接，默认使用"CustomerId"和"sex"连接
full_join(df1,df2)
#内连接，使用"CustomerId"连接，同名字段sex会自动添加后缀
inner_join(df1,df2,by=c("CustomerId"="CustomerId"))
#以CustomerId连接，返回df1中与df2匹配的记录
semi_join(df1,df2,by=c("CustomerId"="CustomerId"))
#以CustomerId和sex连接，返回df1中与df2不匹配的记录
anti_join(df1,df2)

###############################集合操作: set###############################
#########并集，交集，差集
mtcars$model<-rownames(mtcars)
first<-mtcars[1:20,]
second<-mtcars[10:32,]
#取两个集合的交集
intersect(first,second)
#取两个集合的并集，并去重
union(first,second)
#取两个集合的差集，返回first中存在但second中不存在的记录
setdiff(first,second)
#取两个集合的交集，返回second中存在但first中不存在的记录
setdiff(second,first)
#取两个集合的交集,不去重
union_all(first,second)
#判断两个集合是否相等，返回TRUE
setequal(mtcars,mtcars[32:1,])


