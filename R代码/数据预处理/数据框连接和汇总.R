####################################
####数据框合并######

###paste
###paste将两个向量对应相粘（参数 collapse和sep）
x<-1:5
y<-letters[1:5]
paste(x,y)
help(paste)

###cbind 和 rbind
name1 <- c("Bob","Mary","Jane","Kim")
name2 <- c("Bob","Mary","Kim","Jane")
weight <- c(60,65,45,55)
height <- c(170,165,140,135)
birth <- c("1990-1","1980-2","1995-5","1996-4")
accept <- c("no","ok","ok","no")

df1 <- data.frame(name1,weight,height)
df2 <- data.frame(name2,birth,accept)
# 合并
rbind(df1,df2)
cbind(df1,df2)

# dplyr包中高效合并
bind_rows(df1,df2) 
bind_cols(df1,df2)

##merge##
##当两个数据框中有一列表示相同的含义，其他列不同时，可以按照这一列merge起来。
name1 <- c("Bob","Mary","Jane","Kim","Smith")
weight <- c(60,65,45,55,60)
name2 <- c("Bob","Mary","Kim","Jane","Eric")
height <- c(170,165,140,135,170)

df11 <- data.frame(name1,weight,stringsAsFactors=F) # 加这个参数是防止字符串自动转化为因子
df33 <- data.frame(name1,height,stringsAsFactors=F)
df22 <- data.frame(name2,height,stringsAsFactors=F) # 成员与前面不完全一样

merge(df11,df33) # 自动根据相同的列名匹配
merge(df11,df22,by.x="name1",by.y="name2") # 没有相同的列名则指定根据这两列融合
# 上面默认保留了df11和df22共有的行
merge(df11,df22,by.x="name1",by.y="name2",all=T) # 保留所有出现过的行,没有的显示NA
merge(df11,df22,by.x="name1",by.y="name2",all.x=T) # 保留所有x的行
merge(df11,df22,by.x="name1",by.y="name2",all.y=T) # 保留所有y的行

###下面使用dplyr包
library(dplyr)
inner_join(df11,df33) # 自动根据相同的列名匹配 
full_join(df11,df22,by=c("name1"="name2"))
left_join(df11,df22,by=c("name1"="name2")) # 只保留前者的行
right_join(df11,df22,by=c("name1"="name2")) # 只保留后者的行
semi_join(df11,df22,by=c("name1"="name2")) # 保留共有的行，同时只返回前者的列
anti_join(df11,df22,by=c("name1"="name2")) # 返回后者没有的前者的行，同时只返回前者的列,有搜索功能

####计算并增加行列######
name1 <- c("Bob","Mary","Jane","Kim")
weight <- c(60,65,45,55)
height <- c(170,165,140,135)
df1 <- data.frame(name1,weight,height)

#R基础函数
df2 <- transform(df1,BMI=weight/height^2) # 第一种方法
df2
df1$BMI <- df1$weight/df1$height^2 # 第二种方法， 每一步都要$，很麻烦
df1
#使用dplyr包
mutate(df1,BMI=weight/height^2)


##汇总计算
apply(df1[,-1],2,mean) # R基础函数
#使用dplyr包
summarise(df1,arv_weight=mean(weight),arv_height=mean(height))

##summarise_all 和 summarise_if对所有列进行计算或者根据列的数据类型选择计算
name1 <- c("Bob","Mary","Jane","Kim")
weight <- c(60,65,45,55)
height <- c(170,165,140,135)
weta <- 1:4
df1 <- data.frame(name1,weight,height,weta)

summarise(df1,avg_weight=mean(weight),avg_height=mean(height)) # 很麻烦地每个都指定
summarise_all(df1[-1],mean) # 对选出来的所有列都进行计算
summarise_if(df1,is.numeric,mean) # 检验出所有是数值的列，全部求均值

##summarise_at配合vars的用法，筛选出符合条件的列名对应的列
summarise_at(df1,vars(weight,height,weta),mean) # 配合vars函数，一次选择多列
summarise_at(df1,vars(weight:weta),mean) # 从哪到哪
u <- c("weight","height")
summarise_at(df1,vars(one_of(u)),mean) # 可以接字符串向量
summarise_at(df1,u,mean) # 也可以直接接字符串向量
summarise_at(df1,u,mean,trim=1) # mean的参数可以接在后面

summarise_at(df1,vars(contains("eig")),mean) # 匹配含有的
summarise_at(df1,vars(matches(".t.")),mean) # 使用正则表达式
summarise_at(df1,vars(starts_with("w")),mean) # 匹配以此为开头的
summarise_at(df1,vars(ends_with("ht")),mean) # 匹配以此为结尾的
summarise_at(df1[,-1],vars(everything()),mean) # 选择所有列

##funs的用法
summarise_all(df1[,-1],funs(mean,sum))
summarise_all(df1[,-1],funs(sum(.*2))) # 数据用.表示
summarise_all(df1[,-1],funs(medi=median)) # 指定得到的列后缀名
summarise_all(df1[,-1],funs("in"=median)) # 或者加引号
mutate_all(df1[,-1],funs(.^2))
summarise_if(df1,is.numeric,funs(mean,sum))
summarise_at(df1,vars(ends_with("t")),funs(mean,sum))

####分组计算######
name1 <- c("Bob","Mary","Jane","Kim")
weight <- c(60,65,45,55)
height <- c(170,165,140,135)
accept <- c("no","ok","ok","no")
df <- data.frame(name1,weight,height,accept)

tapply(df$height,df$accept,mean)  # 使用tapply函数，按照accept分类

with(df,{ # 使用aggregate函数
  aggregate(height,by=list(accept),FUN=mean)
})

####使用dplyr包中的函数
group_df <- group_by(df,accept)
summarise(group_df,arv_height=mean(height),count=n()) # 其中n()是查数的意思
# 使用扩展函数
summarise_all(group_df[,-1],mean)
summarise_if(group_df,is.numeric,mean)
summarise_at(group_df,vars(contains("eigh")),funs(sum,mean))


######融合重铸###########
library(reshape2)
names(airquality) <- tolower(names(airquality))
View(airquality)
aqm <- melt(airquality, id=c("month", "day"), na.rm=TRUE) # 除了month和day两列，其他列摞起来，为了等长，m和d列循环对齐

dcast(aqm, day + variable ~ month) # 保持day和variable不变，month中的元素分类映射到列上去
dcast(aqm, variable + day ~ month) # 换一下顺序，重复的variable连在一起，对应不一样的day，这样的方式排列
dcast(aqm, day ~ variable + month) # 只保留day列

# 加入计算
dcast(aqm, day ~ month, mean) # 没出现的那个变量被平均掉了

# dcast 和 acast区别
dcast(aqm, variable + month ~ day) 
acast(aqm, variable + month ~ day) # acast和dcast的功能基本上相同，只是dcast会把分组信息作为一列或几列显示，而acast会将其作为行名
acast(aqm, day ~ month, mean) # 保留的列作为合并在一起作为列名
acast(aqm, variable ~ month ~ day) # acast 多出来的功能，生成一个三维数组，按照day的值分成31个矩阵
