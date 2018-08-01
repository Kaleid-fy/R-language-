library(rJava)
library(Rwordseg)
library(wordcloud)
library(jiebaR)
f <- scan('C:/text1.txt',sep='\n',what='',encoding="UTF-8")
seg <- qseg[f] #使用qseg类型分词，并把结果保存到对象seg中
#另一种分词 library(Rwordseg)  dm=segmentCN(f)
seg <- seg[nchar(seg)>1] #去除字符长度小于2的词语
seg <- table(seg) #统计词频
seg <- seg[!grepl('[0-9]+',names(seg))] #去除数字
length(seg) #查看处理完后剩余的词数
seg1 <- sort(seg, decreasing = TRUE)[1:100] #降序排序，并提取出现次数最多的前100个词语
seg1 #查看100个词频最高的
wordcloud(names(seg1), seg1, colors = rainbow(100), random.order=F)