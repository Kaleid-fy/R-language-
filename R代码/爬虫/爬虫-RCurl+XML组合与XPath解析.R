#加载包：
library("XML")
library("stringr")
library("RCurl")
library("dplyr")
library("rvest")
#提供目标网址链接/报头参数
url<-'https://read.douban.com/search?q=Python'
#header =c('User-Agent'='Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/61.0.3163.79 Safari/537.36')
header =c('Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:56.0) Gecko/20100101 Firefox/56.0')

########构建抓取函数：#########
getcontent<-function(url){
  #这个数据框是为最终的数据汇总返回提供的初始值
  myresult=data.frame()
  #这些空向量是遍历单页书籍记录提供的初始值
  title=author=category=subtitle=eveluate_nums=rating=price=c()
  #开始遍历网页
  for (page in seq(0,3)){
    #遍历不同页面
    link<-paste0(url,'&start=',page*10)
    #请求网页并解析
    content<-getURL(link,httpheader=header) %>% htmlParse() 
    #计算单页书籍条目数
    length<-content %>% xpathSApply(.,"//ol[@class='ebook-list column-list']/li") %>% xmlSize()
    ###提取标题：
    title<-content %>% xpathSApply(.,"//ol/li//div[@class='title']/a| //ol/li//h4/a",xmlValue) %>% c(title,.)
    ###提取图书类别：
    category=content %>% xpathSApply(.,"//span[@class='category']/span[2]/span | //p[@class='category']/span[@class='labled-text'] | //div[@class='category']",xmlValue) %>% c(category,.)
    ###提取作者/副标题/评论数/评分/价格信息：
    author_text=subtitle_text=eveluate_nums_text=rating_text=price_text=rep('',length)
    for (i in 1:length){
      ###提取作者
      author_text[i]=content %>% xpathSApply(.,sprintf("//li[%d]//p[@class]//span/following-sibling::span/a | //li[%d]//div[@class='author']/a",i,i),xmlValue) %>% paste(.,collapse='/')
      ###考虑副标题是否存在
      if (content %>% xpathSApply(.,sprintf("//ol/li[%d]//p[@class='subtitle']",i),xmlValue) %>% length!=0){
        subtitle_text[i]=content %>% xpathSApply(.,sprintf("//ol/li[%d]//p[@class='subtitle']",i),xmlValue)
      }
      ###考虑评价是否存在：
      if (content %>% xpathSApply(.,sprintf("//ol/li[%d]//a[@class='ratings-link']/span",i),xmlValue) %>% length!=0){
        eveluate_nums_text[i]=content %>% xpathSApply(.,sprintf("//ol/li[%d]//a[@class='ratings-link']/span",i),xmlValue)
      }
      ###考虑评分是否存在：
      if (content %>% xpathSApply(.,sprintf("//ol/li[%d]//div[@class='rating list-rating']/span[2]",i),xmlValue) %>% length!=0){
        rating_text[i]=content %>% xpathSApply(.,sprintf("//ol/li[%d]//div[@class='rating list-rating']/span[2]",i),xmlValue)
      }
      ###考虑价格是否存在：
      if (content %>% xpathSApply(.,sprintf("//ol/li[%d]//span[@class='price-tag ']",i),xmlValue) %>% length!=0){
        price_text[i]=content %>% xpathSApply(.,sprintf("//ol/li[%d]//span[@class='price-tag ']",i),xmlValue) 
      }
    }
    #拼接以上通过下标遍历的书籍记录数
    author=c(author,author_text)
    subtitle=c(subtitle,subtitle_text) 
    eveluate_nums=c(eveluate_nums,eveluate_nums_text)
    rating=c(rating,rating_text)
    price=c(price,price_text)  
    #打印单页任务状态
    print(sprintf("page %d is over!!!",page))
  }
  #构建数据框
  myresult=data.frame(title,subtitle,author,category,price,rating,eveluate_nums)
  #打印总体任务状态
  print("everything is OK")
  #返回最终汇总的数据框
  return(myresult)
}
###################

myresult=getcontent(url)

str(myresult)

myresult$price<-myresult$price %>% sub("元|免费","",.) %>% as.numeric()
myresult$rating<-as.numeric(myresult$rating)
myresult$eveluate_nums<-as.numeric(myresult$eveluate_nums)

DT::datatable(myresult)
