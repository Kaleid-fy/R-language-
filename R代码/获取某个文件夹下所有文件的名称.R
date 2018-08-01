#获取某个文件夹下所有文件的名称
setwd('F:/00/项目/社交网络/data')
dir()
#dir(pattern='')，利用正则表达式匹配文件名pattern=''
dir(pattern='^stk_1')


# library(openxlsx)
# 
# ##获取工作簿每个sheet的名字
# 
# wb<-loadWorkbook(file = "F:/00/项目/基因数据 - 副本/处理数据/杨宇飞课题组送检患者突变.xlsx")
# ws_names<-names(wb)
# 
# ##ssign()批量生成数据框
# for(i in 1:66){
#   print(i)
#   assign(paste("a",i,sep=""),
#          read.xlsx(xlsxFile ="F:/00/项目/基因数据 - 副本/处理数据/杨宇飞课题组送检患者突变.xlsx", sheet = i, skipEmptyRows = FALSE)[,c("Gene","Gene.ID","Ref","Alt","Hom.Het","AF","Func")])
# }
# 
# ##貌似循环调用起来没有好的方法
# # all_list<-list()
# all_list<-list(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,
#                a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a32,a33,a34,a35,a36,a37,a38,a39,a40,
#                a41,a42,a43,a44,a45,a46,a47,a48,a49,a50,a51,a52,a53,a54,a55,a56,a57,a58,a59,a60,
#                a61,a62,a63,a64,a65,a66)
# 
# for(j in 1:66){
#   mz<-paste("C:/Users/dell/Desktop/基因数据 - 副本/分开/",ws_names[j],".csv")
#   write.csv(all_list[[j]],mz)
# }