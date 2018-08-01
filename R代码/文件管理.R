######################################################################
#参考博客：
#http://www.cnblogs.com/Bfrican/p/4458090.html
#R语言和其他编程语言一样，都有对文件系统的操作，
#包括文件操作和目录操作，函数API都定义在base包中。

##批量生成文件夹,批量读取文件夹名称
##一. dir.create生成文件夹  二. dir读取文件夹名
setwd('F:/00/项目/社交网络/data')
dir()                          #查看当前目录的子目录和文件。
dir('F:/00/项目/社交网络/data')#查看指定目录的子目录和文件。
dir.create("try")
file.rename("try", "try2")    # 对目录重名
dir()                         
list.dirs()                    # 查看当前目录的子目录
dir(pattern='^stk_1_')         #只列出当前目录以字母R开头的子目录或文件
file.info(".")# 查看当前目录权限
dir.create(path="a1/b2/c3",recursive = TRUE)
system("tree")



.Library
.libPaths()
system.file(package = "pryr")
