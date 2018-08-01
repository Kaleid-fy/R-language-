### 变量哑变量处理
# 	caret包中有一个dummyVars函数，可用变量虚拟化批处理。dummyVars()函数的使用格式为： 
#   dummyVars(formula, data, sep = ".", levelsOnly = FALSE, fullRank = FALSE, ...) 
customers <- data.frame(
  id=c(10,20,30,40,50), 
  gender=c('male','female','female','male','female'), 
  mood=c('happy','sad','happy','sad','happy'), 
  outcome=c(1,1,0,0,0))
customers
library(caret)
# 哑变量处理
dmy <- dummyVars(" ~ .", data = customers)
trsf <- data.frame(predict(dmy, newdata = customers))
print(trsf)
