library(lars)
data("diabetes")
attach(diabetes)
kappa(x2)#查看自变量矩阵的条件数

model.step <- step(lm(y~x2))
summary(model.step)

#检验模型是否符合高斯-马尔科夫假设
plot(model.step$fit,model.step$res)
abline(h=0,lty=2)
shapiro.test(model.step$res) #检验残差是否正太

##岭回归
library(MASS)
ridgelm = lm.ridge(y~.,data=x2)
ridgelm$coef
plot(lm.ridge(y~.,data = x2,lambda = seq(0,10,1)))
select(lm.ridge(y~.,data = x2,lambda = seq(0,10,1)))

##lasso回归
model.lasso = lars(x2,y)
plot(model.lasso)
summary(model.lasso)
cv.model.lasso = cv.lars(x2,y,K=10)
select = cv.model.lasso$index[which.min(cv.model.lasso$cv)]
coef = coef.lars(model.lasso,mode="fraction",s=select)
coef[which(coef!=0)]


n.Cp = which.min(model.lasso$Cp)
coef1 = coef.lars(model.lasso,model="step",s=n.Cp)
coef1[which(coef1!=0)]

#适应性lasso
library(msgps)
model.alasso = msgps(x2,as.vector(y),penalty = "alasso",gamma=1,lambda = 0)
summary(model.alasso)
plot(model.alasso)

