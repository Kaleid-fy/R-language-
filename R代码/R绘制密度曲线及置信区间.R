xx <- faithful$eruptions
fit1 <- density(xx)
plot(fit1)

fit2 <- replicate(10000,{
  x <- sample(xx,replace = T);
  density(x,from = min(fit1$x),to=max(fit1$x))$y
})
dim(fit2)

fit3 <- apply(fit2,1,quantile,c(0.025,0.975))
plot(fit1,ylim=range(fit3))

polygon(c(fit1$x,rev(fit1$x)),
        c(fit3[1,],rev(fit3[2,])),
        col='grey',border = F)
lines(fit1)
