sample<-seq(1,14,by=1)
forehead<-c(249,189,128,111,184,233,313,120,151,196,135,157,145,218)
forearm<-c(176,28,136,87,145,109,151,63,101,95,121,98,97,102)
back<-c(95,51,55,51,58,77,121,37,39,49,66,58,49,85)
sweatgland<-data.frame(sample,forehead,forearm,back);sweatgland

plot(forehead~sample,pch=15,col="DarkTurquoise",ylim=c(0,400),ylab="Number of active sweat glands per cm2",main="Number of active sweat glands per cm2 in forehead, forearm , forearm and back")#pch
points(sample,forearm,pch=16,col="DeepPink",cex=1)#cex表示散点的大小
points(sample,back,pch=17,col="RosyBrown",cex=1)
lines(forehead,col="DarkTurquoise",lty=1)#lty=1表示用实线连起来
lines(forearm,col="DeepPink",lty=2)#lty=2表示用虚线连起来
lines(back,col="RosyBrown",lty=3)#lty=3表示用点线连起来
