setwd("//udrive.win.psu.edu/Users/j/q/jql5883/Desktop/math462")
getwd()

bears=read.csv("bears.txt", header=T, sep="")
bears=bears[bears$Obs.No==1,]
head(bears)

attach(bears)

lm.bear=lm(Weight~Head.L+Head.W+Neck.G+Length+Chest.G)

plot(bears[5:9])
cor(bears[,c(10,5:9)])

X=model.matrix(Weight~Head.L+Head.W+Neck.G+Length+Chest.G)
R2=vector("numeric",5)
for(j in 1:5){
  y_tmp=X[,1+j]
  x_tmp=as.matrix(X[,-c(1,1+j)])
  lm_fit=lm(y_tmp~x_tmp)
  R2[j]=summary(lm_fit)$r.squared
  }
VIF=1/(1-R2)
names(VIF)=c('Head.L', 'Head.W', 'Neck.G', 'Length', 'Chest.G')
VIF
R2

lm.bear.drop=lm(Weight~Head.L+Head.W+Length+Chest.G)
summary(lm.bear)
summary(lm.bear.drop)

X2=model.matrix(Weight~Head.L+Head.W+Length+Chest.G)
R2_2=vector("numeric",4)
for(j in 1:4){
  y_tmp=X2[,1+j]
  x_tmp=as.matrix(X2[,-c(1,1+j)])
  lm_fit=lm(y_tmp~x_tmp)
  R2_2[j]=summary(lm_fit)$r.squared
}
VIF2=1/(1-R2_2)
names(VIF2)=c('Head.L', 'Head.W', 'Length', 'Chest.G')
VIF2
R2_2
