setwd("//udrive.win.psu.edu/Users/j/q/jql5883/Desktop/math462")
getwd()

bears=read.csv("bears.txt", header=T, sep="")
bears=bears[bears$Obs.No==1,]
head(bears)

attach(bears)

lm.bear=lm(Weight~Head.W)
plot(Head.W,lm.bear$residuals)

w=1/(Head.W^2)
W=diag(w)
X=model.matrix(Weight~Head.W)
y=Weight
beta_hat_weight=solve(t(X)%*%W%*%X,t(X)%*%W%*%y)
y_hat=X%*%beta_hat_weight
res_weight=sqrt(w)*(y-y_hat)
plot(y_hat,res_weight,xlab='Fitted values',ylab='Weighted residuals',main='Residuals vs Fitted -WLS')
abline(h=0)

lm.bear.transf=lm(sqrt(Weight)~Head.W)
par(mfrow=c(2,2))
plot(lm.bear.transf)

Sex=Sex-1
lm.bear.full=lm(Weight~Head.L+Head.W+Neck.G+Length+Chest.G+Sex,data=bears)
summary(lm.bear.full)
lm.back1=update(lm.bear.full, .~.-Head.W)
summary(lm.back1)
lm.back2=update(lm.back1, .~.-Sex)
summary(lm.back2)

plot(lm.back2)
