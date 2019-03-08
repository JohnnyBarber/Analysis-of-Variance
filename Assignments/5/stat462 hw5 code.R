setwd("//udrive.win.psu.edu/Users/j/q/jql5883/Desktop/stat462")
getwd()

#data check----------------------------------------------------------------------------------
bodyfat=read.table("BODY_FAT.TXT", header=T, sep="")
bodyfat.recompute=(495/bodyfat$Density)-450
head(round(bodyfat.recompute,digits=1))
bodyfat.recompute>100
bodyfat.recompute<0
bodyfat.recompute[182]=0

bodyfat$SiriBFperc==round(bodyfat.recompute,digits=1)

bodyfat[,2]<-round(bodyfat.recompute,digits=1)
colnames(bodyfat)[colnames(bodyfat)=="SiriBFperc"] <- "bodyfat.percent"

bodyfat<-bodyfat[-42,]

attach(bodyfat)
lm_bodyfat_A=lm(bodyfat.percent~AbdomenC,data=bodyfat)
summary(lm_bodyfat_A)

n=251
p=2

residuals=lm_bodyfat_A$residuals
sigma_hat=summary(lm_bodyfat_A)$sigma
X1=model.matrix(bodyfat.percent~AbdomenC)
H=X1%*%solve(t(X1)%*%X1)%*%t(X1)
h=diag(H)
h
r=residuals/(sigma_hat*sqrt(1-h))
t=r*sqrt((n-p-1)/(n-p-r^2))
par(mfrow=c(1,3))
plot(t,xlab='Observation #',ylab='Studentized residuals',main='Studentized residuals')

thresh2=2*p/n
thresh3=3*p/n
plot(h,xlab='Observation #',ylab='Leverage',main='Leverage')
abline(h=thresh2,lty=2,col="red")
abline(h=thresh3,lty=2,col="blue")

D1=(1/p)*r^2*h/(1-h)
plot(D1,xlab='Observation #',ylab='Cook\'s distance',main='Cook\'s distance')

which(D1>0.8)
which(h>0.04) 
which(abs(t)>3)

bodyfat.percent[39]

par(mfrow=c(1,1))
plot(bodyfat.percent,AbdomenC,
     col=ifelse(bodyfat.percent==35.2, "red", "black"))

bodyfat<-bodyfat[-39,]

lm_bodyfat_A_fix=lm(bodyfat$bodyfat.percent~bodyfat$AbdomenC,data=bodyfat)
summary(lm_bodyfat_A_fix)

par(mfrow=c(2,2))
plot(lm_bodyfat_A_fix)
shapiro.test(lm_bodyfat_A_fix$residuals)

n_fix=250

residuals_fix=lm_bodyfat_A_fix$residuals
sigma_hat_fix=summary(lm_bodyfat_A_fix)$sigma
X1_fix=model.matrix(bodyfat$bodyfat.percent~bodyfat$AbdomenC)
H_fix=X1_fix%*%solve(t(X1)%*%X1)%*%t(X1)
h_fix=diag(H_fix)
h_fix
r_fix=residuals_fix/(sigma_hat_fix*sqrt(1-h_fix))
t_fix=r_fix*sqrt((n_fix-p-1)/(n_fix-p-r_fix^2))
par(mfrow=c(1,3))
plot(t_fix,xlab='Observation #',ylab='Studentized residuals',main='Studentized residuals')

thresh2=2*p/n
thresh3=3*p/n
plot(h_fix,xlab='Observation #',ylab='Leverage',main='Leverage')
abline(h=thresh2,lty=2,col="red")
abline(h=thresh3,lty=2,col="blue")

D1_fix=(1/p)*r_fix^2*h_fix/(1-h_fix)
plot(D1_fix,xlab='Observation #',ylab='Cook\'s distance',main='Cook\'s distance')

detach(bodyfat)

#A---------------------------------------------------------------------------------------------
lm.full=lm(bodyfat.percent~Over45+Weight+Height+NeckC+ChestC+AbdomenC+HipC+ThighC+KneeC+AnkleC+BicepsC+ForearmC+WristC,data=bodyfat)
summary(lm.full)

lm.back1=update(lm.full,.~.-Over45)
summary(lm.back1)

lm.back2=update(lm.back1,.~.-KneeC)
summary(lm.back2)

lm.back3=update(lm.back2,.~.-ThighC)
summary(lm.back3)

lm.back4=update(lm.back3,.~.-Weight)
summary(lm.back4)

lm.back5=update(lm.back4,.~.-ForearmC)
summary(lm.back5)

lm.back6=update(lm.back5,.~.-AnkleC)
summary(lm.back6)

lm.backward=lm.back6

#B---------------------------------------------------------------------------------------------
lm.empty=lm(bodyfat.percent~1,data=bodyfat)
summary(lm.empty)

pvalue.for1=numeric(13)
names(pvalue.for1)=c("Over45","Weight","Height","NeckC","ChestC","AbdomenC","HipC","ThighC","KneeC","AnkleC","BicepsC","ForearmC","WristC")
pvalue.for1[1]=summary(update(lm.empty,.~.+Over45))$coefficients[2,4]
pvalue.for1[2]=summary(update(lm.empty,.~.+Weight))$coefficients[2,4]
pvalue.for1[3]=summary(update(lm.empty,.~.+Height))$coefficients[2,4]
pvalue.for1[4]=summary(update(lm.empty,.~.+NeckC))$coefficients[2,4]
pvalue.for1[5]=summary(update(lm.empty,.~.+ChestC))$coefficients[2,4]
pvalue.for1[6]=summary(update(lm.empty,.~.+AbdomenC))$coefficients[2,4]
pvalue.for1[7]=summary(update(lm.empty,.~.+HipC))$coefficients[2,4]
pvalue.for1[8]=summary(update(lm.empty,.~.+ThighC))$coefficients[2,4]
pvalue.for1[9]=summary(update(lm.empty,.~.+KneeC))$coefficients[2,4]
pvalue.for1[10]=summary(update(lm.empty,.~.+AnkleC))$coefficients[2,4]
pvalue.for1[11]=summary(update(lm.empty,.~.+BicepsC))$coefficients[2,4]
pvalue.for1[12]=summary(update(lm.empty,.~.+ForearmC))$coefficients[2,4]
pvalue.for1[13]=summary(update(lm.empty,.~.+WristC))$coefficients[2,4]
pvalue.for1

lm.for1=update(lm.empty,.~.+AbdomenC)
summary(lm.for1)

pvalue.for2=numeric(12)
names(pvalue.for2)=c("Over45","Weight","Height","NeckC","ChestC","HipC","ThighC","KneeC","AnkleC","BicepsC","ForearmC","WristC")
pvalue.for2[1]=summary(update(lm.for1,.~.+Over45))$coefficients[3,4]
pvalue.for2[2]=summary(update(lm.for1,.~.+Weight))$coefficients[3,4]
pvalue.for2[3]=summary(update(lm.for1,.~.+Height))$coefficients[3,4]
pvalue.for2[4]=summary(update(lm.for1,.~.+NeckC))$coefficients[3,4]
pvalue.for2[5]=summary(update(lm.for1,.~.+ChestC))$coefficients[3,4]
pvalue.for2[6]=summary(update(lm.for1,.~.+HipC))$coefficients[3,4]
pvalue.for2[7]=summary(update(lm.for1,.~.+ThighC))$coefficients[3,4]
pvalue.for2[8]=summary(update(lm.for1,.~.+KneeC))$coefficients[3,4]
pvalue.for2[9]=summary(update(lm.for1,.~.+AnkleC))$coefficients[3,4]
pvalue.for2[10]=summary(update(lm.for1,.~.+BicepsC))$coefficients[3,4]
pvalue.for2[11]=summary(update(lm.for1,.~.+ForearmC))$coefficients[3,4]
pvalue.for2[12]=summary(update(lm.for1,.~.+WristC))$coefficients[3,4]
pvalue.for2

lm.for2=update(lm.for1,.~.+Weight)
summary(lm.for2)

pvalue.for3=numeric(11)
names(pvalue.for3)=c("Over45","Height","NeckC","ChestC","HipC","ThighC","KneeC","AnkleC","BicepsC","ForearmC","WristC")
pvalue.for3[1]=summary(update(lm.for2,.~.+Over45))$coefficients[4,4]
pvalue.for3[2]=summary(update(lm.for2,.~.+Height))$coefficients[4,4]
pvalue.for3[3]=summary(update(lm.for2,.~.+NeckC))$coefficients[4,4]
pvalue.for3[4]=summary(update(lm.for2,.~.+ChestC))$coefficients[4,4]
pvalue.for3[5]=summary(update(lm.for2,.~.+HipC))$coefficients[4,4]
pvalue.for3[6]=summary(update(lm.for2,.~.+ThighC))$coefficients[4,4]
pvalue.for3[7]=summary(update(lm.for2,.~.+KneeC))$coefficients[4,4]
pvalue.for3[8]=summary(update(lm.for2,.~.+AnkleC))$coefficients[4,4]
pvalue.for3[9]=summary(update(lm.for2,.~.+BicepsC))$coefficients[4,4]
pvalue.for3[10]=summary(update(lm.for2,.~.+ForearmC))$coefficients[4,4]
pvalue.for3[11]=summary(update(lm.for2,.~.+WristC))$coefficients[4,4]
pvalue.for3

lm.for3=update(lm.for2,.~.+WristC)
summary(lm.for3)

pvalue.for4=numeric(10)
names(pvalue.for4)=c("Over45","Height","NeckC","ChestC","HipC","ThighC","KneeC","AnkleC","BicepsC","ForearmC")
pvalue.for4[1]=summary(update(lm.for3,.~.+Over45))$coefficients[5,4]
pvalue.for4[2]=summary(update(lm.for3,.~.+Height))$coefficients[5,4]
pvalue.for4[3]=summary(update(lm.for3,.~.+NeckC))$coefficients[5,4]
pvalue.for4[4]=summary(update(lm.for3,.~.+ChestC))$coefficients[5,4]
pvalue.for4[5]=summary(update(lm.for3,.~.+HipC))$coefficients[5,4]
pvalue.for4[6]=summary(update(lm.for3,.~.+ThighC))$coefficients[5,4]
pvalue.for4[7]=summary(update(lm.for3,.~.+KneeC))$coefficients[5,4]
pvalue.for4[8]=summary(update(lm.for3,.~.+AnkleC))$coefficients[5,4]
pvalue.for4[9]=summary(update(lm.for3,.~.+BicepsC))$coefficients[5,4]
pvalue.for4[10]=summary(update(lm.for3,.~.+ForearmC))$coefficients[5,4]
pvalue.for4

lm.for4=update(lm.for3,.~.+BicepsC)
summary(lm.for4)

pvalue.for5=numeric(9)
names(pvalue.for5)=c("Over45","Height","NeckC","ChestC","HipC","ThighC","KneeC","AnkleC","ForearmC")
pvalue.for5[1]=summary(update(lm.for4,.~.+Over45))$coefficients[6,4]
pvalue.for5[2]=summary(update(lm.for4,.~.+Height))$coefficients[6,4]
pvalue.for5[3]=summary(update(lm.for4,.~.+NeckC))$coefficients[6,4]
pvalue.for5[4]=summary(update(lm.for4,.~.+ChestC))$coefficients[6,4]
pvalue.for5[5]=summary(update(lm.for4,.~.+HipC))$coefficients[6,4]
pvalue.for5[6]=summary(update(lm.for4,.~.+ThighC))$coefficients[6,4]
pvalue.for5[7]=summary(update(lm.for4,.~.+KneeC))$coefficients[6,4]
pvalue.for5[8]=summary(update(lm.for4,.~.+AnkleC))$coefficients[6,4]
pvalue.for5[9]=summary(update(lm.for4,.~.+ForearmC))$coefficients[6,4]
pvalue.for5

lm.for5=update(lm.for4,.~.+AnkleC)
summary(lm.for5)

pvalue.for6=numeric(8)
names(pvalue.for6)=c("Over45","Height","NeckC","ChestC","HipC","ThighC","KneeC","ForearmC")
pvalue.for6[1]=summary(update(lm.for5,.~.+Over45))$coefficients[7,4]
pvalue.for6[2]=summary(update(lm.for5,.~.+Height))$coefficients[7,4]
pvalue.for6[3]=summary(update(lm.for5,.~.+NeckC))$coefficients[7,4]
pvalue.for6[4]=summary(update(lm.for5,.~.+ChestC))$coefficients[7,4]
pvalue.for6[5]=summary(update(lm.for5,.~.+HipC))$coefficients[7,4]
pvalue.for6[6]=summary(update(lm.for5,.~.+ThighC))$coefficients[7,4]
pvalue.for6[7]=summary(update(lm.for5,.~.+KneeC))$coefficients[7,4]
pvalue.for6[8]=summary(update(lm.for5,.~.+ForearmC))$coefficients[7,4]
pvalue.for6

lm.forward=lm.for5

#C---------------------------------------------------------------------------------------------
install.packages("leaps")
require(leaps)
subset=regsubsets(bodyfat.percent~Over45+Weight+Height+NeckC+ChestC+AbdomenC+HipC+ThighC+KneeC+AnkleC+BicepsC+ForearmC+WristC,data=bodyfat,nvmax=13)
sum.subset=summary(subset)
sum.subset$which

p.full=14
p=2:p.full
RSS.p=sum.subset$rss
RSS.p
totalSS=sum((bodyfat$bodyfat.percent-mean(bodyfat$bodyfat.percent))^2)
R2.p=1-RSS.p/totalSS
R2.p

n=nrow(bodyfat)
R2.adj=1-(RSS.p/(n-p))/(totalSS/(n-1))
R2.adj

par(mfrow=c(2,2))
plot(p,R2.adj,xlab="Number of betas",ylab="Adjusted R-squared")

lm.R2.adj=lm(bodyfat.percent~Height+NeckC+ChestC+AbdomenC+HipC+AnkleC+BicepsC+ForearmC+WristC,data=bodyfat) #p=10

sigma.hat.full=summary(lm.full)$sigma
C.p=RSS.p/(sigma.hat.full^2)+2*p-n
C.p
plot(p,C.p,xlab="Number of betas",ylab="Mallow's Cp")
abline(0,1)

lm.C.p=lm(bodyfat.percent~Weight+AbdomenC+BicepsC+WristC,data=bodyfat) #p=5

aic.p=n*log(RSS.p/n)+2*p
aic.p
plot(p,aic.p,xlab="Number of betas",ylab="AIC")

lm.aic.p=lm(bodyfat.percent~Height+NeckC+ChestC+AbdomenC+HipC+BicepsC+WristC,data=bodyfat) #p=8

bic.p=n*log(RSS.p/n)+p*log(n)
bic.p
plot(p,bic.p,xlab="Number of betas",ylab="BIC")

lm.bic.p=lm(bodyfat.percent~Weight+AbdomenC+WristC,data=bodyfat) #p=4

#D---------------------------------------------------------------------------------------------
#install.packages("car")
library(car)

par(mfrow=c(2,2))

vif(lm.backward)
vif(lm.forward)
vif(lm.R2.adj)
vif(lm.C.p)
vif(lm.aic.p)
vif(lm.bic.p)

plot(lm.backward)
shapiro.test(lm.backward$residuals)
summary(lm.backward)

plot(lm.forward)
shapiro.test(lm.forward$residuals)
summary(lm.forward)

plot(lm.R2.adj)
shapiro.test(lm.R2.adj$residuals)
summary(lm.R2.adj)

plot(lm.C.p)
shapiro.test(lm.C.p$residuals)
summary(lm.C.p)

plot(lm.aic.p)
shapiro.test(lm.aic.p$residuals)
summary(lm.aic.p)

plot(lm.bic.p)
shapiro.test(lm.bic.p$residuals)
summary(lm.bic.p)

x0=data.frame(Weight=mean(bodyfat$Weight),AbdomenC=mean(bodyfat$AbdomenC),WristC=mean(bodyfat$WristC))
PI=predict(lm.bic.p,new=x0,interval='prediction',level=0.95)
PI


