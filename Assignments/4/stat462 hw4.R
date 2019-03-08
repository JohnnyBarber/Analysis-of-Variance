setwd("//udrive.win.psu.edu/Users/j/q/jql5883/Desktop/math462")
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

#A-------------------------------------------------------------------------------------
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

#B-------------------------------------------------------------------------------------
lm_bodyfat_W=lm(bodyfat.percent~WristC,data=bodyfat)
summary(lm_bodyfat_W)

beta0_1=lm_bodyfat_W$coefficients[1]
beta1_1=lm_bodyfat_W$coefficients[2]

par(mfrow=c(1,2))
plot(bodyfat$WristC,bodyfat$bodyfat.percent)
abline(beta0_1,beta1_1,col="blue")

lm_bodyfat_R=lm(residuals_fix~WristC,data=bodyfat)
summary(lm_bodyfat_R)

beta0_2=lm_bodyfat_R$coefficients[1]
beta1_2=lm_bodyfat_R$coefficients[2]

plot(bodyfat$WristC,residuals_fix)
abline(beta0_2,beta1_2,col="red")

#C-------------------------------------------------------------------------------------
attach(bodyfat)

par(mfrow=c(1,1))

over45=bodyfat.percent[Over45==1]
under45=bodyfat.percent[Over45==0]
boxplot(bodyfat.percent~Over45,col=c('magenta','blue'),ylab='Score')
t.test(over45,under45,var.equal=TRUE)

dummy=(Over45==1)
lm_bodyfat_age=lm(bodyfat.percent~dummy)
summary(lm_bodyfat_age)

detach(bodyfat)

#D-------------------------------------------------------------------------------------
lm_bodyfat_combine=lm(bodyfat$bodyfat.percent~bodyfat$AbdomenC+dummy+bodyfat$AbdomenC:dummy)
summary(lm_bodyfat_combine)

anova(lm_bodyfat_A_fix,lm_bodyfat_combine)
