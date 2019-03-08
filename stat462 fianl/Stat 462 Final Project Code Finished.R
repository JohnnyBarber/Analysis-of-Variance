#Read in the dataset and give each columns name--------------------------------------------------------------------------------------------------
setwd("//udrive.win.psu.edu/Users/j/q/jql5883/Desktop/stat462/stat462 fianl")
getwd()

Body=read.table('body.dat.txt',header=FALSE,sep="")
colnames(Body) <- c("biacromialD","biiliacD","bitrochantericD","chest_depth",
                    "chestD","elbowD","wristD","kneeD","ankleD",
                    "shoulderG","chestG","waistG","navelG","hipG",
                    "highG","bicepG","forearmG","kneeG","calfMaxG",
                    "ankleMinG","wristMinG","age","weight","height","gender")



write.table(Body, "//udrive.win.psu.edu/Users/j/q/jql5883/Desktop/stat462/stat462 fianl", sep="\t")

Body_updated=read.table('Body_updated.txt',header=TRUE,sep="")

install.packages("car")

#exploratory data analysis--------------------------------------------------------------------------------------------------
par(mfrow=c(1,1))
plot(Body_updated)

#outliers-------------------------------------------------------------------------------------------------
attach(Body_updated)

#alpha-to-remove=0.10

lm_full=lm(weight~biacromialD+biiliacD+bitrochantericD+chest_depth+chestD+elbowD+wristD+kneeD+ankleD+shoulderG+chestG+waistG+navelG+hipG+highG+bicepG+forearmG+kneeG+calfMaxG+ankleMinG+wristMinG+age+height+gender,data=Body_updated)
summary(lm_full)

par(mfrow=c(1,3))

residuals=lm_full$residuals
sigma_hat=summary(lm_full)$sigma
X1=model.matrix(lm_full)
H=X1%*%solve(t(X1)%*%X1)%*%t(X1)
h=diag(H)
h
r=residuals/(sigma_hat*sqrt(1-h))

n=nrow(Body_updated)
n
p=25
sum(h)
thresh2=2*p/n
thresh3=3*p/n
plot(h,xlab='Observation #',ylab='Leverage',main='Leverage')
abline(h=thresh2,lty=2,col="red")
abline(h=thresh3,lty=2,col="blue")

t=r*sqrt((n-p-1)/(n-p-r^2))
plot(t,xlab='Observation #',ylab='Studentized residuals',main='Studentized residuals')

D1=(1/p)*r^2*h/(1-h)
plot(D1,xlab='Observation #',ylab='Cook\'s distance',main='Cook\'s distance')

#observe point at which observations are extreme
which(h>thresh2)
which(abs(r)>abs(2))
which(D1>0.02)

Body_updated<-Body_updated[-474,]
Body_updated<-Body_updated[-222,]
Body_updated<-Body_updated[-221,]

lm_full=lm(weight~biacromialD+biiliacD+bitrochantericD+chest_depth+chestD+elbowD+wristD+kneeD+ankleD+shoulderG+chestG+waistG+navelG+hipG+highG+bicepG+forearmG+kneeG+calfMaxG+ankleMinG+wristMinG+age+height+gender,data=Body_updated)
summary(lm_full)

par(mfrow=c(1,3))

residuals=lm_full$residuals
sigma_hat=summary(lm_full)$sigma
X1=model.matrix(lm_full)
H=X1%*%solve(t(X1)%*%X1)%*%t(X1)
h=diag(H)
h
r=residuals/(sigma_hat*sqrt(1-h))

n=nrow(Body_updated)
n
p=25
sum(h)
thresh2=2*p/n
thresh3=3*p/n
plot(h,xlab='Observation #',ylab='Leverage',main='Leverage')
abline(h=thresh2,lty=2,col="red")
abline(h=thresh3,lty=2,col="blue")

t=r*sqrt((n-p-1)/(n-p-r^2))
plot(t,xlab='Observation #',ylab='Studentized residuals',main='Studentized residuals')

D1=(1/p)*r^2*h/(1-h)
plot(D1,xlab='Observation #',ylab='Cook\'s distance',main='Cook\'s distance')

#backward--------------------------------------------------------------------------------------------------
lm_full=lm(weight~biacromialD+biiliacD+bitrochantericD+chest_depth+chestD+elbowD+wristD+kneeD+ankleD+shoulderG+chestG+waistG+navelG+hipG+highG+bicepG+forearmG+kneeG+calfMaxG+ankleMinG+wristMinG+age+height+gender,data=Body_updated)
summary(lm_full)

lm_back1=update(lm_full,.~.-ankleMinG)
summary(lm_back1)

lm_back2=update(lm_back1,.~.-navelG)
summary(lm_back2)

lm_back3=update(lm_back2,.~.-biacromialD)
summary(lm_back3)

lm_back4=update(lm_back3,.~.-ankleD)
summary(lm_back4)

lm_back5=update(lm_back4,.~.-bitrochantericD)
summary(lm_back5)

lm_back6=update(lm_back5,.~.-wristD)
summary(lm_back6)

lm_back7=update(lm_back6,.~.-wristMinG)
summary(lm_back7)

lm_back8=update(lm_back7,.~.-bicepG)
summary(lm_back8)

lm_back9=update(lm_back8,.~.-elbowD)
summary(lm_back9)

par(mfrow=c(2,2))
plot(lm_back9)

shapiro.test(lm_back9$residuals)

#transformation-----------------------------------------------------------------------------------
lm_update1=lm(sqrt(weight)~biiliacD+chest_depth+chestD+kneeD+shoulderG+chestG+waistG+hipG+highG+forearmG+kneeG+calfMaxG+age+height+gender,data=Body_updated)
par(mfrow=c(2,2))
plot(lm_update1)

shapiro.test(lm_update1$residuals)

par(mfrow=c(1,3))

summary(lm_update1)

#Collinearity-----------------------------------------------------------------------------------
library(car)
vif(lm_update1)

lm_update2=update(lm_update1,.~.-chestG)
vif(lm_update2)

#Reduced&full-----------------------------------------------------------------------------------
summary(lm_update2)

lm_red1=update(lm_update2,.~.-gender-kneeG-biiliacD)
anova(lm_red1,lm_update2)

lm_red2=update(lm_update2,.~.-gender-kneeG)
anova(lm_red2,lm_update2)

lm_red3=update(lm_update2,.~.-gender-biiliacD)
anova(lm_red3,lm_update2)

lm_red4=update(lm_update2,.~.-kneeG-biiliacD)
anova(lm_red4,lm_update2)

#model selection2-----------------------------------------------------------------------------------
#install.packages("leaps")
require(leaps)

subset=regsubsets(weight~biacromialD+biiliacD+bitrochantericD+chest_depth+chestD+elbowD+wristD+kneeD+ankleD+shoulderG+chestG+waistG+navelG+hipG+highG+bicepG+forearmG+kneeG+calfMaxG+ankleMinG+wristMinG+age+height+gender,method="exhaustive",nbest=1,nvmax=26,data=Body_updated)
sum_subset=summary(subset)
sum_subset$which

par(mfrow=c(2,2))

p_select=25
p=2:p_select
RSS_p=sum_subset$rss
totalSS=sum((sqrt(Body_updated$weight)-mean(sqrt(Body_updated$weight)))^2)
R2_p=1-RSS_p/totalSS
R2_p

n=nrow(Body_updated)
R2_adj=1-(RSS_p/(n-p))/(totalSS/(n-1))
R2_adj
#17 predictors

plot(p,R2_adj,xlab="Number of betas",ylab="Adjusted R-squared")

sigma_hat=summary(lm_full)$sigma
C_p=RSS_p/(sigma_hat^2)+2*p-n
C_p
plot(p,C_p,xlab="Number of betas",ylab="Mallow's Cp")
abline(0,1)
#14 predictors

aic_p=n*log(RSS_p/n)+2*p
aic_p
plot(p,aic_p,xlab="Number of betas",ylab="AIC")
#16 predictors

bic_p=n*log(RSS_p/n)+p*log(n)
bic_p
plot(p,bic_p,xlab="Number of betas",ylab="BIC")
#12 predictors

#models details-----------------------------------------------------------------------------------
par(mfrow=c(2,2))

lm_backward=lm(sqrt(weight)~biiliacD+chest_depth+chestD+kneeD+shoulderG+waistG+hipG+highG+forearmG+kneeG+calfMaxG+age+height+gender,data=Body_updated)
summary(lm_backward)

lm_adjR2=lm(weight~biiliacD+chest_depth+chestD+elbowD+kneeD+shoulderG+chestG+waistG+hipG+highG+bicepG+forearmG+kneeG+calfMaxG+age+height+gender,data=Body_updated)
summary(lm_adjR2)

plot(lm_adjR2)

lm_cp=lm(weight~biiliacD+chest_depth+kneeD+shoulderG+chestG+waistG+hipG+highG+forearmG+kneeG+calfMaxG+age+height+gender,data=Body_updated)
summary(lm_cp)

plot(lm_cp)

lm_aic=lm(weight~biiliacD+chest_depth+chestD+elbowD+kneeD+shoulderG+chestG+waistG+hipG+highG+forearmG+kneeG+calfMaxG+age+height+gender,data=Body_updated)
summary(lm_aic)

plot(lm_aic)

lm_bic=lm(weight~chest_depth+kneeD+shoulderG+chestG+waistG+hipG+highG+forearmG+calfMaxG+age+height+gender,data=Body_updated)
summary(lm_bic)

plot(lm_bic)

#fix non-linearity.
lm_adjR2=lm(sqrt(weight)~biiliacD+chest_depth+chestD+elbowD+kneeD+shoulderG+chestG+waistG+hipG+highG+bicepG+forearmG+kneeG+calfMaxG+age+height+gender,data=Body_updated)
summary(lm_adjR2)

plot(lm_adjR2)

lm_cp=lm(sqrt(weight)~biiliacD+chest_depth+kneeD+shoulderG+chestG+waistG+hipG+highG+forearmG+kneeG+calfMaxG+age+height+gender,data=Body_updated)
summary(lm_cp)

plot(lm_cp)

lm_aic=lm(sqrt(weight)~biiliacD+chest_depth+chestD+elbowD+kneeD+shoulderG+chestG+waistG+hipG+highG+forearmG+kneeG+calfMaxG+age+height+gender,data=Body_updated)
summary(lm_aic)

plot(lm_aic)

lm_bic=lm(sqrt(weight)~chest_depth+kneeD+shoulderG+chestG+waistG+hipG+highG+forearmG+calfMaxG+age+height+gender,data=Body_updated)
summary(lm_bic)

plot(lm_bic)

