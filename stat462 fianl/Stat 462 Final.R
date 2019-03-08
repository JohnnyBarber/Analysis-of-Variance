#Read in the dataset and give each columns name------
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


#Exploratory Data Analysis---------

#inspect unusual observations
attach(Body_updated)

lm_full=lm(weight~biacromialD+chest_depth+chestD+waistG+height,data=Body_updated)
summary(lm_full)
summary(Body_updated[,c(23,1,4,5,12,24)])
sd(Body_updated[,23])
sd(Body_updated[,1])
sd(Body_updated[,4])
sd(Body_updated[,5])
sd(Body_updated[,12])
sd(Body_updated[,24])

par(mfrow=c(2,2))
plot(lm_full)

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
p=5
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

cor(Body_updated[,c(23,c(1,4,5,12,24))])

par(mfrow=c(1,1))
plot(Body_updated[,c(1,4,5,12,24)])

library(car)
vif(lm_full)

#-----------------------------------

#observe point at which observations are extreme

which(h>0.03)
which(abs(r)>4)
which(D1>0.06)

#install.packages("leaps")
#require(leaps)

#subset=regsubsets(height~.-age-weight-gender,method="exhaustive",nbest=1,data=Body_updated)
#sum_subset=summary(subset)

#p_full=22
#p=2:p_full
#RSS_p=sum_subset$rss
#totalSS=sum((Body_updated$weight-mean(Body_updated$weight))^2)
#R2_p=1-RSS_p/totalSS
#R2_p

#step(lm(height~.-age-weight-gender,data=Body_updated),k=2)

Body_updated<-Body_updated[-474,]
Body_updated<-Body_updated[-124,]

lm_update=lm(weight~biacromialD+chest_depth+chestD+waistG+height,data=Body_updated)
par(mfrow=c(2,2))
plot(lm_update)

par(mfrow=c(1,3))

residuals_2=lm_update$residuals
sigma_hat_2=summary(lm_update)$sigma
X1_2=model.matrix(lm_update)
H_2=X1_2%*%solve(t(X1_2)%*%X1_2)%*%t(X1_2)
h_2=diag(H_2)
h_2
r_2=residuals_2/(sigma_hat_2*sqrt(1-h_2))

n_2=nrow(Body_updated)
n_2
p=5
sum(h_2)
thresh2=2*p/n_2
thresh3=3*p/n_2
plot(h_2,xlab='Observation #',ylab='Leverage',main='Leverage')
abline(h=thresh2,lty=2,col="red")
abline(h=thresh3,lty=2,col="blue")

t_2=r_2*sqrt((n_2-p-1)/(n_2-p-r_2^2))
plot(t_2,xlab='Observation #',ylab='Studentized residuals',main='Studentized residuals')

D1_2=(1/p)*r_2^2*h_2/(1-h_2)
plot(D1_2,xlab='Observation #',ylab='Cook\'s distance',main='Cook\'s distance')

summary(lm_update)

