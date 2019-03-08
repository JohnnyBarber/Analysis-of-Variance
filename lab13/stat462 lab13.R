setwd("//udrive.win.psu.edu/Users/j/q/jql5883/Desktop/math462")
getwd()

bears=read.csv("bears.txt", header=T, sep="")
bears=bears[bears$Obs.No==1,]
head(bears)

attach(bears)
Sex=Sex-1

#1----------------------------------------------------------------------
lm.bear=lm(sqrt(Weight)~1,data=bears)
summary(lm.bear)

pvalue_for1=numeric(6)
names(pvalue_for1)=c("Head.L","Head.W","Neck.G","Length","Chest.G","Sex")
pvalue_for1[1]=summary(update(lm.bear,.~.+Head.L))$coefficients[2,4]
pvalue_for1[2]=summary(update(lm.bear,.~.+Head.W))$coefficients[2,4]
pvalue_for1[3]=summary(update(lm.bear,.~.+Neck.G))$coefficients[2,4]
pvalue_for1[4]=summary(update(lm.bear,.~.+Length))$coefficients[2,4]
pvalue_for1[5]=summary(update(lm.bear,.~.+Chest.G))$coefficients[2,4]
pvalue_for1[6]=summary(update(lm.bear,.~.+Sex))$coefficients[2,4]
pvalue_for1

lm.for1=update(lm.bear,.~.+Chest.G)
summary(lm.for1)

pvalue_for2=numeric(5)
names(pvalue_for2)=c("Head.L","Head.W","Neck.G","Length","Sex")
pvalue_for2[1]=summary(update(lm.for1,.~.+Head.L))$coefficients[3,4]
pvalue_for2[2]=summary(update(lm.for1,.~.+Head.W))$coefficients[3,4]
pvalue_for2[3]=summary(update(lm.for1,.~.+Neck.G))$coefficients[3,4]
pvalue_for2[4]=summary(update(lm.for1,.~.+Length))$coefficients[3,4]
pvalue_for2[5]=summary(update(lm.for1,.~.+Sex))$coefficients[3,4]
pvalue_for2

lm.for2=update(lm.for1,.~.+Neck.G)
summary(lm.for2)

pvalue_for3=numeric(4)
names(pvalue_for3)=c("Head.L","Head.W","Length","Sex")
pvalue_for3[1]=summary(update(lm.for2,.~.+Head.L))$coefficients[4,4]
pvalue_for3[2]=summary(update(lm.for2,.~.+Head.W))$coefficients[4,4]
pvalue_for3[3]=summary(update(lm.for2,.~.+Length))$coefficients[4,4]
pvalue_for3[4]=summary(update(lm.for2,.~.+Sex))$coefficients[4,4]
pvalue_for3

lm.for3=update(lm.for2,.~.+Neck.G)
summary(lm.for3)

pvalue_for4=numeric(3)
names(pvalue_for4)=c("Head.L","Head.W","Sex")
pvalue_for4[1]=summary(update(lm.for3,.~.+Head.L))$coefficients[5,4]
pvalue_for4[2]=summary(update(lm.for3,.~.+Head.W))$coefficients[5,4]
pvalue_for4[3]=summary(update(lm.for3,.~.+Sex))$coefficients[5,4]
pvalue_for4

par(mfrow=c(2,2))
plot(lm.for3)

#2----------------------------------------------------------------------
install.packages("leaps")
require(leaps)
subset=regsubsets(sqrt(Weight)~Head.L+Head.W+Neck.G+Length+Chest.G+Sex,data=bears)
sum_subset=summary(subset)
sum_subset$which

#3----------------------------------------------------------------------
p_full=7
p=2:p_full
RSS_p=sum_subset$rss
totalSS=sum((bears$Weight-mean(bears$Weight))^2)
R2_p=1-RSS_p/totalSS
n=nrow(bears)
R2_adj=1-(RSS_p/(n-p))/(totalSS/(n-1))
R2_adj
plot(p,R2_p,xlab="Number of betas",ylab="R-squared")

#4----------------------------------------------------------------------
lm_full=lm(sqrt(Weight)~Head.L+Head.W+Neck.G+Length+Chest.G+Sex,data=bears)
sigma_hat_full=summary(lm_full)$sigma
C_p=RSS_p/(sigma_hat_full^2)+2*p-n
C_p
plot(p,C_p,xlab="Number of betas",ylab="Mallow's Cp")
abline(0,1)

#-----------------------------------------------------------------------
aic_p=n*log(RSS_p/n)+2*p
aic_p

bic_p=n*log(RSS_p/n)+p*log(n)
bic_p