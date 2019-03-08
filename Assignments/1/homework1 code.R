setwd("//udrive.win.psu.edu/Users/j/q/jql5883/Desktop/math462")
getwd()

#A----------------------------------------------------------------------------------
bodyfat=read.table("BODY_FAT.TXT", header=T, sep="")
head(bodyfat)
bodyfat.recompute=(495/bodyfat$Density)-450
head(round(bodyfat.recompute,digits=1))
bodyfat.recompute>100
bodyfat.recompute<0
bodyfat.recompute[182]=0

bodyfat$SiriBFperc==round(bodyfat.recompute,digits=1)

bodyfat[,2]<-round(bodyfat.recompute,digits=1)
colnames(bodyfat)[colnames(bodyfat)=="SiriBFperc"] <- "bodyfat.percent"
head(bodyfat)

weight.dataset=bodyfat[,4]
height.dataset=bodyfat[,5]
AbdomenC.dataset=bodyfat[,8]

plot(weight.dataset,main="Scatter Plot of Weight")
plot(height.dataset,main="Scatter Plot of Height")
plot(AbdomenC.dataset,main="Scatter Plot of Abdoment Circumference")

bodyfat$Weight > 350
bodyfat$AbdomenC > 140
bodyfat<-bodyfat[-39,]

bodyfat$Height < 40
bodyfat<-bodyfat[-41,]

weight.dataset=bodyfat[,4]
height.dataset=bodyfat[,5]
AbdomenC.dataset=bodyfat[,8]

plot(weight.dataset,main="Scatter Plot of Weight")
plot(height.dataset,main="Scatter Plot of Height")
plot(AbdomenC.dataset,main="Scatter Plot of Abdoment Circumference")

#B--------------------------------------------------------------------------------
summary(bodyfat$bodyfat.percent)
sd(bodyfat$bodyfat.percent)
IQR(bodyfat$bodyfat.percent)
hist(bodyfat$bodyfat.percent)
boxplot(bodyfat$bodyfat.percent,
        main="Boxplot of Bodyfat Percentage")

summary(bodyfat$Weight)
sd(bodyfat$Weight)
IQR(bodyfat$Weight)
hist(bodyfat$Weight)
boxplot(bodyfat$Weight,
        main="Boxplot of Weight")

summary(bodyfat$Height)
sd(bodyfat$Height)
IQR(bodyfat$Height)
hist(bodyfat$Height)
boxplot(bodyfat$Height,
        main="Boxplot of Height")

summary(bodyfat$AbdomenC)
sd(bodyfat$AbdomenC)
IQR(bodyfat$AbdomenC)
hist(bodyfat$AbdomenC)
boxplot(bodyfat$AbdomenC,
        main="Boxplot of Abdomen Circumference")

bodyfat.percent.test=as.matrix(bodyfat$bodyfat.percent)
z.bodyfat.percent=(mean(bodyfat.percent.test)-20)/(sd(bodyfat.percent.test)/sqrt(250))
z.bodyfat.percent
pvalue.bodyfat.percent=1-pnorm(z.bodyfat.percent)
pvalue.bodyfat.percent

weight.test=as.matrix(bodyfat$Weight)
z.weight=(mean(weight.test)-180)/(sd(weight.test)/sqrt(250))
z.weight
pvalue.weight=1-pnorm(z.weight)
pvalue.weight

#C-------------------------------------------------------------------------------
attach(bodyfat)

cor(Weight,bodyfat.percent)
model.weight=lm(bodyfat.percent~Weight)
summary(model.weight)
sigma2.weight=sum((model.weight$residuals)^2)/248
sigma2.weight
beta0.weight=-13.94208
beta1.weight=0.18490 
plot(Weight,bodyfat.percent,
     main="Scatter Plot of Weight With Regression Line")
abline(beta0.weight,beta1.weight,col="red")

cor(Height,bodyfat.percent)
model.height=lm(bodyfat.percent~Height)
summary(model.height)
sigma2.height=sum((model.height$residuals)^2)/248
sigma2.height
beta0.height=29.8863
beta1.height=-0.1551 
plot(Height,bodyfat.percent,
     main="Scatter Plot of Height With Regression Line")
abline(beta0.height,beta1.height,col="red")

cor(AbdomenC,bodyfat.percent)
model.abdomenc=lm(bodyfat.percent~AbdomenC)
summary(model.abdomenc)
sigma2.abdomenc=sum((model.abdomenc$residuals)^2)/248
sigma2.abdomenc
beta0.abdomenc=-42.29941
beta1.abdomenc=0.66407 
plot(AbdomenC,bodyfat.percent,
     main="Scatter Plot of Abdoment Circumference With Regresssion Line")
abline(beta0.abdomenc,beta1.abdomenc,col="red")

#D-------------------------------------------------------------------------------
ratio=Weight/Height
cor(ratio,bodyfat.percent)
model.ratio=lm(bodyfat.percent~ratio)
summary(model.ratio)
sigma2.ratio=sum((model.ratio$residuals)^2)/248
sigma2.ratio
beta0.ratio=-22.746
beta1.ratio=16.499
plot(ratio,bodyfat.percent,
     main="Scatter Plot of Ratio of Weight and Height With Regresssion Line")
abline(beta0.ratio,beta1.ratio,col="red")

#E-------------------------------------------------------------------------------
cor(ratio,AbdomenC)
model.rA=lm(AbdomenC~ratio)
summary(model.rA)
sigma2.rA=sum((model.rA$residuals)^2)/248
sigma2.rA
beta0.rA=23.5862
beta1.rA=27.1620
plot(ratio,AbdomenC,
     main="Plot of Ratio of Weight and Height vs AbdomenCricumference With Regresssion Line")
abline(beta0.rA,beta1.rA,col="red")
