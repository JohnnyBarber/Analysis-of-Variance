setwd("//udrive.win.psu.edu/Users/j/q/jql5883/Desktop/math462")
getwd()

#data----------------------------------------------------------------------------------
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
Abd.q=quantile(bodyfat$AbdomenC,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
Abd.q
lm.bodyfat.A=lm(bodyfat.percent~AbdomenC,data=bodyfat)
summary(lm.bodyfat.A)
beta0.Abd=-38.69119
beta1.Abd=0.62417
yhat.q=beta0.Abd+beta1.Abd*Abd.q
yhat.q
mean(bodyfat$bodyfat.percent)

#B-------------------------------------------------------------------------------------
x0=data.frame(AbdomenC=quantile(bodyfat$AbdomenC,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9)))

CI=predict(lm.bodyfat.A,new=x0,interval='confidence',level=0.95)
CI=data.frame(CI)
CI

plot(bodyfat$bodyfat.percent~bodyfat$AbdomenC,main="Confidence Interval For Quantiles")
abline(lm.bodyfat.A)
i=1
while(i<6)
{
  lines(c(x0[i,1],x0[i,1]),c(CI[i,2],CI[i,3]),lwd=2)
  i=i+1
  }

PI=predict(lm.bodyfat.A,new=x0,interval='prediction',level=0.95)
PI=data.frame(PI)
PI

plot(bodyfat$bodyfat.percent~bodyfat$AbdomenC, main="Prediction Interval For Quantiles")
abline(lm.bodyfat.A)
i=1
while(i<6)
{
  lines(c(x0[i,1],x0[i,1]),c(PI[i,2],PI[i,3]),lwd=2)
  i=i+1
}

#C-------------------------------------------------------------------------------------
lm.bodyfat=lm(bodyfat.percent~AbdomenC+Weight+Height+NeckC+ChestC+HipC+ThighC+KneeC+AnkleC+BicepsC+ForearmC+WristC,data=bodyfat)

plot(lm.bodyfat$residuals~lm.bodyfat$fitted.values, main="Scatterplot of Multi-linear Regression Model")
qqnorm(lm.bodyfat$residuals)
qqline(lm.bodyfat$residuals)
shapiro.test(lm.bodyfat$residuals)

plot(lm.bodyfat.A$residuals~lm.bodyfat.A$fitted.values,main="Scatterplot of Simple Linear Regression Model")
qqnorm(lm.bodyfat.A$residuals)
qqline(lm.bodyfat.A$residuals)
shapiro.test(lm.bodyfat.A$residuals)

