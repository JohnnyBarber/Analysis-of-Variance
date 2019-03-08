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

bodyfat$Weight > 350
bodyfat$AbdomenC > 140
bodyfat<-bodyfat[-39,]

bodyfat$Height < 40
bodyfat<-bodyfat[-41,]

weight.dataset=bodyfat[,4]
height.dataset=bodyfat[,5]
AbdomenC.dataset=bodyfat[,8]

#A---------------------------------------------------------------------------------------
lm.bodyfat.W=lm(bodyfat.percent~Weight,data=bodyfat)
summary(lm.bodyfat.W)
confint(lm.bodyfat.W, level=0.99)

lm.bodyfat.H=lm(bodyfat.percent~Height,data=bodyfat)
summary(lm.bodyfat.H)
confint(lm.bodyfat.H, level=0.99)

lm.bodyfat.A=lm(bodyfat.percent~AbdomenC,data=bodyfat)
summary(lm.bodyfat.A)
confint(lm.bodyfat.A, level=0.99)

#B--------------------------------------------------------------------------------------

lm.bodyfat=lm(bodyfat.percent~AbdomenC+Weight+Height+NeckC+ChestC+HipC+ThighC+KneeC+AnkleC+BicepsC+ForearmC+WristC,data=bodyfat)
summary.full=summary(lm.bodyfat)
summary.full$coefficients

T=(summary.full$coefficients[2,1]-0.5)/summary.full$coefficients[2,2]
T

n=nrow(bodyfat)
n

p=length(lm.bodyfat$coefficients)
p

pvalue=pt(T,df=n-p)
pvalue

#C--------------------------------------------------------------------------------------
attach(bodyfat)
plot(lm.bodyfat.A$residuals~Weight)
plot(lm.bodyfat.A$residuals~Height)
plot(lm.bodyfat.A$residuals~NeckC)
plot(lm.bodyfat.A$residuals~ChestC)
plot(lm.bodyfat.A$residuals~HipC)
plot(lm.bodyfat.A$residuals~ThighC)
plot(lm.bodyfat.A$residuals~KneeC)
plot(lm.bodyfat.A$residuals~AnkleC)
plot(lm.bodyfat.A$residuals~BicepsC)
plot(lm.bodyfat.A$residuals~ForearmC)
plot(lm.bodyfat.A$residuals~WristC)

#A--------------------------------------------------------------------------------------
lm.bodyfat=lm(bodyfat.percent~AbdomenC+Weight+Height+NeckC+ChestC+HipC+ThighC+KneeC+AnkleC+BicepsC+ForearmC+WristC,data=bodyfat)
summary(lm.bodyfat)

