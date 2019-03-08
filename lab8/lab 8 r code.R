setwd("//udrive.win.psu.edu/Users/j/q/jql5883/Desktop/math462")
getwd()

bears=read.csv("bears.txt", header=T, sep="")
bears=bears[bears$Obs.No==1,]
head(bears)

attach(bears)

lm.bears=lm(Weight~Length)

plot(lm.bears$fitted.values,as.numeric(lm.bears$residuals),main="Residuals vs Fitted values")

lm.bears.transform=lm(log(Weight)~Length)

plot(lm.bears.transform$fitted.values,as.numeric(lm.bears.transform$residuals),main="Residuals vs Log of Fitted values")

qqnorm(as.numeric(lm.bears.transform$residuals))
qqline(as.numeric(lm.bears.transform$residuals))
shapiro.test(as.numeric(lm.bears.transform$residuals))
