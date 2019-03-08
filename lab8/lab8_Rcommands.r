# Import data in R
nutrient=read.table('nutrient.txt',header=TRUE,sep='\t')
# IMPORTANT: CHECK sep in the txt file!!!


attach(nutrient)

# linear model for y=calcium, x=protein
lm_fit=lm(calcium~protein)
summary(lm_fit)

# obtain the residuals
as.numeric(lm_fit$residuals)

# obtain the fitted values
lm_fit$fitted.values

# qq-plot of residuals
qqnorm(as.numeric(lm_fit$residuals))

# transform the response with a log
lm_fit=lm(I(log(calcium))~protein)
summary(lm_fit)


detach(nutrient)

