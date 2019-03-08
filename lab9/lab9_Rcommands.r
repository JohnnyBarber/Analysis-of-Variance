# Import data in R
nutrient=read.table('nutrient.txt',header=TRUE,sep='\t')
# IMPORTANT: CHECK sep in the txt file!!!


# linear model for y=calcium, x1=protein, x2=iron
lm_fit=lm(calcium~protein+iron,data=nutrient)
summary(lm_fit)

# obtain the residuals
res=as.numeric(lm_fit$residuals)

# obtain the fitted values
y_hat=lm_fit$fitted.values

# obtain the model matrix
X=model.matrix(calcium~protein+iron,data=nutrient)

# inverse of X_transpose*X
solve(t(X)%*%X)

# plot res versus observastion #
plot(res)

# retrieve the number of observations with res>1500
which(res>1500)

# add red points to the plot, corresponding to the observation with res>1500
points(which(res>1500),res[which(res>1500)],col='red')
