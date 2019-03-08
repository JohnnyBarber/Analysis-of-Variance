# Import data in R
nutrient=read.table('nutrient.txt',header=TRUE,sep='\t')
# IMPORTANT: CHECK sep in the txt file!!!


# linear model for y=protein, x1=calcium, x2=iron, x3=vitaminA, x4=vitaminC
lm_fit=lm(protein~calcium+iron+vitaminA+vitaminC,data=nutrient)
summary(lm_fit)

# correlation among pairs of variables
cor(nutrient[,c(2,4:6)])

# obtain the model matrix
X=model.matrix(calcium~protein+iron+vitaminA+vitaminC,data=nutrient)

# select predictor j in the model matrix
X[,1+j]

# select all predictors but predictor j and intercept in the model matrix
X[,-c(1,1+j)]

# create a vector of length 4
vector("numeric",4)
