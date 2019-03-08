#################################################
##    STAT462 - APPLIED REGRESSION ANALYSIS    ##
##               INTRODUCTION TO R             ##
#################################################



# R is a free software for statistical computing
# To download R: http://www.r-project.org
# To download RStudio (nice interface): https://www.rstudio.com
# To have more information on R look at the "help" menu of R


# In R you can:
# - Write commands directly in the console
# - Write commands in a script, save it as .r and then run it (better choice)
#   To execute the selected code: Ctrl+R (Command+Enter on Mac systems) 


# NOTE: Commands written after the symbol '#' are not executed, 
#       and can be used as comments in the script


# NOTE: R needs a WORKING DIRECTORY
#       To set or change it:
#       - After selecting R Console window, click on 'File' and then 'Change dir...'
#       - Use the command 'setwd'
setwd('D:/STAT462/Labs/Lab1')

# To get the current working directory
getwd()





###################
# R as calculator #
###################

# We can use R to compute simple aritmetic expressions using standard mathematical functions
pi+exp(1)
pi/sqrt(log(10))
sin(3^-1)




#############
# R objects #
#############

# We create R symbolic variables assigning them values
# Assignment operator: '<-' (equivalent to '=')

### Scalars ###
a<-1
a

### Vectors ###
# To create vectors we use the functions 'c', ':', 'seq' or 'rep'

# 'c' is used to concatenate values
v1=c(2,4,7,10)
v1

letters=c('a','b','c','d')
letters

v2=1:5
v2

v3=seq(0,5,by=0.5)
v3

v4=seq(0,5,length.out=3)
v4

v5=rep(1,10)
v5

# If we need information about a particuar function we can use the command '?'
?rep

# What if we don't remember the name of the function? Use the command '??'
??sequence


### Matrices ###
M1=matrix(data=1:12,nrow=4,ncol=3)
M1

M2=matrix(data=1:12,nrow=4,ncol=3,byrow=TRUE)
M2

M3=cbind(c(2,4,8),c(3,6,12))
M3

M4=rbind(1:3,4:6)
M4

# NOTE: in R vectors are NOT matrices of dimensions n*1 or 1*n
v2=as.matrix(v2)
v2


### Extract elements from vectors or matrices ###
v1
length(v1) 

v1[2]
v1[1:3]
v1[c(1,3)]
v1[-1]  

M1
dim(M1) 

M1[1,3]
M1[2:4,1]
M1[4,c(1,3)]

M1[3,]
M1[,2]

M1[c(1,3,4),2:3]




#############################
# ALGEBRAIC OPERATIONS IN R #
#############################
# NOTE: The default in R is to perform operation componentwise
a
v1
a+v1

M1
M2
M1*M2

### Useful functions ###
sum(v1) # Sum of the vector components
mean(v1) # Mean of the vector components
var(v1) # Variance
quantile(v1) # quantiles

M3
sum(M3) # Sum of the matrix components
mean(M3) # Mean of the matrix components
colMeans(M3) # Mean column by column of the matrix
rowMeans(M3) # Mean row by row of the matrix
var(M3) # Covariance matrix (treats rows as subjects, cols as variables)




###################
# OTHER R OBJECTS #
###################

### Boolean variables ###
v3
v3>3
sum(v3>3)


### Dataframes ###
# R objects that contain different variables
# They are similar to matrices but with mixed types of data
# Columns represent the variables under study and the rows are the different observations
exam=data.frame(id=as.character(c(45020,45679,46789,43126,42345,47568,45674)),
                written=c(100,91,92,NA,85,86,80), 
                project=c(95,98,76,89,100,89,84)
)
exam

# To access a specific column (variable) of a dataframe we can use the symbol '$'
exam$written

# To look at variable names
names(exam)


### Import data (as a dataframe) ###
# We can use the command 'read.table' for txt files or 'read.csv' for csv files
# NOTE: check before if variable names are in the file, and the field separator character!
### Nutrient Multivariate Dataset
# In 1985, the USDA commissioned a study of women's nutrition.
# Nutrient intake was measured for a random sample of 737 women 
# aged 25-50 years. Five nutritional components were
# measured: calcium, iron, protein, vitamin A and vitamin C.
nutrient=read.table("nutrient.txt",header=TRUE,sep='\t')

# Look at first 6 observations
head(nutrient)

# Compute summary statistics
summary(nutrient)




############
# GRAPHICS #
############
# Scatterplot
plot(nutrient$calcium,nutrient$iron)

# Scatterplot of all variables (excluding ids)
plot(nutrient[,-1])

# Boxplot of vitamin C
boxplot(nutrient$vitaminC)

# Histogram of vitamin C
hist(nutrient$vitaminC)




############
# SIMULATE #
############
?rnorm # random generation for normal distribution
??normal # looks for any function mentioning normals
x=rnorm(n=100,mean=0,sd=1) # specify sample size, mean, and standard deviation
length(x)
hist(x,prob=TRUE)

?qnorm # quantile function for normal distribution
qnorm(0.975)
?qt # quantile function for t distribution
qt(0.975,100)




############
# PACKAGES #
############
# You can install and use a lot of packages (see CRAN, bioconductor...)
# To install the package "faraway", that contains all dataset used in the book
install.packages("faraway") 

# To load the package
require("faraway")

# Load dataset pima
data(pima)




########
# SAVE #
########
# To save dataframes, vectors, matrices... in a text file
write.table(M3,'my_matrix.txt')

# To save R objects in a .RData file
save(v1,M3,file='my_variables.RData')

# NOTE: R saves in the current working directory!

# To load a saved .RData
load('my_variables.RData')




############################
### Manage the workspace ###
############################
# To get all the variables in the workspace
ls()

# If we want to remove all variables...
rm(list=ls())
ls()
