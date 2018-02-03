################################## 452 - Machine Learning - HW4 ###################################

# Set the working directory
setwd("~/Desktop/Classes/452-Machine Learning/HW4")

# Load the required packages
library(dplyr)
library(ggplot2)
library(freqparcoord)
library(regtools)
library(mfp)

################################## CHAPTER 1 ########################################################
##### Exercise 1 ##### 
# In Section 1.12.1.2, the reader was reminded that the results of a cross-validation are random, 
# due to the random partitioning into training and test sets. 
# Try doing several runs of the linear and k-NN code in that section, comparing results.
# Dataset Partition function definition
xvalpart <- function(data,p) { 
  n <- nrow(data)
  ntrain <- round(p*n)
  trainidxs <- sample(1:n, ntrain, replace=FALSE) 
  list(train=data[trainidxs,], valid=data[-trainidxs,]) 
}
# Linear Model Definition
# arguments:
# data: full data
# ycol: column number of resp var
# predvars: column numbers of predictors
# p: prop. for training set 
# meanabs: if TRUE, the mean absolute prediction error; otherwise, an R list containing pred, real Y
xvallm <- function(data, ycol, predvars, p, meanabs=TRUE) { 
  tmp <- xvalpart(data,p)
  train <- tmp$train
  valid <- tmp$valid
  # fit model to training data
  trainy <- train [,ycol]
  trainpreds <- train [,predvars]
  # using matrix form in lm() call
  trainpreds <- as.matrix(trainpreds)
  lmout <- lm(trainy ~ trainpreds)
  # apply fitted model to validation data; note
  # that %∗% works only on matrices , not data frames
  validpreds <- as.matrix(valid[,predvars]) 
  predy <- cbind(1,validpreds) %*% coef(lmout)
  realy <- valid [ , ycol ]
  if (meanabs) 
    return(mean(abs(predy - realy ))) 
  list(predy = predy, realy = realy)
}

# Applying the code to height/weight/age data
data(mlb)
# Run with 60% of data as training set
xvallm(mlb, 5, c(4,6), 0.6) #Mean absolute pred error is 13.51594, off by 13.5 pounds in prediction
# Run with 70% of data as training set
xvallm(mlb, 5, c(4,6), 0.7) #12.72443
# Run with 90% of data as training set
xvallm(mlb, 5, c(4,6), 0.9) #15.45369
# 70% seems the ideal proportion in this case

# KNN Model Definition
# arguments:
# data: full data
# ycol: column number of resp var
# k: number of nearest neighbors 
# p: prop for training set 
# meanabs: if TRUE, the mean absolute prediction error; otherwise, an R list containing pred, real Y
xvalknn <- function(data, ycol, predvars, k, p, meanabs=TRUE) {
  data <- data[, c(predvars, ycol)] 
  ycol <- length(predvars) + 1
  tmp <- xvalpart(data,p) 
  train <- tmp$train
  valid <- tmp$valid
  valid <- as.matrix(valid)
  xd <- preprocessx(train[,-ycol],k)
  kout <- knnest(train[,ycol],xd,k)
  predy <- predict(kout, valid[,-ycol], TRUE) 
  realy <- valid [, ycol]
  if (meanabs) 
    return(mean(abs(predy - realy ))) 
  list(predy = predy, realy = realy)
}
# Applying the code to height/weight/age data
set.seed(1000)
# Run with 60% of data as training set and k=25
xvalknn(mlb, 5 , c(4,6), 25, 0.6) # Mean absolute pred error is 14.02108, off by 14 pounds in prediction
# Run with 60% of data as training set and k=10
xvalknn(mlb, 5 , c(4,6), 10, 0.6) #14.97266
# Run with 70% of data as training set and k=25
xvalknn(mlb, 5 , c(4,6), 25, 0.7) #13.98518
# Run with 80% of data as training set and k=25
xvalknn(mlb, 5 , c(4,6), 25, 0.8) #14.71665


##### Exercise 2 ##### 
# Extend (1.28) to include interaction terms for age and gender, and age2
# and gender. Run the new model, and find the estimated effect of being
# female, for a 32-year-old person with a Master’s degree.
data(prgeng)
prgeng$age2 <- prgeng$age^2
edu <- prgeng$educ
prgeng$ms <- as.integer(edu == 14) 
prgeng$phd <- as.integer(edu == 16) 
prgeng$fem <- prgeng$sex - 1
tmp <- prgeng[edu >= 13,]
pe <- tmp[,c(1,12,9,13,14,15,8)]
pe <- as.matrix(pe)
lmMod <- lm(wageinc ~ age + age2 + wkswrkd + ms + phd + fem + age:fem + age2:fem, data=prgeng)
summary(lmMod)
# With every other variable kept constant, the effect of being female is to have incomes about $33,996 more than males.
# With every year of increase in age, income will go down by about $2,164
# However, after a certain point in age, income will start increasing by about $23 for females
# Predict the wage
df <- data.frame(age=32, age2=32^2, wkswrkd=52, ms=1, phd=0, fem=1)
predict(lmMod, df, interval = "predict")
# For a 32-year, female with with Master's degree - Income will be $69,086.59


##### Exercise 3 #####
# Consider the bodyfat data mentioned in Section 1.2. 
# Use lm() to form a prediction equation for density from the other variables (skipping the first three), 
# and comment on whether use of indirect methods in this way seems feasible.
data(bodyfat)
bodyfat <- bodyfat[,-c(1:3)]
lmBF <- lm(density ~ ., data = bodyfat)
summary(lmBF)
# The Adj R-square is about 72% which means that the model explains 72% variation in the density which is pretty good.
# However, even indirect methods like these can be expensive in terms of measuring so many variables.
# Also, not all variables have significant p-values, so keep only the ones below 0.05 alpha level and relevent ones.
lmBF2 <- lm(density ~ age + weight+ neck + abdomen + hip + forearm + wrist, data = bodyfat)
summary(lmBF2)
# This model still has 0.72 R-square, but at a lesser cost of measuring fewer variables
# This method seems feasible only if there are fewer variables to collect which are available easily and at a lesser cost


##### Exercise 4 #####
# In Section 1.19.5.2, we gave this intuitive explanation:
# In other words, the national mean height is a weighted average of the state means, 
# with the weight for each state being its proportion of the national population. 
# Replace state by gender in the following.
# (a) Write English prose that relates the overall mean height of people and the gender-specific mean heights.
# Ans : The overall mean height is a weighted average of the gender means,
#       with weight for each gender being its proportion of the overall population.
# (b) Write English prose that relates the overall proportion of people taller than 70 inches to the gender-specific proportions.
# Ans : The overall proportion of people taller than 70 inches is a weighted proportion of gender proportion >70 inches,
#       with weights for each gender being its proportion of the overall population.


################################## CHAPTER 2 ########################################################
##### Exercise 1 ##### 
# Consider the census data in Section 1.16.1.
# (a) Form an approximate 95% confidence interval for β6 in the model (1.28).
lm21a <- lm(wageinc ~ age + age2 + wkswrkd + ms + phd + fem, data=prgeng)
summary(lm21a)
confint(lm21a, "fem", level=0.95)
# 95% confidence interval for β6 is -11484.49 +/- 1.96*705.30 = (-12866.88, -10102.05)
# (b) Form an approximate 95% confidence interval for the gender effect for Master’s degree holders, β6 + β7, in the model (1.28).
lm21b <- lm(wageinc ~ age + age2 + wkswrkd + ms + phd + fem + ms:fem + phd:fem, data=prgeng)
summary(lm21b)
confint(lm21b, "ms:fem", level=0.95) 
# 95% confidence interval for β6 + β7 is (-7544.92, -769.5861)


##### Exercise 2 ##### 
# The full bikeshare dataset spans 3 years’ time. Our analyses here have only used the first year. 
# Extend the analysis in Section 2.8.5 to the full data set, adding dummy variables indicating the second and third year. 
# Form an approximate 95% confidence interval for the difference between the coefficients of these two dummies.
# DATASET HAS ONLY 2 YEARS, so dummy already created as yr = 0 for 2011 and 1 for 2012
shar <- read.csv("day.csv", header=TRUE)
shar$reg <- shar$registered
shar$temp2 <- shar$temp^2
shar$clearday <- as.integer(shar$weathersit == 1)
lmBS <- lm(reg ~ temp + temp2 + workingday + clearday + yr , data = shar)
summary(lmBS)
confint(lmBS, "yr", level = 0.95) # (1604.971, 1827.538)

##### Exercise 3 #####
# Suppose we are studying growth patterns in children, at k particular ages. 
# Denote the height of the ith child in our sample data at age j by Hij, with Hi = (Hi1,...,Hik)′ denoting the data for child i. 
# Suppose the population distribution of each Hi is k-variate normal with mean vector μ and covariance matrix Σ. 
# Say we are interested in successive differences in heights,Dij =Hi,j+1−Hij, j=1,2,...,k−1. DefineDi =(Di1,...,Dik)′. 
# Explain why each Di is (k−1)-variate normal, and derive matrix expressions for the mean vector and covariance matrices.
# Ans - If a set of random numbers (X1, X2, X3,.., Xn) follow normal distribution, then their sum and differences will also
#       follow normal distribution. Given mean, E(X1 + X2 + X3 +...+ Xn) = E(X1) + E(X2) + E(X3) +...+ E(Xn) and
#       variance, V(X1 + X2 + X3 +...+ Xn) = V(X1) + V(X2) + V(X3) +...+ V(Xn). Same is true for difference.
#       It will be k-1 varuiate as the successive difference between k variables will leave you with k-1 variables.
#       mean vector = E(Hi,j+1-Hij), covariance matrix = V(Hi,j+1-Hij)

##### Exercise 4 #####
# In the simulation in Section 2.9.3, it is claimed that ρ2 = 0.50. Confirm this through derivation.
simr2 <- function (n , p , nreps ) { 
  r2s <- vector(length=nreps) 
  for (i in 1:nreps) {
    x <- matrix(rnorm(n*p),ncol=p)
    y <- x %*% rep(1,p) + rnorm(n,sd=sqrt(p)) 
    r2s[i] <- getr2(x,y)
  }
  hist(r2s) 
}
getr2 <- function(x,y) {
  smm <- summary(lm(y ~ x)) 
  smm$r.squared
}
simr2(25,8,1000)
simr2(250,8,1000)
# R-square can be shown to be  biased upward. This is confirmed by the two simulations ran above. From the first simulation, 
# we see that the R-square seems to be more towards the higher side, ~0.7. If we increase n to 250, the simulation creates a
# histogram which shows that the values are now more towards 0.5.

