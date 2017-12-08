library(np)
library(tidyverse)
library(MASS)
library(tidyr)
library(dplyr)
library(caret)
library(readxl)
install.packages("randomForest")
library(randomForest)
# load all the packages needed in the the beginning

train <- read.csv("train.csv")
test <- read.csv("test.csv")
# input the training data and the test data

train <- train[-c(1,7,31,32,33,34,36,58,59,61,64,65,73,74,75)]
train
test <- tbl_df(test[-c(1,7,31,32,33,34,36,58,59,61,64,65,73,74,75)])
test
train <- train[complete.cases(train),]
train
sum(is.na(train))
train
test <- test[complete.cases(test),]
test
# From Challenge A, we found that while NA in some variables indeed means "missing value".
# While some NA in other variables has practical meanings. 
# We removed all the NAs from numeric variables and two character varialbes “Electrical” and “MasVnrType”, where NA has no practical meanings.

train1 <- tbl_df(train[-c(66)])
names(train1)
fmla <- as.formula(paste("SalePrice ~", paste(names(train1), collapse = "+")))
fmla
# train1 is not the training data. It is just used to create the formula in a convenient way by removing the dependent variable "SalePrice".


# A  non-parametric statistical method makes no assumption on the population distribution or sample size. 
# Generally people make as few assumptions about the regression function f as possible and as much data as possible to learn about the potential shape of f to make f very flexible yet smooth. 
# We choose Local Linear Method in the np package because it achieves such flexibility by fitting a different, simple model separately at every point. 
# Unlike kernel regression, locally linear estimation would have no bias if the true model were linear. In general, locally linear estimation removes a bias term from the kernel estimator, that makes it have better behavior near the boundary of the x’s and smaller MSE everywhere.
model <- randomForest(fmla, data = train)
summary(model)
print(model)
common <- intersect(names(train), names(test)) 
for (p in common) { 
  if (class(train[[p]]) == "factor") { 
    levels(test[[p]]) <- levels(train[[p]]) 
  } 
}
predict(model, common)

lm_tra <- lm(SalePrice ~ LotFrontage +OverallQual 
             +RoofMatl+ MasVnrArea+   BsmtFinSF1   
             + BsmtFinSF2 + BsmtUnfSF+ X1stFlrSF +X2ndFlrSF +KitchenQual,data=train)
summary(lm_tra)
