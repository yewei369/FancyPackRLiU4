# FancyPackRLiU4

Linreg RC class

## Description
*linreg* is a pilot RC project to carry out linear regression. Given \code{formula} and data source \code{data}, it will find the best fitted linear regression model.

## Fields
*formula*, a suggested linear regression model
*data*, a data source used to train the model
*guo*, a list of key results from regression, including Coefficients, FittedValues, Residuals, FreedomDegree, VarianceResiduals, StdErrorResiduals, VarianceCoefficients, StdError, Tcoefficients, Possibility, X, Y, Formula, Data, VariableNames.

## Methods
print() print out the coefficients and coefficient names, similar as done by the lm class
plot() plot the following two plots using ggplot2. Remember to include ggplot2 in your package
resid() return the vector of residuals
pred() return the predicted values
coef() return the coefficients as a named vector
summary() return a printout including the coefficients with their standard error, t-value and p-value as well as the standard error of  residuals and the degrees of freedom in the model.



Pass status of Travis building:
https://travis-ci.org/yewei369/FancyPack.R.LiU.3.svg?branch=master

name<-"Jun Li"
liuid<-"junli559"