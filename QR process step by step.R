
library(iris)
QR <- function(X,y){
  
  y <- iris$Sepal.Length
  

  X <- as.matrix(iris[-ncol(iris)])
  

  I <- rep(1, length(y))
  
  
  X <- cbind(I, X)
 
#Regression Coefficient 
  betas <- solve(t(X) %*% X) %*% t(X) %*% y
  betas <- round(betas, 2)
  
#The fitted value
  Yhat <- X %*% betas
  
#The residuals  
  e <- as.vector(y - X %*% betas)
  
#degrees of freedom 
  n <- length(y)
  p <- (length(all.vars(formula))-1)
  df <- n - p
  
#Residual variance 
 R = t(e)%*%(e/df)
 
# The variance of the regression coefficients:
 varbeta <- rep(R,length(betas))*diag(solve(t(X)%*%X))
 
#The t-values for each coefficient:
 
 t_value <- betas/(sqrt(varbeta))
  
  
  
  return(list(betas = betas, e= e, R2 = R2, fittedV = Yhat, DegreesFreedom = df,ResidualVarinace= R, TheVarianceOfTheregressionCoefficients = varbeta , t_value = t_value ))
}

QR(X,y)

#instead of QR we multiply by the identity matrix 

