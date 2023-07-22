library(dplyr)
library(readxl)
library(skimr)
library(tidyverse)

path =readline('Enter file path: ')
# we check the extension of the file to open it with the suitable function
if(grepl(".csv", path)){
  data = read.csv(path)%>%
    drop_na()%>%
    distinct()
  head(data)
}else if(grepl(".xls", path)){
  data=read_excel(path)%>%
    drop_na()%>%
    distinct()
  head(data)
}

sum(duplicated(data))
sum(is.na(data))
skim(data)

if(ncol(data) == 2){
  
  predictY = function(x){
    return(Bo + B1 * x)
  }
  
  x = data[,1]
  y = data[,2]
  
  plot(x = x, y = y)
  
  regression_data = data.frame(x=data[,1],y=data[,2])
  head(regression_data)
  
  summation_data <- data.frame(
    x = regression_data[,1],
    y = regression_data[,2],
    x_squared = regression_data[,1]^2,
    xy = regression_data[,1]*regression_data[,2],
    y_squared = regression_data[,2]^2
  )
  head(summation_data)
  
  n = length(regression_data$x)
  
  sumX = sum(summation_data$x)
  
  xbar = sumX/n
  
  sumY = sum(summation_data$y)
  
  ybar = sumY/n
  
  sumXX = sum(summation_data$x_squared)
  
  sumXY = sum(summation_data$xy)
  
  sxy = sumXY - (n * xbar * ybar)
  
  sxx = sumXX - (n * xbar ^ 2)
  
  B1 = sxy / sxx
  print(paste0('B1: ', B1))
  
  Bo = ybar - B1 * xbar
  print(paste0('Bo: ', Bo))
  
  x=regression_data$x
  y=regression_data$y
  plot(x, y) + abline(Bo, B1, col = 'red')
  
  
  #anova table 
  SST = sum(summation_data$y_squared) - (n * ybar ^ 2)
  SSR = B1 ^ 2 * sxx
  
  SSE = SST - SSR
  
  # k = degree of freedom for SSR = number of independent variables(X)
  k = ncol(regression_data) - 1
  k
  
  SSR_d_o_f = k
  print(paste0('SSR D.O.F: ', SSR_d_o_f))
  
  # p = number of parameters 
  p=(k + 1)
  
  # degree of freedom of SSE = n-p
  SSE_d_o_f = n - p
  print(paste0('SSE D.O.F: ', SSE_d_o_f))
  
  # total degree of freedom = n-p+k
  total_d_o_f=(n - p) + k
  print(paste0('SST D.O.F: ', total_d_o_f))
  
  MSR = SSR / SSR_d_o_f
  print(paste0('MSR: ',MSR))
  
  MSE = SSE / SSE_d_o_f
  print(paste0('MSE: ',MSE))
  
  Fc = MSR / MSE
  print(paste0('Fc: ',Fc))
  
  RSquared = SSR / SST
  print(paste0('R²: ',RSquared))
  
  alpha = as.numeric(readline('Enter the level of significance for the tests: '))
  
  Tc = abs(qt(alpha/2,n-p))
  
  Zc = abs(qnorm(alpha/2))
  
  LowerB1 = B1 - Tc * sqrt(MSE / sxx)
  
  UpperB1 = B1 + Tc * sqrt(MSE / sxx)
  
  LowerBo = Bo - Tc * sqrt(MSE * (1 / n + xbar ^ 2 / sxx))
  
  UpperBo = Bo + Tc * sqrt(MSE * (1 / n + xbar ^ 2 / sxx))
  
  print(paste0('p(',LowerB1,' <= B1 <= ',UpperB1,') = ',1-alpha))
  print(paste0('p(',LowerBo,' <= Bo <= ',UpperBo,') = ',1-alpha))
  
  xnew = as.numeric(readline('Enter the new X: '))
  
  ynew = predictY(xnew)
  ynew
  
  
  LowerMeanResponse = ynew - Tc * sqrt(MSE * (1/n + (xnew - xbar) ^ 2 / sxx))
  
  UpperMeanResponse = ynew + Tc * sqrt(MSE * (1/n + (xnew - xbar) ^ 2 / sxx))
  
  LowerNewObservation = ynew - Tc * sqrt(MSE * (1 + 1/n + (xnew - xbar) ^ 2 / sxx))
  
  UpperNewObservation = ynew + Tc * sqrt(MSE * (1 + 1/n + (xnew - xbar) ^ 2 / sxx))
  
  
  print(paste0('p(', LowerMeanResponse, ' <= MeanResponse <= ', UpperMeanResponse, ') = ', 1 - alpha))
  print(paste0('p(', LowerNewObservation, ' <= NewObservation <= ', UpperNewObservation,') = ', 1 - alpha))
  
  lm = lm(y ~ x, data = regression_data)
  
  print(summary(lm))
  
  print(predict(lm, data.frame(x = xnew), interval = 'confidence'))
  print(predict(lm, data.frame(x = xnew), interval = 'predict'))
  confint(lm, 'x')
  
}else{
  
  predictY = function(x,B){
    return(x%*%B)
  }
  
  x_length=length(data)-1
  x_length
  
  y_index=ncol(data)
  y_index
  
  n = length(data[,1])
  
  B0_coff=rep(1,n)
  B0_coff
  
  independent_variables=data[,0:x_length]
  independent_variables
  
  design_matrix=cbind(B0_coff,independent_variables)
  head(design_matrix)
  
  y=data[,y_index]
  y=data.matrix(y)
  head(y)
  y_transpose=t(y)
  head(y_transpose)
  x=data.matrix(design_matrix)
  x_transpose=t(design_matrix)
  x
  xT_times_x=x_transpose%*%x
  x_transpose
  
  
  xT_times_x
  
  C=solve(xT_times_x)
  xT_times_x
  C
  
  xT_times_y=x_transpose%*%y
  
  B=C%*%xT_times_y
  print(B)
  B_transpose=t(B)
  
  y_bar=mean(y)
  
  #anova table 
  
  SSE=y_transpose%*%y-B_transpose%*%x_transpose%*%y
  SSE
  
  SST=y_transpose%*%y-n*y_bar^2
  SST
  
  SSR=SST-SSE
  SSR
  
  # k = degree of freedom for SSR = number of independent variables(X)
  k =x_length
  k
  
  SSR_d_o_f = k
  print(paste0('SSR D.O.F: ', SSR_d_o_f))
  
  # p = number of parameters 
  p=(k + 1)
  
  # degree of freedom of SSE = n-p
  SSE_d_o_f = n - p
  print(paste0('SSE D.O.F: ', SSE_d_o_f))
  
  # total degree of freedom = n-p+k
  total_d_o_f=(n - p) + k
  print(paste0('SST D.O.F: ', total_d_o_f))
  
  MSR = SSR / SSR_d_o_f
  print(paste0('MSR: ',MSR))
  
  MSE = SSE / SSE_d_o_f
  print(paste0('MSE: ',MSE))
  
  Fc = MSR / MSE
  print(paste0('Fc: ',Fc))
  
  RSquared = SSR / SST
  print(paste0('R²: ',RSquared))
  
  AdjRSquared <- 1 - (SSE/(n-p))/(SST/(n-1))
  print(paste0('Adjusted R²: ',AdjRSquared))
  
  
  # The model check using the scaled residuals di
  
  x_altered <- x[,2:ncol(x)]
  
  x_altered
  
  x_means <- c()
  SXXs <- c()
  
  # to calculate the sxx of all Xs and means of them
  for (i in 1:x_length){
    x_means[i] <- mean(data[,i])
    SXXs[i] <- sum(data[,i]**2)-((x_means[i]**2)*length(data[,i]))
  }
  x_means
  SXXs
  
  # to calculate the collinearity matrix
  x__collinearity <- matrix(NA, nrow(data), ncol(data)-1)
  for (i in 1:nrow(data)) {
    for (j in 1:(ncol(data)-1)){
      x__collinearity[i, j] = (data[i, j] - x_means[j]) / sqrt(SXXs[j])
    }
  }
  x__collinearity
  x__collinearity_T = t(x__collinearity)
  x__collinearity_T
  x__collinearity_FULL = x__collinearity_T %*% x__collinearity
  x__collinearity_FULL
  
  # the automatic correlation matrix
  x__collinearity_AUTO = cor(x[,2:ncol(x)])
  x__collinearity_AUTO
  # the automatic correlation matrix
  
  y_predicted = x%*%B
  errors = y - y_predicted
  head(errors)
  Di_matrix <- matrix(NA, nrow(errors),1)
  for (i in 1:nrow(errors)){
    Di_matrix[i,1] = errors[i,1]/sqrt(MSE)
  }
  head(Di_matrix)
  plot(Di_matrix[,1])
  
  # End of model check using the scaled residuals di
  
  #confidence interval for Bj
  
  j = as.numeric(readline('Enter the index of B you want to calc the confidence interval for it:  '))
  
  alpha = as.numeric(readline('Enter the level of significance for the tests: '))
  
  Tc = abs(qt(1-alpha/2,n-p))
  Tc
  
  lowerBj=B[j+1]-Tc*sqrt(MSE*C[j+1,j+1])
  upperBj=B[j+1]+Tc*sqrt(MSE*C[j+1,j+1])
  
  print(paste0('p(',lowerBj,' <= B',j ,'<= ',upperBj,') = ',1-alpha))
  
  xnew=c(1)
  
  for(i in 1:x_length){
    
    print(paste0("enter the value of x[",i,"]"))
    value= as.numeric(readline())
    xnew <- c(xnew,value)
  }
  xnew=data.matrix(xnew)
  xnew
  xnew_transpose=t(xnew)
  xnew_transpose
  ynew = predictY(xnew_transpose,B)
  ynew
  
  LowerMeanResponse = ynew - Tc * sqrt(MSE * xnew_transpose %*% C %*% xnew)
  
  UpperMeanResponse = ynew + Tc * sqrt(MSE * xnew_transpose %*% C %*% xnew)
  
  LowerNewObservation = ynew - Tc * sqrt(MSE * (1 + xnew_transpose %*% C %*% xnew))
  
  UpperNewObservation = ynew + Tc * sqrt(MSE * (1 + xnew_transpose %*% C %*% xnew))
  
  print(paste0('p(', LowerMeanResponse, ' <= MeanResponse <= ', UpperMeanResponse, ') = ', 1 - alpha))
  print(paste0('p(', LowerNewObservation, ' <= NewObservation <= ', UpperNewObservation,') = ', 1 - alpha))
  
  lm = lm(Sales ~ TV + Radio + Newspaper,data = data)
  print(summary(lm))
  
  
}

